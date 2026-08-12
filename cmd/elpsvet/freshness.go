// Copyright © 2026 The ELPS authors

// The elpsfreshness analyzer is the second elpsvet rule: a compile-time
// "freshness" check for LVal field writes.  It is the vet-spelled version of
// a const type — it polices IN-PACKAGE code, which Go visibility cannot
// (both historical corruption bugs, #333 and #334, were in-package writes to
// values the writer did not construct).
//
// Rule: an assignment whose LHS is a field of lisp.LVal (or an element of
// .Cells, the .Bytes() backing array, or a whole-value write through a
// *lisp.LVal) is flagged UNLESS one of:
//
//   - the receiver chain roots at a value constructed in the same function:
//     a &LVal{...} composite literal, new(LVal), a call to a known fresh
//     constructor (lisp.Int, SExpr, Errorf, ... — NOT Nil() or Bool(), which
//     return shared singletons), or a (*LVal).Copy/detach call; freshness is
//     tracked through simple := and = assignments of tracked values;
//
//   - the receiver roots at a fresh pointer used for the Copy/detach struct
//     copy idiom (cp := &LVal{}; *cp = *v; cp.Quoted = ... is fresh — the
//     write lands in memory this function allocated);
//
//   - the enclosing function's doc comment, or a comment on the assignment's
//     line (or the line directly above it), carries //elps:mutates — the
//     audited annotation that the function deliberately mutates a value it
//     does not own.
//
// The analysis is intraprocedural and conservative: an unknown root flags.
// A value-typed LVal variable is always fresh at its top level (Go value
// semantics give the function its own struct), so `v2 := *shared;
// v2.Quoted = false` — the shallowUnquote idiom — passes.
//
// The rule also tracks local slice aliases of LVal backing storage
// (cells := seqCells(list); cells[i] = x) and flags the mutating operations
// on them — the #369 laundering gap.  See alias.go for the taint design.
package main

import (
	"go/ast"
	"go/token"
	"go/types"
	"strings"

	"golang.org/x/tools/go/analysis"
	"golang.org/x/tools/go/types/typeutil"
)

var freshnessAnalyzer = &analysis.Analyzer{
	Name: "elpsfreshness",
	Doc:  "flag writes to lisp.LVal fields on values the writing function did not construct (issues #333/#334's corruption pattern), and mutations laundered through local slice aliases of LVal backing storage (issue #369's laundering gap); suppress with //elps:mutates",
	Run:  runFreshness,
}

// freshConstructors are package-level functions in the lisp package whose
// result is a freshly allocated LVal the caller owns.  Bool and Nil are
// deliberately ABSENT: they return shared singletons (lisp/singleton.go) and
// writing a field on their result is precisely the corruption this rule
// exists to catch.
var freshConstructors = map[string]bool{
	// Exported constructors (lisp/lisp.go).
	"Int": true, "Float": true, "String": true, "Bytes": true,
	"Symbol": true, "QSymbol": true, "Native": true, "Value": true,
	"SExpr": true, "QExpr": true, "Vector": true, "MakeVector": true,
	"Array": true, "SortedMap": true, "SortedMapFromData": true,
	"FunRef": true, "Fun": true, "FunInPackage": true,
	"Macro": true, "MacroInPackage": true,
	"SpecialOp": true, "SpecialOpInPackage": true,
	"Error": true, "ErrorCondition": true,
	"Errorf": true, "ErrorConditionf": true,
	"Quote": true, "Splice": true, "Formals": true,
	"GetType": true, "SplitSymbol": true,
	// In-package lowercase constructors that return fresh allocations.
	"markTailRec": true, "markMacExpand": true, "shallowUnquote": true,
	"makeByteSeq": true,
}

// freshLValMethods are methods on *lisp.LVal whose result is a fresh LVal.
var freshLValMethods = map[string]bool{
	"Copy": true,
	// detach is unexported (lisp/detach.go) so only package lisp itself can
	// call it; its result is a fresh hermetic copy all the same.
	"detach": true,
}

// freshLEnvMethods are methods on *lisp.LEnv whose result is a freshly
// allocated error LVal (lisp/env.go builds a new &LVal{} in each).
var freshLEnvMethods = map[string]bool{
	"Error": true, "Errorf": true,
	"ErrorCondition": true, "ErrorConditionf": true,
}

const mutatesMarker = "elps:mutates"

func runFreshness(pass *analysis.Pass) (interface{}, error) {
	for _, file := range pass.Files {
		ann := mutatesLines(pass.Fset, file)
		for _, decl := range file.Decls {
			switch d := decl.(type) {
			case *ast.FuncDecl:
				if d.Body == nil || hasMutates(d.Doc) {
					continue
				}
				checkFreshness(pass, d.Body, ann)
			case *ast.GenDecl:
				// Function literals in package-level var initializers.
				ast.Inspect(d, func(n ast.Node) bool {
					if lit, ok := n.(*ast.FuncLit); ok {
						checkFreshness(pass, lit.Body, ann)
						return false
					}
					return true
				})
			}
		}
	}
	return nil, nil
}

// mutatesLines collects the file lines carrying an //elps:mutates comment.
func mutatesLines(fset *token.FileSet, file *ast.File) map[int]bool {
	lines := make(map[int]bool)
	for _, cg := range file.Comments {
		for _, c := range cg.List {
			if commentIsMutates(c.Text) {
				lines[fset.Position(c.Pos()).Line] = true
			}
		}
	}
	return lines
}

func hasMutates(cg *ast.CommentGroup) bool {
	if cg == nil {
		return false
	}
	for _, c := range cg.List {
		if commentIsMutates(c.Text) {
			return true
		}
	}
	return false
}

func commentIsMutates(text string) bool {
	text = strings.TrimPrefix(text, "//")
	text = strings.TrimPrefix(text, "/*")
	return strings.HasPrefix(strings.TrimSpace(text), mutatesMarker)
}

// checkFreshness walks a function body in source order, tracking which local
// variables hold values the function constructed, and reports LVal field
// writes whose receiver chain does not root at a tracked-fresh value.
// Nested function literals are walked with the same freshness map: a closure
// writing a field on a variable its enclosing function constructed is
// treated as fresh (intraprocedural approximation).
func checkFreshness(pass *analysis.Pass, body *ast.BlockStmt, ann map[int]bool) {
	fresh := make(map[types.Object]bool)
	aliases := newAliasTracker(pass, fresh, ann)
	ast.Inspect(body, func(n ast.Node) bool {
		switch stmt := n.(type) {
		case *ast.AssignStmt:
			handleAssign(pass, stmt, fresh, ann)
			aliases.handleAssign(stmt)
		case *ast.IncDecStmt:
			checkWrite(pass, stmt.X, stmt.Pos(), fresh, ann)
			aliases.handleIncDec(stmt)
		case *ast.CallExpr:
			aliases.checkCall(stmt)
		case *ast.ValueSpec:
			aliases.handleValueSpec(stmt)
		}
		return true
	})
}

func handleAssign(pass *analysis.Pass, stmt *ast.AssignStmt, fresh map[types.Object]bool, ann map[int]bool) {
	// Multi-value form: x, y := f().
	if len(stmt.Rhs) == 1 && len(stmt.Lhs) > 1 {
		rhsFresh := isFreshExpr(pass, stmt.Rhs[0], fresh)
		for _, lhs := range stmt.Lhs {
			trackOrCheck(pass, stmt, lhs, rhsFresh, fresh, ann)
		}
		return
	}
	for i, lhs := range stmt.Lhs {
		rhsFresh := false
		if i < len(stmt.Rhs) {
			rhsFresh = isFreshExpr(pass, stmt.Rhs[i], fresh)
		}
		trackOrCheck(pass, stmt, lhs, rhsFresh, fresh, ann)
	}
}

func trackOrCheck(pass *analysis.Pass, stmt *ast.AssignStmt, lhs ast.Expr, rhsFresh bool, fresh map[types.Object]bool, ann map[int]bool) {
	lhs = ast.Unparen(lhs)
	if id, ok := lhs.(*ast.Ident); ok {
		if id.Name == "_" {
			return
		}
		obj := pass.TypesInfo.Defs[id]
		if obj == nil {
			obj = pass.TypesInfo.Uses[id]
		}
		if obj != nil {
			fresh[obj] = rhsFresh
		}
		return
	}
	checkWrite(pass, lhs, stmt.Pos(), fresh, ann)
}

// checkWrite reports lhs when it is a write into lisp.LVal storage whose
// receiver chain does not root at a fresh value and no annotation covers it.
func checkWrite(pass *analysis.Pass, lhs ast.Expr, stmtPos token.Pos, fresh map[types.Object]bool, ann map[int]bool) {
	kind, recv := classifyLValWrite(pass, lhs)
	if kind == "" {
		return
	}
	line := pass.Fset.Position(stmtPos).Line
	if ann[line] || ann[line-1] {
		return
	}
	if rootIsFresh(pass, recv, fresh) {
		return
	}
	pass.Reportf(lhs.Pos(),
		"%s on a lisp.LVal this function did not construct"+
			" (the corruption pattern behind issues #333 and #334);"+
			" construct the value locally, Copy/detach it first,"+
			" or annotate //elps:mutates with a justification", kind)
}

// classifyLValWrite decides whether lhs writes into lisp.LVal storage.  It
// returns a description of the write and the receiver expression whose chain
// must root at a fresh value, or ("", nil) when lhs is not an LVal write.
func classifyLValWrite(pass *analysis.Pass, lhs ast.Expr) (string, ast.Expr) {
	lhs = ast.Unparen(lhs)
	switch e := lhs.(type) {
	case *ast.SelectorExpr:
		if name, ok := lvalFieldSelection(pass, e); ok {
			return "write to LVal field ." + name, e.X
		}
	case *ast.IndexExpr:
		x := ast.Unparen(e.X)
		if sel, ok := x.(*ast.SelectorExpr); ok {
			if name, ok := lvalFieldSelection(pass, sel); ok {
				return "write to LVal field ." + name + " element", sel.X
			}
		}
		if call, ok := x.(*ast.CallExpr); ok {
			if fn := methodCallee(pass, call); fn != nil && fn.Name() == "Bytes" && recvIsLisp(fn, "LVal") {
				if sel, ok := ast.Unparen(call.Fun).(*ast.SelectorExpr); ok {
					return "write to LVal .Bytes() backing array", sel.X
				}
			}
		}
	case *ast.StarExpr:
		if isLValPtr(pass.TypesInfo.TypeOf(e.X)) {
			return "whole-value write through *lisp.LVal", e.X
		}
	}
	return "", nil
}

// rootIsFresh walks recv's receiver chain to its root and reports whether the
// root is a value this function constructed.
func rootIsFresh(pass *analysis.Pass, recv ast.Expr, fresh map[types.Object]bool) bool {
	for {
		recv = ast.Unparen(recv)
		switch e := recv.(type) {
		case *ast.SelectorExpr:
			// Only continue the chain through LVal field selections
			// (v.Cells[0].Str roots at v).  A selection on some other
			// struct (p.expr.Str) roots at that struct's value, which
			// this analysis does not track — treat the chain as unknown
			// unless the selection is itself rooted in tracked state; we
			// keep walking so `freshVal.Cells[i].Cells = ...` resolves.
			recv = e.X
		case *ast.IndexExpr:
			recv = e.X
		case *ast.StarExpr:
			recv = e.X
		case *ast.CallExpr:
			// v.Bytes()[i]: continue to the method receiver.  Copy/detach
			// and constructor calls are fresh in their own right.
			if isFreshExpr(pass, e, fresh) {
				return true
			}
			if fn := methodCallee(pass, e); fn != nil && recvIsLisp(fn, "LVal") {
				if sel, ok := ast.Unparen(e.Fun).(*ast.SelectorExpr); ok {
					recv = sel.X
					continue
				}
			}
			return false
		case *ast.Ident:
			obj := pass.TypesInfo.Uses[e]
			if obj == nil {
				obj = pass.TypesInfo.Defs[e]
			}
			if obj == nil {
				return false
			}
			if fresh[obj] {
				return true
			}
			// A value-typed LVal variable is the function's own struct
			// copy: writes to its top-level fields cannot land in shared
			// memory (Go value semantics).
			if v, ok := obj.(*types.Var); ok && isLValValue(v.Type()) {
				return true
			}
			return false
		case *ast.CompositeLit, *ast.UnaryExpr:
			return isFreshExpr(pass, e, fresh)
		default:
			return false
		}
	}
}

// isFreshExpr reports whether e evaluates to an LVal (or pointer to one)
// that the enclosing function constructed.
func isFreshExpr(pass *analysis.Pass, e ast.Expr, fresh map[types.Object]bool) bool {
	e = ast.Unparen(e)
	switch x := e.(type) {
	case *ast.Ident:
		obj := pass.TypesInfo.Uses[x]
		if obj == nil {
			obj = pass.TypesInfo.Defs[x]
		}
		return obj != nil && fresh[obj]
	case *ast.CompositeLit:
		return isLValValue(pass.TypesInfo.TypeOf(x))
	case *ast.UnaryExpr:
		if x.Op != token.AND {
			return false
		}
		return isFreshExpr(pass, x.X, fresh)
	case *ast.StarExpr:
		// *p copies the struct: the resulting value is the function's own.
		return isLValPtr(pass.TypesInfo.TypeOf(x.X))
	case *ast.CallExpr:
		return isFreshCall(pass, x)
	}
	return false
}

func isFreshCall(pass *analysis.Pass, call *ast.CallExpr) bool {
	// new(lisp.LVal)
	if id, ok := ast.Unparen(call.Fun).(*ast.Ident); ok && id.Name == "new" {
		if _, ok := pass.TypesInfo.Uses[id].(*types.Builtin); ok && len(call.Args) == 1 {
			return isLValValue(pass.TypesInfo.TypeOf(call.Args[0]))
		}
	}
	fn, ok := typeutil.Callee(pass.TypesInfo, call).(*types.Func)
	if !ok || fn.Pkg() == nil || fn.Pkg().Path() != lispPkgPath {
		return false
	}
	sig, ok := fn.Type().(*types.Signature)
	if !ok {
		return false
	}
	if sig.Recv() == nil {
		return freshConstructors[fn.Name()]
	}
	if recvIsLisp(fn, "LVal") {
		return freshLValMethods[fn.Name()]
	}
	if recvIsLisp(fn, "LEnv") {
		return freshLEnvMethods[fn.Name()]
	}
	return false
}

func methodCallee(pass *analysis.Pass, call *ast.CallExpr) *types.Func {
	fn, ok := typeutil.Callee(pass.TypesInfo, call).(*types.Func)
	if !ok || fn.Type().(*types.Signature).Recv() == nil {
		return nil
	}
	return fn
}

// lvalFieldSelection reports whether sel selects a struct field of lisp.LVal
// and returns the field name.
func lvalFieldSelection(pass *analysis.Pass, sel *ast.SelectorExpr) (string, bool) {
	s, ok := pass.TypesInfo.Selections[sel]
	if !ok || s.Kind() != types.FieldVal {
		return "", false
	}
	if !isLValValue(s.Recv()) && !isLValPtr(s.Recv()) {
		return "", false
	}
	return sel.Sel.Name, true
}

func recvIsLisp(fn *types.Func, name string) bool {
	sig, ok := fn.Type().(*types.Signature)
	if !ok || sig.Recv() == nil {
		return false
	}
	t := sig.Recv().Type()
	if p, ok := t.Underlying().(*types.Pointer); ok {
		t = p.Elem()
	}
	return isLispNamed(t, name)
}

func isLValValue(t types.Type) bool { return t != nil && isLispNamed(t, "LVal") }

func isLValPtr(t types.Type) bool {
	if t == nil {
		return false
	}
	p, ok := types.Unalias(t).Underlying().(*types.Pointer)
	return ok && isLispNamed(p.Elem(), "LVal")
}

func isLispNamed(t types.Type, name string) bool {
	named, ok := types.Unalias(t).(*types.Named)
	if !ok {
		return false
	}
	obj := named.Obj()
	return obj.Name() == name && obj.Pkg() != nil && obj.Pkg().Path() == lispPkgPath
}
