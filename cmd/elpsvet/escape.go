// Copyright © 2026 The ELPS authors

// The elpsescape analyzer is the third elpsvet rule: an escape-aliasing
// check for runtime-owned *token.Location pointers (issue #375).
//
// The freshness rule (freshness.go) polices the mutation direction: writes
// to LVal storage the writer does not own.  The pre-fix ErrorCondition/
// ErrorConditionf bug (fixed in ac0a326) and the original ErrorAssociate
// aliasing (fixed in d922290) were the mirror image and structurally
// invisible to it: a shared, runtime-owned pointer (env.Loc) stored
// UNCOPIED into a freshly constructed error that escapes the function.
// The write target was fresh, so freshness had nothing to say — the bug
// was in what was stored, not where.
//
// Rule: TAINT every *token.Location read off runtime or LVal state —
// env.Loc, v.source, fun.source, parser/scanner token locations, call-stack
// frame locations; concretely, any *token.Location-typed field read whose
// receiver chain does not root at a value this function constructed, and
// any *token.Location-returning method called on such a receiver
// (p.Location()).  Taint propagates through local assignments
// (last-assignment-wins, path-insensitive, exactly like the freshness and
// alias maps) and through plain functions that return a *token.Location
// when fed a tainted argument.
//
// FLAG any store of a tainted pointer into a field of a value that escapes:
//
//   - an LVal field write (lerr.source = env.Loc — the pre-fix
//     ErrorAssociate shape) or an LVal composite literal field
//     (&LVal{source: env.Loc} — the pre-fix ErrorCondition shape); an LVal
//     is presumed escaping, that is what LVals are for;
//   - a SetSource call with a tainted argument (the setter is the exported
//     spelling of the same field write);
//   - a field write on a local the function returns, and a returned
//     composite literal (or local initialized from one) capturing a tainted
//     location — the return-escape shape;
//   - a field write rooted at a package-level variable — the
//     registry-reachable shape.
//
// CLEAN: a location routed through copyLocation (or any func(*Location)
// *Location named copyLocation), a &token.Location{...} literal, and
// &local where local is a value-typed variable (the parser's deref-copy
// idiom: src := *loc; v.SetSource(&src)) — all of these give the escaping
// value its own memory.
//
// Out of scope (documented limitations, consistent with the freshness
// rule's intraprocedural approximation): function parameters are not taint
// sources in the callee (PushFID storing its src argument is its callers'
// concern); a tainted pointer passed as a call argument escapes tracking
// (stampMacroExpansion receives callSite by parameter); whole-struct copies
// (*cp = *v) alias the source field implicitly and remain the freshness
// rule's and the seal design's concern; index reads (locs[i]) are not taint
// sources.
//
// Escape hatch: //elps:aliases on the flagged line, the line above it, or
// the enclosing function's doc comment.  Every annotation must carry a
// justification a reader can audit — the deliberate aliases are the ones
// whose producers freeze the location before the value can escape
// (post-parse sealed locations) or whose consumers are runtime-internal
// (TaggedValue, Lambda), and the justification must say which.
package main

import (
	"go/ast"
	"go/token"
	"go/types"

	"golang.org/x/tools/go/analysis"
	"golang.org/x/tools/go/types/typeutil"
)

const tokenPkgPath = "github.com/luthersystems/elps/parser/token" //nolint:gosec // an import path (gosec G101 keys on the word "token"), not a credential

const aliasesMarker = "elps:aliases"

var escapeAnalyzer = &analysis.Analyzer{
	Name: "elpsescape",
	Doc:  "flag runtime-owned *token.Location pointers (env.Loc, v.source, parser token locations) stored uncopied into fields of escaping values (the pre-fix ErrorCondition/ErrorConditionf and ErrorAssociate aliasing, issue #375); suppress with //elps:aliases",
	Run:  runEscape,
}

func runEscape(pass *analysis.Pass) (interface{}, error) {
	for _, file := range pass.Files {
		ann := markerLines(pass.Fset, file, aliasesMarker)
		for _, decl := range file.Decls {
			switch d := decl.(type) {
			case *ast.FuncDecl:
				if d.Body == nil || hasMarker(d.Doc, aliasesMarker) {
					continue
				}
				checkEscape(pass, d.Body, ann)
			case *ast.GenDecl:
				// Function literals in package-level var initializers.
				ast.Inspect(d, func(n ast.Node) bool {
					if lit, ok := n.(*ast.FuncLit); ok {
						checkEscape(pass, lit.Body, ann)
						return false
					}
					return true
				})
			}
		}
	}
	return nil, nil
}

// escTracker holds the per-function-body taint state for the escape rule.
type escTracker struct {
	pass     *analysis.Pass
	fresh    map[types.Object]bool // LVal locals this function constructed
	tainted  map[types.Object]bool // *token.Location locals aliasing runtime state
	carrier  map[types.Object]bool // locals whose composite literal captured a tainted location
	returned map[types.Object]bool // locals that appear in a return statement
	ann      map[int]bool
}

// checkEscape walks one function body in source order.  Nested function
// literals share the maps — the same intraprocedural approximation the
// freshness rule uses.
func checkEscape(pass *analysis.Pass, body *ast.BlockStmt, ann map[int]bool) {
	t := &escTracker{
		pass:     pass,
		fresh:    make(map[types.Object]bool),
		tainted:  make(map[types.Object]bool),
		carrier:  make(map[types.Object]bool),
		returned: collectReturned(pass, body),
		ann:      ann,
	}
	ast.Inspect(body, func(n ast.Node) bool {
		switch stmt := n.(type) {
		case *ast.AssignStmt:
			t.handleAssign(stmt)
		case *ast.ValueSpec:
			t.handleValueSpec(stmt)
		case *ast.CallExpr:
			t.checkSetSource(stmt)
		case *ast.CompositeLit:
			t.checkLValComposite(stmt)
		case *ast.ReturnStmt:
			t.checkReturn(stmt)
		}
		return true
	})
}

// collectReturned pre-collects the local identifiers a body returns, so a
// store into their fields earlier in source order is known to escape.
func collectReturned(pass *analysis.Pass, body *ast.BlockStmt) map[types.Object]bool {
	returned := make(map[types.Object]bool)
	ast.Inspect(body, func(n ast.Node) bool {
		ret, ok := n.(*ast.ReturnStmt)
		if !ok {
			return true
		}
		for _, res := range ret.Results {
			res = ast.Unparen(res)
			if u, ok := res.(*ast.UnaryExpr); ok && u.Op == token.AND {
				res = ast.Unparen(u.X)
			}
			if id, ok := res.(*ast.Ident); ok {
				if obj := pass.TypesInfo.ObjectOf(id); obj != nil {
					returned[obj] = true
				}
			}
		}
		return true
	})
	return returned
}

func (t *escTracker) report(pos token.Pos, what string) {
	line := t.pass.Fset.Position(pos).Line
	if t.ann[line] || t.ann[line-1] {
		return
	}
	t.pass.Reportf(pos,
		"%s stores a runtime-owned *token.Location that its producer may still fix up in place"+
			" (the escape-aliasing shape behind the pre-fix ErrorCondition/ErrorConditionf"+
			" and ErrorAssociate bugs);"+
			" store an independent copy (copyLocation) instead,"+
			" or annotate //elps:aliases with a justification", what)
}

// handleAssign tracks freshness/taint/carrier state through identifier
// assignments and flags escaping stores of tainted locations.
func (t *escTracker) handleAssign(stmt *ast.AssignStmt) {
	single := len(stmt.Rhs) == 1
	for i, lhs := range stmt.Lhs {
		var rhs ast.Expr
		if single {
			rhs = stmt.Rhs[0]
		} else if i < len(stmt.Rhs) {
			rhs = stmt.Rhs[i]
		}
		lhs = ast.Unparen(lhs)
		if id, ok := lhs.(*ast.Ident); ok {
			t.trackIdent(id, rhs, single && len(stmt.Lhs) > 1)
			continue
		}
		if rhs != nil {
			t.checkFieldStore(lhs, rhs)
		}
	}
}

// handleValueSpec tracks the same state through plain var declarations.
func (t *escTracker) handleValueSpec(spec *ast.ValueSpec) {
	if len(spec.Values) != len(spec.Names) {
		return
	}
	for i, name := range spec.Names {
		t.trackIdent(name, spec.Values[i], false)
	}
}

// trackIdent updates the fresh/tainted/carrier maps for one assigned
// identifier.  tuple reports the x, y := f() form, where per-value
// expressions are unavailable and everything conservatively clears.
func (t *escTracker) trackIdent(id *ast.Ident, rhs ast.Expr, tuple bool) {
	if id.Name == "_" {
		return
	}
	obj := t.pass.TypesInfo.Defs[id]
	if obj == nil {
		obj = t.pass.TypesInfo.Uses[id]
	}
	if obj == nil {
		return
	}
	if tuple || rhs == nil {
		t.fresh[obj] = false
		t.tainted[obj] = false
		t.carrier[obj] = false
		return
	}
	switch {
	case isLocationPtr(obj.Type()):
		t.tainted[obj] = t.locTaint(rhs)
	case isLValValue(obj.Type()) || isLValPtr(obj.Type()):
		t.fresh[obj] = isFreshExpr(t.pass, rhs, t.fresh)
	default:
		t.carrier[obj] = t.carrierOfLoc(rhs)
	}
}

// checkFieldStore flags an assignment whose LHS is a field of an escaping
// value and whose RHS carries a tainted location.
func (t *escTracker) checkFieldStore(lhs, rhs ast.Expr) {
	taint := t.locTaint(rhs)
	carry := !taint && t.carrierOfLoc(rhs)
	if !taint && !carry {
		return
	}
	lhs = ast.Unparen(lhs)
	sel, ok := lhs.(*ast.SelectorExpr)
	if !ok {
		return
	}
	if name, ok := lvalFieldSelection(t.pass, sel); ok {
		t.report(lhs.Pos(), "write to LVal field ."+name)
		return
	}
	// Only direct location stores matter for the non-LVal targets below;
	// a carrier stored into an arbitrary struct field is out of scope.
	if !taint {
		return
	}
	root := chainRootIdent(sel.X)
	if root == nil {
		return
	}
	obj := t.pass.TypesInfo.ObjectOf(root)
	if obj == nil {
		return
	}
	if v, ok := obj.(*types.Var); ok && v.Parent() == v.Pkg().Scope() {
		t.report(lhs.Pos(), "write to field ."+sel.Sel.Name+" of package-level state")
		return
	}
	if t.returned[obj] {
		t.report(lhs.Pos(), "write to field ."+sel.Sel.Name+" of a value this function returns")
	}
}

// checkSetSource flags v.SetSource(loc) calls with a tainted argument —
// the exported spelling of the LVal .source field write.
func (t *escTracker) checkSetSource(call *ast.CallExpr) {
	fn := methodCallee(t.pass, call)
	if fn == nil || fn.Name() != "SetSource" || !recvIsLisp(fn, "LVal") {
		return
	}
	if len(call.Args) == 1 && t.locTaint(call.Args[0]) {
		t.report(call.Pos(), "SetSource call")
	}
}

// checkLValComposite flags a tainted location captured by a field of an
// LVal composite literal — the pre-fix ErrorCondition/ErrorConditionf
// shape (&LVal{source: env.Loc, ...}).  An LVal literal is presumed
// escaping.
func (t *escTracker) checkLValComposite(lit *ast.CompositeLit) {
	typ := t.pass.TypesInfo.TypeOf(lit)
	if typ == nil || !isLValValue(typ) {
		return
	}
	for _, elt := range lit.Elts {
		name := ""
		val := elt
		if kv, ok := elt.(*ast.KeyValueExpr); ok {
			if id, ok := kv.Key.(*ast.Ident); ok {
				name = "." + id.Name
			}
			val = kv.Value
		}
		if t.locTaint(val) {
			t.report(elt.Pos(), "LVal composite literal field "+name)
		}
	}
}

// checkReturn flags the return-escape shape: returning a composite literal
// (or a local initialized from one) that captured a tainted location.
// LVal literals are excluded — checkLValComposite already reports them
// wherever they appear.
func (t *escTracker) checkReturn(ret *ast.ReturnStmt) {
	for _, res := range ret.Results {
		if t.carrierOfLoc(res) {
			t.report(res.Pos(), "returning a value whose composite literal captured a field")
		}
	}
}

// locTaint reports whether e evaluates to a *token.Location aliasing
// runtime or LVal state this function does not own.
func (t *escTracker) locTaint(e ast.Expr) bool {
	e = ast.Unparen(e)
	switch x := e.(type) {
	case *ast.Ident:
		obj := t.pass.TypesInfo.Uses[x]
		if obj == nil {
			obj = t.pass.TypesInfo.Defs[x]
		}
		return obj != nil && t.tainted[obj]
	case *ast.SelectorExpr:
		// env.Loc, v.source, tok.Source, frame.Source: a location field
		// read off anything this function did not construct.
		s, ok := t.pass.TypesInfo.Selections[x]
		if !ok || s.Kind() != types.FieldVal || !isLocationPtr(s.Type()) {
			return false
		}
		return !rootIsFresh(t.pass, x.X, t.fresh)
	case *ast.UnaryExpr:
		if x.Op != token.AND {
			return false
		}
		// &lit and &local are this function's memory; &shared.LocField
		// aliases shared storage.
		inner := ast.Unparen(x.X)
		if sel, ok := inner.(*ast.SelectorExpr); ok {
			return !rootIsFresh(t.pass, sel.X, t.fresh)
		}
		return false
	case *ast.CallExpr:
		return t.callLocTaint(x)
	}
	return false
}

// callLocTaint decides taint for a call in location position: copyLocation
// cleanses; a location-returning method aliases its receiver's state
// (p.Location()); a plain location-returning function propagates its
// arguments' taint (intraprocedural helper approximation).
func (t *escTracker) callLocTaint(call *ast.CallExpr) bool {
	fn, ok := typeutil.Callee(t.pass.TypesInfo, call).(*types.Func)
	if !ok {
		return false
	}
	sig, ok := fn.Type().(*types.Signature)
	if !ok || sig.Results().Len() != 1 || !isLocationPtr(sig.Results().At(0).Type()) {
		return false
	}
	if fn.Name() == "copyLocation" {
		return false // the sanctioned cleanser (lisp/detach.go)
	}
	if sig.Recv() != nil {
		if sel, ok := ast.Unparen(call.Fun).(*ast.SelectorExpr); ok {
			return !rootIsFresh(t.pass, sel.X, t.fresh)
		}
		return true
	}
	for _, arg := range call.Args {
		if t.locTaint(arg) {
			return true
		}
	}
	return false
}

// carrierOfLoc reports whether e is (or holds) a non-LVal value whose
// composite literal captured a tainted location — the return-escape shape
// (&FunctionInfo{Source: node.source}).
func (t *escTracker) carrierOfLoc(e ast.Expr) bool {
	e = ast.Unparen(e)
	switch x := e.(type) {
	case *ast.Ident:
		obj := t.pass.TypesInfo.Uses[x]
		if obj == nil {
			obj = t.pass.TypesInfo.Defs[x]
		}
		return obj != nil && t.carrier[obj]
	case *ast.UnaryExpr:
		return x.Op == token.AND && t.carrierOfLoc(x.X)
	case *ast.CompositeLit:
		if typ := t.pass.TypesInfo.TypeOf(x); typ != nil && isLValValue(typ) {
			return false // LVal literals are checkLValComposite's to report
		}
		for _, elt := range x.Elts {
			if kv, ok := elt.(*ast.KeyValueExpr); ok {
				elt = kv.Value
			}
			if t.locTaint(elt) {
				return true
			}
		}
	}
	return false
}

// chainRootIdent walks a selector/index/deref chain to its root identifier,
// or nil when the chain roots elsewhere (a call, a literal).
func chainRootIdent(e ast.Expr) *ast.Ident {
	for {
		e = ast.Unparen(e)
		switch x := e.(type) {
		case *ast.Ident:
			return x
		case *ast.SelectorExpr:
			e = x.X
		case *ast.IndexExpr:
			e = x.X
		case *ast.StarExpr:
			e = x.X
		default:
			return nil
		}
	}
}

// isLocationPtr reports whether t is *token.Location.
func isLocationPtr(t types.Type) bool {
	if t == nil {
		return false
	}
	p, ok := types.Unalias(t).Underlying().(*types.Pointer)
	if !ok {
		return false
	}
	named, ok := types.Unalias(p.Elem()).(*types.Named)
	if !ok {
		return false
	}
	obj := named.Obj()
	return obj.Name() == "Location" && obj.Pkg() != nil && obj.Pkg().Path() == tokenPkgPath
}
