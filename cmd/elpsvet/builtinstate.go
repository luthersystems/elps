// Copyright © 2026 The ELPS authors

package main

// elpsbuiltinstate: no builtin may write state that outlives the call
// (issue #680, the class behind #678).
//
// A Template approves a builtin by IDENTITY -- the Go function value it
// wraps -- and shares that one function value with every VM it mints, for
// the life of the process.  Publication never looks inside it, so whatever
// the function's receiver or closure captures is shared on the same terms.
// #678 was exactly that: a setter builtin registered as a method value
// (s.SetFooBuiltin) wrote s.foo, and every VM forked from the template saw
// every other VM's writes.  No isolation test sees it from the outside until
// two transactions happen to interleave on it.
//
// WHAT IS A BUILTIN.  Rather than a list of constructor NAMES that drifts as
// constructors are added, a builtin is any expression handed to a slot whose
// static type is lisp.LBuiltin: a call argument whose parameter is declared
// lisp.LBuiltin (libutil.Function/FunctionDoc, elpsutil.Function/FunctionDoc,
// lisp.Fun, FunInPackage, Macro, MacroInPackage, SpecialOp,
// SpecialOpInPackage, libschema.NewValidator, ...) or a keyed or positional
// composite-literal element whose field is lisp.LBuiltin (the kernel's own
// builtin tables), or the operand of a lisp.LBuiltin(f) conversion.  Of
// those expressions three shapes are analysed:
//
//   - a function literal: its body, with every variable declared OUTSIDE the
//     literal treated as captured;
//   - a method value (s.FooBuiltin) whose method is declared in the package
//     being analysed: the method body, with its receiver tracked;
//   - a plain function declared in the package: its body, for writes to
//     package-level variables only (it has no receiver and captures nothing).
//
// WHAT IS REPORTED, inside those bodies (nested literals included): an
// assignment, op-assignment or ++/-- whose left-hand side is ROOTED at the
// receiver, a captured variable or a package-level variable -- through any
// chain of field selections, indexing (slice, array or map) and
// dereferences -- which covers `x = append(x, ...)` and `m[k] = v`; and a
// delete(m, k) whose map is so rooted.  Writes rooted at the function's own
// parameters (env, args) and locals are per-call and not reported.
//
// EXEMPT: a left-hand side whose type is declared in sync/atomic, and every
// call (atomic.AddInt64(&s.n, 1), s.mu.Lock(), s.once.Do(...)): the rule
// reports write STATEMENTS, not calls.  Mutex- or Once-guarded state is not
// exempt by that fact alone -- a guarded write is still shared state, so it
// needs the marker below with a sentence saying why sharing it across VMs is
// correct.
//
// SUPPRESSION: `//elpsvet:allow-shared <justification>`, trailing on the
// reported line or standalone on the line above, or in the doc comment of
// the method or function whose body is analysed.  As with allow-native, the
// justification must be at least three words; a bare marker does not
// suppress.  The marker is this rule's own: the ownership rule's
// //elpsvet:allow stops at its word boundary so "-shared" does not satisfy
// it, and allow-native's prefix does not match it either.
//
// INVISIBLE, deliberately: a builtin reached through a variable, a returned
// function or reflection; a method declared in another package; calls the
// body makes (a write in a helper the builtin calls is not seen -- only the
// registered body is read); pointer-method calls that mutate (s.buf.Reset());
// writes through a LOCAL alias of shared state (p := s.cache; p[k] = v); and
// callbacks typed other than lisp.LBuiltin (libschema's internal
// three-argument validator callbacks, which carry their state in an LVal
// captures list the template remaps per VM).  A clean run is evidence, not
// proof, that no builtin shares state; the runtime half of #680
// (an opt-in shareability contract on TemplateWithBuiltinPolicy) is what
// would close the rest.

import (
	"go/ast"
	"go/token"
	"go/types"
	"strings"

	"golang.org/x/tools/go/analysis"
)

const (
	sharedAllowMarker   = "elpsvet:allow-shared"
	lBuiltinTypeName    = "LBuiltin"
	syncAtomicPkgPath   = "sync/atomic"
	sharedAllowMinWords = 3
)

var builtinStateAnalyzer = &analysis.Analyzer{
	Name: "elpsbuiltinstate",
	Doc: "flag builtins (function values registered through a lisp.LBuiltin slot) that write their" +
		" receiver, captured variables or package-level variables -- state a template shares with" +
		" every VM it mints (issue #680) -- unless //elpsvet:allow-shared <justification> covers the site",
	Run: runBuiltinState,
}

// justifiedAllow reports whether a comment's text is marker followed by
// whitespace and at least minWords words.
func justifiedAllow(text, marker string, minWords int) bool {
	text = strings.TrimPrefix(text, "//")
	text = strings.TrimPrefix(text, "/*")
	text = strings.TrimSuffix(text, "*/")
	text = strings.TrimSpace(text)
	rest, ok := strings.CutPrefix(text, marker)
	if !ok || rest == "" {
		return false
	}
	if rest[0] != ' ' && rest[0] != '\t' {
		return false // a different marker sharing the prefix
	}
	return len(strings.Fields(rest)) >= minWords
}

func justifiedSharedAllow(text string) bool {
	return justifiedAllow(text, sharedAllowMarker, sharedAllowMinWords)
}

func hasJustifiedSharedAllow(cg *ast.CommentGroup) bool {
	if cg == nil {
		return false
	}
	for _, c := range cg.List {
		if justifiedSharedAllow(c.Text) {
			return true
		}
	}
	return false
}

type lineKey struct {
	file string
	line int
}

type builtinStateRun struct {
	pass     *analysis.Pass
	allow    map[lineKey]bool
	decls    map[*types.Func]*ast.FuncDecl
	analysed map[ast.Node]bool
}

func runBuiltinState(pass *analysis.Pass) (interface{}, error) {
	r := &builtinStateRun{
		pass:     pass,
		allow:    make(map[lineKey]bool),
		decls:    make(map[*types.Func]*ast.FuncDecl),
		analysed: make(map[ast.Node]bool),
	}
	for _, file := range pass.Files {
		name := pass.Fset.Position(file.Pos()).Filename
		for line := range markerLinesMatching(pass.Fset, file, justifiedSharedAllow) {
			r.allow[lineKey{name, line}] = true
		}
		for _, decl := range file.Decls {
			if fd, ok := decl.(*ast.FuncDecl); ok {
				if fn, ok := pass.TypesInfo.Defs[fd.Name].(*types.Func); ok {
					r.decls[fn] = fd
				}
			}
		}
	}
	for _, file := range pass.Files {
		ast.Inspect(file, func(n ast.Node) bool {
			switch x := n.(type) {
			case *ast.CallExpr:
				r.checkCall(x)
			case *ast.CompositeLit:
				r.checkLiteral(x)
			}
			return true
		})
	}
	return nil, nil
}

func isLBuiltin(t types.Type) bool {
	named, ok := types.Unalias(t).(*types.Named)
	if !ok {
		return false
	}
	obj := named.Obj()
	return obj.Name() == lBuiltinTypeName && obj.Pkg() != nil && obj.Pkg().Path() == lispPkgPath
}

func (r *builtinStateRun) checkCall(call *ast.CallExpr) {
	if tv, ok := r.pass.TypesInfo.Types[call.Fun]; ok && tv.IsType() {
		if isLBuiltin(tv.Type) && len(call.Args) == 1 {
			r.checkBuiltin(call.Args[0]) // lisp.LBuiltin(f)
		}
		return
	}
	sig, ok := types.Unalias(r.pass.TypesInfo.TypeOf(call.Fun)).(*types.Signature)
	if !ok {
		return // a Go builtin such as append
	}
	params := sig.Params()
	for i, arg := range call.Args {
		var pt types.Type
		switch {
		case i < params.Len()-1 || (i < params.Len() && !sig.Variadic()):
			pt = params.At(i).Type()
		case sig.Variadic() && params.Len() > 0:
			if s, ok := params.At(params.Len() - 1).Type().(*types.Slice); ok {
				pt = s.Elem()
			}
		}
		if pt != nil && isLBuiltin(pt) {
			r.checkBuiltin(arg)
		}
	}
}

func (r *builtinStateRun) checkLiteral(lit *ast.CompositeLit) {
	t := r.pass.TypesInfo.TypeOf(lit)
	if t == nil {
		return
	}
	st, ok := t.Underlying().(*types.Struct)
	if !ok {
		if p, ok := t.Underlying().(*types.Pointer); ok {
			st, _ = p.Elem().Underlying().(*types.Struct)
		}
	}
	if st == nil {
		return
	}
	for i, elt := range lit.Elts {
		if kv, ok := elt.(*ast.KeyValueExpr); ok {
			key, ok := kv.Key.(*ast.Ident)
			if !ok {
				continue
			}
			if f, ok := r.pass.TypesInfo.ObjectOf(key).(*types.Var); ok && f.IsField() && isLBuiltin(f.Type()) {
				r.checkBuiltin(kv.Value)
			}
			continue
		}
		if i < st.NumFields() && isLBuiltin(st.Field(i).Type()) {
			r.checkBuiltin(elt)
		}
	}
}

// checkBuiltin analyses one registered function value.
func (r *builtinStateRun) checkBuiltin(expr ast.Expr) {
	expr = ast.Unparen(expr)
	switch x := expr.(type) {
	case *ast.FuncLit:
		if r.analysed[x] {
			return
		}
		r.analysed[x] = true
		r.checkBody(x.Body, x, nil, nil)
	case *ast.SelectorExpr:
		sel := r.pass.TypesInfo.Selections[x]
		if sel == nil {
			// A package-qualified function (pkg.F): only this package's
			// declarations are visible.
			if fn, ok := r.pass.TypesInfo.Uses[x.Sel].(*types.Func); ok {
				r.checkDecl(fn)
			}
			return
		}
		if sel.Kind() != types.MethodVal {
			return
		}
		if fn, ok := sel.Obj().(*types.Func); ok {
			r.checkDecl(fn)
		}
	case *ast.Ident:
		if fn, ok := r.pass.TypesInfo.Uses[x].(*types.Func); ok {
			r.checkDecl(fn)
		}
	}
}

func (r *builtinStateRun) checkDecl(fn *types.Func) {
	fd := r.decls[fn.Origin()]
	if fd == nil || fd.Body == nil || r.analysed[fd] {
		return
	}
	r.analysed[fd] = true
	if hasJustifiedSharedAllow(fd.Doc) {
		return
	}
	var recv *types.Var
	if fd.Recv != nil && len(fd.Recv.List) == 1 && len(fd.Recv.List[0].Names) == 1 {
		recv, _ = r.pass.TypesInfo.Defs[fd.Recv.List[0].Names[0]].(*types.Var)
	}
	r.checkBody(fd.Body, nil, recv, fd)
}

// checkBody reports shared-state writes in body.  lit is the registered
// function literal (nil for a declaration), recv the method receiver.
func (r *builtinStateRun) checkBody(body *ast.BlockStmt, lit *ast.FuncLit, recv *types.Var, fd *ast.FuncDecl) {
	name := "builtin"
	if fd != nil {
		name = fd.Name.Name
	}
	report := func(lhs ast.Expr, what string) {
		kind, v := r.sharedRoot(lhs, lit, recv)
		if kind == "" {
			return
		}
		if t := r.pass.TypesInfo.TypeOf(lhs); t != nil && fromSyncAtomic(t) {
			return
		}
		pos := r.pass.Fset.Position(lhs.Pos())
		if r.allow[lineKey{pos.Filename, pos.Line}] {
			return
		}
		r.pass.Reportf(lhs.Pos(),
			"%s %s %s %s, state a template shares with every VM it mints (issue #680, the #678 class);"+
				" keep per-call state in locals or per-VM state in the Runtime, or annotate"+
				" //elpsvet:allow-shared <justification> saying why sharing it is correct",
			name, what, kind, v.Name())
	}
	ast.Inspect(body, func(n ast.Node) bool {
		switch x := n.(type) {
		case *ast.AssignStmt:
			if x.Tok == token.DEFINE {
				return true
			}
			for _, lhs := range x.Lhs {
				report(lhs, "writes")
			}
		case *ast.IncDecStmt:
			report(x.X, "writes")
		case *ast.CallExpr:
			if id, ok := ast.Unparen(x.Fun).(*ast.Ident); ok && len(x.Args) >= 1 {
				if b, ok := r.pass.TypesInfo.Uses[id].(*types.Builtin); ok && b.Name() == "delete" {
					report(x.Args[0], "deletes from")
				}
			}
		}
		return true
	})
}

// sharedRoot walks lhs to its root identifier and classifies it.
func (r *builtinStateRun) sharedRoot(lhs ast.Expr, lit *ast.FuncLit, recv *types.Var) (string, *types.Var) {
	e := lhs
	for {
		switch x := ast.Unparen(e).(type) {
		case *ast.SelectorExpr:
			if r.pass.TypesInfo.Selections[x] == nil {
				// pkg.Var: the selected identifier is the root.
				e = x.Sel
				continue
			}
			e = x.X
			continue
		case *ast.IndexExpr:
			e = x.X
			continue
		case *ast.StarExpr:
			e = x.X
			continue
		case *ast.Ident:
			v, ok := r.pass.TypesInfo.ObjectOf(x).(*types.Var)
			if !ok || v.IsField() {
				return "", nil
			}
			switch {
			case v.Pkg() != nil && v.Parent() == v.Pkg().Scope():
				return "package-level var", v
			case recv != nil && v == recv:
				return "receiver", v
			case lit != nil && (v.Pos() < lit.Pos() || v.Pos() >= lit.End()):
				return "captured var", v
			}
			return "", nil
		default:
			return "", nil
		}
	}
}

func fromSyncAtomic(t types.Type) bool {
	if p, ok := types.Unalias(t).(*types.Pointer); ok {
		t = p.Elem()
	}
	named, ok := types.Unalias(t).(*types.Named)
	return ok && named.Obj().Pkg() != nil && named.Obj().Pkg().Path() == syncAtomicPkgPath
}
