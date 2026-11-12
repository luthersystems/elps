// Copyright © 2026 The ELPS authors

package main

import (
	"fmt"
	"go/ast"
	"go/token"
	"go/types"
	"slices"
	"sort"

	"golang.org/x/tools/go/analysis"
)

// callListAnalyzer checks the functions the evaluator trusts to drop their
// argument list: the entries of lisp.callListDroppers (lisp/calllist.go),
// and each builtin a package marks with lisp.DropsArgList
// (lisp/dropsarglist.go).  The evaluator hands the list of a direct call of
// such a function to the next call form, so a function that kept the list,
// or a slice of its cells, would read the next call's arguments.
//
// The rule is strict and syntactic.  In a checked function the list
// parameter, and every slice of its Cells, may only be:
//
//   - indexed (args.Cells[i], read or written): a single cell is a value
//     the list does not own, and it may go anywhere;
//   - measured, ranged over or sliced again (len, cap, range, [a:b]);
//   - the source of append's spread or of copy, which copy the cells out;
//   - compared with nil;
//   - bound to a local variable, which is then held to the same rule;
//   - stored back into the list's own Cells (args.Cells = args.Cells[1:]);
//   - passed to a function, or used as the receiver of a method call,
//     whose parameter is held to the same rule.
//
// A field of the list other than Cells (args.Type) may be read or written.
// Anything else is reported: returning the list, storing it or a slice of
// it, capturing it in a function literal, binding a method value to it,
// taking the address of one of its fields, or passing it to a function
// this analyzer cannot read.  The rule has no comment suppression: a
// function that needs one keeps its list.  It leaves callListDroppers, or
// loses its DropsArgList mark.
//
// A callee in the same package is read.  A callee in another package is
// accepted only through a callListFact, which says the analyzer proved that
// parameter safe in the callee's own package.  The facts are what let a
// library builtin call an LVal method such as args.ReqArg.  For each
// function of package lisp and of each package that imports it, the
// analyzer decides which *lisp.LVal and []*lisp.LVal parameters, and which
// *lisp.LVal receivers, are safe: no use breaks the rule, and each callee
// the list reaches is safe.  The decision is a least fixed point, so a
// recursion is safe only when no function in it breaks the rule.
//
// Invisible: reflection and unsafe.  The elpscheck build covers what this
// rule cannot see at run time: it poisons each returned list
// (lisp/calllist_elpscheck.go).
var callListAnalyzer = &analysis.Analyzer{
	Name:      "elpscalllist",
	Doc:       "check that each function in lisp.callListDroppers, and each builtin marked with lisp.DropsArgList, keeps neither its argument list nor a slice of its cells",
	Run:       runCallList,
	FactTypes: []analysis.Fact{new(callListFact)},
}

const (
	callListDroppersVar = "callListDroppers"
	dropsArgListFunc    = "DropsArgList"
	freeKeywordsFunc    = "FreeKeywords"
)

// callListFact lists the parameters of a function that keep neither the
// argument list nor a slice of its cells.  Index -1 is the receiver.
type callListFact struct {
	Safe []int
}

func (*callListFact) AFact() {}

func (f *callListFact) String() string { return fmt.Sprintf("callListSafe%v", f.Safe) }

func runCallList(pass *analysis.Pass) (interface{}, error) {
	if pass.Pkg.Path() != lispPkgPath && !importsLisp(pass.Pkg) {
		return nil, nil
	}
	c := &callListChecker{pass: pass, decls: map[*types.Func]*ast.FuncDecl{}, info: map[callListParam]*callListInfo{}}
	var table *ast.CompositeLit
	for _, file := range pass.Files {
		for _, decl := range file.Decls {
			switch decl := decl.(type) {
			case *ast.FuncDecl:
				if fn, ok := pass.TypesInfo.Defs[decl.Name].(*types.Func); ok {
					c.decls[fn] = decl
				}
			case *ast.GenDecl:
				if decl.Tok != token.VAR || pass.Pkg.Path() != lispPkgPath {
					continue
				}
				for _, spec := range decl.Specs {
					vs, ok := spec.(*ast.ValueSpec)
					if !ok {
						continue
					}
					for i, name := range vs.Names {
						if name.Name != callListDroppersVar {
							continue
						}
						if i < len(vs.Values) {
							table, _ = ast.Unparen(vs.Values[i]).(*ast.CompositeLit)
						}
						if table == nil {
							pass.Reportf(name.Pos(), "%s must be a composite literal of function names, so that each function can be checked", callListDroppersVar)
						}
					}
				}
			}
		}
	}
	c.exportFacts()
	if table != nil {
		for _, elt := range table.Elts {
			id, _ := ast.Unparen(elt).(*ast.Ident)
			var fn *types.Func
			if id != nil {
				fn, _ = pass.TypesInfo.Uses[id].(*types.Func)
			}
			if fn == nil || c.decls[fn] == nil {
				pass.Reportf(elt.Pos(), "%s entry is not a top-level function of package lisp; its use of the argument list cannot be checked", callListDroppersVar)
				continue
			}
			c.reportRoot(fn, callListDroppersVar+" says drops its argument list", "take the function out of the list")
		}
	}
	for _, file := range pass.Files {
		ast.Inspect(file, func(n ast.Node) bool {
			call, ok := n.(*ast.CallExpr)
			if ok && isLispFunc(calleeFunc(pass, call), dropsArgListFunc) && len(call.Args) == 1 {
				c.checkMarked(call)
			}
			return true
		})
	}
	return nil, nil
}

// importsLisp reports whether pkg imports package lisp directly.  Only such
// a package can hold a builtin or a helper that takes an argument list.
func importsLisp(pkg *types.Package) bool {
	for _, imp := range pkg.Imports() {
		if imp.Path() == lispPkgPath {
			return true
		}
	}
	return false
}

// isLispFunc reports whether fn is the package-level function name of
// package lisp.
func isLispFunc(fn *types.Func, name string) bool {
	return fn != nil && fn.Name() == name && fn.Pkg() != nil && fn.Pkg().Path() == lispPkgPath && fn.Signature().Recv() == nil
}

// checkMarked checks the builtin that a lisp.DropsArgList call marks.  The
// argument must be the call that builds the definition (libutil.Function,
// elpsutil.FunctionDoc and the like), or a lisp.FreeKeywords of one.  The
// argument of that call in its one lisp.LBuiltin parameter must name a
// top-level function.  Anything else cannot be checked and is reported.
func (c *callListChecker) checkMarked(mark *ast.CallExpr) {
	e := mark.Args[0]
	for {
		call, ok := ast.Unparen(e).(*ast.CallExpr)
		if !ok {
			c.pass.Reportf(e.Pos(), "%s needs the call that builds the definition (for example libutil.Function), so that elpscalllist can find and check the builtin", dropsArgListFunc)
			return
		}
		callee := calleeFunc(c.pass, call)
		if (isLispFunc(callee, freeKeywordsFunc) || isLispFunc(callee, dropsArgListFunc)) && len(call.Args) == 1 {
			e = call.Args[0]
			continue
		}
		var fn *types.Func
		switch f := ast.Unparen(builtinArg(callee, call)).(type) {
		case *ast.Ident:
			fn, _ = c.pass.TypesInfo.Uses[f].(*types.Func)
		case *ast.SelectorExpr:
			fn, _ = c.pass.TypesInfo.Uses[f.Sel].(*types.Func)
		}
		if fn == nil || fn.Signature().Recv() != nil {
			c.pass.Reportf(call.Pos(), "%s marks a definition whose builtin is not a named top-level function; elpscalllist cannot check it, so leave it unmarked", dropsArgListFunc)
			return
		}
		fn = fn.Origin()
		if c.decls[fn] != nil {
			c.reportRoot(fn, dropsArgListFunc+" marks as dropping its argument list", "remove the "+dropsArgListFunc+" mark")
			return
		}
		if !c.imported(fn, 1) {
			c.pass.Reportf(call.Pos(), "%s marks %s, which may keep its argument list or which elpscalllist cannot read; remove the mark", dropsArgListFunc, fn.FullName())
		}
		return
	}
}

// builtinArg returns the argument of call in callee's one lisp.LBuiltin
// parameter, or nil when there is not exactly one.
func builtinArg(callee *types.Func, call *ast.CallExpr) ast.Expr {
	if callee == nil {
		return nil
	}
	params := callee.Signature().Params()
	var fun ast.Expr
	for i, a := range call.Args {
		if i < params.Len() && isLBuiltinType(params.At(i).Type()) {
			if fun != nil {
				return nil
			}
			fun = a
		}
	}
	return fun
}

// isLBuiltinType reports whether t is lisp.LBuiltin.
func isLBuiltinType(t types.Type) bool {
	named, ok := types.Unalias(t).(*types.Named)
	return ok && named.Obj().Name() == "LBuiltin" && named.Obj().Pkg() != nil && named.Obj().Pkg().Path() == lispPkgPath
}

type callListParam struct {
	fn *types.Func
	// index is the parameter's position, or -1 for the receiver.
	index int
}

type callListViolation struct {
	pos token.Pos
	msg string
}

// callListInfo is what the uses of one parameter show: the uses that break
// the rule, and the parameters of this package that the list reaches.
type callListInfo struct {
	violations []callListViolation
	edges      []callListParam
}

type callListChecker struct {
	pass  *analysis.Pass
	decls map[*types.Func]*ast.FuncDecl
	info  map[callListParam]*callListInfo
	// cur collects the violations and edges of the walk that records.
	cur *callListInfo
}

// reportRoot reports each violation reachable from the list parameter of
// root, a function trusted to drop its list.  An LBuiltin is
// func(env *LEnv, args *LVal) *LVal: the list is the second parameter.  A
// helper that keeps the list is reported once for each root that reaches
// it.
func (c *callListChecker) reportRoot(root *types.Func, trust, fix string) {
	start := callListParam{root, 1}
	seen := map[callListParam]bool{start: true}
	queue := []callListParam{start}
	for len(queue) > 0 {
		p := queue[0]
		queue = queue[1:]
		info := c.paramInfo(p)
		for _, v := range info.violations {
			c.pass.Reportf(v.pos, "%s (reached from %s, which %s); copy the cells out, or %s", v.msg, root.Name(), trust, fix)
		}
		for _, e := range info.edges {
			if !seen[e] {
				seen[e] = true
				queue = append(queue, e)
			}
		}
	}
}

// exportFacts decides which list parameters of the package's functions are
// safe, and exports a callListFact for each function that has one.
func (c *callListChecker) exportFacts() {
	for fn, decl := range c.decls {
		if decl.Recv != nil && len(decl.Recv.List) == 1 && isLValPointer(c.pass.TypesInfo.TypeOf(decl.Recv.List[0].Type)) {
			c.paramInfo(callListParam{fn, -1})
		}
		params := fn.Signature().Params()
		for i := range params.Len() {
			if isListType(params.At(i).Type()) {
				c.paramInfo(callListParam{fn, i})
			}
		}
	}
	// paramInfo has read each parameter an edge reaches, so info is
	// complete.  Unsafety flows backward along the edges.
	unsafe := map[callListParam]bool{}
	callers := map[callListParam][]callListParam{}
	var queue []callListParam
	for p, info := range c.info {
		for _, e := range info.edges {
			callers[e] = append(callers[e], p)
		}
		if len(info.violations) > 0 {
			unsafe[p] = true
			queue = append(queue, p)
		}
	}
	for len(queue) > 0 {
		p := queue[0]
		queue = queue[1:]
		for _, q := range callers[p] {
			if !unsafe[q] {
				unsafe[q] = true
				queue = append(queue, q)
			}
		}
	}
	safe := map[*types.Func][]int{}
	for p := range c.info {
		if !unsafe[p] {
			safe[p.fn] = append(safe[p.fn], p.index)
		}
	}
	for fn, idx := range safe {
		sort.Ints(idx)
		c.pass.ExportObjectFact(fn, &callListFact{Safe: idx})
	}
}

// isListType reports whether t is *lisp.LVal or []*lisp.LVal.
func isListType(t types.Type) bool {
	if isLValPointer(t) {
		return true
	}
	sl, ok := types.Unalias(t).(*types.Slice)
	return ok && isLValPointer(sl.Elem())
}

// imported reports whether a callListFact proves parameter index of fn, a
// function of another package, safe.
func (c *callListChecker) imported(fn *types.Func, index int) bool {
	var fact callListFact
	if fn.Pkg() == nil || fn.Pkg() == c.pass.Pkg || !c.pass.ImportObjectFact(fn, &fact) {
		return false
	}
	return slices.Contains(fact.Safe, index)
}

// paramInfo reads the uses of one parameter, once, and then each parameter
// they reach.
func (c *callListChecker) paramInfo(p callListParam) *callListInfo {
	if info := c.info[p]; info != nil {
		return info
	}
	info := &callListInfo{}
	c.info[p] = info
	decl := c.decls[p.fn]
	var name *ast.Ident
	if p.index < 0 {
		if decl.Recv != nil && len(decl.Recv.List) == 1 && len(decl.Recv.List[0].Names) == 1 {
			name = decl.Recv.List[0].Names[0]
		}
	} else {
		var names []*ast.Ident
		for _, field := range decl.Type.Params.List {
			if len(field.Names) == 0 {
				names = append(names, nil)
				continue
			}
			names = append(names, field.Names...)
		}
		if p.index < len(names) {
			name = names[p.index]
		}
	}
	if name == nil || name.Name == "_" {
		return info
	}
	if decl.Body == nil {
		info.violations = append(info.violations, callListViolation{decl.Pos(), "the function has no body this rule can read"})
		return info
	}
	obj := c.pass.TypesInfo.Defs[name]
	if obj == nil {
		return info
	}
	tracked := map[types.Object]bool{obj: true}
	// A local variable bound to the list joins the tracked set, and its
	// uses may bind more.  Walk until the set is stable, then record.
	for n := -1; n != len(tracked); {
		n = len(tracked)
		c.walk(decl, tracked, nil)
	}
	c.walk(decl, tracked, info)
	for _, e := range info.edges {
		c.paramInfo(e)
	}
	return info
}

// walk visits each use of a tracked variable in decl.  With a nil out it
// only grows tracked.  Otherwise it records the uses into out.
func (c *callListChecker) walk(decl *ast.FuncDecl, tracked map[types.Object]bool, out *callListInfo) {
	c.cur = out
	defer func() { c.cur = nil }()
	var stack []ast.Node
	ast.Inspect(decl.Body, func(n ast.Node) bool {
		if n == nil {
			stack = stack[:len(stack)-1]
			return true
		}
		if id, ok := n.(*ast.Ident); ok && tracked[c.pass.TypesInfo.Uses[id]] {
			if msg := c.use(id, stack, tracked); msg != "" && out != nil {
				out.violations = append(out.violations, callListViolation{id.Pos(), msg})
			}
		}
		stack = append(stack, n)
		return true
	})
}

// edge records that the list reaches parameter index of fn, or returns why
// it may not.
func (c *callListChecker) edge(fn *types.Func, index int) string {
	if c.decls[fn] != nil {
		if c.cur != nil {
			c.cur.edges = append(c.cur.edges, callListParam{fn, index})
		}
		return ""
	}
	if c.imported(fn, index) {
		return ""
	}
	return "the argument list, or a slice of its cells, is passed to " + fn.FullName() + ", which this rule cannot read or which may keep it"
}

// isLValPointer reports whether t is *lisp.LVal.
func isLValPointer(t types.Type) bool {
	ptr, ok := types.Unalias(t).(*types.Pointer)
	if !ok {
		return false
	}
	named, ok := types.Unalias(ptr.Elem()).(*types.Named)
	return ok && named.Obj().Name() == "LVal" && named.Obj().Pkg() != nil && named.Obj().Pkg().Path() == lispPkgPath
}

// use classifies one use of a tracked variable.  stack holds the ancestors
// of id, outermost first.  It returns the reason the use is not allowed, or
// the empty string.
func (c *callListChecker) use(id *ast.Ident, stack []ast.Node, tracked map[types.Object]bool) string {
	for _, n := range stack {
		if _, ok := n.(*ast.FuncLit); ok {
			return "the argument list is captured by a function literal"
		}
	}
	var e ast.Expr = id
	isList := isLValPointer(c.pass.TypesInfo.TypeOf(id))
	for i := len(stack) - 1; i >= 0; i-- {
		switch parent := stack[i].(type) {
		case *ast.ParenExpr:
			e = parent
			continue
		case *ast.SelectorExpr:
			if !isList || parent.X != e {
				return "the argument list is used in a way this rule does not allow"
			}
			if parent.Sel.Name == "Cells" {
				e, isList = parent, false
				continue
			}
			sel := c.pass.TypesInfo.Selections[parent]
			if sel != nil && sel.Kind() == types.MethodVal && i > 0 {
				if call, ok := stack[i-1].(*ast.CallExpr); ok && call.Fun == parent {
					// A method call: the list is the method's receiver.
					if fn, ok := sel.Obj().(*types.Func); ok {
						return c.edge(fn.Origin(), -1)
					}
				}
			}
			if sel == nil || sel.Kind() != types.FieldVal {
				return "a method value is bound to the argument list, and it may keep the list"
			}
			if i > 0 {
				if addr, ok := stack[i-1].(*ast.UnaryExpr); ok && addr.Op == token.AND {
					return "the address of a field of the argument list is taken, and it outlives the call"
				}
			}
			return ""
		case *ast.IndexExpr:
			if !isList && parent.X == e {
				return ""
			}
		case *ast.SliceExpr:
			if !isList && parent.X == e {
				e = parent
				continue
			}
		case *ast.RangeStmt:
			if !isList && parent.X == e {
				return ""
			}
		case *ast.BinaryExpr:
			if parent.Op == token.EQL || parent.Op == token.NEQ {
				return ""
			}
		case *ast.CallExpr:
			return c.arg(parent, e, isList)
		case *ast.AssignStmt:
			return c.assign(parent.Lhs, parent.Rhs, e, tracked)
		case *ast.ValueSpec:
			lhs := make([]ast.Expr, len(parent.Names))
			for j, name := range parent.Names {
				lhs[j] = name
			}
			return c.assign(lhs, parent.Values, e, tracked)
		case *ast.ReturnStmt:
			return "the argument list, or a slice of its cells, is returned"
		}
		break
	}
	if isList {
		return "the argument list is used in a way this rule does not allow"
	}
	return "a slice of the argument list's cells is used in a way this rule does not allow"
}

// assign classifies e as one side of an assignment.
func (c *callListChecker) assign(lhs, rhs []ast.Expr, e ast.Expr, tracked map[types.Object]bool) string {
	for _, l := range lhs {
		if l == e {
			// A write to the list's own Cells or to a tracked local.
			return ""
		}
	}
	if len(lhs) != len(rhs) {
		return "the argument list is assigned from a multi-value expression this rule cannot follow"
	}
	for i, r := range rhs {
		if r != e {
			continue
		}
		switch l := ast.Unparen(lhs[i]).(type) {
		case *ast.Ident:
			if l.Name == "_" {
				return ""
			}
			obj := c.pass.TypesInfo.ObjectOf(l)
			if v, ok := obj.(*types.Var); ok && !v.IsField() && v.Parent() != c.pass.Pkg.Scope() {
				tracked[obj] = true
				return ""
			}
		case *ast.SelectorExpr:
			if x, ok := ast.Unparen(l.X).(*ast.Ident); ok && l.Sel.Name == "Cells" && tracked[c.pass.TypesInfo.Uses[x]] {
				return ""
			}
		}
		return "the argument list, or a slice of its cells, is stored where it outlives the call"
	}
	return "the argument list is used in a way this rule does not allow"
}

// arg classifies e as an argument of call.
func (c *callListChecker) arg(call *ast.CallExpr, e ast.Expr, isList bool) string {
	index := -1
	for i, a := range call.Args {
		if a == e {
			index = i
		}
	}
	if index < 0 {
		return "the argument list is called as a function"
	}
	if id, ok := ast.Unparen(call.Fun).(*ast.Ident); ok {
		if b, ok := c.pass.TypesInfo.Uses[id].(*types.Builtin); ok {
			switch {
			case isList:
			case b.Name() == "len", b.Name() == "cap", b.Name() == "clear", b.Name() == "copy":
				return ""
			case b.Name() == "append" && index > 0:
				// append(dst, cells...) copies the cells out.
				return ""
			}
			return "the argument list, or a slice of its cells, is passed to " + b.Name() + ", whose result aliases it"
		}
	}
	fn := calleeFunc(c.pass, call)
	if fn == nil {
		return "the argument list, or a slice of its cells, is passed to a function value, which this rule cannot read"
	}
	sig := fn.Signature()
	if last := sig.Params().Len() - 1; sig.Variadic() && index >= last {
		if index != last || !call.Ellipsis.IsValid() {
			return "the argument list is passed as a variadic argument of " + fn.Name() + ", which wraps it in a new slice"
		}
	}
	return c.edge(fn.Origin(), index)
}
