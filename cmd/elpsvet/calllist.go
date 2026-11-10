// Copyright © 2026 The ELPS authors

package main

import (
	"go/ast"
	"go/token"
	"go/types"

	"golang.org/x/tools/go/analysis"
)

// callListAnalyzer checks the functions the evaluator trusts to drop their
// argument list: the entries of lisp.callListDroppers (lisp/calllist.go).
// The evaluator hands the list of a direct call of such a function to the
// next call form, so a function that kept the list, or a slice of its
// cells, would read the next call's arguments.
//
// The rule is strict and syntactic.  In a listed function the list
// parameter, and every slice of its Cells, may only be:
//
//   - indexed (args.Cells[i], read or written): a single cell is a value
//     the list does not own, and it may go anywhere;
//   - measured, ranged over or sliced again (len, cap, range, [a:b]);
//   - the source of append's spread or of copy, which copy the cells out;
//   - compared with nil;
//   - bound to a local variable, which is then held to the same rule;
//   - stored back into the list's own Cells (args.Cells = args.Cells[1:]);
//   - passed to a function of package lisp, whose parameter is then held
//     to the same rule.
//
// A field of the list other than Cells (args.Type) may be read or written.
// Anything else is reported: returning the list, storing it or a slice of
// it, capturing it in a function literal, calling or binding a method on
// it, taking the address of one of its fields, or passing it to a function
// this analyzer cannot read.  The rule has no comment
// suppression: a function that needs one keeps its list and leaves
// callListDroppers.
//
// Invisible: reflection and unsafe.  The elpscheck build covers what this
// rule cannot see at run time: it poisons each returned list
// (lisp/calllist_elpscheck.go).
var callListAnalyzer = &analysis.Analyzer{
	Name: "elpscalllist",
	Doc:  "check that each function in lisp.callListDroppers keeps neither its argument list nor a slice of its cells",
	Run:  runCallList,
}

const callListDroppersVar = "callListDroppers"

func runCallList(pass *analysis.Pass) (interface{}, error) {
	if pass.Pkg.Path() != lispPkgPath {
		return nil, nil
	}
	c := &callListChecker{pass: pass, decls: map[*types.Func]*ast.FuncDecl{}}
	var table *ast.CompositeLit
	for _, file := range pass.Files {
		for _, decl := range file.Decls {
			switch decl := decl.(type) {
			case *ast.FuncDecl:
				if fn, ok := pass.TypesInfo.Defs[decl.Name].(*types.Func); ok {
					c.decls[fn] = decl
				}
			case *ast.GenDecl:
				if decl.Tok != token.VAR {
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
	if table == nil {
		return nil, nil
	}
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
		// An LBuiltin is func(env *LEnv, args *LVal) *LVal: the list is
		// the second parameter.
		// Each entry is checked on its own, helpers included, so a helper
		// that keeps the list is reported for every entry that reaches it.
		c.root, c.seen = fn.Name(), map[callListParam]bool{}
		c.param(fn, 1)
	}
	return nil, nil
}

type callListParam struct {
	fn    *types.Func
	index int
}

type callListChecker struct {
	pass  *analysis.Pass
	decls map[*types.Func]*ast.FuncDecl
	seen  map[callListParam]bool
	// root is the callListDroppers entry being checked, for messages.
	root string
}

func (c *callListChecker) reportf(pos token.Pos, format string, args ...interface{}) {
	args = append(args, c.root, callListDroppersVar)
	c.pass.Reportf(pos, format+" (reached from %s, which %s says drops its argument list); copy the cells out, or take the function out of the list", args...)
}

// param checks every use of fn's parameter at index.  Each parameter is
// checked once for each root, which also ends a recursion.
func (c *callListChecker) param(fn *types.Func, index int) {
	key := callListParam{fn, index}
	if c.seen[key] {
		return
	}
	c.seen[key] = true
	decl := c.decls[fn]
	var names []*ast.Ident
	for _, field := range decl.Type.Params.List {
		if len(field.Names) == 0 {
			names = append(names, nil)
			continue
		}
		names = append(names, field.Names...)
	}
	if index >= len(names) || names[index] == nil || names[index].Name == "_" || decl.Body == nil {
		return
	}
	obj := c.pass.TypesInfo.Defs[names[index]]
	if obj == nil {
		return
	}
	tracked := map[types.Object]bool{obj: true}
	// A local variable bound to the list joins the tracked set, and its
	// uses may bind more.  Walk until the set is stable, then report.
	for n := -1; n != len(tracked); {
		n = len(tracked)
		c.walk(decl, tracked, false)
	}
	c.walk(decl, tracked, true)
}

func (c *callListChecker) walk(decl *ast.FuncDecl, tracked map[types.Object]bool, report bool) {
	var stack []ast.Node
	ast.Inspect(decl.Body, func(n ast.Node) bool {
		if n == nil {
			stack = stack[:len(stack)-1]
			return true
		}
		if id, ok := n.(*ast.Ident); ok && tracked[c.pass.TypesInfo.Uses[id]] {
			if msg := c.use(id, stack, tracked); msg != "" && report {
				c.reportf(id.Pos(), "%s", msg)
			}
		}
		stack = append(stack, n)
		return true
	})
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
			if sel := c.pass.TypesInfo.Selections[parent]; sel == nil || sel.Kind() != types.FieldVal {
				return "a method is called on the argument list, or bound to it, and the method may keep it"
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
	if fn == nil || c.decls[fn] == nil {
		name := "a function value"
		if fn != nil {
			name = fn.FullName()
		}
		return "the argument list, or a slice of its cells, is passed to " + name + ", which this rule cannot read"
	}
	sig := fn.Signature()
	if last := sig.Params().Len() - 1; sig.Variadic() && index >= last {
		if index != last || !call.Ellipsis.IsValid() {
			return "the argument list is passed as a variadic argument of " + fn.Name() + ", which wraps it in a new slice"
		}
	}
	c.param(fn, index)
	return ""
}
