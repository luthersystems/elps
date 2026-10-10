// Copyright © 2026 The ELPS authors

package main

// Part of elpsfreshness: a Func1E, Func2E or Func3E body must return a fresh
// slice.  The builtin makes a []byte, []*LVal or lisp.Cells result the new
// value's storage (lisp.Bytes, lisp.QExpr), so a body that returns an
// argument's storage makes the result alias the caller's value.  A Text or
// []byte argument is the argument's own bytes, and a *LVal argument's Cells
// or Bytes() are its storage.
//
// Reported: a return whose first result is rooted at a parameter of the
// body other than env, through parentheses, slicing, a conversion between
// slice types, a field selection or a method call (p, p[1:], lisp.Cells(p),
// p.Cells, p.Bytes()).  Bodies seen: a function literal passed to the
// binding, and a function declared in the same package.  Suppress a
// deliberate alias with //elps:aliases <reason> on the return line or the
// line above.
//
// Invisible: an alias held in a local variable first, a helper that returns
// one, and storage reached through a global.

import (
	"go/ast"
	"go/types"

	"golang.org/x/tools/go/analysis"
)

// typedResultBindings are the lisp typed bindings whose body returns a
// result elps converts to an LVal.
var typedResultBindings = map[string]bool{"Func1E": true, "Func2E": true, "Func3E": true}

func checkTypedResults(pass *analysis.Pass) {
	decls := make(map[*types.Func]*ast.FuncDecl)
	for _, file := range pass.Files {
		for _, decl := range file.Decls {
			if fd, ok := decl.(*ast.FuncDecl); ok {
				if fn, ok := pass.TypesInfo.Defs[fd.Name].(*types.Func); ok {
					decls[fn] = fd
				}
			}
		}
	}
	for _, file := range pass.Files {
		allow := markerLines(pass.Fset, file, aliasesMarker)
		ast.Inspect(file, func(n ast.Node) bool {
			call, ok := n.(*ast.CallExpr)
			if !ok || len(call.Args) == 0 {
				return true
			}
			fn := calleeFunc(pass, call)
			if fn == nil || fn.Pkg() == nil || fn.Pkg().Path() != lispPkgPath || !typedResultBindings[fn.Name()] {
				return true
			}
			var ftype *ast.FuncType
			var body *ast.BlockStmt
			switch b := ast.Unparen(call.Args[len(call.Args)-1]).(type) {
			case *ast.FuncLit:
				ftype, body = b.Type, b.Body
			case *ast.Ident:
				if f, ok := pass.TypesInfo.Uses[b].(*types.Func); ok {
					if fd := decls[f.Origin()]; fd != nil && fd.Recv == nil {
						ftype, body = fd.Type, fd.Body
					}
				}
			}
			if body != nil {
				checkTypedResultBody(pass, fn.Name(), ftype, body, allow)
			}
			return true
		})
	}
}

//nolint:revive // each argument is a separate fact about one binding
func checkTypedResultBody(pass *analysis.Pass, binding string, ftype *ast.FuncType, body *ast.BlockStmt, allow map[int]bool) {
	if ftype.Results == nil || len(ftype.Results.List) == 0 || !storageResult(pass.TypesInfo.TypeOf(ftype.Results.List[0].Type)) {
		return
	}
	params := make(map[types.Object]bool)
	first := true
	for _, field := range ftype.Params.List {
		for _, name := range field.Names {
			if first {
				first = false // env
				continue
			}
			if obj := pass.TypesInfo.Defs[name]; obj != nil {
				params[obj] = true
			}
		}
		if len(field.Names) == 0 {
			first = false
		}
	}
	ast.Inspect(body, func(n ast.Node) bool {
		switch x := n.(type) {
		case *ast.FuncLit:
			return false // a nested function's returns are its own
		case *ast.ReturnStmt:
			if len(x.Results) == 0 {
				return true
			}
			obj := rootParam(pass, x.Results[0])
			if obj == nil || !params[obj] {
				return true
			}
			if allow[pass.Fset.Position(x.Pos()).Line] {
				return true
			}
			pass.Reportf(x.Results[0].Pos(),
				"%s body returns the storage of argument %s; a []byte, []*LVal or Cells result becomes the new value's storage,"+
					" so return a fresh slice (copy it), or annotate //elps:aliases <reason>", binding, obj.Name())
		}
		return true
	})
}

// storageResult reports whether t is a result type whose slice the builtin
// keeps as storage: []byte, []*lisp.LVal or lisp.Cells.
func storageResult(t types.Type) bool {
	if t == nil {
		return false
	}
	if isLispNamed(t, "Cells") {
		return true
	}
	s, ok := types.Unalias(t).(*types.Slice)
	if !ok {
		return false
	}
	if b, ok := s.Elem().(*types.Basic); ok && b.Kind() == types.Byte {
		return true
	}
	return isLValPtr(s.Elem())
}

// rootParam returns the variable at the root of e, through parentheses,
// slicing, slice conversions, field selections and method calls.
func rootParam(pass *analysis.Pass, e ast.Expr) types.Object {
	for {
		switch x := ast.Unparen(e).(type) {
		case *ast.Ident:
			if v, ok := pass.TypesInfo.Uses[x].(*types.Var); ok {
				return v
			}
			return nil
		case *ast.SliceExpr:
			e = x.X
		case *ast.SelectorExpr:
			if pass.TypesInfo.Selections[x] == nil {
				return nil
			}
			e = x.X
		case *ast.CallExpr:
			if tv, ok := pass.TypesInfo.Types[x.Fun]; ok && tv.IsType() && len(x.Args) == 1 {
				// A conversion: only a slice-to-slice conversion keeps the
				// storage; string to []byte copies.
				if _, ok := pass.TypesInfo.TypeOf(x.Args[0]).Underlying().(*types.Slice); !ok {
					return nil
				}
				e = x.Args[0]
				continue
			}
			sel, ok := ast.Unparen(x.Fun).(*ast.SelectorExpr)
			if !ok || pass.TypesInfo.Selections[sel] == nil {
				return nil
			}
			e = sel.X
		default:
			return nil
		}
	}
}
