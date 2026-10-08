// Copyright © 2026 The ELPS authors

package main

import (
	"go/ast"
	"go/types"

	"golang.org/x/tools/go/analysis"
)

// calleeFunc resolves a call's callee to its *types.Func, so package aliases
// and dot imports resolve like the compiler resolves them rather than by
// matching the source text.  An EXPLICITLY instantiated generic --
// lisp.NativeOf[*Handle](h) -- wraps the callee in an index expression
// (IndexExpr for one type argument, IndexListExpr for several), which is
// unwrapped first; missing that would leave a spelling the rules cannot see.
func calleeFunc(pass *analysis.Pass, call *ast.CallExpr) *types.Func {
	fun := ast.Unparen(call.Fun)
	switch idx := fun.(type) {
	case *ast.IndexExpr:
		fun = ast.Unparen(idx.X)
	case *ast.IndexListExpr:
		fun = ast.Unparen(idx.X)
	}
	var id *ast.Ident
	switch fun := fun.(type) {
	case *ast.Ident:
		id = fun
	case *ast.SelectorExpr:
		id = fun.Sel
	default:
		return nil
	}
	fn, _ := pass.TypesInfo.Uses[id].(*types.Func)
	return fn
}
