// Copyright © 2026 The ELPS authors

package lisplib_test

import (
	"sort"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/stretchr/testify/require"
)

// TestRegisteredFunctionsHaveWellFormedFormals asserts that every function
// reachable in a fully loaded environment carries a formal argument list
// LEnv.bind can actually walk.
//
// bind reads fun.Cells[0].Cells and then argSym.Str on each cell without
// checking either, because on every path that starts in lisp the formals came
// from the reader or from lisp.Formals and cannot be otherwise. They can be
// otherwise when a Go caller supplies them: a registration that passes a nil
// *LVal for "takes no arguments" -- rather than lisp.Formals(), the empty
// list -- succeeds, and is first dereferenced when a user calls the function,
// surfacing as an opaque internal-panic with nothing pointing at the cause.
// That is luthersystems/elps#351, fixed at the elpsutil boundary; this sweep
// is the standing check that no definition inside elps itself acquires the
// same shape.
//
// The properties pinned here are exactly the ones bind assumes: the formals
// are a non-nil list, and every cell in it is a non-nil symbol.
func TestRegisteredFunctionsHaveWellFormedFormals(t *testing.T) {
	env, err := lisplib.NewDocEnv()
	require.NoError(t, err)

	pkgNames := make([]string, 0, len(env.Runtime.Registry.Packages))
	for name := range env.Runtime.Registry.Packages {
		pkgNames = append(pkgNames, name)
	}
	sort.Strings(pkgNames)

	nfun := 0
	for _, pkgName := range pkgNames {
		pkg := env.Runtime.Registry.Packages[pkgName]
		symNames := make([]string, 0, len(pkg.Symbols))
		for sym := range pkg.Symbols {
			symNames = append(symNames, sym)
		}
		sort.Strings(symNames)

		for _, sym := range symNames {
			v := pkg.Symbols[sym]
			if v == nil || v.Type != lisp.LFun {
				continue
			}
			nfun++
			qualified := pkgName + ":" + sym
			// An LFun keeps its formal list in Cells[0].
			require.NotEmpty(t, v.Cells, "%s has no formals cell", qualified)
			formals := v.Cells[0]
			require.NotNil(t, formals,
				"%s was registered with nil formals; use lisp.Formals() to declare a function that takes no arguments",
				qualified)
			require.Equal(t, lisp.LSExpr, formals.Type,
				"%s was registered with formals of type %v, not a list", qualified, formals.Type)
			for i, cell := range formals.Cells {
				require.NotNil(t, cell, "%s formal argument %d is nil", qualified, i)
				require.Equal(t, lisp.LSymbol, cell.Type,
					"%s formal argument %d is a %v, not a symbol", qualified, i, cell.Type)
			}
		}
	}

	// Guard against the sweep silently finding nothing -- a registry walk that
	// matches zero functions would pass this test while asserting nothing.
	require.NotZero(t, nfun, "found no functions to check; the registry walk is broken")
	t.Logf("checked the formals of %d registered functions", nfun)
}
