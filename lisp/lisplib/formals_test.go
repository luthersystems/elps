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

// TestRegisteredFormalsAreNotSharedAcrossEnvs asserts that two independently
// constructed environments do not share a single formal-argument LVal for any
// function either of them defines.
//
// Nearly every builtin, special operator and macro in the process is described
// by an LBuiltinDef held in a package-level table built once at Go package
// initialization: lisp's own langBuiltins/langSpecialOps/langMacros, and a
// `var builtins = []*libutil.Builtin{...}` in libtime, libregexp, libhelp,
// libschema, libmath, libbase64 and libstring.  Each entry's formals were
// constructed once, by lisp.Formals, at that moment.  Before this test's fix,
// LEnv.AddBuiltins/AddSpecialOps/AddMacros installed that very *LVal into the
// function value, so every environment in the process shared one formals
// object per definition -- time:sleep, math:atan and lisp:map among them,
// while json:dump-string escaped only because libjson happens to build its
// table from a function called per load.  One in-place write to a formals cell
// would then be a cross-environment correctness bug and, for the embedders that
// run many environments concurrently, a data race.  See issue #363; issue #362
// is the same class of assumption, written to.
//
// The sweep covers every function in both registries rather than a hand-picked
// sample, so a new package-level table cannot reintroduce the class unnoticed.
// It compares pointers, which is a stronger statement than "mutating one does
// not disturb the other" and does not depend on finding a mutable field.
func TestRegisteredFormalsAreNotSharedAcrossEnvs(t *testing.T) {
	envA, err := lisplib.NewDocEnv()
	require.NoError(t, err)
	envB, err := lisplib.NewDocEnv()
	require.NoError(t, err)

	funsA := registeredFuns(envA)
	funsB := registeredFuns(envB)

	// The exact reproduction named in issue #363.  These are asserted by name
	// so that a registry walk which stops finding them -- a renamed package, a
	// changed loader -- fails loudly instead of sweeping zero of the functions
	// the bug was reported against.
	reported := []string{"time:sleep", "math:atan", "lisp:map", "json:dump-string"}
	for _, name := range reported {
		require.Contains(t, funsA, name, "issue #363 names %s, but the sweep did not find it", name)
	}

	names := make([]string, 0, len(funsA))
	for name := range funsA {
		names = append(names, name)
	}
	sort.Strings(names)

	nchecked := 0
	for _, name := range names {
		b, ok := funsB[name]
		if !ok {
			continue
		}
		a := funsA[name]
		fa, fb := a.Cells[0], b.Cells[0]
		require.NotSamef(t, fa, fb,
			"%s: both environments hold the SAME formals object %p; a write through either is visible in the other (issue #363)",
			name, fa)
		require.Lenf(t, fb.Cells, len(fa.Cells),
			"%s: formals differ in length between environments", name)
		for i := range fa.Cells {
			require.NotSamef(t, fa.Cells[i], fb.Cells[i],
				"%s: both environments hold the SAME formal argument %d (%v); the copy is shallow (issue #363)",
				name, i, fa.Cells[i])
		}
		nchecked++
	}

	// Guard against the sweep silently finding nothing.
	require.NotZero(t, nchecked, "found no functions to check; the registry walk is broken")
	t.Logf("checked the formals of %d functions defined in both environments", nchecked)
}

// registeredFuns returns every function bound in env's registry, keyed by its
// package-qualified name.
func registeredFuns(env *lisp.LEnv) map[string]*lisp.LVal {
	funs := make(map[string]*lisp.LVal)
	for pkgName, pkg := range env.Runtime.Registry.Packages {
		if pkg == nil {
			continue
		}
		for sym, v := range pkg.Symbols {
			if v == nil || v.Type != lisp.LFun || len(v.Cells) == 0 || v.Cells[0] == nil {
				continue
			}
			funs[pkgName+":"+sym] = v
		}
	}
	return funs
}
