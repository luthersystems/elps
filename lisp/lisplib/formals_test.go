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

	pkgNames := env.Runtime.Registry.PackageNames()

	nfun := 0
	for _, pkgName := range pkgNames {
		pkg := env.Runtime.Registry.Package(pkgName)
		symNames := pkg.SymbolNames()

		for _, sym := range symNames {
			v, _ := pkg.Symbol(sym)
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
// constructed environments never share a MUTABLE formal-argument LVal for any
// function either of them defines.  Sealed sharing is the sanctioned design;
// unsealed sharing is the issue-#363 bug.
//
// Nearly every builtin, special operator and macro in the process is described
// by an LBuiltinDef held in a package-level table built once at Go package
// initialization: lisp's own langBuiltins/langSpecialOps/langMacros, and a
// `var builtins = []*libutil.Builtin{...}` in libtime, libregexp, libhelp,
// libschema, libmath, libbase64 and libstring.  Each entry's formals were
// constructed once, by lisp.Formals, at that moment.  Before issue #363 was
// addressed, LEnv.AddBuiltins/AddSpecialOps/AddMacros installed that very
// *LVal into the function value with no protection at all, so every
// environment in the process shared one MUTABLE formals object per definition
// -- time:sleep, math:atan and lisp:map among them.  One in-place write to a
// formals cell was a cross-environment correctness bug and, for the embedders
// that run many environments concurrently, a data race.  See issue #363;
// issue #362 is the same class of assumption, written to.
//
// Today the tables' formals are SEALED at construction (sealDefaultFormals,
// the libutil constructors) and registration aliases the sealed template into
// each environment (registrationFormals, lisp/env.go; issues #379, #514) --
// the same topology lisp-defined functions have always had, whose formals are
// sealed parser output aliased into every closure.  A sealed value is
// immutable by contract (copy-on-write guards, the -race seal watchdog, the
// checked-mode fingerprint verifier), so pointer sharing of a sealed list is
// safe by design and REQUIRED here as the anti-vacuity floor.  What must
// never happen is two environments holding the same UNSEALED formals -- that
// is the #363 shape, and any def whose formals escape sealing must get a
// private deep copy (formalsCopier, lisp/defformals.go, issue #513).
//
// The sweep covers every function in both registries rather than a hand-picked
// sample, so a new package-level table cannot reintroduce the class unnoticed.
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
	nsealedShared := 0
	for _, name := range names {
		b, ok := funsB[name]
		if !ok {
			continue
		}
		a := funsA[name]
		fa, fb := a.Cells[0], b.Cells[0]
		if fa == fb {
			// Sharing one formals object is legal exactly when it is sealed
			// all the way down: immutability, not per-env confinement, is
			// what protects it (registrationFormals in lisp/env.go).
			require.Truef(t, fa.IsSealed(),
				"%s: both environments hold the SAME UNSEALED formals object %p; a write through either is visible in the other (issue #363)",
				name, fa)
			for i, cell := range fa.Cells {
				require.Truef(t, cell.IsSealed(),
					"%s: shared formals %p is sealed but its cell %d (%v) is not; a write to the cell corrupts every environment (issue #363)",
					name, fa, i, cell)
			}
			nsealedShared++
			nchecked++
			continue
		}
		// Distinct formals objects must be deeply private: a shallow copy
		// still shares the mutable parameter symbols.
		require.Lenf(t, fb.Cells, len(fa.Cells),
			"%s: formals differ in length between environments", name)
		for i := range fa.Cells {
			if fa.Cells[i].IsSealed() && fa.Cells[i] == fb.Cells[i] {
				continue // sealed leaf sharing is as safe as sealed list sharing
			}
			require.NotSamef(t, fa.Cells[i], fb.Cells[i],
				"%s: both environments hold the SAME mutable formal argument %d (%v); the copy is shallow (issue #363)",
				name, i, fa.Cells[i])
		}
		nchecked++
	}

	// Guard against the sweep silently finding nothing, and against the
	// sealed-sharing path silently disappearing: every definition table in
	// this repository seals its formals, so a fully loaded environment must
	// alias hundreds of sealed lists.  A collapse to zero means either the
	// walk went blind or registration quietly stopped sharing -- both worth
	// failing loudly over.
	require.NotZero(t, nchecked, "found no functions to check; the registry walk is broken")
	require.GreaterOrEqualf(t, nsealedShared, 100,
		"only %d of %d functions share a sealed formals template; the sealed-sharing registration path (registrationFormals) is not being exercised",
		nsealedShared, nchecked)
	t.Logf("checked the formals of %d functions defined in both environments (%d sealed-shared)", nchecked, nsealedShared)
}

// registeredFuns returns every function bound in env's registry, keyed by its
// package-qualified name.
func registeredFuns(env *lisp.LEnv) map[string]*lisp.LVal {
	funs := make(map[string]*lisp.LVal)
	for _, pkgName := range env.Runtime.Registry.PackageNames() {
		pkg := env.Runtime.Registry.Package(pkgName)
		if pkg == nil {
			continue
		}
		for _, sym := range pkg.SymbolNames() {
			v, _ := pkg.Symbol(sym)
			if v == nil || v.Type != lisp.LFun || len(v.Cells) == 0 || v.Cells[0] == nil {
				continue
			}
			funs[pkgName+":"+sym] = v
		}
	}
	return funs
}
