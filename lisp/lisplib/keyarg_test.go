// Copyright © 2026 The ELPS authors

package lisplib_test

import (
	"fmt"
	"sort"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/stretchr/testify/require"
)

// TestKeyArgBuiltinsTolerateShortArgLists asserts that no builtin registered
// with an &key formal panics when it is handed fewer argument cells than its
// own formals declare.
//
// The evaluator always passes one cell per named formal, so this cannot happen
// on any path that starts in lisp. It happens when an embedder binds one of
// the exported Builtin functions to formals of its own and gets the count
// wrong -- which is easy to do precisely because an &key argument is invisible
// at the lisp call site, and impossible to catch at compile time because the
// Go signature is LBuiltin either way.
//
// That is not hypothetical. luthersystems/substrate bound libtime.BuiltinSleep
// as `cc:sleep` with lisp.Formals("seconds"), a single cell. When
// luthersystems/elps#346 gave `time:sleep` its optional :max keyword,
// BuiltinSleep began reading Cells[1] and every `cc:sleep` call started
// panicking with an index-out-of-range, surfaced as internal-panic. Nothing in
// elps failed to compile and nothing in elps's own tests failed.
//
// The contract this pins: an absent cell reads as Nil, the same value the
// evaluator passes for an unsupplied &key argument, via LVal.KeyArg. A builtin
// that indexes Cells directly for an optional argument fails this test.
func TestKeyArgBuiltinsTolerateShortArgLists(t *testing.T) {
	env, err := lisplib.NewDocEnv()
	require.NoError(t, err)

	type builtin struct {
		qualified string
		formals   *lisp.LVal
		fn        lisp.LBuiltin
	}
	var keyed []builtin

	pkgNames := make([]string, 0, len(env.Runtime.Registry.Packages))
	for name := range env.Runtime.Registry.Packages {
		pkgNames = append(pkgNames, name)
	}
	sort.Strings(pkgNames)

	for _, pkgName := range pkgNames {
		pkg := env.Runtime.Registry.Packages[pkgName]
		symNames := make([]string, 0, len(pkg.Symbols))
		for sym := range pkg.Symbols {
			symNames = append(symNames, sym)
		}
		sort.Strings(symNames)

		for _, sym := range symNames {
			v := pkg.Symbols[sym]
			if v == nil || v.Type != lisp.LFun || v.Builtin() == nil {
				continue
			}
			// An LFun keeps its formal list in Cells[0].
			if len(v.Cells) == 0 {
				continue
			}
			formals := v.Cells[0]
			if !hasKeyArg(formals) {
				continue
			}
			keyed = append(keyed, builtin{
				qualified: pkgName + ":" + sym,
				formals:   formals,
				fn:        v.Builtin(),
			})
		}
	}

	// Guard against the sweep silently finding nothing -- a registry walk that
	// matches zero builtins would pass this test while asserting nothing at
	// all. time:sleep and the libjson pair are the known members; if the
	// registry shape changes so that none are found, fail rather than pass.
	require.NotEmpty(t, keyed, "found no &key builtins to check; the registry walk is broken")
	t.Logf("checking %d builtins registered with &key formals", len(keyed))

	for _, b := range keyed {
		t.Run(b.qualified, func(t *testing.T) {
			// The evaluator passes one cell per NAMED formal; the &key marker
			// itself is not an argument.
			full := namedFormalCount(b.formals)
			for n := 0; n < full; n++ {
				n := n
				t.Run(fmt.Sprintf("%d_of_%d_cells", n, full), func(t *testing.T) {
					cells := make([]*lisp.LVal, n)
					for i := range cells {
						cells[i] = lisp.Nil()
					}
					// A short list is an embedder error, so any outcome is
					// acceptable -- an error LVal, or a value. The one
					// unacceptable outcome is a panic, which the evaluator can
					// only report as an opaque internal-panic with no argument
					// attached.
					require.NotPanics(t, func() {
						_ = b.fn(env, lisp.SExpr(cells))
					}, "%s panicked when called with %d of %d argument cells",
						b.qualified, n, full)
				})
			}
		})
	}
}

// hasKeyArg reports whether a formal list contains the &key marker.
func hasKeyArg(formals *lisp.LVal) bool {
	if formals == nil {
		return false
	}
	for _, c := range formals.Cells {
		if c != nil && c.Type == lisp.LSymbol && c.Str == lisp.KeyArgSymbol {
			return true
		}
	}
	return false
}

// namedFormalCount returns the number of argument cells the evaluator supplies
// for a formal list, i.e. every named formal, excluding the &key and &optional
// markers themselves.
func namedFormalCount(formals *lisp.LVal) int {
	n := 0
	for _, c := range formals.Cells {
		if c == nil || c.Type != lisp.LSymbol {
			continue
		}
		if c.Str == lisp.KeyArgSymbol || c.Str == lisp.OptArgSymbol || c.Str == lisp.VarArgSymbol {
			continue
		}
		n++
	}
	return n
}
