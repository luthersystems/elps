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

// TestOptionalArgBuiltinsTolerateShortArgLists asserts two things about every
// builtin registered with an &optional or &key formal, for every argument list
// shorter than its own formals declare:
//
//   - it does not panic, and
//   - when the missing cell is a REQUIRED argument, it returns an error rather
//     than an answer.
//
// The evaluator always passes one cell per named formal, so a short list cannot
// arise from lisp. It arises when an embedder binds one of the exported Builtin
// functions to formals of its own and gets the count wrong -- easy to do
// precisely because an &optional or &key argument is invisible at the lisp call
// site, and impossible to catch at compile time because the Go signature is
// LBuiltin either way.
//
// That is not hypothetical. luthersystems/substrate bound libtime.BuiltinSleep
// as `cc:sleep` with lisp.Formals("seconds"), a single cell. When
// luthersystems/elps#346 gave `time:sleep` its optional :max keyword,
// BuiltinSleep began reading Cells[1] and every `cc:sleep` call panicked with an
// index-out-of-range, surfaced as internal-panic. Nothing in elps failed to
// compile and nothing in elps's own tests failed.
//
// The second assertion is the one that matters most, and it is not redundant
// with the first. An earlier version of this fix read every cell through
// KeyArg, which reports an absent cell as Nil. That removed the panic -- and
// silently turned `json:dump-string` bound with no formals into a successful
// "null", because Nil is a legal thing to dump. Trading a panic for a wrong
// answer is not a fix. Required arguments now go through ReqArg, which raises
// CondMissingArgument, and this test pins that distinction.
func TestOptionalArgBuiltinsTolerateShortArgLists(t *testing.T) {
	env, err := lisplib.NewDocEnv()
	require.NoError(t, err)

	type builtin struct {
		qualified string
		formals   *lisp.LVal
		fn        lisp.LBuiltin
	}
	var flexible []builtin

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
			if !hasFlexibleArg(formals) {
				continue
			}
			flexible = append(flexible, builtin{
				qualified: pkgName + ":" + sym,
				formals:   formals,
				fn:        v.Builtin(),
			})
		}
	}

	// Guard against the sweep silently finding nothing -- a registry walk that
	// matches zero builtins would pass this test while asserting nothing at
	// all. time:sleep, math:atan, help:help-package-symbols and the libjson
	// pair are the known members; if the registry shape changes so that none
	// are found, fail rather than pass.
	require.NotEmpty(t, flexible, "found no &optional/&key builtins to check; the registry walk is broken")
	t.Logf("checking %d builtins registered with &optional or &key formals", len(flexible))

	for _, b := range flexible {
		t.Run(b.qualified, func(t *testing.T) {
			// The evaluator passes one cell per NAMED formal; the &optional and
			// &key markers are not themselves arguments.
			full := namedFormalCount(b.formals)
			required := requiredFormalCount(b.formals)
			for n := range full {
				t.Run(fmt.Sprintf("%d_of_%d_cells", n, full), func(t *testing.T) {
					cells := make([]*lisp.LVal, n)
					for i := range cells {
						cells[i] = lisp.Nil()
					}
					var got *lisp.LVal
					require.NotPanics(t, func() {
						got = b.fn(env, lisp.SExpr(cells))
					}, "%s panicked when called with %d of %d argument cells",
						b.qualified, n, full)

					if n >= required {
						// Every optional cell is absent, which is a legitimate
						// call. Any outcome is acceptable.
						return
					}
					// A required argument is missing. Returning a value here
					// would be a silent wrong answer, which is worse than the
					// panic this change removed.
					require.Equalf(t, lisp.LError, got.Type,
						"%s answered %v with %d of %d cells, but %d argument(s) are required;"+
							" a missing required argument must raise an error, not produce a value",
						b.qualified, got, n, full, required)
					require.Equalf(t, lisp.CondMissingArgument, got.Str,
						"%s raised condition %q, want %q so callers can handler-bind it",
						b.qualified, got.Str, lisp.CondMissingArgument)
				})
			}
		})
	}
}

// hasFlexibleArg reports whether a formal list contains an &optional or &key
// marker, i.e. whether a caller may legitimately supply fewer cells than the
// list names.
func hasFlexibleArg(formals *lisp.LVal) bool {
	if formals == nil {
		return false
	}
	for _, c := range formals.Cells {
		if c == nil || c.Type != lisp.LSymbol {
			continue
		}
		if c.Str == lisp.KeyArgSymbol || c.Str == lisp.OptArgSymbol {
			return true
		}
	}
	return false
}

// namedFormalCount returns the number of argument cells the evaluator supplies
// for a formal list, i.e. every named formal, excluding the markers themselves.
func namedFormalCount(formals *lisp.LVal) int {
	n := 0
	for _, c := range formals.Cells {
		if c == nil || c.Type != lisp.LSymbol {
			continue
		}
		if isFormalMarker(c.Str) {
			continue
		}
		n++
	}
	return n
}

// requiredFormalCount returns the number of formals appearing before the first
// &optional, &key or &rest marker -- the arguments a caller must supply.
func requiredFormalCount(formals *lisp.LVal) int {
	n := 0
	for _, c := range formals.Cells {
		if c == nil || c.Type != lisp.LSymbol {
			continue
		}
		if isFormalMarker(c.Str) {
			return n
		}
		n++
	}
	return n
}

func isFormalMarker(s string) bool {
	return s == lisp.KeyArgSymbol || s == lisp.OptArgSymbol || s == lisp.VarArgSymbol
}
