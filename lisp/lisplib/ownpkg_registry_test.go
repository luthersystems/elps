// Copyright © 2026 The ELPS authors

package lisplib_test

import (
	"sort"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// Issue #736: special operators are syntax.  They receive the caller's
// unevaluated forms and lexical environment and always act in the caller's
// package, so only the language defines them.  Every Go function registered
// anywhere in the standard library that is a special operator must have been
// defined in package lisp -- a library that re-exports one (testing:test is
// lisp:test) holds lisp's own value.
func TestNoLibrarySpecialOps(t *testing.T) {
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	require.NoError(t, lisp.GoError(lisp.InitializeUserEnv(env)))
	require.NoError(t, lisp.GoError(lisplib.LoadLibrary(env)))

	lang := env.Runtime.Registry.Lang
	macros := map[string][]string{}
	var nops int
	for _, pkgName := range env.Runtime.Registry.PackageNames() {
		pkg := env.Runtime.Registry.Package(pkgName)
		for _, name := range pkg.SymbolNames() {
			v := pkg.Get(lisp.Symbol(name))
			if v.Type != lisp.LFun || v.Builtin() == nil {
				continue
			}
			if v.IsSpecialOp() {
				nops++
				assert.Equal(t, lang, v.Package(),
					"%s:%s is a special operator defined in package %q; only package %s may define one",
					pkgName, name, v.Package(), lang)
			}
			if v.IsMacro() && v.Package() == pkgName && pkgName != lang {
				macros[pkgName] = append(macros[pkgName], name)
			}
		}
	}
	require.NotZero(t, nops, "found no special operators at all; the scan is broken")
	for _, names := range macros {
		sort.Strings(names)
	}
	// The Go macros libraries define.  A library Go macro expands in its own
	// package and its expansion is evaluated in the caller, so it may only
	// build forms; a new one is added here deliberately.
	assert.Equal(t, map[string][]string{
		"testing": {"assert-equal", "assert-nil", "assert-not", "assert-not-nil", "assert-string=", "assert="},
	}, macros)
}
