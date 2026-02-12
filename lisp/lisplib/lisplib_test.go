// Copyright © 2024 The ELPS authors

package lisplib_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/lisp/lisplib/libbase64"
	"github.com/luthersystems/elps/lisp/lisplib/libgolang"
	"github.com/luthersystems/elps/lisp/lisplib/libhelp"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
	"github.com/luthersystems/elps/lisp/lisplib/libmath"
	"github.com/luthersystems/elps/lisp/lisplib/libregexp"
	"github.com/luthersystems/elps/lisp/lisplib/libschema"
	"github.com/luthersystems/elps/lisp/lisplib/libstring"
	"github.com/luthersystems/elps/lisp/lisplib/libtesting"
	"github.com/luthersystems/elps/lisp/lisplib/libtime"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestLoadLibrary_EachPackageRestoresPrevious(t *testing.T) {
	// Verify that calling each stdlib LoadPackage individually restores the
	// previous active package. This is the fix for
	// https://github.com/luthersystems/elps/issues/99.
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	env.Runtime.Library = &lisp.RelativeFileSystemLibrary{}
	rc := lisp.InitializeUserEnv(env)
	require.True(t, rc.IsNil())
	require.Equal(t, "user", env.Runtime.Package.Name)

	loaders := []struct {
		name string
		load func(*lisp.LEnv) *lisp.LVal
	}{
		{"time", libtime.LoadPackage},
		{"help", libhelp.LoadPackage},
		{"golang", libgolang.LoadPackage},
		{"math", libmath.LoadPackage},
		{"string", libstring.LoadPackage},
		{"base64", libbase64.LoadPackage},
		{"json", libjson.LoadPackage},
		{"regexp", libregexp.LoadPackage},
		{"testing", libtesting.LoadPackage},
		{"schema", libschema.LoadPackage},
	}
	for _, l := range loaders {
		t.Run(l.name, func(t *testing.T) {
			before := env.Runtime.Package.Name
			rc := l.load(env)
			require.Truef(t, rc.IsNil(), "LoadPackage(%s) failed: %v", l.name, rc)
			assert.Equalf(t, before, env.Runtime.Package.Name,
				"LoadPackage(%s) should restore the previous package", l.name)
		})
	}
}

func TestNewDocEnv(t *testing.T) {
	env, err := lisplib.NewDocEnv()
	require.NoError(t, err)
	require.NotNil(t, env)

	// Should have all stdlib packages loaded.
	expectedPkgs := []string{
		"lisp", "user", "time", "help", "golang", "math",
		"string", "base64", "json", "regexp", "testing", "s",
	}
	for _, name := range expectedPkgs {
		assert.NotNilf(t, env.Runtime.Registry.Packages[name],
			"NewDocEnv should include package %q", name)
	}

	// Should be in the user package.
	assert.Equal(t, lisp.DefaultUserPackage, env.Runtime.Package.Name)
}

func TestNewDocEnv_CanLookupSymbols(t *testing.T) {
	env, err := lisplib.NewDocEnv()
	require.NoError(t, err)

	// Look up a stdlib function (not a core builtin) to verify LoadLibrary ran.
	v := env.Get(lisp.Symbol("math:sin"))
	require.Equal(t, lisp.LFun, v.Type, "should resolve 'math:sin' as a function")
	assert.NotEmpty(t, v.Docstring(), "math:sin should have a docstring")

	// Also verify a core builtin works.
	v2 := env.Get(lisp.Symbol("map"))
	assert.Equal(t, lisp.LFun, v2.Type, "should resolve 'map' as a function")
}
