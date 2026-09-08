// Copyright © 2024 The ELPS authors

package lisplib_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/lisp/lisplib/libbase64"
	"github.com/luthersystems/elps/lisp/lisplib/libelpspath"
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
		{"elpspath", libelpspath.LoadPackage},
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
		"string", "base64", "json", "regexp", "elpspath", "testing", "s",
	}
	for _, name := range expectedPkgs {
		assert.NotNilf(t, env.Runtime.Registry.Package(name),
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

func TestLoadRuntimeLibraryKeepsTestingPerVM(t *testing.T) {
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	require.True(t, lisp.InitializeUserEnv(env).IsNil())
	require.True(t, lisplib.LoadRuntimeLibrary(env).IsNil())
	for _, name := range []string{"lisp", "user", "time", "help", "golang", "math", "string", "base64", "json", "regexp", "elpspath", "s"} {
		require.NotNil(t, env.Runtime.Registry.Package(name), name)
	}
	require.Nil(t, env.Runtime.Registry.Package("testing"))
	require.Equal(t, lisp.DefaultUserPackage, env.Runtime.Package.Name)
	tmpl, err := lisp.NewTemplate(env, lisp.TemplateWithBuiltinPolicy(func(*lisp.LVal) bool { return true }))
	require.NoError(t, err)
	first, err := tmpl.NewVM()
	require.NoError(t, err)
	second, err := tmpl.NewVM()
	require.NoError(t, err)
	for _, vm := range []*lisp.LEnv{first, second} {
		require.True(t, libtesting.LoadPackage(vm).IsNil())
		require.True(t, vm.LoadString("tests.lisp", `(testing:test "private" (testing:assert-equal 42 (+ 40 2)))`).IsNil())
		suite := libtesting.EnvTestSuite(vm)
		require.Equal(t, []string{"private"}, suite.Tests())
		require.NotEqual(t, lisp.LError, vm.FunCall(suite.Test(0).Fun, lisp.SExpr(nil)).Type)
	}
	require.NotSame(t, libtesting.EnvTestSuite(first), libtesting.EnvTestSuite(second))
	require.Nil(t, env.Runtime.Registry.Package("testing"))
}

func TestLoadLibraryTestingRegistryRejectsTemplate(t *testing.T) {
	env := lisp.NewEnv(nil)
	require.True(t, lisp.InitializeUserEnv(env).IsNil())
	require.True(t, lisplib.LoadLibrary(env).IsNil())
	suite := libtesting.EnvTestSuite(env)
	require.NotNil(t, suite)
	plan, err := lisp.NewTemplate(env, lisp.TemplateWithBuiltinPolicy(func(*lisp.LVal) bool { return true }))
	require.Nil(t, plan)
	require.ErrorContains(t, err, "native *libtesting.TestSuite has no template immutability declaration")
	require.Same(t, suite, libtesting.EnvTestSuite(env))
	require.Empty(t, suite.Tests())
	require.Empty(t, suite.Benchmarks())
}

// #627: moving shared assembly under internal must preserve the public loader's
// final package selection as well as the runtime-only fixture path.
func TestLoadLibrarySelectsDefaultUserPackage(t *testing.T) {
	for _, tc := range []struct {
		name string
		load func(*lisp.LEnv) *lisp.LVal
	}{
		{"public", lisplib.LoadLibrary},
		{"runtime", lisplib.LoadRuntimeLibrary},
	} {
		t.Run(tc.name, func(t *testing.T) {
			env := lisp.NewEnv(nil)
			env.Runtime.Reader = parser.NewReader()
			require.True(t, lisp.InitializeUserEnv(env).IsNil())
			require.True(t, env.DefinePackage(lisp.Symbol("initial")).IsNil())
			require.True(t, env.InPackage(lisp.Symbol("initial")).IsNil())
			require.Equal(t, "initial", env.Runtime.Package.Name)
			require.True(t, tc.load(env).IsNil())
			require.Equal(t, lisp.DefaultUserPackage, env.Runtime.Package.Name)
		})
	}
}
