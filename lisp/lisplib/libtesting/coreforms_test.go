// Copyright © 2026 The ELPS authors

package libtesting_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/lisp/lisplib/libtesting"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// Issue #736: test, benchmark, test-let, test-let* and benchmark-simple are
// core lisp forms.  A test body is code written in the caller's package, like
// a defun body; package testing re-exports the forms by value and keeps its
// assert macros.

// userEnv is an initialized user-package environment; withLibrary adds the
// full library, testing included.
func userEnv(t *testing.T, withLibrary bool) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	require.NoError(t, lisp.GoError(lisp.InitializeUserEnv(env)))
	if withLibrary {
		require.NoError(t, lisp.GoError(lisplib.LoadLibrary(env)))
	}
	require.NoError(t, lisp.GoError(env.InPackage(lisp.String(lisp.DefaultUserPackage))))
	return env
}

func load(t *testing.T, env *lisp.LEnv, src string) *lisp.LVal {
	t.Helper()
	return env.LoadString("test", src)
}

func errText(v *lisp.LVal) string {
	if v.Type != lisp.LError {
		return "<no error: " + v.String() + ">"
	}
	return lisp.GoError(v).Error()
}

// TestCoreFormsWithoutSuite pins the error outside a test run: every form is
// bound, as core, and signals "no test suite" when evaluated.  Argument
// errors still come first, as they did in package testing.
func TestCoreFormsWithoutSuite(t *testing.T) {
	env := userEnv(t, false)
	assert.Nil(t, libtesting.EnvTestSuite(env))
	for _, tc := range []struct{ src, want string }{
		{`(test "t" (+ 1 1))`, `test:1:1: lisp:test: no test suite: test can only be used while running tests`},
		{`(lisp:test "t" (+ 1 1))`, `test:1:1: lisp:test: no test suite: test can only be used while running tests`},
		{`(benchmark "b" (n) n)`, `test:1:1: lisp:benchmark: no test suite: benchmark can only be used while running tests`},
		{`(test-let "t" ((x 1)) x)`, `test:1:1: lisp:test: no test suite: test can only be used while running tests`},
		{`(test-let* "t" ((x 1)) x)`, `test:1:1: lisp:test: no test suite: test can only be used while running tests`},
		{`(benchmark-simple "b" 1)`, `test:1:1: lisp:benchmark: no test suite: benchmark can only be used while running tests`},
		{`(test 'not-a-string 1)`, `test:1:1: lisp:test: first argument is not a string: symbol`},
		{`(benchmark "b" (n m) n)`, `test:1:1: lisp:benchmark: benchmark doesn't take one argument: 2`},
		{`(test-let "t" (x) x)`, `test:1:1: lisp:test-let: second argument is not a list of pairs: symbol`},
	} {
		assert.Equal(t, tc.want, errText(load(t, env, tc.src)), tc.src)
	}
	// Referencing the testing package is still an error without it, as today.
	v := load(t, env, `(use-package 'testing)`)
	assert.Contains(t, errText(v), "unknown package: testing")
}

// TestCoreFormsWithMalformedSuite: a testing package without a suite
// binding says so.
func TestCoreFormsWithMalformedSuite(t *testing.T) {
	env := userEnv(t, false)
	require.NoError(t, lisp.GoError(env.DefinePackage(lisp.Symbol(libtesting.DefaultPackageName))))
	v := load(t, env, `(test "t" 1)`)
	assert.Equal(t, `test:1:1: lisp:test: no test suite: testing:test-suite is not installed in the calling VM`, errText(v))
}

// TestTestingReexportsCoreForms: package testing's test forms are lisp's own
// function values, so (use-package 'testing), testing:test and an unqualified
// test all mean exactly lisp:test, and no special operator is defined outside
// lisp.
func TestTestingReexportsCoreForms(t *testing.T) {
	env := userEnv(t, true)
	lang := env.Runtime.Registry.Package(lisp.DefaultLangPackage)
	tpkg := env.Runtime.Registry.Package(libtesting.DefaultPackageName)
	require.NotNil(t, tpkg)
	exported := map[string]bool{}
	for _, name := range tpkg.Externals() {
		exported[name] = true
	}
	for _, form := range libtesting.CoreForms {
		core := lang.Get(lisp.Symbol(form))
		re := tpkg.Get(lisp.Symbol(form))
		require.Equal(t, lisp.LFun, core.Type, form)
		require.Equal(t, lisp.LFun, re.Type, form)
		assert.Equal(t, core.FID(), re.FID(), "testing:%s must be lisp:%s", form, form)
		assert.Equal(t, lisp.DefaultLangPackage, re.Package(), form)
		assert.True(t, exported[form], "testing must export %s", form)
	}
	// The assert macros stay in testing.
	for _, name := range []string{"assert=", "assert-equal", "assert-nil", "assert-not-nil", "assert-not", "assert-string="} {
		v := tpkg.Get(lisp.Symbol(name))
		require.Equal(t, lisp.LFun, v.Type, name)
		assert.Equal(t, libtesting.DefaultPackageName, v.Package(), name)
		assert.True(t, exported[name], name)
	}
}

// TestTestBodyIsWrittenInCallerPackage: a test body resolves names in the
// package that defined it, with or without (use-package 'testing), exactly
// like a defun body.
func TestTestBodyIsWrittenInCallerPackage(t *testing.T) {
	env := userEnv(t, true)
	src := `
(in-package 'app)
(defun helper () 41)
(test "unqualified" (testing:assert= 42 (+ 1 (helper))))
(testing:test "qualified" (testing:assert= 42 (+ 1 (helper))))
(test-let "let" ((x 1)) (testing:assert= 42 (+ x (helper))))
(in-package 'app2)
(use-package 'testing)
(defun helper () 1)
(test "use-package" (assert= 2 (+ 1 (helper))))
(benchmark-simple "bench" (helper))
`
	require.NoError(t, lisp.GoError(load(t, env, src)))
	suite := libtesting.EnvTestSuite(env)
	require.NotNil(t, suite)
	require.Equal(t, []string{"unqualified", "qualified", "let", "use-package"}, suite.Tests())
	wantPkg := []string{"app", "app", "app", "app2"}
	for i := range suite.Len() {
		tst := suite.Test(i)
		assert.Equal(t, wantPkg[i], tst.Fun.Package(), tst.Name)
		v := env.FunCall(tst.Fun, lisp.Nil())
		assert.NotEqual(t, lisp.LError, v.Type, "%s: %v", tst.Name, v)
	}
	require.Equal(t, []string{"bench"}, suite.Benchmarks())
	b := suite.Benchmark(0)
	assert.Equal(t, "app2", b.Fun.Package())
	v := env.FunCall(b.Fun, lisp.QExpr([]*lisp.LVal{lisp.Int(3)}))
	assert.NotEqual(t, lisp.LError, v.Type, "%v", v)
}

// TestCoreFormExpansions pins the macro expansions: they name lisp's forms,
// qualified, so they mean the same thing in any package.
func TestCoreFormExpansions(t *testing.T) {
	env := userEnv(t, false)
	for _, tc := range []struct{ src, want string }{
		{`(macroexpand-1 '(test-let "n" ((x 1)) x))`, `'(lisp:test "n" (lisp:let ((x 1)) x))`},
		{`(macroexpand-1 '(test-let* "n" ((x 1) (y x)) y))`, `'(lisp:test "n" (lisp:let* ((x 1) (y x)) y))`},
	} {
		v := load(t, env, tc.src)
		require.NotEqual(t, lisp.LError, v.Type, "%s: %v", tc.src, v)
		assert.Equal(t, tc.want, v.String(), tc.src)
	}
	v := load(t, env, `(macroexpand-1 '(benchmark-simple "b" (f)))`)
	require.NotEqual(t, lisp.LError, v.Type, "%v", v)
	require.Equal(t, lisp.LSExpr, v.Type)
	require.Len(t, v.Cells, 4)
	assert.Equal(t, "lisp:benchmark", v.Cells[0].Str)
	count := v.Cells[2].Cells[0].Str
	assert.Equal(t, "(lisp:dotimes (_ "+count+") (f))", v.Cells[3].String())
}

// TestDeprecatedSuiteAPI: the hand-assembly API still hands out working
// definitions (substrate enumerates Ops and Macros).
func TestDeprecatedSuiteAPI(t *testing.T) {
	s := libtesting.NewTestSuite()
	var names []string
	for _, def := range append(s.Ops(), s.Macros()...) {
		names = append(names, def.Name())
	}
	for _, want := range []string{"test", "benchmark", "test-let", "test-let*", "benchmark-simple", "assert=", "assert-equal"} {
		assert.Contains(t, names, want)
	}
	// A hand-assembled suite registers through lisp's test.
	suite := libtesting.NewTestSuite()
	env := sharedSuiteEnv(t, suite)
	require.NoError(t, lisp.GoError(env.InPackage(lisp.String(lisp.DefaultUserPackage))))
	require.NoError(t, lisp.GoError(load(t, env, `(testing:test "hand" 1) (test-let "hand-let" ((x 1)) x)`)))
	assert.Equal(t, []string{"hand", "hand-let"}, suite.Tests())
}
