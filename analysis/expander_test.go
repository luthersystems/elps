// Copyright © 2026 The ELPS authors

package analysis

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser/rdparser"
	"github.com/luthersystems/elps/parser/token"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// newTestEnv creates a minimal ELPS environment with builtins and stdlib.
func newTestEnv(t *testing.T) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = rdparser.NewReader()
	rc := lisp.InitializeUserEnv(env)
	require.True(t, rc.IsNil(), "InitializeUserEnv failed: %v", rc)
	rc = lisplib.LoadLibrary(env)
	require.True(t, rc.IsNil(), "LoadLibrary failed: %v", rc)
	rc = env.InPackage(lisp.String(lisp.DefaultUserPackage))
	require.True(t, rc.IsNil(), "InPackage failed: %v", rc)
	return env
}

// evalSource evaluates ELPS source in the given environment.
func evalSource(t *testing.T, env *lisp.LEnv, source string) {
	t.Helper()
	s := token.NewScanner("test.lisp", strings.NewReader(source))
	p := rdparser.New(s)
	exprs, err := p.ParseProgram()
	require.NoError(t, err)
	for _, expr := range exprs {
		result := env.Eval(expr)
		require.NotEqual(t, lisp.LError, result.Type, "eval error: %v", result)
	}
}

func TestEnvMacroExpander_SimpleExpansion(t *testing.T) {
	env := newTestEnv(t)
	evalSource(t, env, `
(defmacro my-when (cond &rest body)
  (quasiquote (if (unquote cond) (progn (unquote-splicing body)))))`)

	expander := &EnvMacroExpander{Env: env}

	// Build a form: (my-when true 42)
	form := lisp.SExpr([]*lisp.LVal{
		lisp.Symbol("my-when"),
		lisp.Symbol("true"),
		lisp.Int(42),
	})

	expanded := expander.ExpandMacro(form, lisp.DefaultUserPackage)
	require.NotNil(t, expanded, "expansion should succeed")
	// Expanded form should be (if true (progn 42))
	require.Equal(t, lisp.LSExpr, expanded.Type)
	require.GreaterOrEqual(t, len(expanded.Cells), 3, "expanded form should have at least 3 cells")
	assert.Equal(t, lisp.LSymbol, expanded.Cells[0].Type)
	assert.Equal(t, "if", expanded.Cells[0].Str, "head should be 'if'")
	assert.Equal(t, lisp.LSymbol, expanded.Cells[1].Type)
	assert.Equal(t, "true", expanded.Cells[1].Str, "condition should be 'true'")
	// Third element: (progn 42)
	progn := expanded.Cells[2]
	require.Equal(t, lisp.LSExpr, progn.Type)
	require.GreaterOrEqual(t, len(progn.Cells), 2)
	assert.Equal(t, "progn", progn.Cells[0].Str)
	assert.Equal(t, 42, progn.Cells[1].Int)
}

func TestEnvMacroExpander_NotAMacro(t *testing.T) {
	env := newTestEnv(t)
	expander := &EnvMacroExpander{Env: env}

	// + is a builtin function, not a macro
	form := lisp.SExpr([]*lisp.LVal{
		lisp.Symbol("+"),
		lisp.Int(1),
		lisp.Int(2),
	})
	assert.Nil(t, expander.ExpandMacro(form, lisp.DefaultUserPackage))
}

func TestEnvMacroExpander_ExpansionError(t *testing.T) {
	env := newTestEnv(t)
	evalSource(t, env, `
(defmacro needs-args (x y)
  (quasiquote (+ (unquote x) (unquote y))))`)

	expander := &EnvMacroExpander{Env: env}

	// Wrong arity — should return nil (graceful failure)
	form := lisp.SExpr([]*lisp.LVal{
		lisp.Symbol("needs-args"),
		lisp.Int(1),
		// missing second arg
	})
	assert.Nil(t, expander.ExpandMacro(form, lisp.DefaultUserPackage))
}

func TestEnvMacroExpander_NilEnv(t *testing.T) {
	expander := &EnvMacroExpander{Env: nil}
	form := lisp.SExpr([]*lisp.LVal{lisp.Symbol("foo")})
	assert.Nil(t, expander.ExpandMacro(form, lisp.DefaultUserPackage))
}

func TestEnvMacroExpander_EmptyForm(t *testing.T) {
	env := newTestEnv(t)
	expander := &EnvMacroExpander{Env: env}
	form := lisp.SExpr([]*lisp.LVal{})
	assert.Nil(t, expander.ExpandMacro(form, lisp.DefaultUserPackage))
}

func TestEnvMacroExpander_NotMacroCached(t *testing.T) {
	env := newTestEnv(t)
	expander := &EnvMacroExpander{Env: env}

	form := lisp.SExpr([]*lisp.LVal{
		lisp.Symbol("+"),
		lisp.Int(1),
	})

	// First call — should populate the notMacro cache.
	assert.Nil(t, expander.ExpandMacro(form, lisp.DefaultUserPackage))
	require.NotNil(t, expander.notMacro, "notMacro cache should be initialized")
	cacheKey := lisp.DefaultUserPackage + "\x00+"
	assert.True(t, expander.notMacro[cacheKey], "should cache '+' as not-a-macro")

	// Second call — hits cache (no env.Get needed).
	assert.Nil(t, expander.ExpandMacro(form, lisp.DefaultUserPackage))
	assert.True(t, expander.notMacro[cacheKey], "cache entry should persist across calls")
}

func TestEnvMacroExpander_ExpansionErrorGracefulReturn(t *testing.T) {
	// Verify that an ELPS error during macro expansion returns nil gracefully.
	// The macro body raises an error condition, which MacroCall returns as
	// LError. ExpandMacro catches this and returns nil (fallback to opaque).
	env := newTestEnv(t)
	evalSource(t, env, `
(defmacro bad-macro (&rest args)
  (error 'logic "expansion failure"))`)

	expander := &EnvMacroExpander{Env: env}
	form := lisp.SExpr([]*lisp.LVal{
		lisp.Symbol("bad-macro"),
		lisp.Int(1),
	})
	// Should return nil (error caught), not panic
	assert.Nil(t, expander.ExpandMacro(form, lisp.DefaultUserPackage))
}

// --- LoadWorkspaceMacros tests ---

// parsePreamble parses source containing preamble forms and returns them.
func parsePreamble(t *testing.T, source string) []*lisp.LVal {
	t.Helper()
	s := token.NewScanner("test.lisp", strings.NewReader(source))
	p := rdparser.New(s)
	exprs, err := p.ParseProgram()
	require.NoError(t, err)
	return exprs
}

func TestLoadWorkspaceMacros_Success(t *testing.T) {
	env := newTestEnv(t)

	preamble := parsePreamble(t,
		`(defmacro my-when (cond &rest body) (quasiquote (if (unquote cond) (progn (unquote-splicing body)))))`)

	errs := LoadWorkspaceMacros(env, preamble)
	assert.Empty(t, errs, "loading a valid defmacro should produce no errors")

	mac := env.Get(lisp.Symbol("my-when"))
	assert.Equal(t, lisp.LFun, mac.Type, "my-when should be a function in the env")
	assert.True(t, mac.IsMacro(), "my-when should be a macro")
}

func TestLoadWorkspaceMacros_PackageContext(t *testing.T) {
	// Preamble with in-package switches context naturally.
	env := newTestEnv(t)

	preamble := parsePreamble(t, `
(in-package 'mypkg)
(defmacro pkg-macro () '42)`)

	errs := LoadWorkspaceMacros(env, preamble)
	assert.Empty(t, errs)

	// Verify the macro was registered in mypkg, not user.
	env.InPackage(lisp.String("mypkg"))
	mac := env.Get(lisp.Symbol("pkg-macro"))
	assert.Equal(t, lisp.LFun, mac.Type, "pkg-macro should be defined in mypkg")
	assert.True(t, mac.IsMacro())

	// Verify it's NOT in the user package.
	env.InPackage(lisp.String(lisp.DefaultUserPackage))
	notFound := env.Get(lisp.Symbol("pkg-macro"))
	assert.NotEqual(t, lisp.LFun, notFound.Type,
		"pkg-macro should NOT be defined in user package")
}

func TestLoadWorkspaceMacros_PackageAutoCreated(t *testing.T) {
	// Workspace packages (e.g. "acre") don't exist in the boot env.
	// in-package in the preamble auto-creates them.
	env := newTestEnv(t)

	preamble := parsePreamble(t, `
(in-package 'newpkg)
(defmacro ws-macro () '99)`)

	errs := LoadWorkspaceMacros(env, preamble)
	assert.Empty(t, errs, "should auto-create the package, not error")

	env.InPackage(lisp.String("newpkg"))
	mac := env.Get(lisp.Symbol("ws-macro"))
	assert.Equal(t, lisp.LFun, mac.Type, "ws-macro should be defined in newpkg")
	assert.True(t, mac.IsMacro())
}

func TestLoadWorkspaceMacros_PackageRestored(t *testing.T) {
	// The env's active package should be restored after loading.
	env := newTestEnv(t)
	env.InPackage(lisp.String(lisp.DefaultUserPackage))

	preamble := parsePreamble(t, `
(in-package 'otherpkg)
(defmacro m () '1)`)

	LoadWorkspaceMacros(env, preamble)

	// Should be back in user package.
	assert.Equal(t, lisp.DefaultUserPackage, env.Runtime.Package.Name,
		"env package should be restored after LoadWorkspaceMacros")
}

func TestLoadWorkspaceMacros_UsePackageImports(t *testing.T) {
	// use-package in the preamble should make imported functions available
	// during macro expansion. This mirrors the runtime's file loading.
	env := newTestEnv(t)

	// Define a helper in a "utils" package.
	evalSource(t, env, `
(in-package 'utils)
(export 'helper)
(defun helper () 42)`)
	env.InPackage(lisp.String(lisp.DefaultUserPackage))

	// Preamble: switch to mypkg, import utils, define macro that uses helper.
	preamble := parsePreamble(t, `
(in-package 'mypkg)
(use-package 'utils)
(defmacro my-macro () (quasiquote (helper)))`)

	errs := LoadWorkspaceMacros(env, preamble)
	assert.Empty(t, errs, "macro using imported function should load without error")
}

func TestLoadWorkspaceMacros_DefunAvailableForMacroExpansion(t *testing.T) {
	// Regression test: workspace-defined functions (defun) must be available
	// during macro expansion. Without loading defuns into the env, macros
	// that call workspace functions (like flatten) fail silently.
	env := newTestEnv(t)

	preamble := parsePreamble(t, `
(in-package 'myapp)
(defun my-flatten (seq)
  (if (nil? seq) '()
    (concat 'list (car seq) (my-flatten (cdr seq)))))
(defmacro with-flat-defs (definitions &rest body)
  (quasiquote
    (lambda
      (unquote (map 'list #^(first %) (my-flatten definitions)))
      (progn (unquote-splicing body)))))`)

	errs := LoadWorkspaceMacros(env, preamble)
	assert.Empty(t, errs, "preamble with defun + defmacro should load without error")

	// Verify the function is callable in the env.
	env.InPackage(lisp.String("myapp"))
	fn := env.Get(lisp.Symbol("my-flatten"))
	assert.Equal(t, lisp.LFun, fn.Type, "my-flatten should be a function in the env")

	// Verify the macro can expand (it calls my-flatten during expansion).
	// Use the full analysis path to test end-to-end.
	result := parseAndAnalyzeWithConfig(t,
		`(in-package 'myapp)
(defun test ()
  (with-flat-defs (([x] [y]))
    (list x y)))`,
		&Config{
			MacroExpander: &EnvMacroExpander{Env: env},
		})

	for _, u := range result.Unresolved {
		assert.NotEqual(t, "x", u.Name, "x should resolve as lambda param after macro expansion")
		assert.NotEqual(t, "y", u.Name, "y should resolve as lambda param after macro expansion")
	}
}

func TestLoadWorkspaceMacros_RecursiveDefun(t *testing.T) {
	// Recursive defun should work in preamble loading. defun binds the
	// function name before the body is evaluated, so self-references resolve.
	env := newTestEnv(t)

	preamble := parsePreamble(t, `
(in-package 'myapp)
(defun my-flatten (seq)
  (if (not (list? seq)) (list seq)
    (if (empty? seq) '()
      (concat 'list
        (my-flatten (car seq))
        (my-flatten (cdr seq))))))`)

	errs := LoadWorkspaceMacros(env, preamble)
	assert.Empty(t, errs, "recursive defun should load without error")

	// Verify the recursive function is callable.
	env.InPackage(lisp.String("myapp"))
	fn := env.Get(lisp.Symbol("my-flatten"))
	assert.Equal(t, lisp.LFun, fn.Type, "my-flatten should be defined")
}

func TestLoadWorkspaceMacros_BareFileDefunUsesDefaultPackage(t *testing.T) {
	// Edge case: files without in-package should use DefaultPackage.
	// At runtime, (load-file "helpers.lisp") inherits the caller's package.
	// The prescan computes DefaultPackage from main.lisp; bare files'
	// preamble forms should be prefixed with in-package for that package
	// so defuns register in the right package.
	dir := t.TempDir()

	require.NoError(t, os.WriteFile(filepath.Join(dir, "main.lisp"), []byte(`
(in-package 'myapp)
(defmacro my-macro () '42)
`), 0600))

	// helpers.lisp has NO in-package — at runtime it inherits myapp.
	require.NoError(t, os.WriteFile(filepath.Join(dir, "helpers.lisp"), []byte(`
(defun ws-helper () 99)
`), 0600))

	prescan, err := PrescanWorkspace(dir, nil)
	require.NoError(t, err)
	assert.Equal(t, "myapp", prescan.DefaultPackage)

	env := newTestEnv(t)
	errs := LoadWorkspaceMacros(env, prescan.Preamble)
	assert.Empty(t, errs)

	// ws-helper should be defined in myapp (DefaultPackage), not user.
	env.InPackage(lisp.String("myapp"))
	fn := env.Get(lisp.Symbol("ws-helper"))
	assert.Equal(t, lisp.LFun, fn.Type,
		"bare file defun should be defined in myapp (DefaultPackage)")

	// Verify NOT in user package.
	env.InPackage(lisp.String(lisp.DefaultUserPackage))
	notFound := env.Get(lisp.Symbol("ws-helper"))
	assert.NotEqual(t, lisp.LFun, notFound.Type,
		"bare file defun should NOT be defined in user package")
}

func TestLoadWorkspaceMacros_ErrorReturned(t *testing.T) {
	env := newTestEnv(t)

	preamble := parsePreamble(t, `(defmacro)`)

	errs := LoadWorkspaceMacros(env, preamble)
	require.NotEmpty(t, errs, "malformed defmacro should return an error")
}

func TestLoadWorkspaceMacros_MultiplePackages(t *testing.T) {
	// Macros in different packages via in-package switching.
	env := newTestEnv(t)

	preamble := parsePreamble(t, `
(in-package 'pkgA)
(defmacro mac-a () '1)
(in-package 'pkgB)
(defmacro mac-b () '2)`)

	errs := LoadWorkspaceMacros(env, preamble)
	assert.Empty(t, errs)

	env.InPackage(lisp.String("pkgA"))
	assert.Equal(t, lisp.LFun, env.Get(lisp.Symbol("mac-a")).Type, "mac-a should be in pkgA")

	env.InPackage(lisp.String("pkgB"))
	assert.Equal(t, lisp.LFun, env.Get(lisp.Symbol("mac-b")).Type, "mac-b should be in pkgB")

	// Cross-check: mac-b should NOT be in pkgA.
	env.InPackage(lisp.String("pkgA"))
	assert.NotEqual(t, lisp.LFun, env.Get(lisp.Symbol("mac-b")).Type,
		"mac-b should NOT leak into pkgA")
}

func TestEnvMacroExpander_Reset_ClearsCache(t *testing.T) {
	env := newTestEnv(t)
	expander := &EnvMacroExpander{Env: env}

	// First: my-when is not defined → cached as not-a-macro.
	form := lisp.SExpr([]*lisp.LVal{
		lisp.Symbol("my-when"),
		lisp.Symbol("true"),
		lisp.Int(42),
	})
	assert.Nil(t, expander.ExpandMacro(form, lisp.DefaultUserPackage))
	assert.True(t, expander.notMacro[lisp.DefaultUserPackage+"\x00my-when"], "should be cached as not-a-macro")

	// Define the macro in the env.
	evalSource(t, env, `(defmacro my-when (cond &rest body)
	  (quasiquote (if (unquote cond) (progn (unquote-splicing body)))))`)

	// Without Reset, stale cache prevents expansion.
	assert.Nil(t, expander.ExpandMacro(form, lisp.DefaultUserPackage), "stale cache should prevent expansion")

	// After Reset, expansion succeeds.
	expander.Reset()
	assert.Nil(t, expander.notMacro, "Reset should clear the cache")
	expanded := expander.ExpandMacro(form, lisp.DefaultUserPackage)
	require.NotNil(t, expanded, "after Reset, newly-defined macro should expand")
	assert.Equal(t, "if", expanded.Cells[0].Str)
}
