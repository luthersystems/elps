// Copyright © 2026 The ELPS authors

package analysis

import (
	"fmt"
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
	// Workspace packages (e.g. "app") don't exist in the boot env.
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

func TestLoadWorkspaceMacros_BareFileInheritsLoadContext(t *testing.T) {
	// Bare files (no in-package) inherit the caller's package from the
	// load-file call site, matching runtime behavior. The prescan's
	// loadTree tracks this per-file context.
	dir := t.TempDir()

	// main.lisp loads helpers.lisp while in myapp package.
	require.NoError(t, os.WriteFile(filepath.Join(dir, "main.lisp"), []byte(`
(in-package 'myapp)
(load-file "helpers.lisp")
`), 0600))

	// helpers.lisp has NO in-package — inherits myapp from load context.
	require.NoError(t, os.WriteFile(filepath.Join(dir, "helpers.lisp"), []byte(`
(defun ws-helper () 99)
`), 0600))

	prescan, err := PrescanWorkspace(dir, nil)
	require.NoError(t, err)

	env := newTestEnv(t)
	errs := LoadWorkspaceMacros(env, prescan.Preamble)
	assert.Empty(t, errs)

	// ws-helper should be in myapp (inherited from load context), not user.
	env.InPackage(lisp.String("myapp"))
	fn := env.Get(lisp.Symbol("ws-helper"))
	assert.Equal(t, lisp.LFun, fn.Type,
		"bare file defun should inherit load context package (myapp)")

	env.InPackage(lisp.String(lisp.DefaultUserPackage))
	notFound := env.Get(lisp.Symbol("ws-helper"))
	assert.NotEqual(t, lisp.LFun, notFound.Type,
		"bare file defun should NOT be in user package")
}

func TestLoadWorkspaceMacros_BareFileInheritsPackageSwitch(t *testing.T) {
	// When main.lisp switches packages between load-file calls, each
	// bare file inherits the package active at its load-file call site.
	dir := t.TempDir()

	require.NoError(t, os.WriteFile(filepath.Join(dir, "main.lisp"), []byte(`
(in-package 'pkgA)
(load-file "a.lisp")
(in-package 'pkgB)
(load-file "b.lisp")
`), 0600))

	require.NoError(t, os.WriteFile(filepath.Join(dir, "a.lisp"), []byte(`
(defun fn-a () 1)
`), 0600))
	require.NoError(t, os.WriteFile(filepath.Join(dir, "b.lisp"), []byte(`
(defun fn-b () 2)
`), 0600))

	prescan, err := PrescanWorkspace(dir, nil)
	require.NoError(t, err)

	env := newTestEnv(t)
	errs := LoadWorkspaceMacros(env, prescan.Preamble)
	assert.Empty(t, errs)

	// fn-a should be in pkgA, fn-b in pkgB.
	env.InPackage(lisp.String("pkgA"))
	assert.Equal(t, lisp.LFun, env.Get(lisp.Symbol("fn-a")).Type,
		"fn-a should inherit pkgA from load context")
	env.InPackage(lisp.String("pkgB"))
	assert.Equal(t, lisp.LFun, env.Get(lisp.Symbol("fn-b")).Type,
		"fn-b should inherit pkgB from load context")

	// Cross-check: fn-b NOT in pkgA.
	env.InPackage(lisp.String("pkgA"))
	assert.NotEqual(t, lisp.LFun, env.Get(lisp.Symbol("fn-b")).Type,
		"fn-b should NOT leak into pkgA")
}

func TestLoadWorkspaceMacros_LoadOrderMatters(t *testing.T) {
	// Preamble forms must be in load-tree DFS order so that definitions
	// are available when later macros need them during expansion.
	// helpers.lisp defines a function, macros.lisp defines a macro that
	// calls it. main.lisp loads helpers first — matching load order.
	dir := t.TempDir()

	require.NoError(t, os.WriteFile(filepath.Join(dir, "main.lisp"), []byte(`
(in-package 'myapp)
(load-file "helpers.lisp")
(load-file "macros.lisp")
`), 0600))

	// helpers.lisp defines the function (loaded first).
	require.NoError(t, os.WriteFile(filepath.Join(dir, "helpers.lisp"), []byte(`
(in-package 'myapp)
(defun double (x) (+ x x))
`), 0600))

	// macros.lisp defines a macro that calls double (loaded second).
	require.NoError(t, os.WriteFile(filepath.Join(dir, "macros.lisp"), []byte(`
(in-package 'myapp)
(defmacro with-double (val &rest body)
  (quasiquote (let ([doubled (double (unquote val))]) (unquote-splicing body))))
`), 0600))

	prescan, err := PrescanWorkspace(dir, nil)
	require.NoError(t, err)

	env := newTestEnv(t)
	errs := LoadWorkspaceMacros(env, prescan.Preamble)
	assert.Empty(t, errs, "helpers loaded before macros — double should be available")

	// Verify the macro expands correctly using the workspace helper.
	result := parseAndAnalyzeWithConfig(t,
		`(in-package 'myapp)
(defun test (x) (with-double x (+ doubled 1)))`,
		&Config{
			MacroExpander: &EnvMacroExpander{Env: env},
		})

	for _, u := range result.Unresolved {
		assert.NotEqual(t, "doubled", u.Name,
			"doubled should resolve as let binding from macro expansion")
	}
}

func TestLoadWorkspaceMacros_SetInBareFile(t *testing.T) {
	// set definitions in bare template files should be loaded into the
	// correct package, making globals available during macro expansion.
	dir := t.TempDir()

	require.NoError(t, os.WriteFile(filepath.Join(dir, "main.lisp"), []byte(`
(in-package 'myapp)
(load-file "template.lisp")
`), 0600))

	// template.lisp — no in-package, uses set for a global.
	require.NoError(t, os.WriteFile(filepath.Join(dir, "template.lisp"), []byte(`
(set 'default-template "hello world")
`), 0600))

	prescan, err := PrescanWorkspace(dir, nil)
	require.NoError(t, err)

	env := newTestEnv(t)
	errs := LoadWorkspaceMacros(env, prescan.Preamble)
	assert.Empty(t, errs)

	// default-template should be in myapp (inherited from load context).
	env.InPackage(lisp.String("myapp"))
	val := env.Get(lisp.Symbol("default-template"))
	assert.Equal(t, lisp.LString, val.Type,
		"set global in bare file should be in myapp package")
	assert.Equal(t, "hello world", val.Str)
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

// ---------------------------------------------------------------------------
// Swallowed-panic detection
//
// ExpandMacro's blanket recover sets result = nil, which is ALSO the answer for
// "not a macro" — the overwhelmingly common case. Before ExpansionPanics there
// was no way for any caller, test or fuzz target to tell the two apart, so a
// panic in analysis-time macro expansion was a silent class of defect. These
// tests are the two halves of that claim: the detector fires on a real panic,
// and it does not fire on anything a lisp program can arrange.
// ---------------------------------------------------------------------------

// TestExpandMacroPanicIsCounted drives a real nil-pointer dereference through
// ExpandMacro's own code (a nil form reaches `len(form.Cells)`) rather than
// through a test-only hook, and checks that the recover leaves a record.
func TestExpandMacroPanicIsCounted(t *testing.T) {
	env := newTestEnv(t)
	expander := &EnvMacroExpander{Env: env}

	require.Equal(t, uint64(0), expander.ExpansionPanics())
	require.Nil(t, expander.LastExpansionPanic())

	// A nil form panics inside ExpandMacro. Env is non-nil so the
	// short-circuit in the guard does not save it.
	result := expander.ExpandMacro(nil, "user")

	assert.Nil(t, result, "a recovered panic must still yield the nil the analyzer expects")
	assert.Equal(t, uint64(1), expander.ExpansionPanics(),
		"the recovered panic was not counted; it is invisible again")

	rec := expander.LastExpansionPanic()
	require.NotNil(t, rec)
	assert.NotNil(t, rec.Value, "recover() returned nil for a genuine panic?")
	assert.Contains(t, string(rec.GoStack), "goroutine ",
		"GoStack should be a runtime.Stack dump")
	assert.Contains(t, string(rec.GoStack), "ExpandMacro",
		"GoStack should have been captured before the unwind completed")
	assert.Equal(t, "user", rec.Package)

	// Monotonic, and Reset does not erase the evidence.
	expander.Reset()
	assert.Equal(t, uint64(1), expander.ExpansionPanics(),
		"Reset cleared the abort count; the code being watched must not be able"+
			" to clear its own record")
}

// TestExpandMacroNoPanicNotCounted is the false-positive half. Every one of
// these returns nil, and none of them is a panic — if any bumped the counter
// the signal would be useless, since nil is what ExpandMacro almost always
// returns.
func TestExpandMacroNoPanicNotCounted(t *testing.T) {
	env := newTestEnv(t)
	evalSource(t, env, `
(defmacro good (x) (quasiquote (+ (unquote x) 1)))
(defmacro boom (x) (error 'macro-boom "deliberate lisp-level error"))
(defmacro deep (x) (quasiquote (deep (unquote x))))
(defun notmac (x) x)`)

	expander := &EnvMacroExpander{Env: env}

	sexpr := func(cells ...*lisp.LVal) *lisp.LVal { return lisp.SExpr(cells) }

	cases := []struct {
		name string
		form *lisp.LVal
	}{
		{"successful expansion", sexpr(lisp.Symbol("good"), lisp.Int(1))},
		{"not a macro", sexpr(lisp.Symbol("notmac"), lisp.Int(1))},
		{"unbound symbol", sexpr(lisp.Symbol("no-such-symbol-anywhere"), lisp.Int(1))},
		{"head is not a symbol", sexpr(lisp.Int(1), lisp.Int(2))},
		{"empty form", sexpr()},
		{"macro body raises a lisp error", sexpr(lisp.Symbol("boom"), lisp.Int(1))},
		{"macro expands to itself forever", sexpr(lisp.Symbol("deep"), lisp.Int(1))},
		{"wrong arity", sexpr(lisp.Symbol("good"))},
		{"too many args", sexpr(lisp.Symbol("good"), lisp.Int(1), lisp.Int(2), lisp.Int(3))},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			expander.ExpandMacro(tc.form, "user")
		})
	}

	assert.Equal(t, uint64(0), expander.ExpansionPanics(),
		"a lisp-reachable outcome was counted as a swallowed panic (last: %+v)",
		expander.LastExpansionPanic())
}

// TestEnvMacroExpanderIsPanicReporter pins the optional interface, which is how
// callers holding a MacroExpander (lint's LintConfig, the LSP's analysis.Config)
// reach the count without knowing the concrete type.
func TestEnvMacroExpanderIsPanicReporter(t *testing.T) {
	var expander MacroExpander = &EnvMacroExpander{Env: newTestEnv(t)}
	pr, ok := expander.(PanicReporter)
	require.True(t, ok, "EnvMacroExpander must satisfy PanicReporter")
	assert.Equal(t, uint64(0), pr.ExpansionPanics())
}

// TestExpandMacroDoesNotAliasCallerForm pins elps#396.
//
// ExpandMacro used to build the macro's argument list as
// lisp.SExpr(form.Cells[1:]) — a fresh LVal header over the CALLER'S OWN
// backing array. Macro arguments are not evaluated, so that array travels
// unchanged into the macro's &rest binding: LEnv.bindFormalNext hands the
// variadic parameter QExpr(args.Rest()), and argParser.Rest returns
// p.args[p.i:], another window onto the same array. A macro body that calls
// any in-place mutator (stable-sort here; append! and the rest of the kernel's
// destructive builtins have the same shape) therefore writes straight through
// into the tree the ANALYZER is holding.
//
// The tree the analyzer is holding is not a scratch copy. It is what backs
// diagnostics, go-to-definition and lint results, and since #359 widened where
// ExpandMacro runs — workspace scans across NumCPU workers, updateFileRefs —
// the mutation can also be latched into the LSP's long-lived workspaceRefs
// index. One expansion poisons every later query for the life of the process.
//
// The assertion is deliberately on the SOURCE TREE, not on the expansion's
// result: the expansion returning something sensible is exactly what made this
// invisible. What is wrong is the side effect on the caller's form.
//
// The second subtest states the invariant the fix is actually pinned to.
// LEnv.evalSExprCells is the runtime's only other route to a macro, and on its
// IsSpecialFun branch it copies the caller's cells into a fresh array. So the
// runtime already leaves `(sort-my-args 3 1 2)` alone, and merely ANALYZING a
// file was more destructive than RUNNING it. Comparing the two directly, rather
// than hardcoding "unchanged", is what keeps this honest if the language's
// macro-argument semantics are ever revised: whatever eval does to a form,
// expansion-for-analysis must do no more.
func TestExpandMacroDoesNotAliasCallerForm(t *testing.T) {
	const macros = `
(defmacro sort-my-args (&rest body)
  (stable-sort < body)
  (quasiquote 0))`

	// parseForm parses one top-level form the way the analyzer does, so the
	// cells under test are genuine parse-tree storage and not a hand-built
	// slice that happens to have spare capacity.
	parseForm := func(t *testing.T, src string) *lisp.LVal {
		t.Helper()
		s := token.NewScanner("alias_test.lisp", strings.NewReader(src))
		exprs, err := rdparser.New(s).ParseProgram()
		require.NoError(t, err)
		require.Len(t, exprs, 1)
		return exprs[0]
	}
	argInts := func(form *lisp.LVal) []int {
		out := make([]int, 0, len(form.Cells)-1)
		for _, c := range form.Cells[1:] {
			out = append(out, c.Int)
		}
		return out
	}

	const src = "(sort-my-args 3 1 2)"

	t.Run("expansion leaves the caller's form alone", func(t *testing.T) {
		env := newTestEnv(t)
		evalSource(t, env, macros)

		form := parseForm(t, src)
		require.Len(t, form.Cells, 4)
		require.Equal(t, []int{3, 1, 2}, argInts(form), "precondition: source order")

		expander := &EnvMacroExpander{Env: env}
		_ = expander.ExpandMacro(form, lisp.DefaultUserPackage)

		assert.Equal(t, []int{3, 1, 2}, argInts(form),
			"macro expansion rewrote the caller's parse tree in place —"+
				" the analyzer's own AST was corrupted by expanding a macro over it")
	})

	t.Run("expansion perturbs the form no more than eval does", func(t *testing.T) {
		evalEnv := newTestEnv(t)
		evalSource(t, evalEnv, macros)
		evaled := parseForm(t, src)
		require.NotEqual(t, lisp.LError, evalEnv.Eval(evaled).Type)

		expandEnv := newTestEnv(t)
		evalSource(t, expandEnv, macros)
		expanded := parseForm(t, src)
		_ = (&EnvMacroExpander{Env: expandEnv}).ExpandMacro(expanded, lisp.DefaultUserPackage)

		assert.Equal(t, argInts(evaled), argInts(expanded),
			"analyzing the form mutated it differently than evaluating it did —"+
				" ExpandMacro must build the macro's argument list the same way"+
				" LEnv.evalSExprCells does")
	})
}

// TestExpandMacroDoesNotRestampCallerSourceLocations is the analyzer-side
// guard for elps#370, the sibling of #396 above: not the argument ARRAY this
// time, but the source locations recorded on the caller's nodes.
//
// lisp.stampMacroExpansion runs at the end of every MacroCall — the one
// ExpandMacro makes included — and rewrites LVal.Source on each expanded node
// whose location is synthetic (nil, or Pos < 0). Macro arguments are not
// evaluated, so they reach the expansion as the analyzer's own parse-tree
// nodes; and the reader used to emit two nodes with synthetic locations of its
// own, the "lisp:function" head behind #' and the nil-source "lisp:expr" head
// behind #^. Those were nodes the stamp would write into.
//
// This is a GUARD, not a catch: it passes on main. The stamp does fire on this
// path, but its call site is env.Loc, and an expander env that has just
// finished loading macros is left pointing at lisp.nativeSource — so the write
// lands the same value the node already held and moves nothing observable. The
// damage was demonstrable on the lisp side, where the call site is a real
// location (lisp's TestMacroExpansionDoesNotRestampCallerParseTree, and
// TestMacroExpansionSharedParseTreeIsRaceFree under -race).
//
// It is worth pinning here anyway, because this is where a moved position
// costs the most. Position information is the analyzer's output — hover
// ranges, go-to-definition and diagnostics are computed from it — and since
// #359 the LSP holds this tree across requests in workspaceRefs and expands
// over it from NumCPU workers. Whether the stamp's call site is real depends
// on nothing more than which form the expander env last evaluated.
//
// The fix is in the reader: rdparser now gives those synthesized heads the
// real location of the prefix token they stand for, so there is no node left
// in a parsed form for the stamp to claim.
func TestExpandMacroDoesNotRestampCallerSourceLocations(t *testing.T) {
	env := newTestEnv(t)
	evalSource(t, env, `
(defmacro ident (x) x)
(defun target () 1)`)

	// Parsed, not hand-built: the point is that these are READER nodes, and
	// only the reader produced the synthetic locations at issue.
	s := token.NewScanner("caller.lisp", strings.NewReader(`(ident #'target)`))
	exprs, err := rdparser.New(s).ParseProgram()
	require.NoError(t, err)
	require.Len(t, exprs, 1)
	form := exprs[0]

	type located struct{ desc, loc string }
	var walk func(v *lisp.LVal, path string) []located
	walk = func(v *lisp.LVal, path string) []located {
		loc := "<nil>"
		if v.Source != nil {
			loc = v.Source.String()
		}
		out := []located{{path + "/" + v.Type.String() + " " + v.Str, loc}}
		for i, c := range v.Cells {
			out = append(out, walk(c, fmt.Sprintf("%s/%d", path, i))...)
		}
		return out
	}

	before := walk(form, "")
	expander := &EnvMacroExpander{Env: env}
	expanded := expander.ExpandMacro(form, lisp.DefaultUserPackage)
	require.NotNil(t, expanded, "expansion should succeed")
	after := walk(form, "")

	require.Len(t, after, len(before))
	for i := range before {
		assert.Equal(t, before[i].loc, after[i].loc,
			"expansion moved the source location of %s in the analyzer's parse tree", before[i].desc)
	}
}
