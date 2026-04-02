// Copyright © 2026 The ELPS authors

package analysis

import (
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

	expanded := expander.ExpandMacro(form)
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
	assert.Nil(t, expander.ExpandMacro(form))
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
	assert.Nil(t, expander.ExpandMacro(form))
}

func TestEnvMacroExpander_NilEnv(t *testing.T) {
	expander := &EnvMacroExpander{Env: nil}
	form := lisp.SExpr([]*lisp.LVal{lisp.Symbol("foo")})
	assert.Nil(t, expander.ExpandMacro(form))
}

func TestEnvMacroExpander_EmptyForm(t *testing.T) {
	env := newTestEnv(t)
	expander := &EnvMacroExpander{Env: env}
	form := lisp.SExpr([]*lisp.LVal{})
	assert.Nil(t, expander.ExpandMacro(form))
}

func TestEnvMacroExpander_NotMacroCached(t *testing.T) {
	env := newTestEnv(t)
	expander := &EnvMacroExpander{Env: env}

	form := lisp.SExpr([]*lisp.LVal{
		lisp.Symbol("+"),
		lisp.Int(1),
	})

	// First call — should populate the notMacro cache.
	assert.Nil(t, expander.ExpandMacro(form))
	require.NotNil(t, expander.notMacro, "notMacro cache should be initialized")
	assert.True(t, expander.notMacro["+"], "should cache '+' as not-a-macro")

	// Second call — hits cache (no env.Get needed).
	assert.Nil(t, expander.ExpandMacro(form))
	assert.True(t, expander.notMacro["+"], "cache entry should persist across calls")
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
	assert.Nil(t, expander.ExpandMacro(form))
}
