// Copyright © 2024 The ELPS authors

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
	assert.Equal(t, lisp.LSExpr, expanded.Type)
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
