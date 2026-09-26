// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func docTestEnv(t *testing.T) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	require.NoError(t, lisp.GoError(lisp.InitializeUserEnv(env)))
	require.NoError(t, lisp.GoError(env.InPackage(lisp.String(lisp.DefaultUserPackage))))
	return env
}

// docOf returns a builtin's docstring with its whitespace collapsed.
func docOf(t *testing.T, env *lisp.LEnv, name string) string {
	t.Helper()
	return strings.Join(strings.Fields(env.GetGlobal(lisp.Symbol(name)).Docstring()), " ")
}

// error's docstring said the condition may be "a symbol or string", but a
// string condition is rejected (pinned in valuefp_error_test.go).
func TestErrorDocstringMatchesConditionRule(t *testing.T) {
	env := docTestEnv(t)
	res := env.LoadString("test", `(error "x" "y")`)
	require.Equal(t, lisp.LError, res.Type)
	require.Contains(t, lisp.GoError(res).Error(), "not a symbol")
	doc := docOf(t, env, "error")
	assert.NotContains(t, doc, "symbol or string")
	assert.Contains(t, doc, "condition name (a symbol)")
}

// expr's docstring used printf escapes (%%1) that nothing unescapes, so
// `elps doc expr` showed doubled percent signs.
func TestExprDocstringShowsPlaceholders(t *testing.T) {
	env := docTestEnv(t)
	doc := docOf(t, env, "expr")
	assert.NotContains(t, doc, "%%")
	for _, placeholder := range []string{"% for a single argument", "%1 %2", "%&rest"} {
		assert.Contains(t, doc, placeholder)
	}
}

// gensym was documented as returning an "uninterned" symbol.  It returns an
// ordinary symbol, genNNNNNNNN, numbered by a per-runtime counter -- which
// templates rely on for deterministic names -- so the docs now say that
// instead.  This pins the documented behaviour.
func TestGensymIsAnOrdinaryPerRuntimeSymbol(t *testing.T) {
	a, b := docTestEnv(t), docTestEnv(t)
	first := a.LoadString("test", `(gensym)`)
	require.Equal(t, lisp.LSymbol, first.Type)
	assert.Regexp(t, `^gen[0-9]{8,}$`, first.Str)
	// Deterministic per runtime: a fresh runtime mints the same first name.
	assert.Equal(t, first.Str, b.LoadString("test", `(gensym)`).Str)
	// Unique within a runtime.
	assert.Equal(t, "false", a.LoadString("test", `(equal? (gensym) (gensym))`).String())
	// Ordinary: equal? to the same spelling read from source.
	c := docTestEnv(t)
	assert.Equal(t, "true", c.LoadString("test", `(equal? (gensym) '`+first.Str+`)`).String())

	doc := docOf(t, a, "gensym")
	assert.NotContains(t, doc, "unique, uninterned")
	assert.Contains(t, doc, "ordinary symbol")
}
