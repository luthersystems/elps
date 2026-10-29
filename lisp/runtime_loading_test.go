// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/elpsutil"
	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// TestRuntimeLoading pins Runtime.Loading (luthersystems/elps#908): true
// while a load runs, also in a function the load calls and in load-string
// called after the load, and false after the load returns, also when it
// fails or a builtin panics, and in a template VM.
func TestRuntimeLoading(t *testing.T) {
	env := templateTestEnv(t)
	require.NoError(t, lisp.GoError(env.InPackage(lisp.String(lisp.DefaultUserPackage))))
	env.AddBuiltins(false,
		elpsutil.Function("loading?", lisp.Formals(), func(env *lisp.LEnv, _ *lisp.LVal) *lisp.LVal {
			return lisp.Bool(env.Runtime.Loading())
		}),
		elpsutil.Function("boom", lisp.Formals(), func(*lisp.LEnv, *lisp.LVal) *lisp.LVal {
			panic("boom")
		}))
	require.False(t, env.Runtime.Loading())
	assert.Equal(t, "true", env.LoadString("t", `(loading?)`).String())
	assert.Equal(t, "true", env.LoadString("t", `(progn (defun f () (loading?)) (f))`).String())
	assert.False(t, env.Runtime.Loading())

	// After the load: a call from Go is not in a load, and a load it
	// starts is.
	assert.Equal(t, "false", env.FunCall(env.GetGlobal(lisp.Symbol("f")), lisp.SExpr(nil)).String())
	assert.Equal(t, "false", env.Eval(lisp.SExpr([]*lisp.LVal{lisp.Symbol("f")})).String())
	assert.Equal(t, "true", env.Eval(lisp.SExpr([]*lisp.LVal{lisp.Symbol("load-string"), lisp.String("(f)")})).String())
	assert.False(t, env.Runtime.Loading())

	assert.True(t, env.LoadString("t", `(error 'x "y")`).IsError())
	assert.False(t, env.Runtime.Loading())
	assert.True(t, env.LoadString("t", `(boom)`).IsError())
	assert.False(t, env.Runtime.Loading())

	tmpl, err := lisp.NewTemplate(env, templateCorePolicy())
	require.NoError(t, err)
	vm, err := tmpl.NewVM()
	require.NoError(t, err)
	assert.False(t, vm.Runtime.Loading())
	assert.Equal(t, "false", vm.FunCall(vm.GetGlobal(lisp.Symbol("f")), lisp.SExpr(nil)).String())
}
