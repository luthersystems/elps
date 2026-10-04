// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/internal/funraw"
	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// NewLambdaCode validates a lambda's code once; RestoreLambda rebuilds
// lambdas over frame chains from it without evaluating anything, and the
// lambdas share the code's cells.
func TestRestoreLambda(t *testing.T) {
	env := templateTestEnv(t)
	f := env.LoadString("test", `(let ((n 41)) (lambda (x) (+ n x)))`)
	require.NoError(t, lisp.GoError(f))
	frame := funraw.Env(f)
	require.NotNil(t, frame)
	require.NotNil(t, frame.Parent())

	root := env
	for root.Parent() != nil {
		root = root.Parent()
	}
	code, lerr := env.NewLambdaCode(f.Cells[0], f.Cells[1:])
	require.Nil(t, lerr)
	var restored []*lisp.LVal
	for _, n := range []int{1, 2} {
		fr := lisp.NewEnv(root)
		require.NoError(t, lisp.GoError(fr.Put(lisp.Symbol("n"), lisp.Int(n))))
		g := fr.RestoreLambda(lisp.DefaultUserPackage, code)
		require.NoError(t, lisp.GoError(g))
		assert.Equal(t, lisp.DefaultUserPackage, g.Package())
		assert.Same(t, fr, funraw.Env(g))
		_, located := g.Source()
		assert.False(t, located)
		restored = append(restored, g)
	}
	assert.Same(t, &restored[0].Cells[0], &restored[1].Cells[0], "the lambdas share the code's cells")
	require.NoError(t, lisp.GoError(env.PutGlobal(lisp.Symbol("g"), restored[1])))
	assert.Equal(t, "4", env.LoadString("test", `(g 2)`).String())

	assert.Equal(t, lisp.LError, root.RestoreLambda("no-such-package", code).Type)
	_, lerr = env.NewLambdaCode(lisp.QExpr([]*lisp.LVal{lisp.Int(1)}), nil)
	assert.NotNil(t, lerr)
	// Quoted formals, which lambda accepts, are accepted.
	_, lerr = env.NewLambdaCode(lisp.Formals("x"), []*lisp.LVal{lisp.Symbol("x")})
	assert.Nil(t, lerr)
}

// Code validated under legacy keyword formals is validated again in a
// runtime without them, so RestoreLambda accepts only what that runtime's
// lambda accepts.
func TestRestoreLambdaValidationPolicy(t *testing.T) {
	legacy := templateTestEnv(t)
	legacy.Runtime.LegacyKeywordFormals = true
	strict := templateTestEnv(t)
	formals := lisp.QExpr([]*lisp.LVal{lisp.Symbol(":x")})
	code, lerr := legacy.NewLambdaCode(formals, []*lisp.LVal{lisp.Int(1)})
	require.Nil(t, lerr, "legacy runtimes accept a keyword formal")
	assert.Equal(t, lisp.LFun, legacy.RestoreLambda(lisp.DefaultUserPackage, code).Type)
	assert.Equal(t, lisp.LError, strict.RestoreLambda(lisp.DefaultUserPackage, code).Type)
	assert.Equal(t, lisp.LError, strict.Lambda(formals, []*lisp.LVal{lisp.Int(1)}).Type)
}
