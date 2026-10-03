// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// LambdaEnv returns a lambda's captured frame, and nil for builtins, macros
// and special operators.  RestoreLambda rebuilds a lambda over a frame
// chain without evaluating anything.
func TestLambdaEnvAndRestoreLambda(t *testing.T) {
	env := templateTestEnv(t)
	f := env.LoadString("test", `(let ((n 41)) (lambda (x) (+ n x)))`)
	require.NoError(t, lisp.GoError(f))
	frame := f.LambdaEnv()
	require.NotNil(t, frame)
	require.NotNil(t, frame.Parent())
	assert.Equal(t, 1, frame.NumBindings())
	for _, src := range []string{`+`, `defun`, `if`} {
		assert.Nil(t, env.LoadString("test", src).LambdaEnv(), src)
	}
	assert.Nil(t, lisp.Int(1).LambdaEnv())

	root := env
	for root.Parent() != nil {
		root = root.Parent()
	}
	restored := lisp.NewEnv(root)
	require.NoError(t, lisp.GoError(restored.Put(lisp.Symbol("n"), lisp.Int(1))))
	g := restored.RestoreLambda(lisp.DefaultUserPackage, f.Cells[0], f.Cells[1:])
	require.NoError(t, lisp.GoError(g))
	assert.Equal(t, lisp.DefaultUserPackage, g.Package())
	assert.Same(t, restored, g.LambdaEnv())
	_, located := g.Source()
	assert.False(t, located)
	require.NoError(t, lisp.GoError(env.PutGlobal(lisp.Symbol("g"), g)))
	assert.Equal(t, "3", env.LoadString("test", `(g 2)`).String())

	assert.Equal(t, lisp.LError, restored.RestoreLambda("no-such-package", f.Cells[0], f.Cells[1:]).Type)
	bad := lisp.QExpr([]*lisp.LVal{lisp.Int(1)})
	assert.Equal(t, lisp.LError, restored.RestoreLambda(lisp.DefaultUserPackage, bad, nil).Type)
}
