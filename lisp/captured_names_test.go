// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// CapturedNames lists the names a lambda's captured frames bind, sorted
// and deduplicated, and stops before the root environment.
func TestCapturedNames(t *testing.T) {
	env := templateTestEnv(t)
	// A binding in the root environment is at the global level, not a
	// captured frame, so no closure lists it.
	require.Nil(t, env.Parent())
	require.NoError(t, lisp.GoError(env.Put(lisp.Symbol("root-binding"), lisp.Int(0))))
	require.NoError(t, lisp.GoError(env.LoadString("defs", `
(set 'global-x 1)
(defun top-level (a) (+ a global-x))
(set 'let-closure (let ((n 1) (b 2)) (lambda (x) (+ n b x))))
(set 'nested (let ((outer 1) (shared 2))
               (let ((inner 3) (shared 4))
                 (lambda () (+ outer inner shared)))))
(defun make-adder (k) (lambda (x) (+ k x)))
(set 'adder (make-adder 5))
(set 'no-captures (lambda (x) x))
(set 'unused (let ((z 1)) (lambda () 0)))
`)))
	for _, tc := range []struct {
		name string
		want []string
	}{
		{"top-level", []string{}},
		{"no-captures", []string{}},
		{"let-closure", []string{"b", "n"}},
		{"nested", []string{"inner", "outer", "shared"}},
		{"adder", []string{"k"}},
		// A captured name the code never reads is still captured.
		{"unused", []string{"z"}},
	} {
		t.Run(tc.name, func(t *testing.T) {
			fn := env.Get(lisp.Symbol(tc.name))
			require.Equal(t, lisp.LFun, fn.Type)
			steps := env.Runtime.TotalSteps()
			names, ok := lisp.CapturedNames(fn)
			assert.Equal(t, steps, env.Runtime.TotalSteps(), "CapturedNames counted steps")
			require.True(t, ok)
			assert.NotNil(t, names)
			assert.Equal(t, tc.want, names)
			// The result is a fresh snapshot: changing it changes nothing.
			if len(names) > 0 {
				names[0] = "changed"
				again, _ := lisp.CapturedNames(fn)
				assert.Equal(t, tc.want, again)
			}
		})
	}
}

// Values that are not lambdas report false: builtins, macros (builtin and
// user-defined), special operators, natives and other non-function values.
func TestCapturedNamesNotLambda(t *testing.T) {
	env := templateTestEnv(t)
	require.NoError(t, lisp.GoError(env.LoadString("defs",
		`(defmacro user-macro (x) (list 'quote x))`)))
	for _, name := range []string{"+", "defun", "if", "let", "map", "user-macro"} {
		fn := env.Get(lisp.Symbol(name))
		require.Equal(t, lisp.LFun, fn.Type, name)
		names, ok := lisp.CapturedNames(fn)
		assert.False(t, ok, name)
		assert.Nil(t, names, name)
	}
	for _, v := range []*lisp.LVal{
		nil,
		lisp.Int(1),
		lisp.String("f"),
		lisp.Symbol("f"),
		lisp.Nil(),
		lisp.QExpr([]*lisp.LVal{lisp.Int(1)}),
		lisp.Native(struct{}{}),
		&lisp.LVal{Type: lisp.LFun},
	} {
		names, ok := lisp.CapturedNames(v)
		assert.False(t, ok)
		assert.Nil(t, names)
	}
}

// The names are the same on every call, in any environment that builds the
// same closure, and in a template VM.
func TestCapturedNamesDeterministic(t *testing.T) {
	const src = `(set 'f (let ((m 1) (c 2) (a 3) (q 4) (b 5) (z 6) (k 7))
                         (let ((y 8) (x 9) (a 10))
                           (lambda () (list m c a q b z k y x)))))`
	want := []string{"a", "b", "c", "k", "m", "q", "x", "y", "z"}
	var envs []*lisp.LEnv
	for range 3 {
		env := templateTestEnv(t)
		require.NoError(t, lisp.GoError(env.LoadString("defs", src)))
		envs = append(envs, env)
	}
	tmpl, err := lisp.NewTemplate(envs[0], templateCorePolicy())
	require.NoError(t, err)
	vm, err := tmpl.NewVM()
	require.NoError(t, err)
	envs = append(envs, vm)
	for _, env := range envs {
		for range 20 {
			names, ok := lisp.CapturedNames(env.Get(lisp.Symbol("f")))
			require.True(t, ok)
			require.Equal(t, want, names)
		}
	}
}
