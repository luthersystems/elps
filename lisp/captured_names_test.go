// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// CapturedNames lists the names a lambda's captured frames bind, sorted
// and deduplicated, up to and including the root environment's own scope
// (empty here; TestCapturedNamesRootBindings binds names there).
func TestCapturedNames(t *testing.T) {
	env := templateTestEnv(t)
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

// rootComparatorEnv returns a root environment with direction bound in its
// own scope, as a host binds it with Put, and a comparator closing over it.
func rootComparatorEnv(t *testing.T, direction int) *lisp.LEnv {
	t.Helper()
	env := templateTestEnv(t)
	require.Nil(t, env.Parent())
	require.NoError(t, lisp.GoError(env.Put(lisp.Symbol("direction"), lisp.Int(direction))))
	require.NoError(t, lisp.GoError(env.LoadString("defs", `
(set 'global-scale 3)
(set 'compare (lambda (a b) (< (* direction a) (* direction b))))
(set 'scaled (let ((k 2)) (lambda (a b) (< (* direction k a) (* global-scale b)))))
`)))
	return env
}

// A name the root environment's own scope binds is captured: evaluation
// finds it before package globals.  Package globals (global-scale) are not.
func TestCapturedNamesRootBindings(t *testing.T) {
	env := rootComparatorEnv(t, -1)
	// The comparator reads the root binding: with direction -1, 1 sorts
	// after 2.
	got := env.LoadString("check", `(compare 1 2)`)
	require.NoError(t, lisp.GoError(got))
	assert.False(t, lisp.True(got))

	names, ok := lisp.CapturedNames(env.Get(lisp.Symbol("compare")))
	require.True(t, ok)
	assert.Equal(t, []string{"direction"}, names)
	names, ok = lisp.CapturedNames(env.Get(lisp.Symbol("scaled")))
	require.True(t, ok)
	assert.Equal(t, []string{"direction", "k"}, names)
}

// A template fork keeps the root's bindings, and so the captured names.
func TestCapturedNamesTemplateFork(t *testing.T) {
	env := rootComparatorEnv(t, -1)
	tmpl, err := lisp.NewTemplate(env, templateCorePolicy())
	require.NoError(t, err)
	vm, err := tmpl.NewVM()
	require.NoError(t, err)
	for name, want := range map[string][]string{
		"compare": {"direction"},
		"scaled":  {"direction", "k"},
	} {
		names, ok := lisp.CapturedNames(vm.Get(lisp.Symbol(name)))
		require.True(t, ok, name)
		assert.Equal(t, want, names, name)
	}
}
