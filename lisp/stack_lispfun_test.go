// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"strconv"
	"testing"
	"unsafe"

	"github.com/luthersystems/elps/elpsutil"
	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// TestInLispFunction pins CallStack.InLispFunction (luthersystems/elps#908):
// true under a lambda, defun, defmacro, flet or labels function, and false
// for top-level forms, also inside special operators, Go macros and loads.
// The answer must be the same in a cold-loaded VM and in a template VM.
func TestInLispFunction(t *testing.T) {
	setup := `
(defun f () (in-lisp?))
(defun tail (n) (if (= n 0) (in-lisp?) (tail (- n 1))))
(defmacro m () (if (in-lisp?) ''yes ''no))
`
	cases := []struct {
		src  string
		want string
	}{
		{`(in-lisp?)`, "false"},
		{`(let ([x 1]) (progn (when x (in-lisp?))))`, "false"},
		{`(list (in-lisp?))`, "'(false)"},
		{`(flet ([g () 1]) (in-lisp?))`, "false"},
		{`(load-string "(in-lisp?)")`, "false"},
		{`(ignore-errors (in-lisp?))`, "false"},
		{`(f)`, "true"},
		{`(tail 50)`, "true"},
		{`(funcall (lambda () (in-lisp?)))`, "true"},
		{`(flet ([g () (in-lisp?)]) (g))`, "true"},
		{`(labels ([g () (in-lisp?)]) (g))`, "true"},
		{`(m)`, "'yes"},
		{`(funcall (lambda () (load-string "(in-lisp?)")))`, "true"},
		// A Go function counts as Go whatever its FID says.
		{`(go-fun)`, "false"},
	}
	load := func() *lisp.LEnv {
		env := templateTestEnv(t)
		require.NoError(t, lisp.GoError(env.InPackage(lisp.String(lisp.DefaultUserPackage))))
		env.AddBuiltins(false, elpsutil.Function("in-lisp?", lisp.Formals(),
			func(env *lisp.LEnv, _ *lisp.LVal) *lisp.LVal {
				return lisp.Bool(env.Runtime.Stack.InLispFunction())
			}))
		inLisp := func(env *lisp.LEnv, _ *lisp.LVal) *lisp.LVal {
			return lisp.Bool(env.Runtime.Stack.InLispFunction())
		}
		require.True(t, env.PutGlobal(lisp.Symbol("go-fun"), lisp.FunInPackage(lisp.DefaultUserPackage, "_fun99999", lisp.Formals(), inLisp)).IsNil())
		require.NotEqual(t, lisp.LError, env.LoadString("setup", setup).Type)
		return env
	}
	tmpl, err := lisp.NewTemplate(load(), templateCorePolicy())
	require.NoError(t, err)
	vm, err := tmpl.NewVM()
	require.NoError(t, err)
	cold := load()
	for _, tc := range cases {
		assert.Equal(t, tc.want, cold.LoadString("case", tc.src).String(), "cold %s", tc.src)
		assert.Equal(t, tc.want, vm.LoadString("case", tc.src).String(), "template VM %s", tc.src)
	}
	assert.False(t, (*lisp.CallStack)(nil).InLispFunction())

	// A frame an embedder pushes is not a Lisp function, whatever its FID.
	var stack lisp.CallStack
	require.NoError(t, stack.PushFID(nil, "_fun1", "user", "f"))
	assert.False(t, stack.InLispFunction())
}

// TestCallFrameSize pins the size of a CallFrame, which the evaluator
// allocates per call: the Lisp-function mark sits in padding.
func TestCallFrameSize(t *testing.T) {
	if strconv.IntSize != 64 {
		t.Skip("the size is pinned for 64-bit platforms")
	}
	assert.Equal(t, uintptr(72), unsafe.Sizeof(lisp.CallFrame{}))
}
