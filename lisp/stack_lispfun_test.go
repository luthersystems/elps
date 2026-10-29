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
	assert.False(t, stack.Top().LispFunction())
	assert.False(t, (*lisp.CallFrame)(nil).LispFunction())
}

// TestCallFrameSize pins the size of a CallFrame, which the evaluator
// allocates per call: the Lisp-function mark sits in padding.
func TestCallFrameSize(t *testing.T) {
	if strconv.IntSize != 64 {
		t.Skip("the size is pinned for 64-bit platforms")
	}
	assert.Equal(t, uintptr(72), unsafe.Sizeof(lisp.CallFrame{}))
}

// TestInLispFunctionSharedFIDTailCall pins the mark across tail calls
// (luthersystems/elps#908).  TRO matches frames by FID, and a Go function
// may take the FID of a lambda, so a lambda and a Go function with one FID
// tail-call each other in a single frame.  Each must see the mark of its
// own kind.
func TestInLispFunctionSharedFIDTailCall(t *testing.T) {
	env := templateTestEnv(t)
	require.NoError(t, lisp.GoError(env.InPackage(lisp.String(lisp.DefaultUserPackage))))
	type step struct {
		who    string
		inLisp bool
		depth  int
	}
	var log []step
	record := func(env *lisp.LEnv, who string) {
		stack := env.Runtime.Stack
		// The caller's frame, under the frame of record-lambda or g.
		caller := stack.Top()
		if who == "lambda" {
			caller = &stack.Frames[len(stack.Frames)-2]
		}
		assert.Equal(t, who == "lambda", caller.LispFunction(), "the frame of the %s", who)
		log = append(log, step{who, stack.InLispFunction(), len(stack.Frames)})
	}
	env.AddBuiltins(false, elpsutil.Function("record-lambda", lisp.Formals(),
		func(env *lisp.LEnv, _ *lisp.LVal) *lisp.LVal {
			record(env, "lambda")
			return lisp.Nil()
		}))
	// The lambda tail-calls the Go function g.
	f := env.LoadString("setup", `(lambda () (record-lambda) (g))`)
	require.Equal(t, lisp.LFun, f.Type, f.String())
	remaining := 3
	g := lisp.FunInPackage(lisp.DefaultUserPackage, f.FID(), lisp.Formals(),
		func(env *lisp.LEnv, _ *lisp.LVal) *lisp.LVal {
			record(env, "go")
			if remaining == 0 {
				return lisp.Symbol("done")
			}
			remaining--
			// A tail call from Go, as funcall makes one.
			env.Runtime.Stack.Top().Terminal = true
			return env.FunCall(f, lisp.SExpr(nil))
		})
	require.True(t, env.PutGlobal(lisp.Symbol("g"), g).IsNil())
	require.True(t, env.PutGlobal(lisp.Symbol("f"), f).IsNil())

	// Start once in the Go function and once in the lambda, so the frame
	// changes kind in both directions.
	for _, start := range []string{"go", "lambda"} {
		log, remaining = nil, 3
		src := map[string]string{"go": "(g)", "lambda": "(f)"}[start]
		require.Equal(t, "done", env.LoadString("case", src).String(), start)
		require.Len(t, log, 7+map[string]int{"go": 0, "lambda": 1}[start], start)
		for i, s := range log {
			want := (i%2 == 1) == (start == "go")
			assert.Equal(t, want, s.inLisp, "start %s, call %d (%s)", start, i, s.who)
			assert.Equal(t, map[bool]string{false: "go", true: "lambda"}[want], s.who, "start %s, call %d", start, i)
		}
		// The calls reuse one frame, so each kind runs at one depth.
		for i := 2; i < len(log); i++ {
			assert.Equal(t, log[i-2].depth, log[i].depth, "start %s: call %d did not reuse the frame", start, i)
		}
	}
}
