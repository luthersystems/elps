// Copyright © 2026 The ELPS authors

//go:build !elpscheck

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/elpsutil"
	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// The call of a Lisp function and of a kernel operator returns its list to
// the VM, and the next call takes it.  The elpscheck build never reuses a
// list, so these run in the default build only.
func TestCallListIsReused(t *testing.T) {
	env := newCallSemanticsEnv(t)
	require.NotEqual(t, lisp.LError, env.LoadString("t.lisp", `(defun id (x) x)`).Type)
	free := lisp.CallListsFree(env.Runtime)

	require.Equal(t, "7", env.LoadString("t.lisp", `(id 7)`).String())
	require.Equal(t, free+1, lisp.CallListsFree(env.Runtime), "the call did not return its list")
	require.Equal(t, "8", env.LoadString("t.lisp", `(id 8)`).String())
	require.Equal(t, free+1, lisp.CallListsFree(env.Runtime), "the second call did not take the free list")

	// A builtin may keep its list, and a &rest list is the call's cells.
	free = lisp.CallListsFree(env.Runtime)
	require.Equal(t, "'(1 2)", env.LoadString("t.lisp", `(list 1 2)`).String())
	require.NotEqual(t, lisp.LError, env.LoadString("t.lisp", `(defun rest-of (&rest xs) xs)`).Type)
	require.Equal(t, "'(1 2)", env.LoadString("t.lisp", `(rest-of 1 2)`).String())
	require.Equal(t, free, lisp.CallListsFree(env.Runtime), "a list that its call may keep was returned")
}

// A deep recursion returns one list per frame.  The free lists are bounded.
func TestCallListFreeListsAreBounded(t *testing.T) {
	env := newCallSemanticsEnv(t)
	got := env.LoadString("t.lisp", `
		(defun count-up (n) (if (= n 0) 0 (+ 1 (count-up (- n 1)))))
		(count-up 500)`)
	require.Equal(t, "500", got.String())
	assert.LessOrEqual(t, lisp.CallListsFree(env.Runtime), 3*lisp.CallListKeep)
	assert.Positive(t, lisp.CallListsFree(env.Runtime))
}

// A call of a Lisp function in a warm VM allocates no list.  The call's
// scope is the allocation that remains.
func TestCallListReuseSavesTheAllocation(t *testing.T) {
	env := newCallSemanticsEnv(t)
	require.NotEqual(t, lisp.LError, env.LoadString("t.lisp", `(defun id (x) x)`).Type)
	call := lisp.SExpr([]*lisp.LVal{lisp.Symbol("id"), lisp.Symbol("true")})
	require.Equal(t, "true", env.Eval(call).String())
	allocs := testing.AllocsPerRun(200, func() { env.Eval(call) })
	assert.LessOrEqual(t, allocs, 1.0, "a Lisp call in a warm VM allocated its list")
}

// reusesCallList is asked once for each call form.  A debugger that arrives
// or leaves while the arguments are evaluated does not change what happens
// to the list: one that arrived may have been shown the call, and a list
// built while one watched was not built for reuse.
func TestCallListRuleIsDecidedOncePerCall(t *testing.T) {
	env := newCallSemanticsEnv(t)
	env.AddBuiltins(true,
		elpsutil.Function("attach", lisp.Formals(), func(env *lisp.LEnv, _ *lisp.LVal) *lisp.LVal {
			env.Runtime.Debugger = dormantDebugger{}
			return lisp.Int(1)
		}),
		elpsutil.Function("detach", lisp.Formals(), func(env *lisp.LEnv, _ *lisp.LVal) *lisp.LVal {
			env.Runtime.Debugger = nil
			return lisp.Int(2)
		}))
	require.NotEqual(t, lisp.LError, env.LoadString("t.lisp", `(defun id (x) x)`).Type)

	// The debugger arrives inside the argument.  The list of (id ...) was
	// taken for reuse, and it is not returned.
	free := lisp.CallListsFree(env.Runtime)
	require.Equal(t, "1", env.LoadString("t.lisp", `(id (attach))`).String())
	assert.LessOrEqual(t, lisp.CallListsFree(env.Runtime), free,
		"a call list was returned although a debugger was attached during the call")

	// The debugger leaves inside the argument.  The list of (id ...) was
	// built while it watched, and it is not returned either.
	free = lisp.CallListsFree(env.Runtime)
	require.Equal(t, "2", env.LoadString("t.lisp", `(id (detach))`).String())
	assert.Equal(t, free, lisp.CallListsFree(env.Runtime),
		"a call list built under a debugger was returned")
}
