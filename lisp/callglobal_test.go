// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestCallGlobal(t *testing.T) {
	env := newLimitTestEnv(t)
	require.NotEqual(t, lisp.LError, env.LoadString("test", `(defun twice (x) (* 2 x))`).Type)
	env.AddBuiltins(false,
		&testBuiltinDef{name: "nop0", formals: lisp.Formals(), fn: func(*lisp.LEnv, *lisp.LVal) *lisp.LVal { return lisp.Nil() }},
		&testBuiltinDef{name: "nop1", formals: lisp.Formals("x"), fn: func(*lisp.LEnv, *lisp.LVal) *lisp.LVal { return lisp.Nil() }},
		&testBuiltinDef{name: "go-twice", formals: lisp.Formals(), fn: func(env *lisp.LEnv, _ *lisp.LVal) *lisp.LVal {
			return env.CallGlobal("user:twice", lisp.Int(21))
		}},
		&testBuiltinDef{name: "go-call", formals: lisp.Formals("sym"), fn: func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
			return env.CallGlobal(args.Cells[0].Str, lisp.Int(1))
		}})

	v, goSteps := stepsOf(t, env, `(go-twice)`)
	require.Equal(t, 42, v.Int)
	_, nop0 := stepsOf(t, env, `(nop0)`)
	w, lispSteps := stepsOf(t, env, `(twice 21)`)
	require.Equal(t, 42, w.Int)
	_, nop1 := stepsOf(t, env, `(nop1 21)`)
	// Net of call overhead, CallGlobal costs exactly what the callee's body
	// evaluates: no step of its own.
	assert.Equal(t, lispSteps-nop1, goSteps-nop0)

	// The binding is resolved at call time: rebinding is seen.
	env.LoadString("test", `(defun twice (x) (+ x x x))`)
	v, _ = stepsOf(t, env, `(go-twice)`)
	assert.Equal(t, 63, v.Int)

	for sym, want := range map[string]string{
		"user:no-such": "unbound symbol",
		"lisp:if":      "not a regular function",
	} {
		v, _ := stepsOf(t, env, `(go-call "`+sym+`")`)
		require.Equal(t, lisp.LError, v.Type, sym)
		assert.Contains(t, lisp.GoError(v).Error(), want, sym)
	}
}
