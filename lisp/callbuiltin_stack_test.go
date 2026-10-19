// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// CallBuiltin must restore the calling builtin's frame even when the callee
// grows the call stack and moves its frames (luthersystems/elps#852).
// funcall marks the caller's frame terminal; CallBuiltin must clear that
// mark on the frame itself, not on a copy the stack left behind.
func TestCallBuiltinStackGrowthRestoresFrame(t *testing.T) {
	env := newLimitTestEnv(t)
	var before, after bool
	env.AddBuiltins(false,
		&testBuiltinDef{name: "run-hook", formals: lisp.Formals("f"),
			fn: func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
				before = env.Runtime.Stack.Top().Terminal
				v := env.CallBuiltin(refFuncall, args.Cells[0])
				after = env.Runtime.Stack.Top().Terminal
				return v
			}},
	)
	v := env.LoadString("test", `
(defun deep (n) (if (> n 0) (+ 1 (deep (- n 1))) 0))
(run-hook (lambda () (deep 2000)))`)
	require.NotEqual(t, lisp.LError, v.Type, "%v", v)
	assert.Equal(t, "2000", v.String())
	assert.Equal(t, before, after, "the calling builtin's frame changed")
}

// A callee that grows the stack and then tail-calls the builtin that
// called CallBuiltin runs that call (luthersystems/elps#852): the
// calling frame must not stay terminal and let the tail call escape
// CallBuiltin as a marker.
func TestCallBuiltinStackGrowthTailCallRuns(t *testing.T) {
	env := newLimitTestEnv(t)
	env.AddBuiltins(false,
		&testBuiltinDef{name: "run-hooks", formals: lisp.Formals("&rest", "fs"),
			fn: func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
				last := lisp.Nil()
				for _, f := range args.Cells {
					last = env.CallBuiltin(refFuncall, f)
					if last.Type == lisp.LError {
						return last
					}
					if last.Type != lisp.LInt && last.Type != lisp.LSymbol && !last.IsNil() {
						return env.Errorf("CallBuiltin leaked a %v", last.Type)
					}
				}
				return last
			}},
	)
	v := env.LoadString("test", `
(set 'ran 0)
(defun deep (n) (if (> n 0) (+ 1 (deep (- n 1))) 0))
(defun mark () (set 'ran (+ ran 1)) 'marked)
(run-hooks
  (lambda () (deep 2000) (run-hooks mark))
  (lambda () (deep 10) (run-hooks mark)))
ran`)
	require.NotEqual(t, lisp.LError, v.Type, "%v", v)
	assert.Equal(t, "2", v.String())
}
