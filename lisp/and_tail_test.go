// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"fmt"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// U11 (#657): and must preserve the same final-argument tail position as or.
func TestLogicalTailSelfCallMillion(t *testing.T) {
	for _, tc := range []struct {
		name string
		body string
		want string
	}{
		{"and", `(and (> n 0) (process (- n 1) (+ acc 1)))`, "false"},
		{"or", `(or (= n 0) (process (- n 1) (+ acc 1)))`, "true"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			env := newLimitTestEnv(t)
			// Recording the accumulator proves all one million calls ran.
			source := fmt.Sprintf(`(set 'completed 0)
				(defun process (n acc) (set! completed acc) %s)
				(process 1000000 0)`, tc.body)
			result := env.LoadString("tail.lisp", source)
			require.NoError(t, lisp.GoError(result))
			assert.Equal(t, tc.want, result.String())
			assert.Equal(t, "1000000", env.Get(lisp.Symbol("completed")).String())
			assert.Empty(t, env.Runtime.Stack.Frames)
		})
	}
}

func TestAndValueAndShortCircuitSemantics(t *testing.T) {
	for _, tc := range []struct {
		name   string
		expr   string
		want   string
		visits string
		err    bool
	}{
		{"empty", `(and)`, "true", "0", false},
		{"single true", `(and true)`, "true", "0", false},
		{"single false", `(and false)`, "false", "0", false},
		{"single nil", `(and ())`, "()", "0", false},
		{"single value once", `(and (visit 42))`, "42", "1", false},
		{"first false", `(and false (visit 42))`, "false", "0", false},
		{"first nil", `(and () (error 'unexpected "ran"))`, "()", "0", false},
		{"middle false", `(and (visit 1) (visit false) (visit 3))`, "false", "2", false},
		{"middle nil", `(and (visit 1) (visit ()) (visit 3))`, "()", "2", false},
		{"last value and order", `(and (visit 1) (visit 2) (visit visits))`, "2", "3", false},
		{"last false", `(and 1 false)`, "false", "0", false},
		{"last nil", `(and 1 ())`, "()", "0", false},
		{"quoted value", `(and true '(1 2))`, "'(1 2)", "0", false},
		{"zero is truthy", `(and 0 42)`, "42", "0", false},
		{"first error", `(and (error 'expected "stop") (visit 1))`, "stop", "0", true},
		{"middle error", `(and (visit 1) (error 'expected "stop") (visit 3))`, "stop", "1", true},
		{"last error", `(and (visit 1) (error 'expected "stop"))`, "stop", "1", true},
		{"single error", `(and (error 'expected "stop"))`, "stop", "0", true},
	} {
		t.Run(tc.name, func(t *testing.T) {
			env := newLimitTestEnv(t)
			require.NoError(t, lisp.GoError(env.LoadString("and.lisp", `
				(set 'visits 0)
				(defun visit (value) (set! visits (+ visits 1)) value)`)))
			result := env.LoadString("and.lisp", tc.expr)
			if tc.err {
				require.Equal(t, lisp.LError, result.Type)
				assert.Equal(t, "expected", result.Str)
				assert.Contains(t, result.String(), tc.want)
				assert.False(t, lisp.IsInternalPanic(result))
			} else {
				require.NoError(t, lisp.GoError(result))
				assert.Equal(t, tc.want, result.String())
			}
			assert.Equal(t, tc.visits, env.Get(lisp.Symbol("visits")).String())
		})
	}
}

func TestRelatedFormsPreserveTailPosition(t *testing.T) {
	for _, tc := range []struct {
		name  string
		setup string
		body  string
		want  string
	}{
		{"and single", "", `(and (if (= n 0) 42 (loop (- n 1))))`, "42"},
		{"progn single", "", `(progn (if (= n 0) 42 (loop (- n 1))))`, "42"},
		{"cond single clause", "", `(cond (true (if (= n 0) 42 (loop (- n 1)))))`, "42"},
		{"let body", "", `(let ([next (- n 1)]) (if (= n 0) 42 (loop next)))`, "42"},
		{"let star body", "", `(let* ([next (- n 1)] [done (= n 0)]) true (if done 42 (loop next)))`, "42"},
		{"documented when macro", `(defmacro when (test &rest body)
			(quasiquote (if (unquote test) (progn (unquote-splicing body)) ())))`, `(when (> n 0) (loop (- n 1)))`, "()"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			env := newLimitTestEnv(t, lisp.WithMaximumPhysicalStackHeight(50))
			source := fmt.Sprintf(`%s (defun loop (n) %s) (loop 1000)`, tc.setup, tc.body)
			result := env.LoadString("tail.lisp", source)
			require.NoError(t, lisp.GoError(result))
			assert.Equal(t, tc.want, result.String())
			assert.Empty(t, env.Runtime.Stack.Frames)
		})
	}
}
