// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"context"
	"testing"

	"github.com/luthersystems/elps/elpstest"
	"github.com/luthersystems/elps/elpsutil"
	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/require"
)

// try is the fallback chain of luthersystems/elps#973.  These tests pin the
// two properties that make it more than (or (ignore-errors a) b): a result
// of () or false is a success, and a condition that stops an evaluation is
// never turned into a fallback value.

func TestTry(t *testing.T) {
	tests := elpstest.TestSuite{
		{"empty and single", elpstest.TestSequence{
			{`(try)`, `()`, ``},
			{`(try 1)`, `1`, ``},
			{`(try (error 'boom "x"))`, `test:1:6: boom: x`, ``},
		}},
		{"first success wins", elpstest.TestSequence{
			{`(try 1 2)`, `1`, ``},
			{`(try (error 'a "1") 2 3)`, `2`, ``},
			{`(try (error 'a "1") (error 'b "2") 'fallback)`, `'fallback`, ``},
			{`(try (car 1) (aref (vector) 0) "default")`, `"default"`, ``},
		}},
		{"nil and false are successes", elpstest.TestSequence{
			// (or (ignore-errors ...) ...) falls through on these.
			{`(try () 'not-reached)`, `()`, ``},
			{`(try false 'not-reached)`, `false`, ``},
			{`(or (ignore-errors ()) 'reached)`, `'reached`, ``},
		}},
		{"last error propagates unchanged", elpstest.TestSequence{
			{`(handler-bind ((condition (lambda (c &rest d) (list c d))))
			    (try (error 'first "1") (error 'last "2" 3)))`, `'('last '("2" 3))`, ``},
			{`(try (error 'a "1") (try (error 'b "2") (error 'c "3")))`, `test:1:41: c: 3`, ``},
		}},
		{"order and side effects", elpstest.TestSequence{
			{`(set 'trace ())`, `()`, ``},
			{`(try (progn (set! trace (cons 1 trace)) (error 'a "x"))
			       (progn (set! trace (cons 2 trace)) 'ok)
			       (progn (set! trace (cons 3 trace)) 'never))`, `'ok`, ``},
			// The failed form's side effect stays; the form after the
			// first success never runs.
			{`trace`, `'(2 1)`, ``},
		}},
		{"forged internal-panic is ordinary", elpstest.TestSequence{
			{`(try (error 'internal-panic "forged") 'caught)`, `'caught`, ``},
		}},
		{"deep data lookup", elpstest.TestSequence{
			{`(set 'm (sorted-map "api" (vector (sorted-map "target" "x.example.com"))))`, `(sorted-map "api" (vector (sorted-map "target" "x.example.com")))`, ``},
			{`(try (get (aref (get m "api") 0) "target") ())`, `"x.example.com"`, ``},
			{`(try (get (aref (get m "nope") 0) "target") ())`, `()`, ``},
		}},
	}
	elpstest.RunTestSuite(t, tests)
}

// A recovered Go panic is a host defect.  try must not drop it and run a
// fallback over unknown state, for the same reason ignore-errors does not.
func TestTryDoesNotCatchInternalPanic(t *testing.T) {
	env := withCleanupTestEnv(t)
	res := env.LoadString("try_test.lisp", `(try (host-panic) 'fallback)`)
	require.True(t, lisp.IsInternalPanic(res), "got %v", res)
}

// The step limit, the step budget and context cancellation stop an
// evaluation.  try must let each one propagate.  Each case fails with the
// condition, never with the fallback 5, a literal that a step-free
// evaluator path could return.  TestTryCatchesRefusesStops pins the rule
// itself, because today the evaluator also raises a stop again on the
// fallback's first step.
func TestTryDoesNotCatchEvaluationStops(t *testing.T) {
	const spin = `(dotimes (i 1000000) i)`

	t.Run("step limit", func(t *testing.T) {
		env := newLimitTestEnv(t, lisp.WithMaxSteps(500))
		res := env.LoadString("try_test.lisp", `(try `+spin+` 5)`)
		requireStepLimit(t, res)
	})

	t.Run("step budget", func(t *testing.T) {
		env := newLimitTestEnv(t)
		env.Runtime.SetStepBudget(500)
		res := env.LoadString("try_test.lisp", `(try `+spin+` 5)`)
		requireStepBudget(t, res)
	})

	t.Run("context cancelled", func(t *testing.T) {
		env := newLimitTestEnv(t)
		ctx, cancel := context.WithCancel(t.Context())
		defer cancel()
		env.AddBuiltins(true, elpsutil.Function("cancel-now", lisp.Formals(),
			func(*lisp.LEnv, *lisp.LVal) *lisp.LVal {
				cancel()
				return lisp.Nil()
			}))
		res := env.LoadStringContext(ctx, "try_test.lisp",
			`(try (progn (cancel-now) `+spin+`) 5)`)
		require.Equal(t, lisp.LError, res.Type, "got %v", res)
		require.Equal(t, lisp.CondContextCancelled, res.Str, "got %v", res)
	})
}
