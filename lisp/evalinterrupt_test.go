// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"context"
	"testing"
	"time"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// Interruptibility of Go-level loops driven by program-supplied counts.
//
// Every evaluation limit ELPS offers -- WithMaxSteps, the context deadline,
// the stack heights -- is enforced by LEnv.checkLimits, which only runs where
// the interpreter calls it.  A builtin or special operator that loops inside
// Go without reaching a check is therefore outside ALL of them: the budget is
// configured, the deadline has passed, and the loop keeps going.  That is not
// a slow program; it is an unbounded one, and in substrate it is a wedged
// transaction that no timeout unwedges.
//
// These tests assert interruptibility directly -- budget configured, budget
// honoured, in bounded wall-clock time -- rather than asserting termination,
// which is what the fuzz harness's watchdog already does.  Both sites below
// were found by FuzzEval (issue #320).

// runBounded evaluates src with the given configs and fails if the evaluation
// has not returned within limit.  The evaluation goroutine is deliberately
// leaked on timeout: if it were interruptible it would already have stopped,
// and the test has failed either way.
func runBounded(t *testing.T, limit time.Duration, src string, cfg ...lisp.Config) (*lisp.LVal, time.Duration) {
	t.Helper()
	env := newLimitTestEnv(t, cfg...)
	type done struct {
		res     *lisp.LVal
		elapsed time.Duration
	}
	ch := make(chan done, 1)
	go func() {
		start := time.Now()
		res := env.LoadString("test", src)
		ch <- done{res: res, elapsed: time.Since(start)}
	}()
	select {
	case d := <-ch:
		return d.res, d.elapsed
	case <-time.After(limit):
		t.Fatalf("evaluation of %q did not return within %s despite its configured limits", src, limit)
		return nil, 0
	}
}

// TestDoTimesHonoursStepBudget pins that dotimes is interruptible even with an
// EMPTY body.
//
// dotimes drives its own Go loop over a count the program supplies.  With a
// body, the body's Eval reaches checkLimits and the loop is bounded as a side
// effect.  With no body there is nothing to evaluate, so nothing checked
// anything: `(dotimes (i 1000000000))` ignored a 2s context deadline and a
// 2,000,000-step budget alike and was still running 30 seconds later.
//
// The empty-body case is the one that matters and the one a test is most
// likely to omit, so it is asserted first.
func TestDoTimesHonoursStepBudget(t *testing.T) {
	t.Parallel()
	for _, tt := range []struct {
		name string
		src  string
	}{
		{"empty body", `(dotimes (i 1000000000))`},
		{"with body", `(dotimes (i 1000000000) 1)`},
		{"with return expression", `(dotimes (i 1000000000 'done))`},
	} {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()
			res, elapsed := runBounded(t, 30*time.Second, tt.src, lisp.WithMaxSteps(10000))
			require.Equal(t, lisp.LError, res.Type,
				"a 1e9-iteration dotimes must be stopped by a 10,000-step budget, got %v after %s",
				res, elapsed)
			assert.Contains(t, res.String(), "step limit exceeded",
				"expected the step budget to be the limit that fired")
			// 10,000 steps of an empty loop body is microseconds of work.  A
			// second of slack absorbs a loaded CI machine while still failing
			// loudly if the loop is running to completion.
			assert.Less(t, elapsed, time.Second,
				"the step budget should stop this almost immediately, took %s", elapsed)
		})
	}
}

// TestDoTimesHonoursContextDeadline pins the same property for the only limit
// that measures TIME.  A step budget bounds work, not duration; a host that
// wants "stop after 50ms" has nothing else to reach for.
func TestDoTimesHonoursContextDeadline(t *testing.T) {
	t.Parallel()
	env := newLimitTestEnv(t)
	ctx, cancel := context.WithTimeout(context.Background(), 50*time.Millisecond)
	defer cancel()

	type done struct {
		res     *lisp.LVal
		elapsed time.Duration
	}
	ch := make(chan done, 1)
	go func() {
		start := time.Now()
		res := env.LoadStringContext(ctx, "test", `(dotimes (i 1000000000))`)
		ch <- done{res: res, elapsed: time.Since(start)}
	}()
	select {
	case d := <-ch:
		require.Equal(t, lisp.LError, d.res.Type,
			"an empty-bodied 1e9-iteration dotimes must honour a 50ms deadline, got %v", d.res)
		assert.Contains(t, d.res.String(), "context cancelled")
		assert.Less(t, d.elapsed, 5*time.Second,
			"the deadline should fire promptly, took %s", d.elapsed)
	case <-time.After(30 * time.Second):
		t.Fatal("dotimes ignored a 50ms context deadline for 30s")
	}
}

// TestDoTimesStillIterates is the false-positive guard for the two tests
// above: a limit check added to a loop must not change what the loop does.
func TestDoTimesStillIterates(t *testing.T) {
	t.Parallel()
	env := newLimitTestEnv(t)
	res := env.LoadString("test", `(let ([n 0]) (dotimes (i 10) (set! n (+ n 1))) n)`)
	require.NotEqual(t, lisp.LError, res.Type, "got %v", res)
	assert.Equal(t, "10", res.String())

	// The loop variable is left bound to the iteration count, and the
	// optional third element of the control sequence is the return value.
	res = env.LoadString("test", `(dotimes (i 5 i))`)
	require.NotEqual(t, lisp.LError, res.Type, "got %v", res)
	assert.Equal(t, "5", res.String())

	// A zero or negative count runs no iterations rather than erroring.
	for _, src := range []string{`(dotimes (i 0) 1)`, `(dotimes (i -1) 1)`} {
		res = env.LoadString("test", src)
		assert.NotEqual(t, lisp.LError, res.Type, "%s: got %v", src, res)
	}
}

// TestExprFormalsBoundIsInclusive pins both ends of MaxExprFormals.  The
// at-the-bound case builds a 1024-formal lambda, whose printed form is far too
// large for a TestSequence row, so it lives here rather than in op_test.go
// alongside the rest of the placeholder-index rows.
func TestExprFormalsBoundIsInclusive(t *testing.T) {
	t.Parallel()
	env := newLimitTestEnv(t)

	at := env.LoadString("test", `#^(list %1024)`)
	require.NotEqual(t, lisp.LError, at.Type,
		"an index exactly at MaxExprFormals must be accepted, got: %v", at)
	require.Equal(t, lisp.LFun, at.Type)

	over := env.LoadString("test", `#^(list %1025)`)
	require.Equal(t, lisp.LError, over.Type, "an index past MaxExprFormals must be rejected")
	assert.Contains(t, over.String(), "exceeds the maximum of 1024")

	// The bound is documented as a constant embedders can read.
	assert.Equal(t, 1024, lisp.MaxExprFormals)
}

// TestExprFormalsLoopHonoursStepBudget pins that opExpr's formals loop is
// interruptible.
//
// MaxExprFormals already bounds this loop at 1024 iterations, so today the
// check cannot be the difference between terminating and not.  It is asserted
// anyway because the loop's step-blindness was a distinct defect from its
// unboundedness -- before the bound existed, WithMaxSteps(10) plus a 100ms
// context deadline did not stop `#^(list %8000000)` until it had run for 2.4
// seconds -- and because a future change to the bound must not silently
// reintroduce an uninterruptible allocation loop.
func TestExprFormalsLoopHonoursStepBudget(t *testing.T) {
	t.Parallel()
	env := newLimitTestEnv(t, lisp.WithMaxSteps(8))
	res := env.LoadString("test", `#^(list %512)`)
	require.Equal(t, lisp.LError, res.Type,
		"constructing 512 formals must consume the step budget, got: %v", res)
	assert.Contains(t, res.String(), "step limit exceeded")
}
