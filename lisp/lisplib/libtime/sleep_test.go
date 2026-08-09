// Copyright © 2026 The ELPS authors

// NOTE:  This file uses package name suffixed with _test to avoid an import
// cycle.  packages outside the standard library shouldn't need to use a _test
// suffix in their test files.
package libtime_test

import (
	"context"
	"strings"
	"testing"
	"time"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libtime"
	"github.com/luthersystems/elps/parser"
)

// forever is longer than any test is willing to wait.  It stands in for the
// unbounded duration issue #314 is about ("9223372036854775807ns" parses to
// time.Duration(math.MaxInt64), roughly 292 years); a value this size proves
// the same property without making a failing test hang for the full
// -timeout.
const forever = time.Hour

// slack is how long past the intended wake-up a correct implementation may
// take before the test calls it a hang.  Generous because CI runners are
// noisy; still four orders of magnitude below `forever`.
const slack = 30 * time.Second

// sleepEnv builds a minimal environment with the time package loaded and ctx
// installed as the evaluation context.  A nil ctx leaves the environment at
// its default, where LEnv.Context() reports context.Background().
func sleepEnv(t *testing.T, ctx context.Context) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	var configs []lisp.Config
	if ctx != nil {
		configs = append(configs, lisp.WithContext(ctx))
	}
	if rc := lisp.InitializeUserEnv(env, configs...); rc.Type == lisp.LError {
		t.Fatalf("initialize-user-env: %v", rc)
	}
	if rc := libtime.LoadPackage(env); rc.Type == lisp.LError {
		t.Fatalf("load time package: %v", rc)
	}
	if rc := env.InPackage(lisp.String(lisp.DefaultUserPackage)); rc.Type == lisp.LError {
		t.Fatalf("in-package: %v", rc)
	}
	return env
}

// callSleep invokes the builtin directly with a native duration.  Direct
// application is what the fuzz sweep does and it keeps the timing assertions
// free of parser and evaluator noise.
func callSleep(env *lisp.LEnv, d time.Duration) *lisp.LVal {
	return libtime.BuiltinSleep(env, lisp.SExpr([]*lisp.LVal{libtime.Duration(d)}))
}

// requireCancelled asserts v is the context-cancelled condition.  Any other
// error type would mean the sleep failed for an unrelated reason and the test
// would otherwise pass on the wrong evidence.
func requireCancelled(t *testing.T, v *lisp.LVal) {
	t.Helper()
	if v.Type != lisp.LError {
		t.Fatalf("expected an error, got %v (%v)", v.Type, v)
	}
	if v.Str != lisp.CondContextCancelled {
		t.Fatalf("expected condition %q, got %q (%v)", lisp.CondContextCancelled, v.Str, v)
	}
}

// runBounded runs fn on its own goroutine and fails if it outlives limit.
//
// A bare `go test -timeout` would also catch a hang, but it kills the whole
// binary with a goroutine dump and no indication of which assertion was being
// made.  The goroutine is deliberately leaked on timeout: an uninterruptible
// sleep is precisely the defect under test, so there is nothing to cancel.
func runBounded(t *testing.T, limit time.Duration, fn func() *lisp.LVal) (*lisp.LVal, time.Duration) {
	t.Helper()
	type result struct {
		v       *lisp.LVal
		elapsed time.Duration
	}
	done := make(chan result, 1)
	go func() {
		start := time.Now()
		v := fn()
		done <- result{v, time.Since(start)}
	}()
	select {
	case r := <-done:
		return r.v, r.elapsed
	case <-time.After(limit):
		t.Fatalf("sleep did not return within %v", limit)
		return nil, 0
	}
}

// TestSleepInterruptedByContextDeadline is the regression test for issue
// #314: a sleep far longer than the context deadline must end at the
// deadline, not at the caller's duration.
func TestSleepInterruptedByContextDeadline(t *testing.T) {
	t.Parallel()
	const budget = 50 * time.Millisecond
	ctx, cancel := context.WithTimeout(context.Background(), budget)
	defer cancel()
	env := sleepEnv(t, ctx)

	v, elapsed := runBounded(t, slack, func() *lisp.LVal { return callSleep(env, forever) })
	requireCancelled(t, v)
	if elapsed >= forever {
		t.Fatalf("slept %v, expected to wake at the %v deadline", elapsed, budget)
	}
}

// TestSleepInterruptedByCancel covers the other half of the contract: an
// explicit cancel, with no deadline involved, wakes the sleep too.
func TestSleepInterruptedByCancel(t *testing.T) {
	t.Parallel()
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	env := sleepEnv(t, ctx)

	time.AfterFunc(50*time.Millisecond, cancel)
	v, elapsed := runBounded(t, slack, func() *lisp.LVal { return callSleep(env, forever) })
	requireCancelled(t, v)
	if elapsed >= forever {
		t.Fatalf("slept %v, expected to wake on cancel", elapsed)
	}
}

// TestSleepInterruptedThroughEval proves the context actually reaches the
// builtin through ordinary evaluation, not just through a direct Go call.
// LEnv.call bridges the evaluation context onto the environment at the
// builtin boundary; if that bridge broke, the direct-call tests above would
// still pass while real ELPS programs stayed unbounded.
func TestSleepInterruptedThroughEval(t *testing.T) {
	t.Parallel()
	env := sleepEnv(t, nil)
	ctx, cancel := context.WithTimeout(context.Background(), 50*time.Millisecond)
	defer cancel()

	v, elapsed := runBounded(t, slack, func() *lisp.LVal {
		// 9223372036854775807ns is time.Duration(math.MaxInt64) -- the
		// literal from the issue, reachable from source alone.
		return env.LoadStringContext(ctx, "sleep_test.lisp",
			`(time:sleep (time:parse-duration "9223372036854775807ns"))`)
	})
	requireCancelled(t, v)
	if elapsed >= slack {
		t.Fatalf("slept %v, expected to wake at the deadline", elapsed)
	}
}

// TestSleepCompletesWithinDeadline guards the other direction: a sleep that
// fits inside the deadline must run to completion and return nil.  Without
// this a "cap everything" implementation that always errored would look
// correct.
func TestSleepCompletesWithinDeadline(t *testing.T) {
	t.Parallel()
	const nap = 20 * time.Millisecond
	ctx, cancel := context.WithTimeout(context.Background(), slack)
	defer cancel()
	env := sleepEnv(t, ctx)

	v, elapsed := runBounded(t, slack, func() *lisp.LVal { return callSleep(env, nap) })
	if v.Type == lisp.LError {
		t.Fatalf("expected nil, got error: %v", v)
	}
	if !v.IsNil() {
		t.Fatalf("expected nil, got %v", v)
	}
	if elapsed < nap {
		t.Fatalf("returned after %v, expected at least %v", elapsed, nap)
	}
}

// TestSleepNoContextUnaffected pins the no-context path, which is the
// compatibility promise: with no context configured the full duration is
// slept and nil is returned, exactly as before issue #314.
//
// It cannot assert the 292-year case directly, so it asserts the property
// that would break it: the default environment's context has no deadline and
// no Done channel, so there is nothing for the sleep to be truncated
// against, and a real sleep of a measurable length runs its full course.
func TestSleepNoContextUnaffected(t *testing.T) {
	t.Parallel()
	env := sleepEnv(t, nil)

	ctx := env.Context()
	if _, ok := ctx.Deadline(); ok {
		t.Fatalf("default environment context unexpectedly has a deadline")
	}
	if ctx.Done() != nil {
		t.Fatalf("default environment context unexpectedly has a Done channel")
	}

	const nap = 20 * time.Millisecond
	v, elapsed := runBounded(t, slack, func() *lisp.LVal { return callSleep(env, nap) })
	if v.Type == lisp.LError {
		t.Fatalf("expected nil, got error: %v", v)
	}
	if !v.IsNil() {
		t.Fatalf("expected nil, got %v", v)
	}
	if elapsed < nap {
		t.Fatalf("returned after %v, expected at least the full %v", elapsed, nap)
	}
}

// TestSleepAlreadyCancelled covers entry with a context that is already done:
// the sleep must not start at all.
func TestSleepAlreadyCancelled(t *testing.T) {
	t.Parallel()
	ctx, cancel := context.WithCancel(context.Background())
	cancel()
	env := sleepEnv(t, ctx)

	v, _ := runBounded(t, slack, func() *lisp.LVal { return callSleep(env, forever) })
	requireCancelled(t, v)
}

// TestSleepNonPositive keeps the degenerate durations a no-op under every
// context, matching time.Sleep.
func TestSleepNonPositive(t *testing.T) {
	t.Parallel()
	for _, d := range []time.Duration{0, -time.Hour} {
		ctx, cancel := context.WithTimeout(context.Background(), slack)
		env := sleepEnv(t, ctx)
		v := callSleep(env, d)
		if v.Type == lisp.LError || !v.IsNil() {
			t.Errorf("sleep(%v) = %v, want nil", d, v)
		}
		cancel()
	}
}

// TestSleepRejectsNonDuration keeps the argument checks intact -- they run
// before any waiting, so a bad argument is still an immediate error.
func TestSleepRejectsNonDuration(t *testing.T) {
	t.Parallel()
	env := sleepEnv(t, nil)
	for _, arg := range []*lisp.LVal{lisp.Int(5), lisp.Native("not-a-duration")} {
		v := libtime.BuiltinSleep(env, lisp.SExpr([]*lisp.LVal{arg}))
		if v.Type != lisp.LError {
			t.Errorf("sleep(%v) = %v, want an error", arg, v)
			continue
		}
		if !strings.Contains(v.String(), "not a duration") {
			t.Errorf("sleep(%v) error = %v, want a duration type error", arg, v)
		}
	}
}
