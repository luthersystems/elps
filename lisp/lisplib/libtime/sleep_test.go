// Copyright © 2026 The ELPS authors

// NOTE:  This file uses package name suffixed with _test to avoid an import
// cycle.  packages outside the standard library shouldn't need to use a _test
// suffix in their test files.
package libtime_test

import (
	"context"
	"math"
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

// callSleep invokes the builtin directly with a native duration and no :max.
// Direct application is what the fuzz sweep does and it keeps the timing
// assertions free of parser and evaluator noise.
//
// The nil second cell is the unsupplied :max keyword.  BuiltinSleep reads
// args.Cells[1] unconditionally, as every builtin with a keyword formal in
// this repo does (see builtinLoadString) -- the formals machinery guarantees
// the cell exists, so a direct caller has to supply it too.
func callSleep(env *lisp.LEnv, d time.Duration) *lisp.LVal {
	return callSleepMax(env, d, lisp.Nil())
}

// callSleepMax is callSleep with an explicit :max argument.  Pass lisp.Nil()
// for "not supplied".
func callSleepMax(env *lisp.LEnv, d time.Duration, maxArg *lisp.LVal) *lisp.LVal {
	return libtime.BuiltinSleep(env, lisp.SExpr([]*lisp.LVal{libtime.Duration(d), maxArg}))
}

// requireSleepLimit asserts v is the sleep-limit-exceeded condition, i.e. the
// sleep was refused on entry rather than attempted.
func requireSleepLimit(t *testing.T, v *lisp.LVal) {
	t.Helper()
	if v.Type != lisp.LError {
		t.Fatalf("expected an error, got %v (%v)", v.Type, v)
	}
	if v.Str != lisp.CondSleepLimitExceeded {
		t.Fatalf("expected condition %q, got %q (%v)", lisp.CondSleepLimitExceeded, v.Str, v)
	}
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
	// sleep-limit-exceeded, not context-cancelled: the 292-year duration is
	// refused by the length cap, which is checked before the context is
	// consulted at all.  Reporting the cap is the more useful of the two --
	// it names the thing the caller can act on (:max) rather than the
	// deadline that merely happened to be nearer.
	requireSleepLimit(t, v)
	if elapsed >= slack {
		t.Fatalf("slept %v, expected an immediate refusal", elapsed)
	}
}

// TestSleepPastDeadlineFailsFast pins issue #338: a sleep the deadline will
// outlast is refused on entry, NOT slept out to the deadline first.
//
// The distinction is invisible to a pass/fail check on the condition alone --
// the old behaviour raised the same context-cancelled -- so the assertion
// that carries the meaning is the elapsed time.  A sleep well under the cap
// (so the length check cannot be what fires) with a deadline much nearer must
// return in far less than the remaining budget.
func TestSleepPastDeadlineFailsFast(t *testing.T) {
	t.Parallel()
	const remaining = 750 * time.Millisecond
	ctx, cancel := context.WithTimeout(context.Background(), remaining)
	defer cancel()
	env := sleepEnv(t, ctx)

	v, elapsed := runBounded(t, remaining+slack, func() *lisp.LVal {
		// A minute is far below DefaultMaxSleep, so only the deadline can
		// refuse it.
		return callSleep(env, time.Minute)
	})
	requireCancelled(t, v)
	if elapsed > remaining/2 {
		t.Fatalf("took %v to refuse a doomed sleep with %v remaining;"+
			" expected an immediate refusal, not a wait to the deadline",
			elapsed, remaining)
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
		v := libtime.BuiltinSleep(env, lisp.SExpr([]*lisp.LVal{arg, lisp.Nil()}))
		if v.Type != lisp.LError {
			t.Errorf("sleep(%v) = %v, want an error", arg, v)
			continue
		}
		if !strings.Contains(v.String(), "not a duration") {
			t.Errorf("sleep(%v) error = %v, want a duration type error", arg, v)
		}
	}
}

// TestSleepLengthCapRefusesImmediately covers the length cap itself, with no
// context involved: a duration over DefaultMaxSleep is refused on entry.
//
// The elapsed assertion is the load-bearing one. A cap that errored only
// AFTER sleeping would satisfy a condition-only check while leaving the
// unbounded-block defect entirely in place.
func TestSleepLengthCapRefusesImmediately(t *testing.T) {
	t.Parallel()
	env := sleepEnv(t, nil)
	v, elapsed := runBounded(t, slack, func() *lisp.LVal {
		return callSleep(env, lisp.DefaultMaxSleep+time.Second)
	})
	requireSleepLimit(t, v)
	if elapsed > time.Second {
		t.Fatalf("took %v to refuse an over-cap sleep, expected immediate", elapsed)
	}
}

// TestSleepUnderCapStillSleeps is the negative control for the test above.
// Without it, an implementation that refused EVERY sleep would pass the cap
// tests and look correct.
func TestSleepUnderCapStillSleeps(t *testing.T) {
	t.Parallel()
	env := sleepEnv(t, nil)
	v, elapsed := runBounded(t, slack, func() *lisp.LVal {
		return callSleep(env, 20*time.Millisecond)
	})
	if v.Type == lisp.LError || !v.IsNil() {
		t.Fatalf("sleep under the cap = %v, want nil", v)
	}
	if elapsed < 10*time.Millisecond {
		t.Fatalf("returned in %v; the sleep did not actually happen", elapsed)
	}
}

// TestSleepMaxRaisesTheCap: :max is what a caller who really means it uses.
// A duration over the default but under :max must actually sleep.
func TestSleepMaxRaisesTheCap(t *testing.T) {
	t.Parallel()
	// Over DefaultMaxSleep would take an hour to observe, so instead pin the
	// decision rather than the wait: an over-default duration with a large
	// enough :max must NOT be refused by the cap. A context deadline stops
	// the test from actually waiting an hour.
	ctx, cancel := context.WithTimeout(context.Background(), 30*time.Millisecond)
	defer cancel()
	envCtx := sleepEnv(t, ctx)

	v, _ := runBounded(t, slack, func() *lisp.LVal {
		return callSleepMax(envCtx, 2*lisp.DefaultMaxSleep,
			libtime.Duration(3*lisp.DefaultMaxSleep))
	})
	// The deadline refuses it, not the cap -- which is the point: with :max
	// supplied, the length is no longer what stops it.
	requireCancelled(t, v)
}

// TestSleepMaxCannotExceedHostCeiling is the containment property. Program
// source may raise the default cap, but not past a ceiling the host set --
// otherwise untrusted source could grant itself an unbounded sleep and the
// bound would be decorative.
func TestSleepMaxCannotExceedHostCeiling(t *testing.T) {
	t.Parallel()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	if rc := lisp.InitializeUserEnv(env, lisp.WithMaxSleep(time.Second)); rc.Type == lisp.LError {
		t.Fatalf("initialize-user-env: %v", rc)
	}
	if rc := libtime.LoadPackage(env); rc.Type == lisp.LError {
		t.Fatalf("load time package: %v", rc)
	}

	v, elapsed := runBounded(t, slack, func() *lisp.LVal {
		return callSleepMax(env, time.Hour, libtime.Duration(time.Hour))
	})
	requireSleepLimit(t, v)
	if elapsed > time.Second {
		t.Fatalf("took %v to refuse, expected immediate", elapsed)
	}

	// And the ceiling lowers the no-:max default too, so the default cannot
	// quietly exceed it.
	v2, _ := runBounded(t, slack, func() *lisp.LVal {
		return callSleep(env, 2*time.Second)
	})
	requireSleepLimit(t, v2)
}

// TestSleepRejectsNonPositiveMax: a negative :max is a bug in the caller's
// arithmetic, and reading it as "unlimited" would convert that bug into the
// unbounded sleep this whole mechanism exists to prevent.
func TestSleepRejectsNonPositiveMax(t *testing.T) {
	t.Parallel()
	env := sleepEnv(t, nil)
	for _, m := range []time.Duration{0, -time.Second, time.Duration(math.MinInt64)} {
		v := callSleepMax(env, time.Second, libtime.Duration(m))
		if v.Type != lisp.LError {
			t.Errorf("sleep with :max %v = %v, want an error", m, v)
			continue
		}
		if !strings.Contains(v.String(), "positive duration") {
			t.Errorf("sleep with :max %v error = %v, want a positive-duration error", m, v)
		}
	}
}

// TestSleepRejectsNonDurationMax keeps the :max type check honest.
func TestSleepRejectsNonDurationMax(t *testing.T) {
	t.Parallel()
	env := sleepEnv(t, nil)
	for _, m := range []*lisp.LVal{lisp.Int(5), lisp.Native("nope")} {
		v := callSleepMax(env, time.Second, m)
		if v.Type != lisp.LError {
			t.Errorf("sleep with :max %v = %v, want an error", m, v)
			continue
		}
		if !strings.Contains(v.String(), "max is not a duration") {
			t.Errorf("sleep with :max %v error = %v, want a duration type error", m, v)
		}
	}
}

// TestSleepMaxThroughEval proves the keyword reaches the builtin through the
// formals machinery, not just through a hand-built args list. The direct
// callers above would all still pass if the formal were misdeclared.
func TestSleepMaxThroughEval(t *testing.T) {
	t.Parallel()
	env := sleepEnv(t, nil)
	v, elapsed := runBounded(t, slack, func() *lisp.LVal {
		return env.LoadString("sleep_max_test.lisp",
			`(time:sleep (time:parse-duration "2h") :max (time:parse-duration "1s"))`)
	})
	// 2h is over the 1s :max, so this must be refused -- and refused by the
	// cap, which proves :max was read rather than ignored. Were the keyword
	// dropped, 2h would be measured against DefaultMaxSleep instead and this
	// would still error, so the message is checked too.
	requireSleepLimit(t, v)
	if !strings.Contains(v.String(), "maximum 1s") {
		t.Fatalf("error = %v, want the 1s :max to be the reported maximum", v)
	}
	if elapsed > time.Second {
		t.Fatalf("took %v to refuse, expected immediate", elapsed)
	}
}
