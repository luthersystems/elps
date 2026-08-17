// Copyright © 2026 The ELPS authors

// NOTE:  This file uses package name suffixed with _test to avoid an import
// cycle.  packages outside the standard library shouldn't need to use a _test
// suffix in their test files.
package libtime_test

import (
	"context"
	"fmt"
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
// The nil second cell is the unsupplied :max keyword, which is what the
// evaluator passes for a keyword the caller omitted.  BuiltinSleep reads it
// through args.KeyArg(1) rather than indexing Cells, so a caller that supplies
// a shorter list gets Nil instead of a panic -- see
// TestBuiltinSleepShortArgList and lisplib's
// TestKeyArgBuiltinsTolerateShortArgLists for why that matters.
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

// The two tests below split what used to be one, because the two properties
// need opposite contexts and the single test asserted one of them under the
// context that belongs to the other (issue #455).
//
// The cap and the context are checked in different places.  time:sleep checks
// the length cap before it consults the context -- but only ONCE IT RUNS.
// Getting there goes through LEnv.eval, which calls checkLimits(ctx) on every
// step, before evaluating anything.  So for a form under a deadline the real
// order is: read, parse, step through the call (the argument is itself a
// call), checking the context at each step, and only then the builtin and its
// cap.  A deadline that can expire while steps 1-3 are still running does not
// pick "cap first"; it picks whichever came due first, and which one that is
// is a function of machine load.
//
// The old test asserted sleep-limit-exceeded under a 50ms deadline and its
// comment explained the cap-first ordering as though it governed the whole
// path.  Under -race with the package suite running in parallel, 50ms is not
// a lot of budget for "load a package-qualified form and evaluate two calls",
// and it lost the race in CI, reporting context-cancelled.
//
// The fix is not a bigger deadline -- that is the same threshold-tuning move,
// and it would leave the outcome a function of load.  Each test now runs
// under the context that makes its own assertion the only reachable one.

// TestSleepLimitThroughEval covers the length cap on the path an ordinary
// ELPS program takes: source text, read and parsed, evaluated by LEnv.eval.
//
// There is NO deadline and nothing cancels, so the evaluator's per-step
// checkLimits cannot produce an error at all -- it only reports a context
// that has already erred, and context.Background() never does.  The cap is
// the only thing left that can refuse the sleep, which makes
// sleep-limit-exceeded the outcome regardless of how slow the machine is.
func TestSleepLimitThroughEval(t *testing.T) {
	t.Parallel()
	env := sleepEnv(t, nil)

	// runBounded is the whole of the timing assertion, deliberately.  A cap
	// that slept the 292 years before reporting would not return at all, so
	// "it returned" is the property, and runBounded's 30s is a hang detector
	// rather than a tolerance: nothing asserts success on the strength of a
	// wall-clock reading.  An `elapsed >= slack` check after it would be
	// unreachable, since runBounded has already failed the test by then.
	v, _ := runBounded(t, slack, func() *lisp.LVal {
		// 9223372036854775807ns is time.Duration(math.MaxInt64) -- the
		// literal from the issue, reachable from source alone.
		return env.LoadStringContext(context.Background(), "sleep_test.lisp",
			`(time:sleep (time:parse-duration "9223372036854775807ns"))`)
	})
	requireSleepLimit(t, v)
}

// evalDeadline and evalSleep are TestSleepInterruptedThroughEval's two
// durations.  They are ordered so that the property under test is the only
// reachable outcome, and the test checks the ordering rather than trusting
// it:
//
// slack (30s) << evalDeadline (5m) < evalSleep (30m) <= DefaultMaxSleep (1h)
//
// evalSleep <= DefaultMaxSleep, so the length cap CANNOT refuse this sleep.
// Only the context can.  evalDeadline < evalSleep, so the builtin's fail-fast
// refuses on entry.  And slack << evalDeadline, so the evaluator's per-step
// context check provably never fires: for it to fire the deadline must
// already have passed, and runBounded fails the test ten times sooner than
// that.  The margin is not a tuned tolerance -- any run in which the deadline
// could expire is a run runBounded has already failed.
const (
	evalDeadline = 5 * time.Minute
	evalSleep    = 30 * time.Minute
)

// TestSleepInterruptedThroughEval proves the context actually reaches the
// builtin through ordinary evaluation, not just through a direct Go call.
// LEnv.call bridges the evaluation context onto the environment at the
// builtin boundary; if that bridge broke, the direct-call tests above would
// still pass while real ELPS programs stayed unbounded.
//
// The evidence is the builtin's fail-fast (issue #338): a sleep the deadline
// will outlast is refused ON ENTRY, while the context is still live.  That is
// something only the builtin can produce -- LEnv.eval's checkLimits reports a
// context that has ALREADY erred -- so asserting context-cancelled together
// with a context that has not yet expired names the builtin as the source,
// which a condition name alone cannot do.
func TestSleepInterruptedThroughEval(t *testing.T) {
	t.Parallel()
	if evalSleep > lisp.DefaultMaxSleep {
		t.Fatalf("evalSleep %v exceeds DefaultMaxSleep %v: the length cap could refuse"+
			" this sleep and the test would pass on the wrong evidence",
			evalSleep, lisp.DefaultMaxSleep)
	}
	if evalDeadline >= evalSleep {
		t.Fatalf("evalDeadline %v is not nearer than evalSleep %v: the builtin has"+
			" nothing to fail fast about", evalDeadline, evalSleep)
	}
	if slack >= evalDeadline {
		t.Fatalf("runBounded's %v bound is not comfortably inside the %v deadline:"+
			" the evaluator's per-step context check could fire first (issue #455)",
			slack, evalDeadline)
	}

	env := sleepEnv(t, nil)
	ctx, cancel := context.WithTimeout(context.Background(), evalDeadline)
	defer cancel()

	src := fmt.Sprintf(`(time:sleep (time:parse-duration %q))`, evalSleep.String())
	// As in TestSleepLimitThroughEval, runBounded is the only wall clock and
	// it only detects a hang -- a sleep that was not refused on entry runs for
	// evalSleep and never returns within slack.  The two assertions below are
	// the ones that carry meaning, and neither reads a clock.
	v, _ := runBounded(t, slack, func() *lisp.LVal {
		return env.LoadStringContext(ctx, "sleep_test.lisp", src)
	})
	requireCancelled(t, v)
	if err := ctx.Err(); err != nil {
		t.Fatalf("the %v deadline expired during a test bounded at %v (%v):"+
			" context-cancelled here could have come from LEnv.eval's per-step check"+
			" rather than from time:sleep, so this run proves nothing about the bridge",
			evalDeadline, slack, err)
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

// The three tests below -- this one, TestSleepMaxCannotExceedHostCeiling and
// TestSleepMaxThroughEval -- each used to follow their condition check with an
// `elapsed > time.Second` assertion.  Those are gone, and this is why (#475).
//
// THEY WERE REDUNDANT.  Each enforced "the sleep was refused on entry rather
// than performed", and each was already inside a runBounded(t, slack, ...)
// that fails the test after 30s.  The durations being refused are 1h+1s, 1h
// and 2h, and BuiltinSleep has no partial-sleep path: it either refuses
// before reaching sleepContext, or sleeps the caller's full duration bounded
// by the context -- and all three environments run on context.Background(),
// so nothing can cut a sleep short.  elapsed is therefore either microseconds
// or at least an hour.  There is no implementation that lands in the gap
// between 1s and 30s by SLEEPING; the only way to land there is scheduling
// delay, which is machine load.  That is the #435 shape -- a correctness
// property enforced by a wall clock, where a correct implementation fails the
// test for being slow -- and it is not fixed by widening the bound (#443/#452,
// #435/#447).
//
// CHECKED BY MUTATION rather than by argument alone.  With the three checks
// removed, a build in which the length cap refuses only AFTER sleeping (the
// exact failure the old comment here said the elapsed assertion was
// load-bearing for) still fails all three, through runBounded:
//
//	--- FAIL: TestSleepMaxThroughEval (30.03s)   sleep did not return within 30s
//	--- FAIL: TestSleepLengthCapRefusesImmediately (30.03s)
//	--- FAIL: TestSleepMaxCannotExceedHostCeiling (30.03s)
//
// So runBounded catches everything the deleted checks could, and nothing is
// lost by letting it be the only clock.  It is a hang detector: no assertion
// in these three tests succeeds on the strength of a wall-clock reading.
//
// Where a tighter statement IS wanted, TestSleepPastDeadlineFailsFast's
// `elapsed > remaining/2` is the pattern -- relative to a duration the test
// controls rather than to an absolute second.
//
// TestSleepLengthCapRefusesImmediately covers the length cap itself, with no
// context involved: a duration over DefaultMaxSleep is refused on entry.
func TestSleepLengthCapRefusesImmediately(t *testing.T) {
	t.Parallel()
	env := sleepEnv(t, nil)
	v, _ := runBounded(t, slack, func() *lisp.LVal {
		return callSleep(env, lisp.DefaultMaxSleep+time.Second)
	})
	requireSleepLimit(t, v)
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
//
// The first call's `elapsed > time.Second` check is gone; see the block above
// TestSleepLengthCapRefusesImmediately (#475).  It is refused inside sleepCap,
// before a sleep is reachable at all, and the only way past that is for
// sleepCap to accept the 1h :max -- at which point the call sleeps an hour and
// runBounded fails the test.
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

	v, _ := runBounded(t, slack, func() *lisp.LVal {
		return callSleepMax(env, time.Hour, libtime.Duration(time.Hour))
	})
	requireSleepLimit(t, v)

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
//
// Its `elapsed > time.Second` check is gone; see the block above
// TestSleepLengthCapRefusesImmediately (#475).  This was the most exposed of
// the three, because it goes through LoadString -- read, parse, evaluate two
// calls -- which is the same work the 2.29s LoadStringContext measurement on
// #455 covers.
func TestSleepMaxThroughEval(t *testing.T) {
	t.Parallel()
	env := sleepEnv(t, nil)
	v, _ := runBounded(t, slack, func() *lisp.LVal {
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
}

// TestBuiltinSleepShortArgList reproduces, at the elps end, the defect that
// luthersystems/substrate hit when it moved to elps v1.49.0.
//
// substrate binds BuiltinSleep under its own name with its own formals:
//
//	ielpsutil.FunctionDoc("sleep", lisp.Formals("seconds"), libtime.BuiltinSleep, ...)
//
// One formal, so one argument cell. That was correct until
// luthersystems/elps#346 added the optional :max keyword and BuiltinSleep
// started reading Cells[1]; from then on every call panicked with an
// index-out-of-range, which the evaluator could only report as an opaque
// internal-panic with no argument attached. BuiltinSleep's Go signature never
// changed, so nothing failed to compile.
//
// A one-cell call must now behave exactly as if :max were omitted.
func TestBuiltinSleepShortArgList(t *testing.T) {
	env := sleepEnv(t, nil)
	d := 10 * time.Millisecond

	var short, full *lisp.LVal
	assertNotPanics(t, "one-cell call", func() {
		short = libtime.BuiltinSleep(env, lisp.SExpr([]*lisp.LVal{libtime.Duration(d)}))
	})
	assertNotPanics(t, "zero-cell call", func() {
		_ = libtime.BuiltinSleep(env, lisp.SExpr(nil))
	})
	full = libtime.BuiltinSleep(env, lisp.SExpr([]*lisp.LVal{libtime.Duration(d), lisp.Nil()}))

	if short.Type == lisp.LError {
		t.Fatalf("one-cell sleep returned an error: %v", short)
	}
	if short.Type != full.Type {
		t.Errorf("one-cell sleep returned %v, want the same as an explicit nil :max (%v)",
			short.Type, full.Type)
	}
}

func assertNotPanics(t *testing.T, what string, fn func()) {
	t.Helper()
	defer func() {
		t.Helper()
		if r := recover(); r != nil {
			t.Fatalf("%s panicked: %v", what, r)
		}
	}()
	fn()
}
