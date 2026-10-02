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

	"github.com/luthersystems/elps/internal/testdeadline"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/internal/libutil"
	"github.com/luthersystems/elps/lisp/lisplib/libtime"
	"github.com/luthersystems/elps/parser"
)

// forever is longer than any test is willing to wait.  It stands in for the
// unbounded duration issue #314 is about ("9223372036854775807ns" parses to
// time.Duration(math.MaxInt64), roughly 292 years).  It is DefaultMaxSleep,
// so the length cap does not refuse it, and it is longer than CI's 10m
// -timeout.
const forever = time.Hour

// outlast returns a duration that no run of t can wait out: at least a day,
// and twice the time left before the binary's -timeout.  A test that must
// tell "refused on entry" from "slept, then refused" asks for a sleep this
// long.  The sleep that waits then cannot return before runBounded's
// backstop, whatever -timeout is in effect (#789).
func outlast(t *testing.T) time.Duration {
	t.Helper()
	d := 24 * time.Hour
	if deadline, ok := t.Deadline(); ok {
		d = max(d, 2*time.Until(deadline))
	}
	return d
}

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
	registerSleep(t, env)
	if rc := env.InPackage(lisp.String(lisp.DefaultUserPackage)); rc.Type == lisp.LError {
		t.Fatalf("in-package: %v", rc)
	}
	return env
}

// registerSleep binds libtime.BuiltinSleep as time:sleep the way a host
// does.  LoadPackage no longer registers it (#757), but the exported builtin
// is still what hosts install, so its behaviour stays under test here.
func registerSleep(t *testing.T, env *lisp.LEnv) {
	t.Helper()
	prev := env.Runtime.Package.Name
	defer env.InPackage(lisp.Symbol(prev))
	if rc := env.InPackage(lisp.Symbol(libtime.DefaultPackageName)); rc.Type == lisp.LError {
		t.Fatalf("in-package time: %v", rc)
	}
	env.AddBuiltins(true, libutil.Function("sleep",
		lisp.Formals("time-duration", lisp.KeyArgSymbol, "max"), libtime.BuiltinSleep))
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

// runBounded runs fn on its own goroutine and fails if fn does not return
// before testdeadline.Backstop, which ends just before the binary's -timeout.
//
// A sleep that does not wake uses no CPU, so a CPU bound cannot see it, and
// a wall-clock timer fails a correct test on a starved process (#789).  The
// backstop cannot fail a run that the -timeout would pass.  It does make the
// hung test fail by name, which a bare -timeout does not: that kills the
// whole binary with a goroutine dump.  The goroutine is deliberately leaked
// on timeout: an uninterruptible sleep is the defect under test, so there is
// nothing to cancel.  With no -timeout, a hang hangs.
func runBounded(t *testing.T, fn func() *lisp.LVal) (*lisp.LVal, time.Duration) {
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
	backstop, stop := testdeadline.Backstop(t)
	defer stop()
	select {
	case r := <-done:
		return r.v, r.elapsed
	case <-backstop.Done():
		t.Fatal("sleep did not return before the test's -timeout")
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

	v, elapsed := runBounded(t, func() *lisp.LVal { return callSleep(env, forever) })
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
	v, elapsed := runBounded(t, func() *lisp.LVal { return callSleep(env, forever) })
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
	// "it returned" is the property, and runBounded is a hang detector
	// rather than a tolerance: nothing asserts success on the strength of a
	// wall-clock reading.
	v, _ := runBounded(t, func() *lisp.LVal {
		// 9223372036854775807ns is time.Duration(math.MaxInt64) -- the
		// literal from the issue, reachable from source alone.
		return env.LoadStringContext(context.Background(), "sleep_test.lisp",
			`(time:sleep (time:parse-duration "9223372036854775807ns"))`)
	})
	requireSleepLimit(t, v)
}

// TestSleepInterruptedThroughEval proves the context actually reaches the
// builtin through ordinary evaluation, not just through a direct Go call.
// LEnv.call bridges the evaluation context onto the environment at the
// builtin boundary; if that bridge broke, the direct-call tests above would
// still pass while real ELPS programs stayed unbounded.
//
// The evidence is the builtin's fail-fast (issue #338): a sleep the deadline
// will outlast is refused ON ENTRY.  The context is a stalledDeadline, which
// reports a deadline but never expires.  LEnv.eval's per-step checkLimits
// only reports a context that has ALREADY erred, so it cannot fire here, on
// any machine (issue #455).  Only the builtin can raise context-cancelled,
// so the condition names the builtin as the source.
//
// The durations are ordered so that the deadline is the only thing that can
// refuse the sleep:
//
//	deadline (outlast) < sleep (2x) < :max (3x)
//
// The :max keeps the length cap out of it.  A build whose bridge is broken
// sees no deadline and sleeps twice outlast, so runBounded reports a hang.
func TestSleepInterruptedThroughEval(t *testing.T) {
	t.Parallel()
	far := outlast(t)
	ctx := stalledDeadline{Context: context.Background(), deadline: time.Now().Add(far)}
	env := sleepEnv(t, nil)

	src := fmt.Sprintf(`(time:sleep (time:parse-duration %q) :max (time:parse-duration %q))`,
		(2 * far).String(), (3 * far).String())
	v, _ := runBounded(t, func() *lisp.LVal {
		return env.LoadStringContext(ctx, "sleep_test.lisp", src)
	})
	requireCancelled(t, v)
}

// TestSleepPastDeadlineFailsFast pins issue #338: a sleep the deadline will
// outlast is refused on entry, NOT slept out to the deadline first.
//
// The distinction is invisible to a pass/fail check on the condition alone --
// the old behaviour raised the same context-cancelled -- so the test makes
// the wait impossible instead of timing it (#789).  stalledDeadline reports a
// deadline 750ms away but never expires: its Done channel never closes and
// Err stays nil.  A sleep refused on entry raises context-cancelled at once.
// A sleep that waits instead can only end when its own minute is up, and it
// then returns nil, so runBounded reports a hang or requireCancelled fails.
// Neither outcome depends on how fast this process runs.
func TestSleepPastDeadlineFailsFast(t *testing.T) {
	t.Parallel()
	ctx := stalledDeadline{Context: context.Background(), deadline: time.Now().Add(750 * time.Millisecond)}
	env := sleepEnv(t, ctx)

	v, _ := runBounded(t, func() *lisp.LVal {
		// A minute is far below DefaultMaxSleep, so only the deadline can
		// refuse it.  A sleep that waits returns nil after the minute, and
		// requireCancelled fails.
		return callSleep(env, time.Minute)
	})
	requireCancelled(t, v)
}

// stalledDeadline is a context with a deadline that never expires.  Deadline
// reports the time; Done and Err are those of context.Background.
type stalledDeadline struct {
	context.Context
	deadline time.Time
}

func (c stalledDeadline) Deadline() (time.Time, bool) { return c.deadline, true }

// TestSleepCompletesWithinDeadline guards the other direction: a sleep that
// fits inside the deadline must run to completion and return nil.  Without
// this a "cap everything" implementation that always errored would look
// correct.
func TestSleepCompletesWithinDeadline(t *testing.T) {
	t.Parallel()
	const nap = 20 * time.Millisecond
	ctx, cancel := context.WithTimeout(context.Background(), forever)
	defer cancel()
	env := sleepEnv(t, ctx)

	v, elapsed := runBounded(t, func() *lisp.LVal { return callSleep(env, nap) })
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
	v, elapsed := runBounded(t, func() *lisp.LVal { return callSleep(env, nap) })
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

	v, _ := runBounded(t, func() *lisp.LVal { return callSleep(env, forever) })
	requireCancelled(t, v)
}

// TestSleepNonPositive keeps the degenerate durations a no-op under every
// context, matching time.Sleep.
func TestSleepNonPositive(t *testing.T) {
	t.Parallel()
	for _, d := range []time.Duration{0, -time.Hour} {
		ctx, cancel := context.WithTimeout(context.Background(), forever)
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
// TestSleepMaxThroughEval -- assert that a sleep is refused on entry rather
// than performed.  None of them reads elapsed time (#475, #789).
//
// Each asks for a sleep of at least outlast(t), which is longer than the time
// left before the binary's -timeout.  BuiltinSleep has no partial-sleep path:
// it either refuses before reaching sleepContext, or sleeps the caller's full
// duration.  These environments run on context.Background(), so nothing can
// cut a sleep short.  A build that refuses only AFTER sleeping therefore does
// not return before runBounded's backstop, and the test fails by name.  A
// correct build returns at once.  Neither outcome depends on how fast this
// process runs, so a starved process cannot fail these tests.
//
// An `elapsed > time.Second` check used to sit after each condition check.
// It added nothing the hang detector does not catch, and it failed correct
// builds on a loaded machine (#435).
//
// TestSleepLengthCapRefusesImmediately covers the length cap itself, with no
// context involved: a duration over DefaultMaxSleep is refused on entry.
func TestSleepLengthCapRefusesImmediately(t *testing.T) {
	t.Parallel()
	env := sleepEnv(t, nil)
	d := max(lisp.DefaultMaxSleep+time.Second, outlast(t))
	v, _ := runBounded(t, func() *lisp.LVal {
		return callSleep(env, d)
	})
	requireSleepLimit(t, v)
}

// TestSleepUnderCapStillSleeps is the negative control for the test above.
// Without it, an implementation that refused EVERY sleep would pass the cap
// tests and look correct.
func TestSleepUnderCapStillSleeps(t *testing.T) {
	t.Parallel()
	env := sleepEnv(t, nil)
	v, elapsed := runBounded(t, func() *lisp.LVal {
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

	v, _ := runBounded(t, func() *lisp.LVal {
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
// Both calls ask for ceilingProbe, which is outlast(t); see the block above
// TestSleepLengthCapRefusesImmediately (#475, #499, #789).  The first is
// refused inside sleepCap, before a sleep is reachable at all.  The second
// must be refused by the default cap, which the ceiling lowers.  A build that
// slept ceilingProbe out and only then refused does not return before
// runBounded's backstop, so it fails as a hang.
func TestSleepMaxCannotExceedHostCeiling(t *testing.T) {
	t.Parallel()
	// ceiling is the host's limit; ceilingProbe is the duration both calls
	// ask for.  The property being tested is the relation between them:
	// ceilingProbe > ceiling is what makes the sleep refusable at all.
	const ceiling = time.Minute
	ceilingProbe := outlast(t)
	if ceilingProbe <= ceiling {
		t.Fatalf("ceilingProbe %v does not exceed the %v ceiling: the default cap"+
			" has nothing to refuse and the test would pass on the wrong evidence",
			ceilingProbe, ceiling)
	}

	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	if rc := lisp.InitializeUserEnv(env, lisp.WithMaxSleep(ceiling)); rc.Type == lisp.LError {
		t.Fatalf("initialize-user-env: %v", rc)
	}
	if rc := libtime.LoadPackage(env); rc.Type == lisp.LError {
		t.Fatalf("load time package: %v", rc)
	}
	registerSleep(t, env)

	v, _ := runBounded(t, func() *lisp.LVal {
		return callSleepMax(env, ceilingProbe, libtime.Duration(ceilingProbe))
	})
	requireSleepLimit(t, v)

	// And the ceiling lowers the no-:max default too, so the default cannot
	// quietly exceed it.
	v2, _ := runBounded(t, func() *lisp.LVal {
		return callSleep(env, ceilingProbe)
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
// It asks for outlast(t); see the block above
// TestSleepLengthCapRefusesImmediately (#475, #789).  It goes through
// LoadString (read, parse, evaluate two calls), the same work the 2.29s
// LoadStringContext measurement on #455 covers.
func TestSleepMaxThroughEval(t *testing.T) {
	t.Parallel()
	env := sleepEnv(t, nil)
	src := fmt.Sprintf(`(time:sleep (time:parse-duration %q) :max (time:parse-duration "1s"))`,
		outlast(t).String())
	v, _ := runBounded(t, func() *lisp.LVal {
		return env.LoadString("sleep_max_test.lisp", src)
	})
	// The sleep is over the 1s :max, so this must be refused -- and refused
	// by the cap, which proves :max was read rather than ignored.  Were the
	// keyword dropped, the sleep would be measured against DefaultMaxSleep
	// instead and this would still error, so the message is checked too.
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
