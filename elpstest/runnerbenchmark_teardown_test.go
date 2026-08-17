// Copyright © 2026 The ELPS authors

package elpstest

import (
	"strings"
	"testing"
	"time"

	"github.com/luthersystems/elps/elpsutil"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
)

// This file covers issue #474: (*Runner).RunBenchmark started the timer
// before the benchmark body and never stopped it, so everything that ran
// afterwards inside the same *testing.B function was charged to the
// measurement.  Two things were:
//
//  1. the `defer env.Runtime.Stderr.(*Logger).Flush()`, on every path,
//     costing whatever the benchmark left buffered on stderr; and
//
//  2. with a TeardownFn configured, the b.StartTimer() at the end of the
//     teardown defer -- which is what re-exposed (1) after teardown itself
//     had correctly run untimed.
//
// The issue measured 650ns on a 15.6us benchmark.  That number is not what
// these tests assert, and deliberately so.  A threshold on a duration is the
// #443/#452 shape: it sits near its own noise floor and it turns machine load
// into a test failure.
//
// THE ASSERTION IS THE TIMER ITSELF, following the standard #476 set for the
// SetupFn half of this same function.  A *testing.B whose timer is stopped
// has a frozen B.Elapsed(), so sleeping and differencing B.Elapsed() across
// the sleep yields EXACTLY zero when the region is untimed and at least the
// sleep when it is not.  There is no threshold to tune and no arithmetic on a
// measured duration.
//
// TestBenchmarkElapsedFreezesWhenTimerStopped in the sibling file is the
// guard for that premise, and is not restated here.

// teardownProbeDelay is how long a probe blocks while watching the timer.  It
// only has to be long enough that a RUNNING timer's B.Elapsed() advances
// measurably; a stopped one does not advance at all.
const teardownProbeDelay = 20 * time.Millisecond

// teardownProbeSource is a benchmark file with nothing in it but the
// benchmark, so the measured region is as close to empty as it can be and
// anything charged after the body stands out.
const teardownProbeSource = `
(use-package 'testing)

(benchmark-simple "probe"
  (+ 1 1))
`

// TestRunBenchmarkReturnsWithTheTimerStopped is the catch for #474.
//
// It observes the timer at the moment RunBenchmark returns -- after every one
// of its defers, including the teardown defer and the log flush, has run.  A
// RunBenchmark that has stopped timing leaves B.Elapsed() frozen there; the
// one on 95e2e1a left it running, on BOTH arms:
//
//   - without a TeardownFn, because nothing ever stopped the timer after the
//     body, so the deferred Flush was inside the measurement; and
//
//   - with one, because the teardown defer ended in b.StartTimer().
//
// This is the strongest form the property has.  "Is the flush timed?" is
// awkward to observe directly -- the flush is a defer inside a function the
// test does not control -- whereas "does the timer end up stopped?" is
// observable from the caller and implies it, because nothing stops the timer
// after the flush.
func TestRunBenchmarkReturnsWithTheTimerStopped(t *testing.T) {
	benchtime1x(t)
	for _, test := range []struct {
		name     string
		teardown func(*lisp.LEnv) *lisp.LVal
	}{
		{"no-teardown", nil},
		{"with-teardown", func(*lisp.LEnv) *lisp.LVal { return lisp.Nil() }},
	} {
		t.Run(test.name, func(t *testing.T) {
			var delta time.Duration
			r := &Runner{TeardownFn: test.teardown}
			res := testing.Benchmark(func(b *testing.B) {
				r.RunBenchmark(b, 0, "probe_test.lisp", strings.NewReader(teardownProbeSource))
				// Every defer RunBenchmark registered has now run.
				before := b.Elapsed()
				time.Sleep(teardownProbeDelay)
				delta = b.Elapsed() - before
			})
			if res.N == 0 {
				t.Fatalf("benchmark failed; RunBenchmark reported an error")
			}
			if delta != 0 {
				t.Errorf("RunBenchmark returned with the benchmark timer RUNNING:"+
					" B.Elapsed() advanced %v across a %v sleep after it returned."+
					" Everything the deferred work does -- the teardown defer's"+
					" StartTimer and the Logger.Flush after it -- is charged to the"+
					" measurement (issue #474); a stopped timer advances by exactly 0.",
					delta, teardownProbeDelay)
			}
		})
	}
}

// TestRunBenchmarkDoesNotTimeTeardown pins the half of #474 that is about
// TeardownFn specifically, from inside the teardown itself.
//
// It is not redundant with the test above.  That one says the timer is
// stopped when RunBenchmark RETURNS; this one says it is stopped while
// TeardownFn is RUNNING, which is a separate claim -- an implementation that
// timed the teardown and then stopped the timer on the way out would satisfy
// the first and not this.
//
// GUARD on 95e2e1a: the teardown defer already began with b.StopTimer(), so
// the teardown body itself was untimed there too.  It is here because the
// fix DELETES that StopTimer/StartTimer pair, and this is what says the
// deletion did not cost the property the pair was providing.
func TestRunBenchmarkDoesNotTimeTeardown(t *testing.T) {
	benchtime1x(t)
	var delta time.Duration
	var reached bool
	var b0 *testing.B

	r := &Runner{TeardownFn: func(*lisp.LEnv) *lisp.LVal {
		reached = true
		before := b0.Elapsed()
		time.Sleep(teardownProbeDelay)
		delta = b0.Elapsed() - before
		return lisp.Nil()
	}}
	res := testing.Benchmark(func(b *testing.B) {
		b0 = b
		r.RunBenchmark(b, 0, "probe_test.lisp", strings.NewReader(teardownProbeSource))
	})
	if res.N == 0 {
		t.Fatalf("benchmark failed; RunBenchmark reported an error")
	}
	if !reached {
		t.Fatal("TeardownFn was never called: this test observed nothing")
	}
	if delta != 0 {
		t.Errorf("TeardownFn ran with the benchmark timer RUNNING:"+
			" B.Elapsed() advanced %v across a %v sleep inside it (issue #474);"+
			" a stopped timer advances by exactly 0.",
			delta, teardownProbeDelay)
	}
}

// TestRunBenchmarkStillTimesTheBody is the negative control for both tests
// above.  Without it, a RunBenchmark that stopped the timer too EARLY -- or
// never started it -- would satisfy every "the timer is stopped" assertion in
// this file and measure nothing at all.
//
// It reads the timer from inside the benchmark body, so it says the body is
// inside the measured region without comparing two durations to each other.
// GUARD: passes on 95e2e1a.
func TestRunBenchmarkStillTimesTheBody(t *testing.T) {
	benchtime1x(t)
	var delta time.Duration
	var reached bool
	var b0 *testing.B

	probe := &bodyTimerProbe{
		observe: func() {
			reached = true
			before := b0.Elapsed()
			time.Sleep(teardownProbeDelay)
			delta = b0.Elapsed() - before
		},
	}
	r := &Runner{
		LoaderFn:   probe.loader(),
		TeardownFn: func(*lisp.LEnv) *lisp.LVal { return lisp.Nil() },
	}
	res := testing.Benchmark(func(b *testing.B) {
		b0 = b
		r.RunBenchmark(b, 0, "probe_test.lisp", strings.NewReader(bodyProbeSource))
	})
	if res.N == 0 {
		t.Fatalf("benchmark failed; RunBenchmark reported an error")
	}
	if !reached {
		t.Fatal("the probe builtin was never evaluated in the benchmark body")
	}
	if delta < teardownProbeDelay/2 {
		t.Errorf("the benchmark BODY ran with the timer stopped:"+
			" B.Elapsed() advanced only %v across a %v sleep inside it."+
			" RunBenchmark is measuring nothing.",
			delta, teardownProbeDelay)
	}
}

// bodyProbeSource calls the probe from inside the benchmark body rather than
// at top level, so what it observes is the measured region.
const bodyProbeSource = `
(use-package 'testing)

(benchmark-simple "probe"
  (probe-body-timer))
`

// bodyTimerProbe registers a builtin the benchmark body calls.  The loader
// mirrors loadTimerProbe.loader in the sibling file (#476); the two are left
// separate rather than unified so this change stays confined to the defect it
// is about.
type bodyTimerProbe struct {
	observe func()
}

func (p *bodyTimerProbe) loader() func(*lisp.LEnv) *lisp.LVal {
	return func(env *lisp.LEnv) *lisp.LVal {
		if rc := lisplib.LoadLibrary(env); rc.Type == lisp.LError {
			return rc
		}
		if rc := env.InPackage(lisp.String(lisp.DefaultUserPackage)); rc.Type == lisp.LError {
			return rc
		}
		env.AddBuiltins(true, elpsutil.Function("probe-body-timer", lisp.Formals(),
			func(*lisp.LEnv, *lisp.LVal) *lisp.LVal {
				p.observe()
				return lisp.Nil()
			}))
		return lisp.Nil()
	}
}
