// Copyright © 2026 The ELPS authors

package elpstest

import (
	"flag"
	"strings"
	"testing"
	"time"

	"github.com/luthersystems/elps/elpsutil"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
)

// This file covers issue #434: (*Runner).RunBenchmark charged the whole
// benchmark file's load -- parsing it and evaluating every top-level form,
// including every benchmark definition -- to the timed region, but only when
// the Runner had a SetupFn.  The SetupFn branch ended in b.StartTimer()
// instead of restoring the timer to the (stopped) state it found it in, so
// env.Load ran with the timer on.
//
// Two Runners differing only in whether they set SetupFn therefore reported
// numbers that were not comparable, and the difference had nothing to do with
// the code under benchmark.  No Runner in this repository sets SetupFn, so no
// in-tree number moved -- but elpstest is an exported package and SetupFn is
// an exported field documented for exactly this use.
//
// The assertion here is not a comparison of two timings.  It observes the
// timer directly, from inside env.Load, which makes it deterministic: a
// *testing.B whose timer is stopped has a frozen B.Elapsed(), so sleeping
// inside the load and differencing B.Elapsed() across the sleep yields
// exactly zero when the load is untimed and at least the sleep when it is
// not.  There is no threshold to tune.

// loadProbeDelay is how long the probe builtin blocks inside env.Load.  It
// only has to be long enough that a running timer's B.Elapsed() advances
// measurably; a stopped timer's does not advance at all.
const loadProbeDelay = 20 * time.Millisecond

// loadProbeSource is a benchmark file whose top-level forms call the probe.
// Everything before the benchmark body runs during env.Load.
const loadProbeSource = `
(use-package 'testing)

(probe-load-timer)

(benchmark-simple "probe"
  (+ 1 1))
`

// loadTimerProbe records what the benchmark timer was doing while env.Load
// was running.  RunBenchmark is called on the benchmark goroutine and the
// probe runs synchronously inside it, so no synchronisation is needed.
type loadTimerProbe struct {
	b       *testing.B    // the benchmark currently running
	reached bool          // the probe builtin was evaluated at least once
	delta   time.Duration // largest B.Elapsed() advance observed across the probe's sleep
}

// loader returns a Runner.LoaderFn that loads the standard library and then
// registers the probe builtin in the user package.
func (p *loadTimerProbe) loader() func(*lisp.LEnv) *lisp.LVal {
	return func(env *lisp.LEnv) *lisp.LVal {
		if rc := lisplib.LoadLibrary(env); rc.Type == lisp.LError {
			return rc
		}
		if rc := env.InPackage(lisp.String(lisp.DefaultUserPackage)); rc.Type == lisp.LError {
			return rc
		}
		env.AddBuiltins(true, elpsutil.Function("probe-load-timer", lisp.Formals(),
			func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
				before := p.b.Elapsed()
				time.Sleep(loadProbeDelay)
				after := p.b.Elapsed()
				p.reached = true
				if d := after - before; d > p.delta {
					p.delta = d
				}
				return lisp.Nil()
			}))
		return lisp.Nil()
	}
}

// benchtime1x forces -benchtime=1x for the duration of the test.  Each
// RunBenchmark invocation pays loadProbeDelay of untimed sleep, and the
// framework sizes b.N from the timed duration alone, so without this the
// ramp would run for a long time to measure a body that does nothing.
//
// runbenchmark_test.go (#441) pins -test.benchtime the same way, inline and
// for the same reason.  The two are left separate rather than unified here so
// this change stays confined to the defect it is about.
func benchtime1x(t *testing.T) {
	t.Helper()
	f := flag.Lookup("test.benchtime")
	if f == nil {
		return
	}
	prev := f.Value.String()
	if err := f.Value.Set("1x"); err != nil {
		t.Fatalf("set benchtime: %v", err)
	}
	t.Cleanup(func() {
		if err := f.Value.Set(prev); err != nil {
			t.Errorf("restore benchtime: %v", err)
		}
	})
}

// TestRunnerRunBenchmarkDoesNotTimeLoad is the catch for #434.  The
// with-setup subtest fails on main: env.Load runs with the timer on, so the
// probe's B.Elapsed() advances by a full loadProbeDelay.  The no-setup
// subtest is a GUARD -- it passes on main and pins the behaviour the fix
// makes uniform.
func TestRunnerRunBenchmarkDoesNotTimeLoad(t *testing.T) {
	benchtime1x(t)
	for _, test := range []struct {
		name  string
		setup func(*lisp.LEnv) *lisp.LVal
	}{
		{"no-setup", nil},
		{"with-setup", func(*lisp.LEnv) *lisp.LVal { return lisp.Nil() }},
	} {
		t.Run(test.name, func(t *testing.T) {
			probe := &loadTimerProbe{}
			r := &Runner{LoaderFn: probe.loader(), SetupFn: test.setup}
			res := testing.Benchmark(func(b *testing.B) {
				probe.b = b
				r.RunBenchmark(b, 0, "probe_test.lisp", strings.NewReader(loadProbeSource))
			})
			if res.N == 0 {
				t.Fatalf("benchmark failed; RunBenchmark reported an error")
			}
			if !probe.reached {
				t.Fatalf("probe builtin was never evaluated: the benchmark file did not load," +
					" so this test observed nothing")
			}
			if probe.delta != 0 {
				t.Errorf("env.Load ran with the benchmark timer RUNNING:"+
					" B.Elapsed() advanced %v across a %v sleep inside the load."+
					" The whole file's parse and top-level evaluation is charged to the"+
					" measurement (issue #434); a stopped timer advances by exactly 0.",
					probe.delta, loadProbeDelay)
			}
		})
	}
}

// TestBenchmarkElapsedFreezesWhenTimerStopped is a GUARD, not a catch: it
// passes on main.  It pins the premise the test above depends on -- that
// testing.B.Elapsed() does not advance while the timer is stopped and does
// advance while it runs -- so that a change in the testing package's
// bookkeeping fails here, saying so, rather than quietly making the probe
// above unable to observe anything.
func TestBenchmarkElapsedFreezesWhenTimerStopped(t *testing.T) {
	benchtime1x(t)
	var stopped, running time.Duration
	res := testing.Benchmark(func(b *testing.B) {
		b.StopTimer()
		before := b.Elapsed()
		time.Sleep(loadProbeDelay)
		stopped = b.Elapsed() - before
		b.StartTimer()
		before = b.Elapsed()
		time.Sleep(loadProbeDelay)
		running = b.Elapsed() - before
	})
	if res.N == 0 {
		t.Fatalf("benchmark failed")
	}
	if stopped != 0 {
		t.Errorf("B.Elapsed() advanced %v with the timer stopped; expected exactly 0", stopped)
	}
	if running < loadProbeDelay/2 {
		t.Errorf("B.Elapsed() advanced only %v across a %v sleep with the timer running",
			running, loadProbeDelay)
	}
}
