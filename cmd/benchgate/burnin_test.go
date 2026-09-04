package main

import (
	"bytes"
	"math"
	"os"
	"strings"
	"testing"
	"time"
)

// The burn-in's arithmetic is tested with INJECTED durations, deliberately.
// A unit test that timed the real loop and asserted a spread would be a test of
// how busy the machine running `go test` happens to be -- which is the exact
// confusion this whole feature exists to remove, and it would be the flakiest
// test in the repository. The real loop is exercised by one smoke test at the
// bottom, which asserts what is true regardless of load.

// fixedSampler replays a canned list of durations, one per run, with a constant
// checksum. It is the seam that makes the verdict deterministic.
func fixedSampler(ds ...time.Duration) func(int) sample {
	i := 0
	return func(int) sample {
		d := ds[i%len(ds)]
		i++
		return sample{d: d, sum: referenceChecksum}
	}
}

func ms(n float64) time.Duration { return time.Duration(n * float64(time.Millisecond)) }

func runBurninCase(t *testing.T, args []string, env map[string]*string, sampler func(int) sample) (int, string) {
	t.Helper()
	for _, k := range benchEnvVars {
		old, had := os.LookupEnv(k)
		t.Cleanup(func() {
			if had {
				_ = os.Setenv(k, old)
			} else {
				_ = os.Unsetenv(k)
			}
		})
		_ = os.Unsetenv(k)
	}
	for k, v := range env {
		if v != nil {
			_ = os.Setenv(k, *v)
		}
	}
	var out bytes.Buffer
	rc := runBurnin(args, &out, &out, sampler)
	return rc, out.String()
}

// TestBurninStats pins the two things the verdict is computed from: which
// samples are kept, and how far apart they are called.
func TestBurninStats(t *testing.T) {
	cases := []struct {
		name       string
		samples    []time.Duration
		warmup     int
		wantKept   int
		wantMedian time.Duration
		wantSpread float64
	}{{
		// The warmup samples are DISCARDED, not merely down-weighted. This is
		// the case that proves it: the two leading samples are wildly out and
		// must not appear in either statistic.
		name:    "warmup samples are discarded entirely",
		samples: []time.Duration{ms(500), ms(200), ms(100), ms(100), ms(100)},
		warmup:  2, wantKept: 3, wantMedian: ms(100), wantSpread: 0,
	}, {
		name:    "a perfectly reproducing machine spreads 0%",
		samples: []time.Duration{ms(80), ms(80), ms(80), ms(80), ms(80)},
		warmup:  0, wantKept: 5, wantMedian: ms(80), wantSpread: 0,
	}, {
		// ONE disrupted sample out of five is the shape this check exists to
		// catch, and a range statistic reports it at full size. (A standard
		// deviation would dilute it across the other four, which is why the
		// spread is defined as the furthest sample from the median.)
		name:    "one disrupted sample sets the spread on its own",
		samples: []time.Duration{ms(100), ms(100), ms(150), ms(100), ms(100)},
		warmup:  0, wantKept: 5, wantMedian: ms(100), wantSpread: 50,
	}, {
		name:    "the spread is symmetric: a fast outlier counts the same",
		samples: []time.Duration{ms(100), ms(100), ms(50), ms(100), ms(100)},
		warmup:  0, wantKept: 5, wantMedian: ms(100), wantSpread: 50,
	}, {
		// Even sample counts take the mean of the two middle values, so the
		// median is not silently one of the samples.
		name:    "an even number of kept samples averages the two middles",
		samples: []time.Duration{ms(90), ms(100), ms(110), ms(120)},
		warmup:  0, wantKept: 4, wantMedian: ms(105), wantSpread: 100.0 * 15 / 105,
	}, {
		name:    "warmup discard and an even kept count together",
		samples: []time.Duration{ms(400), ms(90), ms(100), ms(110), ms(120)},
		warmup:  1, wantKept: 4, wantMedian: ms(105), wantSpread: 100.0 * 15 / 105,
	}, {
		// A clock that reports nothing for a fixed loop is not a spread of 0%.
		// Reported as an impossible spread so it can never read as "fit".
		name:    "a zero median is not a perfect measurement",
		samples: []time.Duration{0, 0, 0},
		warmup:  0, wantKept: 3, wantMedian: 0, wantSpread: math.Inf(1),
	}}

	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			st := computeStats(c.samples, c.warmup)
			if len(st.kept) != c.wantKept {
				t.Errorf("kept %d samples, want %d", len(st.kept), c.wantKept)
			}
			if math.Abs(st.median-float64(c.wantMedian)) > 1 {
				t.Errorf("median %v, want %v", time.Duration(st.median), c.wantMedian)
			}
			if math.IsInf(c.wantSpread, 1) {
				if !math.IsInf(st.spread, 1) {
					t.Errorf("spread %v, want +Inf", st.spread)
				}
			} else if math.Abs(st.spread-c.wantSpread) > 1e-9 {
				t.Errorf("spread %v, want %v", st.spread, c.wantSpread)
			}
			// The kept samples keep their ORDER, so the report lists the runs
			// as they happened rather than sorted.
			for i, d := range st.kept {
				if d != c.samples[c.warmup+i] {
					t.Errorf("kept[%d] = %v, want %v (the kept samples were reordered)", i, d, c.samples[c.warmup+i])
				}
			}
		})
	}
}

// TestBurninVerdict drives the whole subcommand -- flags, env, report, exit
// code -- with the timing injected.
func TestBurninVerdict(t *testing.T) {
	cases := []struct {
		name        string
		env         map[string]*string
		sampler     func(int) sample
		args        []string
		want        int
		contains    []string
		notContains []string
	}{{
		name:    "a machine that reproduces the loop is FIT",
		args:    []string{"-runs", "5", "-warmup", "2"},
		sampler: fixedSampler(ms(200), ms(120), ms(100), ms(101), ms(99)),
		want:    0, contains: []string{"FIT", "3 kept sample(s)", "±1%"},
		// The warmup samples were 100% and 20% out. If they reached the
		// statistics, this machine would read as unfit.
		notContains: []string{"UNFIT"},
	}, {
		// THE POINT OF THE CHECK. A fixed loop taking 100ms and then 180ms on
		// the same machine within the same second is a machine that cannot
		// resolve a 10% gate on anything.
		name:    "a machine that cannot reproduce the loop is UNFIT",
		args:    []string{"-runs", "5", "-warmup", "0"},
		sampler: fixedSampler(ms(100), ms(100), ms(180), ms(100), ms(100)),
		want:    3, contains: []string{"UNFIT", "±80%", "Re-run on another runner"},
	}, {
		// The boundary is at-or-above, the same convention the gate uses for a
		// threshold. Exactly ±10% against a ±10% ceiling is unfit.
		name:    "a spread exactly AT the ceiling is unfit",
		args:    []string{"-runs", "3", "-warmup", "0"},
		sampler: fixedSampler(ms(100), ms(110), ms(100)),
		want:    3, contains: []string{"UNFIT", "±10%"},
	}, {
		name:    "a spread just below the ceiling is fit",
		args:    []string{"-runs", "3", "-warmup", "0"},
		sampler: fixedSampler(ms(100), ms(109), ms(100)),
		want:    0, contains: []string{"FIT"},
	}, {
		name:    "the ceiling is configurable, and a tighter one reds the same samples",
		args:    []string{"-runs", "3", "-warmup", "0", "-spread", "5"},
		sampler: fixedSampler(ms(100), ms(109), ms(100)),
		want:    3, contains: []string{"UNFIT", "ceiling ±5%"},
	}, {
		name:    "the ceiling comes from the environment too",
		env:     map[string]*string{"BENCH_BURNIN_SPREAD_PCT": s("5")},
		args:    []string{"-runs", "3", "-warmup", "0"},
		sampler: fixedSampler(ms(100), ms(109), ms(100)),
		want:    3, contains: []string{"UNFIT", "ceiling ±5%"},
	}, {
		name:    "runs and warmup come from the environment too",
		env:     map[string]*string{"BENCH_BURNIN_RUNS": s("5"), "BENCH_BURNIN_WARMUP": s("2")},
		sampler: fixedSampler(ms(500), ms(400), ms(100), ms(100), ms(100)),
		want:    0, contains: []string{"5 run(s)", "first 2 discarded", "3 kept sample(s)"},
	}, {
		// Every run is printed, warmup included and labelled. A discarded
		// sample that is never shown is a measurement nobody can review.
		name:    "warmup runs are printed, and marked as discarded",
		args:    []string{"-runs", "4", "-warmup", "1"},
		sampler: fixedSampler(ms(500), ms(100), ms(100), ms(100)),
		want:    0, contains: []string{"run 1", "(warmup, discarded)", "run 4"},
	}, {
		// A machine that computes a different answer from the same fixed input
		// is not merely slow. Exit 2, not 3: nothing was measured at all.
		name: "a run that disagrees with run 1 about the answer is a hard error",
		args: []string{"-runs", "3", "-warmup", "0", "-rounds", "16"},
		sampler: func() func(int) sample {
			var i uint64
			return func(int) sample {
				i++
				return sample{d: ms(100), sum: i}
			}
		}(),
		want: 2, contains: []string{"two different answers"},
	}, {
		// At the DEFAULT workload size the checksum is pinned, so a workload
		// that was edited (or optimised away) cannot quietly go on reporting
		// fitness from a measurement of nothing.
		name:    "a wrong checksum at the default size is a hard error",
		args:    []string{"-runs", "3", "-warmup", "0"},
		sampler: func(int) sample { return sample{d: ms(100), sum: 1} },
		want:    2, contains: []string{"not the one this check was calibrated against"},
	}, {
		// ...and at a NON-default size there is nothing to pin it against, so
		// the run proceeds on self-consistency alone.
		name:    "a non-default workload size is judged on self-consistency alone",
		args:    []string{"-runs", "3", "-warmup", "0", "-rounds", "16"},
		sampler: func(int) sample { return sample{d: ms(100), sum: 1} },
		want:    0, contains: []string{"FIT"},
	}, {
		// A single kept sample has a spread of exactly zero no matter what the
		// machine did: a check that cannot fail. Refused rather than reported.
		name:    "keeping fewer than three samples is a usage error",
		args:    []string{"-runs", "3", "-warmup", "2"},
		sampler: fixedSampler(ms(100)),
		want:    2, contains: []string{"at least 3 are needed", "cannot fail"},
	}, {
		name:    "a zero spread ceiling is a usage error, not an impossible bar",
		args:    []string{"-runs", "3", "-warmup", "0", "-spread", "0"},
		sampler: fixedSampler(ms(100)),
		want:    2, contains: []string{"must be a positive percentage"},
	}, {
		name:    "a negative warmup is a usage error",
		args:    []string{"-runs", "5", "-warmup", "-1"},
		sampler: fixedSampler(ms(100)),
		want:    2, contains: []string{"must not be negative"},
	}, {
		name:    "a zero workload size is a usage error",
		args:    []string{"-runs", "3", "-warmup", "0", "-rounds", "0"},
		sampler: fixedSampler(ms(100)),
		want:    2, contains: []string{"-rounds must be at least 1"},
	}, {
		name:    "a fractional run count is a typo, not a policy",
		env:     map[string]*string{"BENCH_BURNIN_RUNS": s("7.5")},
		sampler: fixedSampler(ms(100)),
		want:    2, contains: []string{"is not a whole number"},
	}, {
		name:    "a positional argument is a usage error",
		args:    []string{"table.txt"},
		sampler: fixedSampler(ms(100)),
		want:    2, contains: []string{"takes no positional arguments"},
	}, {
		name:    "an unknown flag is a usage error",
		args:    []string{"-nope"},
		sampler: fixedSampler(ms(100)),
		want:    2,
	}}

	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			rc, out := runBurninCase(t, c.args, c.env, c.sampler)
			if rc != c.want {
				t.Errorf("exit %d, want %d\n%s", rc, c.want, out)
			}
			for _, want := range c.contains {
				if !strings.Contains(out, want) {
					t.Errorf("output does not contain %q\n%s", want, out)
				}
			}
			for _, bad := range c.notContains {
				if strings.Contains(out, bad) {
					t.Errorf("output unexpectedly contains %q\n%s", bad, out)
				}
			}
		})
	}
}

// TestBurninUnfitExitMatchesTheAdjudicator pins the two halves of #542 to the
// SAME exit code. They are one claim ("this machine did not produce a usable
// measurement -- re-measure"), and a caller that learns to handle one must not
// have to learn a second code for the other.
func TestBurninUnfitExitMatchesTheAdjudicator(t *testing.T) {
	burninRC, _ := runBurninCase(t, []string{"-runs", "3", "-warmup", "0"}, nil,
		fixedSampler(ms(100), ms(200), ms(100)))
	gateRC, out := runGate(t, gateCase{
		env:  noWaivers(),
		args: elpsArgs("benchstat-runner-unfit-542.txt"),
	})
	if burninRC != 3 || gateRC != 3 {
		t.Errorf("burnin exited %d and the adjudicator exited %d; both must be 3 (RUNNER-UNFIT)\n%s", burninRC, gateRC, out)
	}
}

// TestReferenceWorkloadIsPinned is what stops the burn-in becoming a
// measurement of nothing. If a compiler learns to elide the loop, or someone
// edits it, the constant changes and this fails -- rather than the check
// quietly reporting every machine fit in 4ns.
func TestReferenceWorkloadIsPinned(t *testing.T) {
	if got := referenceWork(referenceRounds); got != referenceChecksum {
		t.Fatalf("referenceWork(%d) = %#x, want %#x -- the reference workload changed; if that was deliberate, re-pin referenceChecksum and say what the new cost profile is",
			referenceRounds, got, referenceChecksum)
	}
	// Different sizes must produce different answers, or the checksum is not
	// actually observing the loop count.
	if referenceWork(referenceRounds/2) == referenceChecksum {
		t.Error("half the workload produced the same checksum -- the checksum does not observe the loop")
	}
}

// TestBurninSmoke runs the REAL loop, through the real subcommand dispatch, at
// a tiny size. It asserts only what is true regardless of how loaded the
// machine is: the samples are real, the checksum path is exercised, and the
// verdict is one of the two the contract allows.
func TestBurninSmoke(t *testing.T) {
	if testing.Short() {
		t.Skip("burn-in smoke test runs the real reference workload")
	}
	var out bytes.Buffer
	// Dispatched through run(), so the `burnin` subcommand is proven reachable
	// from argv and not only as a Go function.
	rc := run([]string{"burnin", "-runs", "4", "-warmup", "1", "-rounds", "2000", "-spread", "10000"}, &out, &out)
	s := out.String()
	if rc != 0 {
		t.Fatalf("exit %d, want 0 (a ±10000%% ceiling is unreachable even on a badly loaded machine)\n%s", rc, s)
	}
	for _, want := range []string{"run 1", "run 4", "(warmup, discarded)", "FIT", "3 kept sample(s)"} {
		if !strings.Contains(s, want) {
			t.Errorf("output does not contain %q\n%s", want, s)
		}
	}
	// A sample of 0ns would mean the workload was optimised away and the
	// "measurement" is of nothing at all.
	if strings.Contains(s, "  0ns") {
		t.Errorf("a run took 0ns -- the reference workload did not run\n%s", s)
	}
}

// bestOf returns the fastest of n real samples at the given size. The minimum
// is the quantity that tracks the fixed iteration count; any single sample also
// carries whatever the machine did to it.
func bestOf(n, rounds int) sample {
	best := sample{d: time.Duration(math.MaxInt64)}
	for range n {
		if s := realSampler(rounds); s.d < best.d {
			best = s
		}
	}
	return best
}

// TestBurninTakesRealTime is the same claim as the 0ns check above, made
// directly: a fixed workload four times the size must take measurably longer
// than the small one. Ratios are not asserted (that would be a benchmark of the
// test machine); only the ordering. One sample per size is not enough for even
// that: the whole workload is under 10ms, so a single GC pause or scheduler
// preemption landing in the small run inverts it (#590). The best of several
// samples is compared instead.
func TestBurninTakesRealTime(t *testing.T) {
	if testing.Short() {
		t.Skip("runs the real reference workload")
	}
	small := bestOf(5, 2000)
	large := bestOf(5, 8000)
	if small.d <= 0 || large.d <= 0 {
		t.Fatalf("the reference workload reported no elapsed time (%v, %v)", small.d, large.d)
	}
	if large.d <= small.d {
		t.Errorf("4x the work took %v against %v for the smaller loop -- the duration does not track the fixed iteration count", large.d, small.d)
	}
	if small.sum == large.sum {
		t.Error("two different workload sizes produced the same checksum")
	}
}

func TestDurRendering(t *testing.T) {
	cases := []struct {
		in   time.Duration
		want string
	}{
		{950 * time.Nanosecond, "950ns"},
		{1500 * time.Nanosecond, "1.5µs"},
		{84231847 * time.Nanosecond, "84.2ms"},
		{1213 * time.Millisecond, "1.21s"},
	}
	for _, c := range cases {
		if got := dur(float64(c.in)); got != c.want {
			t.Errorf("dur(%v) = %q, want %q", c.in, got, c.want)
		}
	}
}
