package main

import (
	"flag"
	"fmt"
	"io"
	"math"
	"runtime"
	"sort"
	"time"
)

// This file is the FIRST of the two runner-fitness checks (issue #542): a
// self-check the machine runs BEFORE it is trusted to adjudicate anything.
//
// The gate in adjudicate.go judges the CODE. It has no way to ask whether the
// machine underneath it was in a fit state to measure code at all, and on a
// shared CI runner that is not a theoretical concern:
//
//	substrate#424, head 850f118ef, two consecutive jobs
//	  run 1   base arm ±71% (samples ~30-130ms), pr arm ±3%  -> +83% "REGRESSION"
//	  run 2   base arm ±30%,                     pr arm ±2%  -> +34% "REGRESSION"
//
// on a tree with ZERO Go-code delta from one the same gate had measured at
// parity ±3% an hour earlier. Every package on that runner ran 1.5-2x slower in
// absolute terms; three independent measurements (a fresh runner, a local
// interleaved n=12, a GOMAXPROCS=2 starvation A/B) all read parity-to-better.
//
// The two checks are complementary, and the split is the point:
//
//	burnin (this file)      a machine that is ALREADY sick when the job starts.
//	                        Code-independent, so it cannot be confounded by the
//	                        change under test, and it runs before any benchmark
//	                        has been sampled -- the only point at which "do not
//	                        measure on this machine" is still cheap.
//	variance ceiling        a machine that goes sick PARTWAY THROUGH, which is
//	(adjudicate.go)         what happened above: the base arm's samples were
//	                        disrupted and the pr arm's were not.
//
// Neither subsumes the other. A machine can pass burn-in and then acquire a
// noisy co-tenant; a machine can be born sick and still produce rows whose
// intervals happen to look tight. Both are cheap.
//
// # Why a fixed loop rather than a benchmark
//
// The workload below is deliberately NOT one of the repository's benchmarks.
// It is a fixed number of iterations of a loop that does not read the clock,
// does not depend on the code under test, and computes a value that is checked
// against a pinned constant. So:
//
//   - It cannot be confounded by the change under test. A burn-in that ran the
//     PR's own code would report "unfit" for a PR that made something slower,
//     which is the opposite of the question being asked.
//   - Its ITERATION COUNT is fixed and its DURATION is measured -- never the
//     other way round. A workload that runs "for 100ms" adapts to the machine
//     and reports the same duration on a sick one; only a fixed amount of work
//     can show that the machine took longer to do it.
//   - It asserts its own checksum, so a compiler that optimises half of it
//     away, or an edit that changes what it costs, is caught rather than
//     silently turning the check into a no-op.
//
// # What it asserts
//
// K runs, the first W discarded as warmup (cold caches, CPU frequency ramp,
// cgroup CPU credit), and the remaining samples must agree with each other to
// within a spread ceiling. The claim is narrow and mechanical:
//
//	A machine that cannot reproduce a FIXED loop to within ±10% cannot
//	resolve a 10% gate on anything else either.
//
// It is not a claim that the machine is fast, or that it matches some absolute
// reference: an arm64 runner and an amd64 runner disagree wildly on the
// absolute duration and both are perfectly fit. Only self-consistency is
// asserted, because only self-consistency is what a percentage gate needs.
//
// The heap is collected before each run so the samples differ by what the
// MACHINE did rather than by where a GC cycle happened to land.

const (
	// defaultBurninRuns and defaultBurninWarmup: 7 runs, 2 discarded, 5 kept --
	// the same order of magnitude as the n=5..10 the benchmark workflows
	// themselves sample at, so the check has roughly the sampling power of the
	// measurement it is vouching for. Two warmup runs rather than one: the
	// first pays for cold caches AND the first GC cycle, and the second is
	// where CPU frequency scaling settles on the runners measured.
	defaultBurninRuns   = 7
	defaultBurninWarmup = 2

	// defaultBurninSpread is the ceiling the kept samples must agree within.
	// ±10% is the tightest of the gates this tool adjudicates (substrate's
	// timing gate is 10%, elps' allocation gate is 5%), so a machine that
	// passes can at least resolve the loosest question it will be asked. It is
	// deliberately not tighter: a healthy shared runner does show a few percent
	// of jitter, and a burn-in that reds healthy machines would be turned off.
	defaultBurninSpread = 10.0

	// minKept is the smallest number of kept samples a spread can be computed
	// from and still mean something. With one kept sample the spread is
	// identically zero -- a check that cannot fail, which is precisely the
	// defect this tool exists to prevent -- and with two it is one arithmetic
	// step away from that.
	minKept = 3
)

// referenceRounds is the size of the reference workload: fixed, so the
// DURATION is the measurement. Sized so one run takes long enough to span
// several scheduler quanta (tens of milliseconds on the runners measured) and
// short enough that the default 7 runs are under a second.
const referenceRounds = 48000

// referenceChecksum is what referenceWork(referenceRounds) must return. It is
// pinned so that a compiler that elides the loop, or an edit that changes what
// the loop costs, fails loudly instead of quietly turning the burn-in into a
// measurement of nothing. The arithmetic is integer-only and so is identical on
// every architecture.
const referenceChecksum uint64 = 0xe5784d860f36dc7b

const (
	fnvOffset uint64 = 14695981039346656037
	fnvPrime  uint64 = 1099511628211
)

// referenceWork runs the reference workload: rounds iterations of a
// fixed-length FNV-1a mixing loop over a freshly allocated buffer. Each round
// does both halves of what a benchmark run does -- arithmetic the CPU must
// retire in order, and an allocation the collector must eventually deal with --
// so a machine that is starved of either shows up.
//
// The returned checksum is what keeps the work alive: it is consumed by the
// caller and compared against referenceChecksum, so nothing here is dead code
// the compiler may delete.
func referenceWork(rounds int) uint64 {
	h := fnvOffset
	// A uint64 counter rather than the loop indices: it is the same
	// position-dependent mixing without an int -> uint64 conversion on a value
	// that came from a flag, and it keeps the arithmetic identical on a 32-bit
	// platform.
	var n uint64
	for range rounds {
		// Allocated inside the loop on purpose: the collector is part of what
		// is being measured, and a buffer hoisted out of the loop would make
		// this a pure-CPU check.
		buf := make([]byte, 512)
		for i := range buf {
			n++
			h ^= n
			h *= fnvPrime
			buf[i] = byte(h >> 56)
		}
		for _, b := range buf {
			h ^= uint64(b)
			h *= fnvPrime
		}
	}
	return h
}

// sample is one timed run of the reference workload, plus the checksum it
// computed. Tests inject a substitute so the spread arithmetic and the
// warmup discard are exercised with no dependence on real timing.
type sample struct {
	d   time.Duration
	sum uint64
}

// realSampler times referenceWork. The heap is collected first so a GC cycle
// left over from the previous run is not charged to this one.
func realSampler(rounds int) sample {
	runtime.GC()
	start := time.Now()
	sum := referenceWork(rounds)
	return sample{d: time.Since(start), sum: sum}
}

// burninConfig is the whole burn-in policy.
type burninConfig struct {
	ceilingStr string
	ceiling    float64
	runs       int
	warmup     int
	rounds     int
}

// burninStats is the arithmetic over the KEPT samples, separated from the
// timing and the I/O so it can be tested with injected durations.
type burninStats struct {
	kept   []time.Duration
	median float64 // nanoseconds
	spread float64 // percent
}

// computeStats discards the first warmup samples and summarises the rest.
//
// spread is the furthest any kept sample lands from their median, as a percent
// of that median. It is deliberately a RANGE statistic rather than a standard
// deviation: one disrupted sample out of five is exactly the shape this check
// is looking for, and an SD over five samples dilutes it into the average
// instead of reporting it.
func computeStats(samples []time.Duration, warmup int) burninStats {
	kept := append([]time.Duration(nil), samples[warmup:]...)
	sorted := append([]time.Duration(nil), kept...)
	sort.Slice(sorted, func(i, j int) bool { return sorted[i] < sorted[j] })

	n := len(sorted)
	var median float64
	if n%2 == 1 {
		median = float64(sorted[n/2])
	} else {
		median = (float64(sorted[n/2-1]) + float64(sorted[n/2])) / 2
	}

	st := burninStats{kept: kept, median: median}
	if median <= 0 {
		// A zero or negative median means the clock reported nothing for the
		// workload, which is not a spread of 0% -- it is an unusable sample.
		// Reported as an impossible spread so it can never read as "fit".
		st.spread = math.Inf(1)
		return st
	}
	for _, d := range kept {
		if dev := math.Abs(float64(d)-median) / median * 100; dev > st.spread {
			st.spread = dev
		}
	}
	return st
}

// dur renders a duration the way the report prints it: three significant
// figures in the unit that suits its magnitude, so 84.2ms and 1.21s both read
// cleanly. time.Duration.String() prints 84.231847ms, which invites the reader
// to compare digits the measurement does not have.
func dur(ns float64) string {
	d := math.Round(ns)
	switch {
	case d >= float64(time.Second):
		return fmt.Sprintf("%.3gs", d/float64(time.Second))
	case d >= float64(time.Millisecond):
		return fmt.Sprintf("%.3gms", d/float64(time.Millisecond))
	case d >= float64(time.Microsecond):
		return fmt.Sprintf("%.3gµs", d/float64(time.Microsecond))
	default:
		return fmt.Sprintf("%dns", int64(d))
	}
}

const burninUsage = `usage: benchgate burnin [flags]

Is THIS MACHINE fit to adjudicate a percentage gate? Runs a fixed,
code-independent reference workload K times, discards the first few as warmup,
and requires the rest to agree with each other.

A machine that cannot reproduce a fixed loop cannot resolve a percentage gate
on anything else either, so the answer is worth having BEFORE the benchmarks
are sampled -- which is the only point at which "do not measure here" is cheap.

exit 0  fit
exit 2  invalid usage, or the workload did not compute its pinned checksum
exit 3  RUNNER-UNFIT: re-measure elsewhere (the same code the adjudicator uses
        for a row it could not measure)

flags (each falls back to the matching BENCH_* environment variable):
  -runs K        timed runs of the reference workload
                 (env BENCH_BURNIN_RUNS, default 7)
  -warmup W      leading runs discarded before the spread is computed
                 (env BENCH_BURNIN_WARMUP, default 2)
  -spread P      ceiling, in percent, for how far a kept sample may land from
                 the median of the kept samples
                 (env BENCH_BURNIN_SPREAD_PCT, default 10)
  -rounds N      size of the reference workload, in iterations. Fixed work,
                 measured duration -- never the other way round.
                 (env BENCH_BURNIN_ROUNDS, default 48000)
`

// runBurnin is the `benchgate burnin` subcommand, with its I/O and its sampler
// injected so the whole thing -- flags, env fallbacks, verdict, exit code -- is
// testable without depending on how busy the machine running the tests is.
func runBurnin(args []string, stdout, stderr io.Writer, sampler func(rounds int) sample) int {
	fs := flag.NewFlagSet("benchgate burnin", flag.ContinueOnError)
	fs.SetOutput(stderr)
	fs.Usage = func() { pr(stderr, burninUsage) }

	runsDef, err1 := envDefaultInt("BENCH_BURNIN_RUNS", defaultBurninRuns)
	warmDef, err2 := envDefaultInt("BENCH_BURNIN_WARMUP", defaultBurninWarmup)
	roundsDef, err3 := envDefaultInt("BENCH_BURNIN_ROUNDS", referenceRounds)
	spreadDef, spreadStr, err4 := envDefaultFloat("BENCH_BURNIN_SPREAD_PCT", defaultBurninSpread)
	for _, err := range []error{err1, err2, err3, err4} {
		if err != nil {
			pf(stderr, "benchgate burnin: %v.\n", err)
			return 2
		}
	}

	runs := fs.Int("runs", runsDef, "timed runs of the reference workload")
	warmup := fs.Int("warmup", warmDef, "leading runs discarded as warmup")
	rounds := fs.Int("rounds", roundsDef, "size of the reference workload, in iterations")
	spread := fs.Float64("spread", spreadDef, "spread ceiling, percent")

	if err := fs.Parse(args); err != nil {
		return 2
	}
	if len(fs.Args()) > 0 {
		pf(stderr, "benchgate burnin: takes no positional arguments (got %q).\n", fs.Args()[0])
		pr(stderr, burninUsage)
		return 2
	}
	fs.Visit(func(f *flag.Flag) {
		if f.Name == "spread" {
			spreadStr = f.Value.String()
		}
	})

	cfg := burninConfig{runs: *runs, warmup: *warmup, rounds: *rounds, ceiling: *spread, ceilingStr: spreadStr}
	switch {
	case cfg.warmup < 0:
		pf(stderr, "benchgate burnin: -warmup must not be negative (got %d).\n", cfg.warmup)
		return 2
	case cfg.rounds < 1:
		pf(stderr, "benchgate burnin: -rounds must be at least 1 (got %d).\n", cfg.rounds)
		return 2
	case cfg.ceiling <= 0:
		// A ceiling of zero demands that a real machine reproduce a duration to
		// the nanosecond. It would fail every run, which is a check nobody
		// leaves switched on -- refused rather than honoured.
		pf(stderr, "benchgate burnin: -spread/BENCH_BURNIN_SPREAD_PCT must be a positive percentage (got %s).\n", trimFloat(cfg.ceiling))
		return 2
	case cfg.runs-cfg.warmup < minKept:
		pf(stderr, "benchgate burnin: -runs %d with -warmup %d keeps %d sample(s); at least %d are needed for a spread to mean anything (over one sample it is identically zero, which is a check that cannot fail).\n",
			cfg.runs, cfg.warmup, cfg.runs-cfg.warmup, minKept)
		return 2
	}

	return burnin(cfg, stdout, stderr, sampler)
}

// burnin runs the workload and reports. Split from flag parsing so tests drive
// the verdict directly.
func burnin(cfg burninConfig, stdout, stderr io.Writer, sampler func(rounds int) sample) int {
	pf(stdout, "benchgate burnin: %d run(s) of the reference workload (%d rounds), first %d discarded as warmup; GOMAXPROCS=%d NumCPU=%d %s/%s.\n",
		cfg.runs, cfg.rounds, cfg.warmup, runtime.GOMAXPROCS(0), runtime.NumCPU(), runtime.GOOS, runtime.GOARCH)

	samples := make([]time.Duration, 0, cfg.runs)
	var first uint64
	for i := range cfg.runs {
		s := sampler(cfg.rounds)
		if i == 0 {
			first = s.sum
		}
		// Every run must compute the same answer as every other run, and -- at
		// the default size -- the same answer as the pinned constant. A machine
		// that disagrees with itself about arithmetic is not merely slow, and a
		// workload that no longer produces the pinned value is no longer the
		// workload whose cost profile this check was calibrated against.
		if s.sum != first {
			pf(stderr, "benchgate burnin: the reference workload computed %#x on run %d and %#x on run 1 -- the same fixed loop returned two different answers, so this machine (or this build) cannot be trusted to measure anything.\n", s.sum, i+1, first)
			return 2
		}
		if cfg.rounds == referenceRounds && s.sum != referenceChecksum {
			pf(stderr, "benchgate burnin: the reference workload returned %#x, want %#x -- the workload is not the one this check was calibrated against (edited, or optimised away). Refusing to report fitness from a measurement of nothing.\n", s.sum, referenceChecksum)
			return 2
		}
		samples = append(samples, s.d)
		mark := ""
		if i < cfg.warmup {
			mark = "  (warmup, discarded)"
		}
		pf(stdout, "  run %-2d  %10s%s\n", i+1, dur(float64(s.d)), mark)
	}

	st := computeStats(samples, cfg.warmup)
	if st.spread >= cfg.ceiling {
		// One line, on stderr, saying what was measured and what to do. Exit 3
		// is the SAME code the adjudicator uses for a row it could not measure:
		// both mean "this machine did not produce a usable measurement --
		// re-measure", and a caller that handles one handles the other.
		pf(stderr, "benchgate burnin: UNFIT -- %d kept sample(s) of a FIXED loop spread ±%s%% around their %s median (ceiling ±%s%%); a machine that cannot reproduce a fixed loop cannot resolve a percentage gate, so any comparison measured here is not evidence. Re-run on another runner.\n",
			len(st.kept), awkNum(roundSpread(st.spread)), dur(st.median), cfg.ceilingStr)
		return 3
	}
	pf(stdout, "benchgate burnin: FIT -- %d kept sample(s) agree to within ±%s%% of their %s median (ceiling ±%s%%).\n",
		len(st.kept), awkNum(roundSpread(st.spread)), dur(st.median), cfg.ceilingStr)
	return 0
}
