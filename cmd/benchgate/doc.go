// Command benchgate decides whether a benchmark comparison contains a
// statistically significant performance REGRESSION.
//
// It is the Go replacement for scripts/benchstat-gate.sh (elps) and
// scripts/benchstat-gate.sh (substrate), which adjudicated by scraping
// benchstat's human-formatted text output with awk. Both repositories now run
// this one binary, so a fix lands once instead of twice -- the divergence
// between the two shell copies is what motivated issue #538.
//
// # Two front ends, one adjudicator
//
// The policy below is expressed once, over a typed []row. Rows reach it two
// ways:
//
//   - a positional FILE argument (the default, and what CI uses): read a
//     benchstat table. This is the same input the shell gate took, which is
//     what makes verdict parity with it checkable on every fixture the two
//     repositories had accumulated -- and those fixtures ARE the test suite
//     (see gate_test.go).
//
//   - -base FILE -head FILE: read raw `go test -bench` output with
//     golang.org/x/perf/benchfmt and compute the comparison with
//     golang.org/x/perf/benchmath -- benchstat's own libraries, doing what
//     benchstat itself does: median summary with a 95% confidence interval
//     under benchmath.AssumeNothing, Mann-Whitney U for the p-value,
//     (new/old-1)*100 for the delta. No benchstat binary, no text scraping.
//
// Both front ends round the delta and the per-arm spread to the precision
// benchstat PRINTS (%+.2f%% and %.0f%%). The gate then adjudicates the same
// numbers a reader sees in the table, and the two front ends cannot disagree
// on a boundary case purely because one of them carried more digits.
//
// # Metric direction
//
// elps' parser benchmarks call b.SetBytes(), so `go test -bench` emits a
// throughput metric and benchstat renders a `B/s` column. B/s is
// HIGHER-IS-BETTER: a naive "any positive delta is a regression" rule flags
// throughput IMPROVEMENTS as regressions. In a measured elps run, comparing
// the workflow's exact benchmark command against ITSELF produced
//
//	Parser/diff.lisp-4    1.354Mi -> 3.767Mi   +178.17% (p=0.008 n=5)   [B/s]
//
// a 2.8x throughput gain. So each metric column is classified:
//
//   - `*/s` (B/s, MB/s, ops/s) and the old format's `speed` -> higher is
//     better; the regression direction is NEGATIVE.
//   - everything else (sec/op, ns/op, B/op, allocs/op) -> lower is better;
//     the regression direction is POSITIVE.
//
// An unrecognised metric falls back to lower-is-better, which is the safe
// default: it can over-report, never under-report.
//
// # What counts as a regression
//
//   - The row must carry a p-value and it must be <= alpha (benchstat prints
//     "~" instead of a delta when the difference is not significant, so "~"
//     rows are skipped by construction).
//   - The delta must move in the BAD direction for that metric.
//   - The move must be >= the threshold for that metric's CLASS.
//     Significant-but-small moves are REPORTED (so they appear in the job log)
//     but do not fail the build.
//   - geomean rows carry no p-value and are informational only -- a real
//     regression always shows up in at least one of the rows the geomean
//     summarizes.
//
// # Two thresholds, because the metrics have wildly different noise
//
// elps' benchmark command was run three times against IDENTICAL code and the
// runs compared pairwise. Every "significant" delta in such a comparison is by
// construction pure noise. Counting only moves in the BAD direction:
//
//	metric      significant bad-direction moves   worst
//	allocs/op   0                                 --
//	B/op        1                                  0.19%
//	B/s         8                                 23.48%
//	sec/op      15                                33.83%
//
// The allocation metrics are effectively DETERMINISTIC; the timing metrics are
// extremely noisy at these sampling parameters. A SINGLE threshold is
// therefore the wrong shape: set it low enough to be useful for allocations
// and every PR reds on timing noise; set it high enough to survive timing
// noise and real allocation regressions sail through. So there are two:
//
//	-alloc-threshold  (default 5)   B/op, allocs/op
//	-threshold        (default 15)  sec/op, B/s, unrecognised
//
// A real CI null comparison (no Go code changed) is preserved as
// testdata/benchstat-clean-ci.txt; its worst bad-direction moves are +1.52%
// sec/op and +0.18% B/op, so real CI timing noise is ~1.5%, not the ~34% a
// contended sandbox shows.
//
// substrate's shell gate had a single 10% threshold and no class split at all.
// Adopting this tool gives it the split; see the migration note in that
// repository's benchmark workflows.
//
// # A threshold cannot be right for every row: the resolution check
//
// Both thresholds are single numbers applied to every row of their metric
// class, which is exactly as good as the assumption behind it: that the rows
// in a class have comparable noise. For timing it does not hold.
//
// Issue #443 is the worked example. BenchmarkPackageGetFunParallel failed the
// gate on PR #442 -- a parser-only change with no path to a map lookup in the
// lisp package -- at +15.96% (p=0.035), and a re-run with no code change
// turned it green. Its noise floor was then measured directly: two independent
// checkouts of the SAME commit, interleaved, at the runner's GOMAXPROCS.
//
//	base vs base2 (identical code, n=20)
//	PackageGetFunParallel-2   38.70n ± 24%   38.94n ± 24%   ~ (p=0.841 n=20)
//
// A row with a ±24% spread on identical code cannot resolve a 16% move. So the
// gate asks the row instead of the constant:
//
//	A TIMING row at or above its threshold is called a REGRESSION only when
//	the move is LARGER than the row's own measured spread. When it is not,
//	the row is reported as NOISE-FLOOR and excluded from the verdict.
//
// Note what it does NOT do:
//
//   - It never applies to B/op or allocs/op. Those are the metrics that have
//     caught every real regression this gate has caught. It is skipped
//     explicitly regardless -- an allocation column that DOES jitter must not
//     be able to buy itself the timing metrics' leniency, because a jittery
//     allocation column is a benchmark defect and not a noise floor. Rows
//     where an allocation column jitters are handled by the quantisation check
//     below instead, which can only ever discard a ONE-COUNT move.
//   - It cannot suppress a large move. On the row above it would take a >24%
//     regression to fire -- which is what "this row cannot resolve less than
//     that" means.
//   - It does nothing when no interval could be computed ("± ∞ ¹", printed
//     below 6 samples). Those rows fall back to the threshold alone and SAY SO
//     on the regression line, so a check that did not run is never mistaken
//     for one that ran and passed.
//
// # A count is not a measurement: the quantisation check
//
// `go test` reports allocs/op as int64(memstats.Mallocs-before)/int64(b.N) --
// INTEGER DIVISION. The numerator is not a multiple of b.N, so the printed
// column is a TRUNCATION of a continuous cost, and a row whose true cost sits
// near an integer prints either side of it from run to run with no code change
// at all.
//
// libjson's BenchmarkEncodeOwnMessageLarge is the worked example. Measured by
// reading MemStats around 2000 encodes of the same ~295 KB document at the
// default GOGC: true allocs/op = 9.985, reported allocs/op = 9 or 10. Ten
// consecutive samples of that row from ONE binary come back as a mix, and
// benchstat -- correctly -- summarises the arm as "9.000 ± 11%". When the mix
// falls the wrong way across two arms the table reads
//
//	EncodeOwnMessageLarge-2   9.000 ± 11%   10.00 ± 10%   +11.11% (p=0.023 n=10)
//
// on code that did not change. The delta cannot be small: the SMALLEST move
// this row can express is one allocation out of nine, and 1/9 is 11.11%.
//
//	A move of ONE COUNT on an integer-count metric (allocs/op) is called a
//	REGRESSION only when the row reproduces its own count -- that is, only
//	when no spread was measured on either arm. A one-count move on a row
//	that disagrees with itself is reported as QUANTISED and excluded from
//	the verdict.
//
// It is deliberately the tightest rule that fixes the class: it can only ever
// discard a ONE-COUNT move (two counts is not reachable by truncation); one
// count clears a 5% allocation gate only at 20 allocs/op and below, so from 21
// up it changes no verdict that was not already "below-gate"; and it does NOT
// fire on a stable row, because it keys on a row disagreeing WITH ITSELF,
// which a real regression does not do.
//
// # Reviewed waivers
//
// Sometimes a regression is real, understood and deliberately accepted. The
// answer to that is NOT a higher threshold -- the thresholds are per-metric-
// class noise floors, and raising one to accept a single benchmark blinds
// every other benchmark in the repository to the same magnitude of move,
// permanently. It is a per-row waiver, declared in a waiver file, reviewed in
// the diff that needs it, and bounded:
//
//   - it names ONE package, ONE benchmark and ONE metric column, all matched
//     exactly, so it cannot reach a row it was not written for;
//   - it records a CEILING, and the row fails again the moment its regression
//     grows past what was accepted;
//   - it carries a reason and a tracking issue, and an entry missing either is
//     a hard exit 2 rather than a silently-ignored line;
//   - it EXPIRES, after which it stops suppressing and the row is judged
//     normally again.
//
// A waived row is still parsed, still counted, and still printed -- as WAIVED,
// with its delta, its ceiling and its issue. Dropping it from the report would
// recreate this gate's founding defect (a check that looks green because it
// stopped looking) one benchmark at a time, so the waiver changes the VERDICT
// and never the visibility. Waivers that match nothing are reported too:
// WAIVER-STALE when the row is absent from the comparison entirely,
// waiver-unused when the row is present and no longer regressing.
//
// # Why there is no config file
//
// The whole policy is five numbers and two paths. Both repositories already
// declare them as workflow-level `env:` entries next to the prose explaining
// how each was measured, and this tool reads those same names
// (BENCH_REGRESSION_THRESHOLD_PCT, BENCH_ALLOC_THRESHOLD_PCT, BENCH_ALPHA,
// BENCH_WAIVERS, BENCH_WAIVER_TODAY) so the migration moved no policy. A
// config file would add a second place for policy to live and a parse surface
// that itself needs a "what if it does not parse" rule; flags plus those env
// vars need neither.
//
// # Exit codes
//
//	0  no regression at or above the threshold
//	1  regression detected
//	2  the input could not be interpreted (missing/empty file, or NO
//	   comparison row at all -- which means benchstat's output format changed
//	   or benchstat crashed), or the waiver file could not be interpreted.
//
// Exiting 2 rather than 0 is deliberate: an uninterpretable comparison must
// fail loudly instead of reporting "no regression", which is exactly how the
// old inline `grep -E '^\S.*\+$'` gate stayed green for 473 runs. The same
// reasoning covers the waiver file: a malformed waiver list must never be read
// as an empty one.
package main
