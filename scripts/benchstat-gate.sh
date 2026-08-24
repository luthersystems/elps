#!/usr/bin/env bash
#
# Benchmark regression gate: decide whether a benchstat comparison contains a
# statistically significant performance REGRESSION.
#
# Why this script exists
# ----------------------
# The gate used to live inline in .github/workflows/benchmark.yml as:
#
#     if grep -E '^\S.*\+$' benchstat-output.txt | grep -qv '^name'; then
#
# That pattern requires a line ENDING in a literal '+'.  No benchstat line ever
# ends in '+': a regression row ends in "(p=0.008 n=5)" and a geomean row ends
# in a percentage.  The gate therefore could not fire.  Across 473 workflow
# runs the "Fail on regressions" step never once executed -- the workflow was
# reporting success by construction, not by measurement.
#
# Moving the logic into a script makes it testable; scripts/ci-gates-test.sh
# feeds it known-regression and known-clean fixtures on every PR, so this class
# of silent death fails a PR instead of hiding.
#
# Metric direction: elps needs this, substrate did not
# ----------------------------------------------------
# elps' parser benchmarks call b.SetBytes() (parser/rdparser/bench_test.go and
# elpstest/lisptest.go), so `go test -bench` emits a THROUGHPUT metric and
# benchstat renders it as a `B/s` column alongside sec/op, B/op and allocs/op.
#
# B/s is HIGHER-IS-BETTER.  A naive "any positive delta is a regression" rule --
# which is what the upstream reference implementation this was ported from uses,
# because that repo has no throughput metric -- flags throughput IMPROVEMENTS as
# regressions.  In a measured elps run, comparing the workflow's exact benchmark
# command against ITSELF produced rows like:
#
#     Parser/diff.lisp-4    1.354Mi -> 3.767Mi   +178.17% (p=0.008 n=5)   [B/s]
#
# That is a 2.8x throughput *gain*.  Gating on the raw sign would have failed
# that PR.  So this gate classifies each metric column:
#
#     * `*/s` (B/s, MB/s, ops/s) and the old format's `speed` -> higher is
#       better; the regression direction is NEGATIVE.
#     * everything else (sec/op, ns/op, B/op, allocs/op) -> lower is better;
#       the regression direction is POSITIVE.
#
# An unrecognised metric falls back to lower-is-better, which is the safe
# default: it can over-report, never under-report.
#
# What counts as a regression
# ---------------------------
#   * The row must carry a p-value and it must be <= BENCH_ALPHA (benchstat
#     prints "~" instead of a delta when the difference is not significant, so
#     "~" rows are skipped by construction).
#   * The delta must move in the BAD direction for that metric (see above).
#   * The move must be >= the threshold for that metric's CLASS (see below).
#     Significant-but-small moves are REPORTED (so they appear in the job log)
#     but do not fail the build.
#   * geomean rows carry no p-value and are informational only -- a real
#     regression always shows up in at least one of the rows the geomean
#     summarizes.
#
# Two thresholds, because elps' metrics have wildly different noise
# ------------------------------------------------------------------
# The workflow's exact benchmark command
#
#     go test -bench=. -benchmem -benchtime=100ms -count=5 -run='^$' ./...
#
# was run three times against IDENTICAL code and the runs compared pairwise with
# benchstat.  Every "significant" delta in such a comparison is by construction
# pure noise.  Counting only moves in the BAD direction (the ones that would red
# an innocent PR):
#
#     metric      significant bad-direction moves   worst
#     allocs/op   0                                 --
#     B/op        1                                  0.19%
#     B/s         8                                 23.48%
#     sec/op      15                                33.83%
#
# The allocation metrics are effectively DETERMINISTIC -- identical code
# reproduces them exactly, and benchstat routinely annotates them "all samples
# are equal".  The timing metrics are extremely noisy at these sampling
# parameters (-benchtime=100ms with -count=5 is a very small sample, and the
# baseline and PR measurements are taken in different jobs on different shared
# runners).
#
# A SINGLE threshold is therefore the wrong shape for elps: set it low enough to
# be useful for allocations and every PR reds on timing noise; set it high enough
# to survive timing noise and real allocation regressions -- which are perfectly
# measurable -- sail through.  So there are two:
#
#   BENCH_ALLOC_THRESHOLD_PCT      (default 5)   B/op, allocs/op
#       Worst-case bad-direction noise measured at 0.19% (sandbox) and 0.18%
#       (real CI), so 5% leaves ~26x headroom.  This is the gate that will
#       actually catch things, and it is trustworthy today.
#
#   BENCH_REGRESSION_THRESHOLD_PCT (default 15)  sec/op, B/s, unrecognised
#       See the two measurements below.  Coarser than the allocation gate by an
#       order of magnitude, because timing on shared runners genuinely is.
#
# The sandbox numbers above bound the noise from ABOVE; they do not predict CI.
# A real datapoint is now available and is much kinder.  The CI run for the very
# commit that introduced this script changed NO Go code, so its benchstat
# comparison against the main baseline is a genuine null comparison on the real
# infrastructure (GitHub ubuntu-latest, AMD EPYC 7763).  It is preserved verbatim
# as scripts/testdata/benchstat-clean-ci.txt.  Worst bad-direction moves there:
#
#     Package/dump-array-4   +1.52% (p=0.008 n=5)   [sec/op]
#     Package/load-nested-4  +0.88% (p=0.032 n=5)   [sec/op]
#     Parser/sicp.lisp-4     +0.18% (p=0.016 n=5)   [B/op]
#
# So real CI timing noise is ~1.5%, not the ~34% a contended sandbox shows.
# 15% is set against that: ~10x headroom over the observed CI floor, and still
# above the 7.14% worst case the sibling substrate repo measured on its own
# (noisier, self-hosted ARM) runners, so an unlucky run does not red an innocent
# PR.  It should come down further as more real runs accumulate -- both numbers
# are env vars in .github/workflows/benchmark.yml, so tuning them needs no change
# to this script.
#
# Do not drop the timing gate straight to 5% -- the number
# .claude/skills/benchmark/SKILL.md quotes for local, same-machine comparisons --
# on the strength of one quiet run.  If it reds innocent PRs the gate gets
# switched off again, recreating the exact problem this script fixes.
#
# The root fix for the timing headroom is quieter benchmarks (a longer
# -benchtime, a higher -count, or dropping the sub-millisecond microbenchmarks
# from the comparison set), after which the timing threshold should come down.
# That is left as a follow-up so this change stays a gate fix rather than a
# benchmark-tuning change.
#
# A threshold cannot be right for every row: the resolution check
# --------------------------------------------------------------
# Both thresholds above are single numbers applied to every row of their metric
# class, which is exactly as good as the assumption behind it: that the rows in
# a class have comparable noise.  For most allocation rows it holds -- identical
# code reproduces them EXACTLY, benchstat prints "± 0%" and routinely annotates
# the row "all samples are equal".  For timing it does not, and for one shape of
# allocation row it does not either; see the quantisation check below.
#
# Issue #443 is the worked example.  BenchmarkPackageGetFunParallel failed the
# gate on PR #442 -- a parser-only change with no path to a map lookup in the
# lisp package -- at +15.96% (p=0.035), and a re-run with no code change turned
# it green.  Its noise floor was then measured directly: two independent
# checkouts of the SAME commit, interleaved, at the runner's GOMAXPROCS.
#
#     base vs base2 (identical code, n=20)
#     PackageGetFunParallel-2   38.70n ± 24%   38.94n ± 24%   ~ (p=0.841 n=20)
#
# A row with a ±24% spread on identical code cannot resolve a 16% move.  The
# 15% threshold is BELOW that row's own measurement resolution, so a p<=alpha
# draw over 15% will happen there by chance at some rate on every PR that runs
# the comparison -- on code that cannot reach the benchmark.  It is a sub-100ns
# body measured under RunParallel at -benchtime=100ms: the worst case for these
# sampling parameters, and the only such row in the comparison set today.
#
# Raising the threshold is the wrong answer, for the reason the waivers file
# already gives: 15% is a per-class noise floor, and moving it to accommodate
# one row blinds every serial benchmark in the repository to the same magnitude
# of move, permanently.  A waiver is wrong too -- a waiver records a cost that
# is REAL and accepted, and this one is not real.
#
# So the gate asks the row instead.  benchstat already prints, per arm, the 95%
# confidence interval of that arm's median as a percentage -- the "± 24%" above.
# That IS the row's measurement resolution, computed from the very samples being
# adjudicated, and it is already in the table this script parses.  The rule:
#
#   A TIMING row at or above its threshold is called a REGRESSION only when the
#   move is LARGER than the row's own measured spread.  When it is not, the row
#   is reported as NOISE-FLOOR and excluded from the verdict.
#
# That is the "significant AND large enough to see" rule, with "large enough"
# denominated in the row's own dispersion rather than in a constant somebody
# chose once.  Note what it does NOT do:
#
#   * It never applies to B/op or allocs/op.  Those are the metrics that have
#     caught every real regression this gate has caught, and on the rows where
#     they are exact their spread is "± 0%", so a rule keyed on spread would be
#     a no-op there anyway.  It is skipped explicitly regardless -- an
#     allocation column that DOES jitter must not be able to buy itself the
#     timing metrics' leniency, because a jittery allocation column is a
#     benchmark defect and not a noise floor.  Rows where an allocation column
#     jitters are handled by the quantisation check below instead, which is a
#     far narrower rule: it can only ever discard a ONE-COUNT move.
#   * It cannot suppress a large move.  On the row above it would take a >24%
#     regression to fire -- which is what "this row cannot resolve less than
#     that" means.  A row whose noise is genuinely that large is not being
#     protected; it is being reported as unmeasurable, every run, by name.
#   * It does nothing when benchstat could not compute an interval ("± ∞ ¹",
#     which it prints below 6 samples).  Those rows fall back to the threshold
#     alone and SAY SO on the regression line, so a check that did not run is
#     never mistaken for one that ran and passed.  CI runs n=10, so this is the
#     fixtures' case rather than CI's.
#
# The honest cost: on a row whose spread exceeds the threshold, a real
# regression BETWEEN the threshold and the spread is now reported rather than
# gated.  It was never detectable there -- it sat inside the row's own null
# distribution, and the gate's "detection" of it was a coin flip that landed the
# other way on the re-run.  The fix for such a row is to make it quieter (a
# longer -benchtime for that benchmark, or keeping a sub-100ns RunParallel body
# out of the comparison set), and the NOISE-FLOOR line is what tells you which
# rows need it.
#
# A count is not a measurement: the quantisation check
# ----------------------------------------------------
# `go test` reports allocs/op as
#
#     int64(memstats.Mallocs - before) / int64(b.N)
#
# INTEGER DIVISION.  The numerator is not a multiple of b.N, so the printed
# column is a TRUNCATION of a continuous cost, and a row whose true cost sits
# near an integer prints either side of it from run to run with no code change
# at all.
#
# libjson's BenchmarkEncodeOwnMessageLarge is the worked example.  Measured
# directly, by reading MemStats around 2000 encodes of the same ~295 KB document
# at the DEFAULT GOGC:
#
#     true allocs/op = 9.985          reported allocs/op = 9  or  10
#
# a fifth of a percent below the boundary.  Ten consecutive samples of that row
# from ONE binary, at CI's -benchtime=100ms, come back as a mix of 9s and 10s,
# and benchstat -- correctly -- summarises the arm as "9.000 ± 11%" on a column
# this file used to describe as exact.  When the mix falls the wrong way across
# two arms the table reads
#
#     EncodeOwnMessageLarge-2   9.000 ± 11%   10.00 ± 10%   +11.11% (p=0.023 n=10)
#
# on code that did not change -- which is what reddened a PR touching only the
# nested tree-sitter module.  Note the delta cannot be small: the SMALLEST move
# this row can express is one allocation out of nine, and 1/9 is 11.11%.  A 5%
# gate on a 9-count row has exactly two reachable verdicts, "no change" and
# "+11.11%".
#
# Where the fractional part comes from, on this row, is sync.Pool: runtime
# poolCleanup drops every pool at every GC, so the next Get goes through
# pin -> pinSlow and reallocates the per-P array.  An alloc-profile diff over
# 2000 encodes attributes the difference to sync.(*Pool).pinSlow, NOT to a pool
# miss -- the encoder itself comes back from the pool essentially every time
# (0.001 misses/op).  That cost is per-GC, so dividing it by the handful of
# operations between two collections lands it at "about one allocation per op",
# and which side of 10.0 it lands on is GC cadence rather than code.  Warming
# the pool before b.ResetTimer does not touch it: nothing survives poolCleanup.
#
# The rule:
#
#   A move of ONE COUNT on an integer-count metric (allocs/op) is called a
#   REGRESSION only when the row reproduces its own count -- that is, only when
#   benchstat measured NO spread on either arm.  A one-count move on a row that
#   disagrees with itself is reported as QUANTISED and excluded from the
#   verdict.
#
# It is deliberately the tightest rule that fixes the class:
#
#   * It can only ever discard a ONE-COUNT move.  Two counts is not reachable by
#     truncation -- that needs the true values to differ by more than a whole
#     allocation -- so it is adjudicated normally.
#   * One count clears the 5% allocation gate only at 20 allocs/op and below
#     (1/20 = 5.00%, 1/21 = 4.76%).  From 21 up a single-count move is already
#     under the gate, so this rule changes no verdict that was not already
#     "below-gate".
#   * It does NOT fire on a stable row.  A genuine one-allocation regression on
#     a 9-count row that reproduces exactly still reds the build at +11.11%,
#     with "± 0%" on both arms.  That is measured rather than assumed: adding
#     one escaping allocation to the encode path moves
#     BenchmarkEncodeOwnMessageSmall from 9.000 ± 0% to 10.000 ± 0%, and the
#     gate calls it a REGRESSION.  The check keys on a row disagreeing WITH
#     ITSELF, which a real regression does not do.
#   * It does nothing when benchstat could not compute an interval ("± ∞ ¹",
#     below 6 samples) or when the base cell cannot be parsed.  Those rows fall
#     back to the threshold alone and SAY SO on the regression line, so a check
#     that did not run is never mistaken for one that ran and passed.
#
# The honest cost: on a row that does not reproduce its own allocation count, a
# real ONE-allocation regression is now reported rather than gated.  It was
# never distinguishable there -- the same one-count step is what the row
# produces on identical code -- and the QUANTISED line is what says which rows
# need fixing.  The fix for such a row is to make the count reproduce: stop the
# benchmark forcing a collection every few operations, or keep it out of the
# comparison set.  Raising the allocation threshold is NOT the fix -- 5% is
# already far below the 11.11% quantum of a 9-count row, so no reachable
# threshold separates the two cases.
#
# Reviewed waivers
# ---------------
# Sometimes a regression is real, understood and deliberately accepted.  The
# answer to that is NOT a higher threshold -- the thresholds are per-metric-class
# noise floors, and raising one to accept a single benchmark blinds every other
# benchmark in the repository to the same magnitude of move, permanently.  It is
# a per-row waiver, declared in scripts/benchstat-waivers.txt, reviewed in the
# diff that needs it, and bounded:
#
#   * it names ONE package, ONE benchmark and ONE metric column, all matched
#     exactly, so it cannot reach a row it was not written for;
#   * it records a CEILING, and the row fails again the moment its regression
#     grows past what was accepted;
#   * it carries a reason and a tracking issue, and an entry missing either is a
#     hard exit 2 rather than a silently-ignored line;
#   * it EXPIRES, after which it stops suppressing and the row is judged
#     normally again.
#
# A waived row is still parsed, still counted, and still printed -- as WAIVED,
# with its delta, its ceiling and its issue -- in both the job log and the PR
# comment.  Dropping it from the report would recreate this gate's founding
# defect (a check that looks green because it stopped looking) one benchmark at
# a time, so the waiver changes the VERDICT and never the visibility.
#
# Waivers that match nothing are reported too: WAIVER-STALE when the row is
# absent from the comparison entirely (renamed benchmark, deleted package,
# typo), waiver-unused when the row is present and no longer regressing.
#
# See scripts/benchstat-waivers.txt for the file format; BENCH_WAIVERS overrides
# the path (set it EMPTY to adjudicate with no waivers at all, which can only
# ever make the gate stricter).
#
# Both benchstat table formats are handled:
#   new (golang.org/x/perf, box-drawing columns):
#     EnvGet-4   27.13m ± ∞ ¹   29.16m ± ∞ ¹  +7.14% (p=0.008 n=5)
#   old (pre-2022 "name/delta" table):
#     EnvGet-4   27.1ms ± 2%   29.2ms ± 1%   +7.14%  (p=0.008 n=5+5)
#
# Usage:  scripts/benchstat-gate.sh <benchstat-output-file>
#
# Exit codes:
#   0  no regression at or above the threshold
#   1  regression detected
#   2  the input could not be interpreted (missing/empty file, or NO comparison
#      row at all -- which means benchstat's output format changed or benchstat
#      crashed), or the waiver file could not be interpreted.  Exiting 2 rather
#      than 0 is deliberate: an uninterpretable comparison must fail loudly
#      instead of reporting "no regression", which is exactly how the old gate
#      stayed green for 473 runs.  The same reasoning covers the waiver file: a
#      malformed waiver list must never be read as an empty one.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

ALPHA="${BENCH_ALPHA:-0.05}"
THRESHOLD="${BENCH_REGRESSION_THRESHOLD_PCT:-15}"
ALLOC_THRESHOLD="${BENCH_ALLOC_THRESHOLD_PCT:-5}"

# Overridable so the self-test can drive the waiver logic with fixtures, and so
# a run can be adjudicated with waivers switched OFF (BENCH_WAIVERS= empty).
# Disabling them only ever makes the gate stricter, so it is not a bypass.
DEFAULT_WAIVERS="${SCRIPT_DIR}/benchstat-waivers.txt"
waivers_set=1
if [ -z "${BENCH_WAIVERS+x}" ]; then
	waivers_set=0
fi
WAIVERS="${BENCH_WAIVERS-$DEFAULT_WAIVERS}"

# Today, as the waiver expiry check sees it. Overridable so the self-test can
# exercise both sides of an expiry without waiting for a date to arrive.
TODAY="${BENCH_WAIVER_TODAY:-$(date -u +%Y-%m-%d)}"

if [ "$#" -ne 1 ]; then
	echo "usage: $0 <benchstat-output-file>" >&2
	echo "  env: BENCH_REGRESSION_THRESHOLD_PCT (default 15)  timing metrics: sec/op, B/s" >&2
	echo "       BENCH_ALLOC_THRESHOLD_PCT      (default 5)   allocation metrics: B/op, allocs/op" >&2
	echo "       BENCH_ALPHA                    (default 0.05)" >&2
	echo "       BENCH_WAIVERS                  (default scripts/benchstat-waivers.txt; empty = none)" >&2
	echo "       BENCH_WAIVER_TODAY             (default today, UTC; YYYY-MM-DD)" >&2
	exit 2
fi

input="$1"

if [ ! -f "$input" ]; then
	echo "benchstat-gate: no such file: $input" >&2
	exit 2
fi

if [ ! -s "$input" ]; then
	echo "benchstat-gate: $input is empty -- benchstat produced no output" >&2
	exit 2
fi

if ! printf '%s' "$TODAY" | grep -qE '^[0-9]{4}-[0-9]{2}-[0-9]{2}$'; then
	echo "benchstat-gate: BENCH_WAIVER_TODAY='${TODAY}' is not a YYYY-MM-DD date." >&2
	exit 2
fi

# An EXPLICIT BENCH_WAIVERS pointing at nothing is an error -- you asked for a
# specific file and it is not there. The default file being absent is not: the
# gate still works, it just has no waivers, which is the strict direction.
waiver_input=$(mktemp)
trap 'rm -f "$waiver_input"' EXIT
if [ -n "$WAIVERS" ]; then
	if [ -f "$WAIVERS" ]; then
		cat "$WAIVERS" >"$waiver_input"
	elif [ "$waivers_set" -eq 1 ]; then
		echo "benchstat-gate: BENCH_WAIVERS points at a file that does not exist: $WAIVERS" >&2
		exit 2
	else
		echo "benchstat-gate: no waiver file at ${WAIVERS}; adjudicating with no waivers."
	fi
fi

report=$(mktemp)
trap 'rm -f "$report" "$waiver_input"' EXIT

# Two inputs: the waiver list first, then the benchstat table. They are told
# apart by FILENAME rather than by the usual FNR==NR trick, which mis-attributes
# the first record of the second file whenever the first file is empty -- and an
# empty waiver list is the normal case.
#
# The awk program emits one human-readable line per interesting row, then a
# final machine-readable line:
#   "VERDICT <regressions> <significant> <compared> <tilde> <badp> <waived>
#            <waiver_bad> <waiver_stale> <waiver_unused> <waiver_expired>
#            <waivers> <waiver_outscope> <unresolved>".
awk -v alpha="$ALPHA" -v threshold="$THRESHOLD" -v alloc_threshold="$ALLOC_THRESHOLD" \
	-v wfile="$waiver_input" -v wsource="${WAIVERS:-<none>}" -v today="$TODAY" '
	# Last signed-percentage token (e.g. +7.14% / -1.20%) inside s, "" if none.
	function last_signed_pct(s,   pos, tok, best, rest) {
		best = ""
		pos = 1
		while (1) {
			rest = substr(s, pos)
			if (!match(rest, /[+-][0-9]+(\.[0-9]+)?%/)) break
			tok = substr(rest, RSTART, RLENGTH)
			best = tok
			pos = pos + RSTART + RLENGTH - 1
		}
		return best
	}

	# Byte offset of the last "±" in s, 0 if none.  mawk is byte oriented and
	# "±" is a multi-byte sequence, but index() on the literal still works.
	function last_spread(s,   pos, off, best) {
		best = 0
		pos = 1
		while (1) {
			off = index(substr(s, pos), "±")
			if (off == 0) break
			best = pos + off - 1
			pos = best + 1
		}
		return best
	}

	# The LARGEST per-arm spread benchstat printed on this row, as a percent, or
	# -1 when it printed none it could compute.
	#
	# benchstat writes each arm as "<median> ± <pct>%": the half-width of the
	# 95% confidence interval of the median of that arm, relative to it. That
	# is how finely this comparison can see on this row, measured
	# on the same samples the verdict is drawn from. Below 6 samples it cannot
	# compute one and prints "± ∞ ¹" instead; that is reported as -1 (unknown)
	# rather than as 0, because treating "no interval" as "a perfect interval"
	# would suppress nothing while looking like it had checked.
	#
	# The LARGER of the two arms, not the base arm alone: dispersion in both
	# arms feeds the uncertainty of the delta. Taking the max is the
	# lenient direction of the two (it suppresses more), but the alternative --
	# base only -- would let a change that ADDS variance be judged against the
	# quiet arm it replaced, and this rule must never be easier to trip by
	# making a benchmark noisier.
	function max_spread(s,   pos, off, rest, tok, num, best, sawinf) {
		best = -1
		sawinf = 0
		pos = 1
		while (1) {
			rest = substr(s, pos)
			off = index(rest, "±")
			if (off == 0) break
			pos = pos + off + length("±") - 1
			rest = substr(s, pos)
			# "± 24%" and "±24%" both occur; "± ∞ ¹" is the no-interval case.
			if (match(rest, /^[ \t]*[0-9]+(\.[0-9]+)?%/)) {
				tok = substr(rest, RSTART, RLENGTH)
				sub(/%$/, "", tok)
				num = tok + 0
				if (num > best) best = num
			} else {
				sawinf = 1
			}
		}
		if (best < 0 && sawinf) return -1
		return best
	}

	# 1 when a LARGER value of this metric is better (throughput), else 0.
	# elps emits B/s because the parser benchmarks call b.SetBytes(); the old
	# benchstat table labelled the same column "speed".
	function higher_is_better(m) {
		if (m ~ /\/s$/) return 1
		if (m == "speed") return 1
		return 0
	}

	# 1 for the near-deterministic allocation metrics, which get the tight
	# threshold.  Everything else -- timing (sec/op, ns/op, time/op), throughput
	# (B/s), and any unrecognised or custom b.ReportMetric column -- gets the
	# loose timing threshold.  Unrecognised falls to the LOOSE side on purpose:
	# a custom metric of unknown noise must not red PRs on arrival.
	function is_alloc_metric(m) {
		if (m ~ /^(B|MB|KB|bytes)\/op$/) return 1
		if (m ~ /^allocs?\/op$/) return 1
		return 0
	}

	function trim(s) {
		gsub(/^[ \t]+|[ \t]+$/, "", s)
		return s
	}

	# 1 for a metric that counts WHOLE THINGS, which today means allocs/op
	# alone.  B/op is an allocation metric too, but it is not a count: its
	# quantum is one byte out of thousands, so the quantisation rule below is a
	# no-op there and is not applied to it.
	function is_count_metric(m) {
		return (m ~ /^allocs?\/op$/)
	}

	# The numeric magnitude of a benchstat value cell, or -1 when it is not one
	# this gate can read.  benchstat SCALES large values and prints the scale as
	# a suffix -- "128.0k" is 128000 allocations, not 128 -- so the printed
	# token is not the number, and the quantisation rule below needs the number.
	# Unreadable is -1 rather than 0 so a cell this gate cannot parse can never
	# be mistaken for a row with no allocations, which would suppress it.
	function parse_magnitude(s,   mant, suf, mult) {
		if (s !~ /^[0-9]+(\.[0-9]+)?[A-Za-z]*$/) return -1
		if (!match(s, /^[0-9]+(\.[0-9]+)?/)) return -1
		mant = substr(s, RSTART, RLENGTH) + 0
		suf = substr(s, RSTART + RLENGTH)
		if (suf == "") return mant
		mult = 0
		if (suf == "k" || suf == "K") mult = 1000
		else if (suf == "M") mult = 1000000
		else if (suf == "G") mult = 1000000000
		else if (suf == "Ki") mult = 1024
		else if (suf == "Mi") mult = 1048576
		else if (suf == "Gi") mult = 1073741824
		if (mult == 0) return -1
		return mant * mult
	}

	# `go test` appends -<GOMAXPROCS> to every benchmark name (and omits it
	# entirely at GOMAXPROCS=1), so the suffix follows the RUNNER, not the code.
	# Waivers are written without it and rows are stripped down to match; that is
	# what keeps a waiver from silently unbinding when `runs-on` changes.
	function base_name(n) {
		sub(/-[0-9]+$/, "", n)
		return n
	}

	function waiver_err(lineno, msg) {
		wbad++
		printf "  WAIVER-BAD  %s:%d  %s\n", wsource, lineno, msg
	}

	# One or more tracking references, space separated: elps#412, #412,
	# luthersystems/elps#412, or a GitHub issue/PR URL. Anything else is not a
	# reference somebody can be sent to.
	function issues_ok(s,   i, n, parts, t, good) {
		n = split(s, parts, /[ \t,]+/)
		good = 0
		for (i = 1; i <= n; i++) {
			t = parts[i]
			if (t == "") continue
			if (t ~ /^[A-Za-z0-9._\/-]*#[0-9]+$/) { good++; continue }
			if (t ~ /^https:\/\/github\.com\/[A-Za-z0-9._-]+\/[A-Za-z0-9._-]+\/(issues|pull)\/[0-9]+$/) { good++; continue }
			return 0
		}
		# At least one reference, and nothing that is not one. Counting rather
		# than merely not-rejecting matters: a field of "," splits into two
		# empty tokens, every one of which passes a not-rejected test.
		return good > 0
	}

	# Index of the waiver covering this row, or 0. Exact on all three keys.
	function find_waiver(p, b, m,   i) {
		for (i = 1; i <= nw; i++) {
			if (wpkg[i] == p && wbench[i] == b && wmetric[i] == m) return i
		}
		return 0
	}

	# ---- waiver file -----------------------------------------------------
	FILENAME == wfile {
		wl = $0
		sub(/\r$/, "", wl)
		if (wl ~ /^[ \t]*(#|$)/) next

		nf = split(wl, wf, /\|/)
		if (nf != 7) {
			waiver_err(FNR, sprintf("expected 7 |-separated fields (pkg | benchmark | metric | ceiling | expires | issue | reason), found %d: %s", nf, trim(wl)))
			next
		}
		for (i = 1; i <= 7; i++) wf[i] = trim(wf[i])

		bad = 0
		if (wf[1] == "") { waiver_err(FNR, "empty pkg field; a waiver must name the package it covers"); bad = 1 }
		if (wf[2] == "") { waiver_err(FNR, "empty benchmark field; a waiver must name the benchmark it covers"); bad = 1 }
		else if (wf[2] ~ /-[0-9]+$/) {
			waiver_err(FNR, sprintf("benchmark %s carries a -<GOMAXPROCS> suffix; write it as %s so the waiver does not unbind when the runner changes", wf[2], base_name(wf[2])))
			bad = 1
		}
		if (wf[3] == "") { waiver_err(FNR, "empty metric field; a waiver covers one metric column, not the whole row"); bad = 1 }
		if (wf[4] !~ /^[0-9]+(\.[0-9]+)?$/ || wf[4] + 0 <= 0) {
			waiver_err(FNR, sprintf("ceiling %s is not a positive percentage; an unbounded waiver is a threshold increase in disguise", (wf[4] == "" ? "<empty>" : wf[4])))
			bad = 1
		}
		if (wf[5] !~ /^[0-9]{4}-[0-9]{2}-[0-9]{2}$/) {
			waiver_err(FNR, sprintf("expires %s is not a YYYY-MM-DD date; a waiver with no end date is never revisited", (wf[5] == "" ? "<empty>" : wf[5])))
			bad = 1
		}
		if (!issues_ok(wf[6])) {
			waiver_err(FNR, sprintf("issue %s is not a tracking reference (elps#412, #412, owner/repo#412 or a github.com issue/PR URL); a waiver nobody has to come back to is just a silent threshold increase", (wf[6] == "" ? "<empty>" : wf[6])))
			bad = 1
		}
		if (length(wf[7]) < 10) {
			waiver_err(FNR, "reason is missing or too short; say what the regression buys and what the alternative cost")
			bad = 1
		}
		if (bad) next

		nw++
		wpkg[nw] = wf[1]; wbench[nw] = wf[2]; wmetric[nw] = wf[3]
		wceil[nw] = wf[4] + 0; wceilstr[nw] = wf[4]
		wexp[nw] = wf[5]; wissue[nw] = wf[6]
		wline[nw] = FNR
		# ISO dates compare correctly as strings, so no date arithmetic and no
		# dependency on how the platform date(1) parses things.
		wexpired[nw] = (today > wf[5]) ? 1 : 0
		next
	}

	# ---- benchstat table -------------------------------------------------
	{
		line = $0

		if (line ~ /^[ \t]*$/) next

		# `#` comments. benchstat never emits one, but the fixtures in
		# scripts/testdata/ are annotated with the history of the run they
		# capture -- and those annotations QUOTE benchstat rows, deltas and
		# p-values included. Without this, the explanation a fixture carries
		# is adjudicated as data: the note above the table in
		# benchstat-libjson-encode-411.txt produced four phantom comparison
		# rows, one of them a "below-gate" verdict on a sentence. A fixture
		# must not be able to move the verdict by explaining itself.
		if (line ~ /^[ \t]*#/) next

		# Context headers.  elps benchmark output carries all four (the `cpu:`
		# line names the runner CPU, e.g. "Intel(R) Xeon(R) @ 2.80GHz").
		if (line ~ /^(goos|goarch|pkg|cpu):/) {
			if (line ~ /^pkg:/) {
				pkg = substr(line, 6)
				gsub(/^[ \t]+|[ \t]+$/, "", pkg)
				pkgseen[pkg] = 1
			}
			next
		}

		# Footnotes always START with a superscript marker ("¹ need >= 6
		# samples ...", "² all samples are equal").  Anchor on that rather than
		# on the substring: an old-format DATA row reads "~ (all equal)", and a
		# substring rule discarded it, so an all-equal comparison counted zero
		# rows and tripped the exit-2 "cannot interpret" path.
		if (line ~ /^[ \t]*(¹|²|³|⁴|⁵|⁶|⁷|⁸|⁹)/) next

		# Table header rows.  The new format draws them with box characters and
		# always carries the literal "vs base"; the old format starts with
		# "name" and ends with a "delta" column.  Remember the metric name
		# (sec/op, B/op, allocs/op, B/s, ...) -- it sets the direction in which
		# a change counts as a regression.
		if (index(line, "vs base") > 0 || line ~ /^name[ \t]/) {
			metric = "?"
			for (i = 2; i <= NF; i++) {
				if ($i == "vs" || $i == "delta") { metric = $(i - 1); break }
			}
			next
		}
		# Any other box-drawing line is a header continuation, not data.
		if (index(line, "│") > 0) next

		name = $1
		# The median of the base arm, as benchstat printed it.  Both table formats
		# put it in the first column after the name ("9.000", "128.0k"), and
		# the quantisation rule below needs its magnitude to know how many
		# WHOLE allocations a percentage delta stands for.
		basev = parse_magnitude($2)

		# Bound the delta search: stop before "(p=" so the p-value is never
		# scanned, and start after the last "±" so an old-format "± 2%" spread
		# column (and any "%" inside a benchmark name) cannot be mistaken for
		# the delta.
		region = line
		pidx = index(region, "(p=")
		if (pidx > 0) {
			# Take the whole token up to the next space or ")". Do NOT truncate
			# at the first non-digit: that turns "p=1.5e-05" into 1.5 and drops
			# a real regression as insignificant -- the one parse path that
			# would fail OPEN. benchstat formats %.3f today, so scientific
			# notation is unreachable, but it is handled rather than silently
			# mis-read. Anything that is not a number at all is a format
			# change: flag it and let the caller exit 2.
			pval_str = substr(region, pidx + 3)
			sub(/[ \t)].*$/, "", pval_str)
			if (pval_str ~ /^[0-9]*\.?[0-9]+([eE][+-]?[0-9]+)?$/) {
				pval = pval_str + 0
				has_p = 1
			} else {
				badp++
				printf "  BAD-PVALUE  unparseable p-value %s on row: %s\n", pval_str, name
				next
			}
			region = substr(region, 1, pidx - 1)
		} else {
			has_p = 0
			pval = 0
			pval_str = "n/a"
		}

		# Read the per-arm spreads BEFORE the region is truncated past them:
		# this is the measurement resolution of the row, and it is what the
		# resolution check below is judged against. -1 means benchstat printed
		# no interval it could compute.
		spread = max_spread(region)

		lastpm = last_spread(region)
		if (lastpm > 0) region = substr(region, lastpm + 1)

		# The waiver keyed to this row, if any. Looked up here -- before the
		# significance and threshold tests -- so that a row which is present but
		# NOT regressing still counts as "the waiver found its row". That is what
		# separates a waiver that is merely no longer needed (delete it) from one
		# pointing at a benchmark that no longer exists (it is protecting
		# nothing, while looking like it is).
		wi = find_waiver(pkg, base_name(name), metric)

		delta_tok = last_signed_pct(region)
		if (delta_tok == "") {
			# A "~" row IS a successfully interpreted comparison -- it just
			# found no significant difference. Tally it separately so a table
			# in which nothing moved is not mistaken for "could not parse
			# anything" and turned into a spurious exit 2.
			if (index(line, "~") > 0) {
				tilde++
				if (wi) wseen[wi] = 1
			}
			next
		}

		compared++
		if (wi) wseen[wi] = 1

		numstr = substr(delta_tok, 1, length(delta_tok) - 1)
		sub(/^\+/, "", numstr)
		delta = numstr + 0

		# Fold metric direction in: `regr` is how much WORSE this row got, so a
		# positive `regr` always means "worse" regardless of metric.
		if (higher_is_better(metric)) {
			regr = -delta
			dir = "(higher is better)"
		} else {
			regr = delta
			dir = ""
		}

		gate = is_alloc_metric(metric) ? alloc_threshold : threshold

		if (!has_p) {
			# geomean and other summary rows carry no significance information.
			printf "  info        %-46s %-9s %-40s delta=%s (no p-value; informational) %s\n",
				pkg, metric, name, delta_tok, dir
			next
		}

		if (pval > alpha) next       # not statistically significant
		if (regr <= 0) next          # improvement or no change

		significant++
		if (regr < gate) {
			printf "  below-gate  %-46s %-9s %-40s delta=%s p=%s (gate %s%%) %s\n",
				pkg, metric, name, delta_tok, pval_str, gate, dir
			next
		}

		# RESOLUTION. A timing row is only adjudicated when the move is larger
		# than what that row can measure. See the "resolution check" note in the
		# header: the threshold is one number for a whole metric class, and on a
		# sub-100ns RunParallel body it sits BELOW the row own null distribution,
		# so p<=alpha over the threshold happens there by chance (#443).
		#
		# Deliberately not applied to the allocation metrics, and deliberately
		# skipped when benchstat could not compute an interval -- in that case
		# the regression line says so, so a check that did not run is never
		# mistaken for one that ran and passed.
		if (!is_alloc_metric(metric) && spread >= 0 && regr <= spread) {
			unresolved++
			printf "  NOISE-FLOOR %-46s %-9s %-40s delta=%s p=%s (gate %s%%) spread ±%s%% -- the OWN measured spread of this row on these samples is at or above this move, so the comparison cannot resolve it; not a regression, and not suppressed either: make the benchmark quieter (longer -benchtime, or keep it out of the comparison set) if this row needs to be gateable %s\n",
				pkg, metric, name, delta_tok, pval_str, gate, spread, dir
			next
		}
		# QUANTISATION.  `go test` reports allocs/op as
		# int64(memstats.Mallocs)/int64(b.N) -- INTEGER DIVISION of a quantity
		# that is not an integer.  A row whose true cost is 9.99 allocations
		# per operation prints 9 on one sample and 10 on the next, from GC
		# cadence alone, and benchstat reads that as an 11.11% move.  See the
		# quantisation-check note in the header for the measurement.
		#
		# So a move of ONE COUNT on an integer-count metric is only adjudicated
		# when the row is reproducing that count exactly.  When either arm
		# disagrees with itself -- a nonzero spread on a metric that is
		# supposed to be exact -- one count is the smallest thing it can say
		# and the change is indistinguishable from the reported integer landing
		# on the other side of the boundary.
		#
		# The bound is deliberately the tightest one that fixes the class: it
		# can only ever discard a ONE-COUNT move, and one count clears the 5%
		# allocation gate only on a row under 20 allocs/op.  Above that a
		# single-count move is already below the gate and this rule changes
		# nothing.
		if (is_count_metric(metric) && basev > 0 && spread > 0 &&
		    basev * regr / 100.0 < 1.5) {
			quantised++
			printf "  QUANTISED   %-46s %-9s %-40s delta=%s p=%s (gate %s%%) base %s allocs/op, so this is a ONE-ALLOCATION step -- and the row does not reproduce its own count (spread ±%s%% on a metric that should be exact), so a one-step move cannot be told from `go test` truncating a fractional allocs/op to the other integer; not a regression, and not suppressed either: pin the count (drop the per-op allocation below the GC-cadence noise, or cut b.N variance) if this row needs to be gateable at one allocation %s\n",
				pkg, metric, name, delta_tok, pval_str, gate, basev, spread, dir
			next
		}
		noquant = ""
		if (is_count_metric(metric) && regr > 0) {
			if (basev <= 0) {
				noquant = " [base cell unreadable, so the quantisation check did not run]"
			} else if (spread < 0) {
				noquant = " [no interval: benchstat needs >= 6 samples, so the quantisation check did not run]"
			}
		}
		nospread = (!is_alloc_metric(metric) && spread < 0) ? " [no interval: benchstat needs >= 6 samples, so the resolution check did not run]" : ""

		# At or above the gate. A waiver can turn this into a PASS, but only
		# a live one, and only while the move stays inside the ceiling it
		# recorded. Every outcome below is printed either way: the waiver
		# changes the verdict, never the visibility.
		if (wi && wexpired[wi]) {
			regressions++
			wexpiredhit[wi] = 1
			printf "  REGRESSION  %-46s %-9s %-40s delta=%s p=%s (gate %s%%) WAIVER EXPIRED %s (%s), no longer suppressing %s%s\n",
				pkg, metric, name, delta_tok, pval_str, gate, wexp[wi], wissue[wi], dir, nospread noquant
			next
		}
		if (wi && regr > wceil[wi]) {
			regressions++
			wexceeded[wi] = 1
			printf "  REGRESSION  %-46s %-9s %-40s delta=%s p=%s (gate %s%%) EXCEEDS its waiver ceiling %s%% (%s) %s%s\n",
				pkg, metric, name, delta_tok, pval_str, gate, wceilstr[wi], wissue[wi], dir, nospread noquant
			next
		}
		if (wi) {
			waived++
			wused[wi] = 1
			printf "  WAIVED      %-46s %-9s %-40s delta=%s p=%s (gate %s%%) accepted: ceiling %s%%, expires %s, %s %s\n",
				pkg, metric, name, delta_tok, pval_str, gate, wceilstr[wi], wexp[wi], wissue[wi], dir
			next
		}

		regressions++
		printf "  REGRESSION  %-46s %-9s %-40s delta=%s p=%s (gate %s%%) %s%s\n",
			pkg, metric, name, delta_tok, pval_str, gate, dir, nospread noquant
	}

	END {
		# A waiver that matched no row in this comparison, or matched a row it
		# did not need to suppress, is reported EVERY run. A stale waiver that
		# rots quietly is how a per-row exception turns back into a blanket one.
		for (i = 1; i <= nw; i++) {
			if (!(wpkg[i] in pkgseen)) {
				# The comparison did not cover this package AT ALL, so there is
				# nothing to say about the waiver -- it was not exercised, and
				# calling that "stale" would flood every partial comparison
				# (every fixture in scripts/testdata/, for one) with warnings
				# about waivers that are perfectly healthy. Counted, not
				# printed, so a package that has genuinely disappeared still
				# shows up as a nonzero number rather than as silence.
				woutscope++
			} else if (!(i in wseen)) {
				wstale++
				printf "  WAIVER-STALE  %s:%d waives %s / %s / %s -- that package IS in this comparison and that row is not, so the benchmark was renamed or removed and the waiver is protecting nothing. %s\n",
					wsource, wline[i], wpkg[i], wbench[i], wmetric[i], wissue[i]
			} else if (!(i in wused) && !(i in wexceeded) && !(i in wexpiredhit)) {
				wunused++
				printf "  waiver-unused %s:%d waives %s / %s / %s, and that row is not regressing above its gate -- the waiver can be deleted. %s\n",
					wsource, wline[i], wpkg[i], wbench[i], wmetric[i], wissue[i]
			}
			if (wexpired[i] && (wpkg[i] in pkgseen)) {
				wexp_n++
				printf "  WAIVER-EXPIRED %s:%d %s / %s / %s expired on %s and no longer suppresses anything. %s\n",
					wsource, wline[i], wpkg[i], wbench[i], wmetric[i], wexp[i], wissue[i]
			}
		}
		printf "VERDICT %d %d %d %d %d %d %d %d %d %d %d %d %d %d\n",
			regressions + 0, significant + 0, compared + 0, tilde + 0, badp + 0,
			waived + 0, wbad + 0, wstale + 0, wunused + 0, wexp_n + 0, nw + 0,
			woutscope + 0, unresolved + 0, quantised + 0
	}
' "$waiver_input" "$input" >"$report"

verdict_line=$(tail -n 1 "$report")
sed '$d' "$report"

read -r _ n_regressions n_significant n_compared n_tilde n_badp \
	n_waived n_waiver_bad n_waiver_stale n_waiver_unused n_waiver_expired n_waivers \
	n_waiver_outscope n_unresolved n_quantised <<<"$verdict_line"

if [ "$n_waiver_bad" -gt 0 ]; then
	cat >&2 <<-EOF
		benchstat-gate: ${n_waiver_bad} malformed entr(y/ies) in ${WAIVERS} -- see the
		WAIVER-BAD line(s) above.

		Refusing to report a verdict rather than skipping the bad entries: a waiver
		list that cannot be read must never be treated as an empty one, and a waiver
		that silently does not parse is a regression nobody is told about. Fix the
		entry, or delete it. The format is documented at the top of that file.
	EOF
	exit 2
fi

if [ "$n_badp" -gt 0 ]; then
	echo "benchstat-gate: ${n_badp} row(s) carried a p-value this gate cannot read -- refusing to report a verdict." >&2
	exit 2
fi

if [ "$((n_compared + n_tilde))" -eq 0 ]; then
	cat >&2 <<-EOF
		benchstat-gate: found NO comparison rows in $input (no delta rows and no
		"~" no-change rows).

		Either benchstat failed, or its output format changed and this gate can no
		longer read it.  Failing rather than reporting "no regression" -- a gate
		that cannot parse its input must never report success.  Run
		scripts/ci-gates-test.sh and refresh the fixtures in scripts/testdata/ if
		benchstat's table format has genuinely changed.
	EOF
	exit 2
fi

echo "benchstat-gate: interpreted ${n_compared} delta row(s) + ${n_tilde} no-change row(s); ${n_significant} significant move(s) in the bad direction; ${n_regressions} at or above the gate (timing ${THRESHOLD}%, allocation ${ALLOC_THRESHOLD}%)."

# Printed whenever it is nonzero, and never folded into the regression count. A
# NOISE-FLOOR row is a benchmark this comparison cannot adjudicate at all, which
# is a standing problem with the benchmark rather than a clean result -- see the
# resolution-check note in the header. Silence here would turn "we could not
# measure it" into "it was fine", which is the shape of defect this whole gate
# exists to fix.
if [ "$n_unresolved" -gt 0 ]; then
	echo "benchstat-gate: ${n_unresolved} timing row(s) moved past the gate but by LESS than their own measured spread, so this comparison cannot resolve them (reported as NOISE-FLOOR above, excluded from the verdict). They are not gateable as sampled; make them quieter or keep them out of the comparison set."
fi

# Same doctrine as NOISE-FLOOR above, for the integer-count metrics: printed
# whenever it is nonzero, never folded into the regression count. A QUANTISED
# row is a benchmark whose allocs/op does not reproduce, which is a standing
# problem with the benchmark rather than a clean result.
if [ "$n_quantised" -gt 0 ]; then
	echo "benchstat-gate: ${n_quantised} allocation-count row(s) moved past the gate by exactly ONE allocation on a row that does not reproduce its own count, so the move cannot be told from \`go test\` truncating a fractional allocs/op (reported as QUANTISED above, excluded from the verdict). They are not gateable at one allocation as sampled."
fi

# Printed on EVERY run, including clean ones. A waiver is a standing decision,
# and a standing decision that stops being visible stops being reviewed.
if [ "$n_waivers" -gt 0 ] || [ "$n_waived" -gt 0 ]; then
	echo "benchstat-gate: ${n_waivers} reviewed waiver(s) loaded from ${WAIVERS}; ${n_waived} row(s) WAIVED (measured, reported, and excluded from the verdict), ${n_waiver_stale} stale, ${n_waiver_unused} currently unused, ${n_waiver_expired} expired, ${n_waiver_outscope} for a package this comparison does not cover."
fi

if [ "$n_regressions" -gt 0 ]; then
	exit 1
fi

exit 0
