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
#      crashed).  Exiting 2 rather than 0 is deliberate: an uninterpretable
#      comparison must fail loudly instead of reporting "no regression", which
#      is exactly how the old gate stayed green for 473 runs.

set -euo pipefail

ALPHA="${BENCH_ALPHA:-0.05}"
THRESHOLD="${BENCH_REGRESSION_THRESHOLD_PCT:-15}"
ALLOC_THRESHOLD="${BENCH_ALLOC_THRESHOLD_PCT:-5}"

if [ "$#" -ne 1 ]; then
	echo "usage: $0 <benchstat-output-file>" >&2
	echo "  env: BENCH_REGRESSION_THRESHOLD_PCT (default 15)  timing metrics: sec/op, B/s" >&2
	echo "       BENCH_ALLOC_THRESHOLD_PCT      (default 5)   allocation metrics: B/op, allocs/op" >&2
	echo "       BENCH_ALPHA                    (default 0.05)" >&2
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

report=$(mktemp)
trap 'rm -f "$report"' EXIT

# The awk program emits one human-readable line per interesting row, then a
# final machine-readable line:
#   "VERDICT <regressions> <significant> <compared> <tilde> <badp>".
awk -v alpha="$ALPHA" -v threshold="$THRESHOLD" -v alloc_threshold="$ALLOC_THRESHOLD" '
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

	{
		line = $0

		if (line ~ /^[ \t]*$/) next

		# Context headers.  elps benchmark output carries all four (the `cpu:`
		# line names the runner CPU, e.g. "Intel(R) Xeon(R) @ 2.80GHz").
		if (line ~ /^(goos|goarch|pkg|cpu):/) {
			if (line ~ /^pkg:/) {
				pkg = substr(line, 6)
				gsub(/^[ \t]+|[ \t]+$/, "", pkg)
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

		lastpm = last_spread(region)
		if (lastpm > 0) region = substr(region, lastpm + 1)

		delta_tok = last_signed_pct(region)
		if (delta_tok == "") {
			# A "~" row IS a successfully interpreted comparison -- it just
			# found no significant difference. Tally it separately so a table
			# in which nothing moved is not mistaken for "could not parse
			# anything" and turned into a spurious exit 2.
			if (index(line, "~") > 0) tilde++
			next
		}

		compared++

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
		if (regr >= gate) {
			regressions++
			printf "  REGRESSION  %-46s %-9s %-40s delta=%s p=%s (gate %s%%) %s\n",
				pkg, metric, name, delta_tok, pval_str, gate, dir
		} else {
			printf "  below-gate  %-46s %-9s %-40s delta=%s p=%s (gate %s%%) %s\n",
				pkg, metric, name, delta_tok, pval_str, gate, dir
		}
	}

	END {
		printf "VERDICT %d %d %d %d %d\n",
			regressions + 0, significant + 0, compared + 0, tilde + 0, badp + 0
	}
' "$input" >"$report"

verdict_line=$(tail -n 1 "$report")
sed '$d' "$report"

read -r _ n_regressions n_significant n_compared n_tilde n_badp <<<"$verdict_line"

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

if [ "$n_regressions" -gt 0 ]; then
	exit 1
fi

exit 0
