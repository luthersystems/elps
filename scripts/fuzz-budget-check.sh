#!/usr/bin/env bash
#
# Derived budget gate for the fuzz sweep: prove the job's timeout-minutes can
# actually contain the work the matrix is about to schedule.
#
# Why this exists
# ---------------
# The fuzz job's `timeout-minutes` was a hand-maintained number. Target
# discovery is dynamic, so adding a fuzz target silently added another FUZZTIME
# to the nightly sweep, and nobody recomputed the backstop. The failure mode is
# the bad kind: the job hits its timeout partway through, the LAST targets in
# the sweep never execute at all, and the run is simply... cancelled. Nothing
# says "coverage was dropped". A truncated sweep and a finished one look alike
# in the check list.
#
# It has already gone stale twice in this repo -- 120 sized for 10 targets when
# there were 12, and 140 vs 165 when two branches each counted only their own
# additions.
#
# So the number is derived here instead of remembered:
#
#     ceil(targets / shards) x FUZZTIME + overhead + FUZZTIME  <=  timeout-minutes
#     \___________________ needed ____________________/  \_ margin _/
#
# and CI fails when it stops holding. `ceil(targets/shards)` is the largest
# shard, which is what sets wall clock for a matrix -- the shards run in
# parallel, so the total is irrelevant.
#
# Why the trailing margin (issue #458)
# ------------------------------------
# This condition used to be `needed > timeout` with no margin, so a sweep sized
# at EXACTLY timeout-minutes printed "the sweep fits" and exited 0. Zero
# headroom is not a fit. Two things make it a truncated sweep:
#
#   * The estimate is not the observation. OVERHEAD_MINUTES is a fixed guess
#     and per-target cost is not constant -- observed shard durations already
#     vary from 4m25s to 5m42s. A budget that is exactly consumed on paper is
#     over-run in practice by whichever shard runs slow.
#   * A truncated sweep is silent. The job is cancelled partway through, the
#     last targets in the largest shard never execute, and the run looks the
#     same as a finished one. That is the precise failure this gate exists to
#     prevent, so passing at zero headroom made the gate unable to report it.
#
# The margin is DERIVED, not chosen. `needed` is
# `ceil(targets/shards) x FUZZTIME + overhead`; the only term that moves as the
# repo grows is `ceil(targets/shards)`, an integer. Adding targets therefore
# steps `needed` up in units of exactly one FUZZTIME -- never a fraction of
# one. So:
#
#   * a margin SMALLER than one FUZZTIME cannot survive the next step: the very
#     next target that pushes the largest shard up by one lands straight past
#     the timeout, which is the stale-matrix accident this gate is for;
#   * one FUZZTIME is thus the smallest margin that is still standing after the
#     next target lands, which is the whole job of a backstop.
#
# It is not a round number picked to feel safe -- it is the quantum this
# arithmetic moves in. Concretely, at the 30 targets / 8 shards / 60 min that
# main carries today: needed = 4 x 10 + 10 = 50, margin = 10, and 60 <= 60
# holds with nothing to spare. That is deliberate. One FUZZTIME is at once the
# minimum the derivation allows and the maximum the current matrix can pay, so
# the gate is as tight as it can be without being wrong in either direction.
#
# Everything is read from the sources of truth rather than passed in: targets
# from `scripts/fuzz.sh --list` (the same discovery the sweep uses), and the
# shard count, FUZZTIME and timeout-minutes from .github/workflows/fuzz.yml
# itself. A gate fed its numbers by hand would go stale exactly like the value
# it is guarding.
#
# Usage:  scripts/fuzz-budget-check.sh
#
# Exit codes:
#   0  the budget fits, with at least one FUZZTIME of margin
#   1  it does not -- add shards, lower FUZZTIME, or (last resort) raise
#      timeout-minutes.  Exiting 1 at zero headroom is intentional: see the
#      margin note above.
#   2  the inputs could not be read (missing workflow, no targets discovered,
#      unparsable duration).  2 rather than 0 on purpose: a budget gate that
#      cannot read its inputs must not report "fits".

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
WORKFLOW="${FUZZ_WORKFLOW:-${REPO_ROOT}/.github/workflows/fuzz.yml}"

# Wall-clock cost of a run that is not fuzzing: checkout, Go setup, module
# download, build, corpus cache restore/save, and the gate self-test. Measured
# at roughly 3 minutes; 10 is deliberately generous, since the consequence of
# under-estimating it is a silently truncated sweep and the consequence of
# over-estimating is a slightly larger backstop.
OVERHEAD_MINUTES="${FUZZ_OVERHEAD_MINUTES:-10}"

if [ ! -f "$WORKFLOW" ]; then
	echo "fuzz-budget-check: no such workflow: $WORKFLOW" >&2
	exit 2
fi

# Read the scheduled (worst-case) FUZZTIME, the shard count and the job's
# timeout-minutes out of the workflow.
read -r sched_fuzztime shards timeout_minutes < <(python3 - "$WORKFLOW" <<'PY'
import re
import sys

path = sys.argv[1]
text = open(path).read()

try:
    import yaml
    doc = yaml.safe_load(text)
except Exception:  # noqa: BLE001 -- fall back to regex below
    doc = None

shards = 1
timeout = 0
if isinstance(doc, dict):
    job = (doc.get("jobs") or {}).get("fuzz") or {}
    timeout = int(job.get("timeout-minutes") or 0)
    matrix = ((job.get("strategy") or {}).get("matrix") or {})
    entries = matrix.get("shard")
    if isinstance(entries, list) and entries:
        shards = len(entries)

# The scheduled budget lives inside a `${{ ... }}` expression, which no YAML
# parser will evaluate. Pull the branch guarded by `schedule` textually: that
# is the worst case, and the worst case is the one a backstop must cover.
m = re.search(r"schedule'\s*&&\s*'([0-9]+[smh])'", text)
sched = m.group(1) if m else ""

print(sched, shards, timeout)
PY
)

if [ -z "${sched_fuzztime:-}" ]; then
	echo "fuzz-budget-check: could not find the scheduled FUZZTIME in $WORKFLOW" >&2
	echo "  expected an expression like: github.event_name == 'schedule' && '10m'" >&2
	exit 2
fi
if [ "${timeout_minutes:-0}" -le 0 ]; then
	echo "fuzz-budget-check: the fuzz job has no timeout-minutes in $WORKFLOW" >&2
	echo "  a sweep with no outer bound is the thing this gate exists to prevent" >&2
	exit 2
fi

to_minutes() {
	local d="$1" n="${1%[smh]}" unit="${1##*[0-9]}"
	case "$unit" in
	s) echo $(((n + 59) / 60)) ;;
	m) echo "$n" ;;
	h) echo $((n * 60)) ;;
	*)
		echo "fuzz-budget-check: cannot parse duration '$d'" >&2
		return 1
		;;
	esac
}

per_target_minutes="$(to_minutes "$sched_fuzztime")" || exit 2

# Discovery, from the same code path the sweep uses.
#
# stderr is CAPTURED AND REPLAYED, not discarded (issue #479). fuzz.sh reports
# a package that failed to build on stderr and exits non-zero; with `2>/dev/null`
# this printed the bare line "scripts/fuzz.sh --list failed" and threw away the
# build errors that say WHICH package and WHY -- turning an actionable compile
# error into a dead end, and re-hiding on this side exactly what fuzz.sh was
# fixed to make loud.
discovery_err="$(mktemp)"
trap 'rm -f "$discovery_err"' EXIT
if ! targets="$("${SCRIPT_DIR}/fuzz.sh" --list 2>"$discovery_err")"; then
	echo "fuzz-budget-check: scripts/fuzz.sh --list failed" >&2
	sed 's/^/  | /' "$discovery_err" >&2
	echo "fuzz-budget-check: no trustworthy target count, so no budget can be" >&2
	echo "fuzz-budget-check: derived -- refusing to report that the sweep fits." >&2
	exit 2
fi
n_targets="$(printf '%s\n' "$targets" | grep -c . || true)"
if [ "${n_targets:-0}" -eq 0 ]; then
	echo "fuzz-budget-check: discovered NO fuzz targets -- nothing to budget for" >&2
	exit 2
fi

if [ "$shards" -gt "$n_targets" ]; then
	echo "fuzz-budget-check: ${shards} shards for ${n_targets} targets -- some shards would draw nothing" >&2
	exit 1
fi

# Largest shard, not the total: shards run in parallel, so wall clock is set by
# whichever one draws the most targets. Round-robin assignment makes that
# ceil(targets / shards).
per_shard=$(((n_targets + shards - 1) / shards))
needed=$((per_shard * per_target_minutes + OVERHEAD_MINUTES))

# One FUZZTIME, because that is the step `needed` moves in when a target is
# added (see the margin note in the header). Anything less is a margin that the
# next target walks straight through.
margin=$per_target_minutes
required_timeout=$((needed + margin))

# The smallest shard count that would satisfy the constraint, so the failure
# message can name the actual remedy instead of leaving the reader to solve for
# it -- the preferred fix is adding shards, and "add shards" without a number
# invites picking one at random. Bounded above by n_targets: more shards than
# targets draws empty shards, which fuzz.sh rejects outright.
next_better_shards() {
	local s ps
	for ((s = shards + 1; s <= n_targets; s++)); do
		ps=$(((n_targets + s - 1) / s))
		if [ $((ps * per_target_minutes + OVERHEAD_MINUTES + margin)) -le "$timeout_minutes" ]; then
			echo "$s"
			return 0
		fi
	done
	# Sharding alone cannot fix it: even one target per shard costs
	# FUZZTIME + overhead + margin. Say so rather than naming a bogus count.
	echo "no shard count (not even ${n_targets}, one target each)"
}

echo "fuzz-budget-check:"
echo "  targets discovered      ${n_targets}"
echo "  shards                  ${shards}"
echo "  largest shard           ${per_shard} target(s)"
echo "  scheduled FUZZTIME      ${sched_fuzztime} (${per_target_minutes} min/target)"
echo "  fixed overhead          ${OVERHEAD_MINUTES} min"
echo "  required                ${needed} min"
echo "  required margin         ${margin} min (one FUZZTIME -- the next target's cost)"
echo "  required + margin       ${required_timeout} min"
echo "  timeout-minutes         ${timeout_minutes} min"

if [ "$required_timeout" -gt "$timeout_minutes" ]; then
	# Two distinct states end up here and they read very differently to a
	# human, so say which one this is. "Over by 10" and "exactly on the line"
	# both have to fail, but only the first is obviously broken on sight --
	# the second is the one that used to pass, and a reader who is told only
	# "does not fit" while the arithmetic plainly says 60 <= 60 will assume
	# the gate is wrong rather than that the headroom is gone.
	if [ "$needed" -gt "$timeout_minutes" ]; then
		cat >&2 <<-EOF

			fuzz-budget-check: the nightly sweep does NOT fit in its timeout.

			  ${per_shard} x ${per_target_minutes} min + ${OVERHEAD_MINUTES} min = ${needed} min > ${timeout_minutes} min
		EOF
	else
		cat >&2 <<-EOF

			fuzz-budget-check: the nightly sweep fits with NO usable margin.

			  ${per_shard} x ${per_target_minutes} min + ${OVERHEAD_MINUTES} min = ${needed} min
			  vs timeout-minutes ${timeout_minutes} min -- headroom $((timeout_minutes - needed)) min,
			  which is less than the ${margin} min (one FUZZTIME) a sweep must keep spare.

			This is not a rounding complaint. \`needed\` grows in steps of exactly
			one FUZZTIME, so the next fuzz target to land pushes this over the
			timeout outright -- and observed shard durations already vary from
			4m25s to 5m42s, so a budget that is exactly consumed on paper is
			over-run in practice by whichever shard runs slow.
		EOF
	fi
	cat >&2 <<-EOF

		Left alone, the job is cancelled partway through and the last targets in
		the largest shard never run -- silently, because a truncated sweep looks
		exactly like a finished one.

		Fix by one of, in order of preference:
		  * add shards to strategy.matrix.shard -- they are parallel, so this
		    cuts wall clock rather than extending the backstop. ${shards} shards
		    currently; $(next_better_shards) would bring this back under.
		  * lower the scheduled FUZZTIME
		  * raise timeout-minutes on the fuzz job to at least ${required_timeout}
		    (${needed} needed + ${margin} margin). LAST RESORT: raising the
		    backstop to make a red gate green is the same move as raising a
		    benchmark threshold -- it silences the gate instead of the problem.
	EOF
	exit 1
fi

headroom=$((timeout_minutes - needed))
echo "  headroom                ${headroom} min"

# Report how many more targets this matrix absorbs before the gate goes red, so
# the number is in front of whoever is adding one. `needed` steps by one
# FUZZTIME each time ceil(targets/shards) increments, i.e. every `shards`
# targets; spare_steps is how many such steps still fit inside the headroom
# after the mandatory margin is set aside.
spare_steps=$(((headroom - margin) / per_target_minutes))
slack_targets=$((per_shard * shards - n_targets + spare_steps * shards))
echo "  room for                ${slack_targets} more target(s) before this gate goes red"
echo "fuzz-budget-check: the sweep fits, with ${headroom} min headroom (>= ${margin} min margin)."
exit 0
