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
#     ceil(targets / shards) x FUZZTIME + overhead  <=  timeout-minutes
#
# and CI fails when it stops holding. `ceil(targets/shards)` is the largest
# shard, which is what sets wall clock for a matrix -- the shards run in
# parallel, so the total is irrelevant.
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
#   0  the budget fits
#   1  it does not -- raise timeout-minutes, add shards, or lower FUZZTIME
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
if ! targets="$("${SCRIPT_DIR}/fuzz.sh" --list 2>/dev/null)"; then
	echo "fuzz-budget-check: scripts/fuzz.sh --list failed" >&2
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

echo "fuzz-budget-check:"
echo "  targets discovered      ${n_targets}"
echo "  shards                  ${shards}"
echo "  largest shard           ${per_shard} target(s)"
echo "  scheduled FUZZTIME      ${sched_fuzztime} (${per_target_minutes} min/target)"
echo "  fixed overhead          ${OVERHEAD_MINUTES} min"
echo "  required                ${needed} min"
echo "  timeout-minutes         ${timeout_minutes} min"

if [ "$needed" -gt "$timeout_minutes" ]; then
	cat >&2 <<-EOF

		fuzz-budget-check: the nightly sweep does NOT fit in its timeout.

		  ${per_shard} x ${per_target_minutes} min + ${OVERHEAD_MINUTES} min = ${needed} min > ${timeout_minutes} min

		Left alone, the job is cancelled partway through and the last targets in
		the largest shard never run -- silently, because a truncated sweep looks
		exactly like a finished one.

		Fix by one of:
		  * raise timeout-minutes on the fuzz job to at least ${needed}
		  * add shards to strategy.matrix.shard (parallel, so this cuts wall clock)
		  * lower the scheduled FUZZTIME
	EOF
	exit 1
fi

headroom=$((timeout_minutes - needed))
echo "  headroom                ${headroom} min"
echo "fuzz-budget-check: the sweep fits."
exit 0
