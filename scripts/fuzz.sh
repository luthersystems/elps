#!/usr/bin/env bash
#
# Run every native go fuzz target in the repository for a BOUNDED time.
#
# `go test -fuzz` has no default limit: left alone it runs until it finds a
# crash or something kills it.  -fuzztime is therefore the only thing standing
# between this and a CI job that never finishes, so this script refuses to run
# without a parsable bound and additionally caps each `go test` invocation with
# a hard -timeout computed from that bound.  Nothing here may grow an unbounded
# code path.
#
# Discovery is dynamic -- targets are found with `go test -list`, not listed in
# a table here or in the workflow -- so a fuzz target added in a new package is
# covered the day it lands rather than the day someone remembers to register it.
#
# Usage:
#   scripts/fuzz.sh [package...]      # default: ./...
#   FUZZTIME=5m scripts/fuzz.sh
#   scripts/fuzz.sh --list            # print discovered targets, run nothing
#   scripts/fuzz.sh --shard 2/4       # run only this shard's share of them
#
# Environment:
#   FUZZTIME           per-target fuzzing budget (default 30s).  Accepts the
#                      go duration forms this script can bound: <n>s, <n>m,
#                      <n>h.
#   FUZZ_TIMEOUT_SLACK seconds added to FUZZTIME for the go test -timeout
#                      backstop (default 300).  Covers build time and the
#                      minimisation pass that follows a crash.
#   FUZZMINIMIZETIME   budget for minimising a crasher (default 30s).  Only
#                      spent when a target actually fails.
#   FUZZ_SHARD         "i/n" -- run only shard i of n (1-based).  Equivalent
#                      to --shard; the env form is what the CI matrix uses.
#
# Sharding
# --------
# The nightly sweep is serial and costs (targets x FUZZTIME), which grows every
# time a target is added -- the exact quantity that made the job's
# timeout-minutes a hand-maintained number that goes stale silently.  Splitting
# the targets across parallel shards makes wall-clock a function of the LARGEST
# shard instead of the total, and scripts/fuzz-budget-check.sh derives the
# timeout from that rather than from anyone's memory.
#
# Assignment is round-robin over the discovered list sorted by (package,
# target), so it is deterministic: the same target lands in the same shard on
# every run, which is what makes a shard's failure reproducible with a single
# --shard argument.  Round-robin rather than contiguous blocks because target
# COUNT is the only thing balanced here -- targets are not equal-cost, but the
# per-target budget is fixed, so count is the right proxy.
#
# Exit status: 0 if every target survived its budget, 1 if any target failed.
# A failing target writes its crasher to <pkg>/testdata/fuzz/<Target>/ , which
# the CI job uploads as an artifact and a developer commits as a regression
# case.

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

# ${FUZZTIME-30s}, not ${FUZZTIME:-30s}: an UNSET budget takes the default, but
# one that is set and EMPTY (FUZZTIME="$SOMETHING_UNSET") is a mistake and must
# be rejected below rather than silently collapsing a 10m nightly into 30s.
FUZZTIME="${FUZZTIME-30s}"
FUZZ_TIMEOUT_SLACK="${FUZZ_TIMEOUT_SLACK:-300}"
FUZZMINIMIZETIME="${FUZZMINIMIZETIME:-30s}"

list_only=0
shard_spec="${FUZZ_SHARD-}"
packages=()
want_shard=0
for arg in "$@"; do
	if [ "$want_shard" -eq 1 ]; then
		shard_spec="$arg"
		want_shard=0
		continue
	fi
	case "$arg" in
	--list) list_only=1 ;;
	--shard) want_shard=1 ;;
	--shard=*) shard_spec="${arg#--shard=}" ;;
	-*)
		echo "fuzz.sh: unknown flag $arg" >&2
		exit 2
		;;
	*) packages+=("$arg") ;;
	esac
done
if [ "$want_shard" -eq 1 ]; then
	echo "fuzz.sh: --shard requires an argument of the form i/n" >&2
	exit 2
fi

# Shard spec parsing is strict for the same reason FUZZTIME parsing is: a
# mis-read shard silently runs a SUBSET of the targets and still exits 0, which
# is indistinguishable from a clean full sweep.
shard_index=1
shard_total=1
if [ -n "$shard_spec" ]; then
	if ! [[ "$shard_spec" =~ ^[0-9]+/[0-9]+$ ]]; then
		echo "fuzz.sh: shard must be of the form i/n (got '${shard_spec}')" >&2
		exit 2
	fi
	shard_index="${shard_spec%%/*}"
	shard_total="${shard_spec##*/}"
	if [ "$shard_total" -lt 1 ] || [ "$shard_index" -lt 1 ] || [ "$shard_index" -gt "$shard_total" ]; then
		echo "fuzz.sh: shard ${shard_spec} is out of range (need 1 <= i <= n, n >= 1)" >&2
		exit 2
	fi
fi
if [ "${#packages[@]}" -eq 0 ]; then
	packages=("./...")
fi

# duration_seconds converts a go duration of the forms <n>s / <n>m / <n>h into
# whole seconds.  Anything else is rejected rather than guessed: silently
# mis-reading the budget is how a "bounded" job becomes an unbounded one.
duration_seconds() {
	local d="$1" n unit
	n="${d%[smh]}"
	unit="${d##*[0-9]}"
	case "$unit" in
	s) echo "$n" ;;
	m) echo $((n * 60)) ;;
	h) echo $((n * 3600)) ;;
	*) return 1 ;;
	esac
}

if ! [[ "$FUZZTIME" =~ ^[0-9]+[smh]$ ]]; then
	echo "fuzz.sh: FUZZTIME must be <n>s, <n>m or <n>h (got '${FUZZTIME}')" >&2
	echo "fuzz.sh: refusing to run unbounded" >&2
	exit 2
fi
fuzz_seconds="$(duration_seconds "$FUZZTIME")"
hard_timeout=$((fuzz_seconds + FUZZ_TIMEOUT_SLACK))

cd "$REPO_ROOT" || exit 2

# list_targets prints the Fuzz* targets defined in one package.  `go test
# -list` prints the matching names followed by a summary line ("ok <pkg> ..."),
# so the output is filtered to bare target names.
list_targets() {
	go test -list '^Fuzz' "$1" 2>/dev/null | grep -E '^Fuzz[A-Za-z0-9_]*$'
}

mapfile -t pkg_list < <(go list "${packages[@]}" 2>/dev/null)
if [ "${#pkg_list[@]}" -eq 0 ]; then
	echo "fuzz.sh: no packages matched ${packages[*]}" >&2
	exit 2
fi

# Discover EVERYTHING first, then select this shard's slice. Discovery has to
# see the whole list for shard membership to be stable -- assigning as packages
# stream past would make a target's shard depend on how many targets happened to
# precede it.
declare -a all_pairs=()
for pkg in "${pkg_list[@]}"; do
	mapfile -t targets < <(list_targets "$pkg")
	for target in "${targets[@]}"; do
		all_pairs+=("${pkg}	${target}")
	done
done
if [ "${#all_pairs[@]}" -gt 0 ]; then
	# Guarded: `printf '%s\n'` with NO arguments still prints one newline, so
	# an unguarded sort turns "no targets" into one empty target -- which made
	# the zero-discovery error path below unreachable and a gate that
	# discovered nothing exit 0.
	mapfile -t all_pairs < <(printf '%s\n' "${all_pairs[@]}" | LC_ALL=C sort)
fi

discovered="${#all_pairs[@]}"
declare -a pairs=()
for i in "${!all_pairs[@]}"; do
	if [ $(((i % shard_total) + 1)) -eq "$shard_index" ]; then
		pairs+=("${all_pairs[$i]}")
	fi
done

if [ "$shard_total" -gt 1 ]; then
	echo "fuzz.sh: shard ${shard_index}/${shard_total} -- ${#pairs[@]} of ${discovered} target(s)" >&2
fi

total=0
failed=0
declare -a failures=()

for pair in ${pairs+"${pairs[@]}"}; do
	pkg="${pair%%	*}"
	target="${pair##*	}"
	{
		total=$((total + 1))
		if [ "$list_only" -eq 1 ]; then
			echo "${pkg}	${target}"
			continue
		fi
		echo "=== fuzz ${target}  (${pkg}, ${FUZZTIME}) ==================="
		start=$SECONDS
		# -run '^$' skips the ordinary unit tests: the seed corpus is
		# executed by the fuzzing engine anyway, and re-running the package's
		# whole test suite per target would multiply the job's cost by the
		# number of targets.
		go test "$pkg" \
			-run '^$' \
			-fuzz "^${target}\$" \
			-fuzztime "$FUZZTIME" \
			-fuzzminimizetime "$FUZZMINIMIZETIME" \
			-timeout "${hard_timeout}s"
		rc=$?
		elapsed=$((SECONDS - start))
		if [ "$rc" -ne 0 ]; then
			failed=$((failed + 1))
			failures+=("${pkg} ${target} (exit ${rc}, ${elapsed}s)")
			echo "--- FAIL ${target} after ${elapsed}s ---"
		else
			echo "--- ok ${target} in ${elapsed}s ---"
		fi
	}
done

if [ "$list_only" -eq 1 ]; then
	echo "fuzz.sh: ${total} target(s) discovered" >&2
	exit 0
fi

echo
echo "======================================================================"
if [ "$total" -eq 0 ]; then
	# Zero targets is a broken gate, not a clean run: the whole point of
	# dynamic discovery is that it can silently discover nothing.
	if [ "$discovered" -gt 0 ]; then
		# The sweep found targets but this shard drew none of them. That is a
		# misconfiguration (more shards than targets), not a clean run, and it
		# must not pass: a matrix of empty shards is a green fuzz job that
		# fuzzed nothing.
		echo "fuzz.sh: shard ${shard_index}/${shard_total} drew 0 of ${discovered} target(s)"
		echo "fuzz.sh: more shards than targets — reduce the shard count"
		exit 2
	fi
	echo "fuzz.sh: NO fuzz targets discovered in ${packages[*]}"
	echo "fuzz.sh: a fuzz gate that runs nothing cannot fail — treating as an error"
	exit 2
fi
echo "fuzz.sh: ${total} target(s), ${failed} failure(s), ${FUZZTIME} each"
if [ "$failed" -ne 0 ]; then
	for f in "${failures[@]}"; do
		echo "  FAIL  $f"
	done
	echo
	echo "Crashing inputs were written under <package>/testdata/fuzz/<Target>/."
	echo "Reproduce with: go test <package> -run '<Target>/<file>'"
	exit 1
fi
echo "fuzz.sh: all targets survived their budget"
