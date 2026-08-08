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
packages=()
for arg in "$@"; do
	case "$arg" in
	--list) list_only=1 ;;
	-*)
		echo "fuzz.sh: unknown flag $arg" >&2
		exit 2
		;;
	*) packages+=("$arg") ;;
	esac
done
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

total=0
failed=0
declare -a failures=()

for pkg in "${pkg_list[@]}"; do
	mapfile -t targets < <(list_targets "$pkg")
	for target in "${targets[@]}"; do
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
	done
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
