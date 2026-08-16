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
#   FUZZ_TAGS          go build tags to fuzz under, e.g. FUZZ_TAGS=elpscheck.
#                      Empty (the default) fuzzes the ordinary build.  Tagged
#                      code is INVISIBLE to the default build, so a target
#                      guarded by a tag is not merely unfuzzed -- it is not
#                      even discovered, and the sweep reports success having
#                      never seen it.  The same blind spot golangci-lint has
#                      (it analyses one build), which is why `make
#                      static-checks` makes a second pass under -tags
#                      elpscheck.
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
# Did it find a bug, or did the harness fall over?
# ------------------------------------------------
# `go test -fuzz` exits 1 for both, and from the outside they are identical --
# which is how issue #318 ended up carrying an "unexplained" FuzzFormat failure
# for weeks.  A REAL finding always leaves a new file under
# <pkg>/testdata/fuzz/<Target>/ (or says why it could not write one), so this
# script snapshots that directory around every target and reports which
# happened.  Three classifications:
#
#   CRASHER    new file(s) appeared.  A defect.  Always fails the sweep, and
#              the files are copied to $FUZZ_CRASH_DIR so CI can upload the
#              genuinely-new inputs instead of the whole committed corpus.
#   SEED       the seed corpus itself failed ("failure while testing seed
#              corpus entry").  Also a defect; the input is already committed,
#              which is why no new file appears.  Always fails.
#   HARNESS    the run failed without producing any input to blame.  The one
#              signature recognised here is the upstream `-fuzztime` race
#              described below; anything else in this class still fails, loudly
#              labelled as unattributed.
#
# The upstream -fuzztime race (issue #335)
# ----------------------------------------
# internal/fuzz.CoordinateFuzzing applies -fuzztime as a context deadline and
# suppresses it as normal termination by comparing the PARENT context's error
# against the CHILD's:
#
#     case <-doneC:  stop(ctx.Err())
#     stop := func(err error) { if err == fuzzCtx.Err() { err = nil } ... }
#
# context.cancelCtx.cancel stores the parent's error and CLOSES the parent's
# done channel before it walks its children, and cancelCtx.Err() is a lock-free
# atomic load.  If the cancelling thread is preempted in that window, the
# coordinator reads parent=DeadlineExceeded / child=nil, the suppression misses,
# and the target is failed with a bare "context deadline exceeded" -- having
# found nothing.  Reproduced by widening that window with a build overlay; see
# issue #335.  It is target-independent and writes no crasher.
#
# A target that fails ONLY that way, with no new crasher, is retried once (see
# FUZZ_MAX_RETRIES).  The retry runs the full budget, so nothing is detected
# less; if the retry fails too, the sweep fails.
#
# Exit status: 0 if every target survived its budget, 1 if any target failed.

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

# ${FUZZTIME-30s}, not ${FUZZTIME:-30s}: an UNSET budget takes the default, but
# one that is set and EMPTY (FUZZTIME="$SOMETHING_UNSET") is a mistake and must
# be rejected below rather than silently collapsing a 10m nightly into 30s.
FUZZTIME="${FUZZTIME-30s}"
FUZZ_TIMEOUT_SLACK="${FUZZ_TIMEOUT_SLACK:-300}"
FUZZMINIMIZETIME="${FUZZMINIMIZETIME:-30s}"
FUZZ_TAGS="${FUZZ_TAGS:-}"

# Where genuinely-new crashers are collected, so CI uploads those rather than
# `**/testdata/fuzz/**` -- which is the entire COMMITTED corpus and therefore
# uploads ~60 files whether or not anything was found.  An artifact that always
# exists cannot be evidence that a crasher exists.
FUZZ_CRASH_DIR="${FUZZ_CRASH_DIR:-${REPO_ROOT}/fuzz-crashers}"

# Retries are budgeted for the WHOLE sweep, not per target, because the fuzz
# job's timeout-minutes is derived from ceil(targets/shards) x FUZZTIME plus a
# fixed overhead (scripts/fuzz-budget-check.sh).  One retry is exactly the
# headroom that arithmetic leaves; per-target retries would silently overrun the
# job timeout, which truncates the sweep -- the failure mode the budget check
# exists to prevent.  Set to 0 to make the harness race fail immediately.
FUZZ_MAX_RETRIES="${FUZZ_MAX_RETRIES:-1}"

# Built once and reused, so discovery and execution can never disagree about
# which build they are looking at -- a sweep that DISCOVERS the default build
# and FUZZES the tagged one would report on targets it never ran.
declare -a tagflags=()
if [ -n "$FUZZ_TAGS" ]; then
	tagflags=(-tags "$FUZZ_TAGS")
fi

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

# Created BEFORE discovery, not after: discovery needs somewhere to put the
# stderr of the `go list` / `go test -list` probes so it can be replayed to the
# operator when one of them fails (issue #479).
tmpdir="$(mktemp -d)"
trap 'rm -rf "$tmpdir"' EXIT

# list_targets prints the Fuzz* targets defined in one package.  `go test
# -list` prints the matching names followed by a summary line ("ok <pkg> ..."),
# so the output is filtered to bare target names.
#
# Silence must not be able to mean "no targets here" (issue #479)
# ---------------------------------------------------------------
# This used to be a single pipeline ending in `2>/dev/null | grep`, which threw
# away BOTH the stderr and the exit status of `go test -list`. A package that
# failed to BUILD therefore contributed zero targets in a way that was
# byte-for-byte identical to a package that legitimately HAS none: no message,
# no non-zero status, just a shorter list. Observed for real -- 8 of 30 targets
# disappeared on a cold cache -- and the sweep still exited 0, so the fuzz job
# went green having never run them.
#
# It compounds with the budget gate: scripts/fuzz-budget-check.sh derives the
# job's timeout-minutes from THIS count, so a build failure quietly shrank the
# number the backstop was computed from. The gate was checking arithmetic over
# a target count that was not real.
#
# So the two cases are now separated explicitly:
#
#   exit 0  -- the package compiled. Zero matching targets is a legitimate
#              answer and is returned as an empty list.
#   exit 1  -- `go test -list` failed. The package did NOT compile (or its
#              dependencies did not). The caller records it and the run dies;
#              the captured stderr is replayed so the build error is visible
#              rather than inferred from a count that looks a bit low.
#
# The distinction is the whole point: "no targets" and "no build" are the same
# empty output, and only the exit status tells them apart.
list_targets() {
	local pkg="$1" rc=0 out
	out="$(go test ${tagflags+"${tagflags[@]}"} -list '^Fuzz' "$pkg" 2>"${tmpdir}/list.err")" || rc=$?
	if [ "$rc" -ne 0 ]; then
		{
			echo "fuzz.sh: target discovery FAILED for ${pkg} (go test -list exit ${rc})"
			sed 's/^/    | /' "${tmpdir}/list.err"
		} >&2
		return 1
	fi
	# `|| true`: grep exits 1 when nothing matches, and for a package that
	# built cleanly that means "this package has no fuzz targets" -- an
	# ordinary, correct answer. Letting grep's status escape here would put
	# every target-free package into the build-failure branch above and make
	# the loud path meaningless by firing it constantly.
	printf '%s\n' "$out" | grep -E '^Fuzz[A-Za-z0-9_]*$' || true
	return 0
}

# Import path AND source directory in one pass: the directory is where
# testdata/fuzz/<Target>/ lives, and the crasher snapshot below needs it.
# Resolving it lazily per target would re-invoke `go list` once per target.
declare -A pkg_dir=()
declare -a pkg_list=()
# Same treatment as list_targets, and for the same reason (issue #479): this
# also used to be `2>/dev/null` inside a process substitution, so its exit
# status was unobservable. `go list` reports a package whose NON-test source
# does not parse by writing to stderr and exiting non-zero while still printing
# the packages it could load -- so the broken one simply never entered
# pkg_list, was never probed for targets, and vanished without trace.
go_list_rc=0
go_list_out="$(go list ${tagflags+"${tagflags[@]}"} -f '{{.ImportPath}}'$'\t''{{.Dir}}' "${packages[@]}" 2>"${tmpdir}/golist.err")" || go_list_rc=$?
if [ "$go_list_rc" -ne 0 ]; then
	{
		echo "fuzz.sh: go list failed for ${packages[*]} (exit ${go_list_rc})"
		sed 's/^/    | /' "${tmpdir}/golist.err"
		echo "fuzz.sh: a package that cannot be LOADED cannot be searched for fuzz"
		echo "fuzz.sh: targets, and skipping it would silently shrink the sweep."
	} >&2
	exit 2
fi
while IFS=$'\t' read -r _ip _dir; do
	[ -n "$_ip" ] || continue
	pkg_dir["$_ip"]="$_dir"
	pkg_list+=("$_ip")
done <<<"$go_list_out"
if [ "${#pkg_list[@]}" -eq 0 ]; then
	echo "fuzz.sh: no packages matched ${packages[*]}" >&2
	exit 2
fi

# Discover EVERYTHING first, then select this shard's slice. Discovery has to
# see the whole list for shard membership to be stable -- assigning as packages
# stream past would make a target's shard depend on how many targets happened to
# precede it.
declare -a all_pairs=()
declare -a broken_pkgs=()
for pkg in "${pkg_list[@]}"; do
	# Command substitution rather than `mapfile < <(list_targets ...)`: process
	# substitution runs in a subshell whose exit status the parent never sees,
	# which is precisely how the build failure went unnoticed. `if cmd_sub`
	# evaluates the status HERE, in the main shell, where it can be acted on.
	if ! targets_out="$(list_targets "$pkg")"; then
		broken_pkgs+=("$pkg")
		continue
	fi
	while IFS= read -r target; do
		# A clean package with no targets yields one empty line from the
		# here-string; that is not a target.
		[ -n "$target" ] || continue
		all_pairs+=("${pkg}	${target}")
	done <<<"$targets_out"
done

# A package that did not build is a HOLE in the sweep, not a package with
# nothing to fuzz -- and the sweep must not be able to report success over a
# target list it knows is incomplete. Fail before any fuzzing starts, and name
# every broken package rather than only the first, so one CI run tells the
# whole story instead of one package per run.
if [ "${#broken_pkgs[@]}" -gt 0 ]; then
	{
		echo
		echo "fuzz.sh: ${#broken_pkgs[@]} package(s) FAILED TO BUILD during target discovery:"
		for pkg in "${broken_pkgs[@]}"; do
			echo "    ${pkg}"
		done
		echo
		echo "fuzz.sh: their fuzz targets, if any, were NOT discovered and would NOT"
		echo "fuzz.sh: have been run. Refusing to continue: a sweep that silently"
		echo "fuzz.sh: skips a package is indistinguishable from one that covered it,"
		echo "fuzz.sh: and scripts/fuzz-budget-check.sh derives the job timeout from"
		echo "fuzz.sh: this same count. Fix the build errors above and re-run."
	} >&2
	exit 2
fi
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

# corpus_snapshot prints the sorted file names currently in a target's
# committed-crasher directory, or nothing if it does not exist yet.
corpus_snapshot() {
	local dir="$1"
	[ -d "$dir" ] || return 0
	find "$dir" -maxdepth 1 -type f -printf '%f\n' 2>/dev/null | LC_ALL=C sort
}

# classify_failure names what a non-zero `go test -fuzz` actually means, given
# the log and whether new crasher files appeared.  Ordered most-specific first:
# any evidence of a real finding wins over the harness signature, so a genuine
# crash can never be reclassified as a flake.
classify_failure() {
	local log="$1" new_count="$2"
	if [ "$new_count" -gt 0 ]; then
		echo crasher
		return
	fi
	# A seed-corpus failure writes no new file -- the input is already
	# committed -- so it MUST be checked before the harness signature.
	if grep -q 'failure while testing seed corpus entry' "$log"; then
		echo seed
		return
	fi
	# A crash the engine found but could not persist. Still a finding.
	if grep -qE 'Failing input written to|could not write.*corpus' "$log"; then
		echo crasher
		return
	fi
	# The upstream -fuzztime race (issue #335): the ONLY output is the bare
	# deadline error. Every other marker of a real failure disqualifies it --
	# a panic, a signal death, a named failing input, or a build error.
	if grep -qE '^[[:space:]]*context deadline exceeded[[:space:]]*$' "$log" &&
		! grep -qE 'panic:|terminated by unexpected signal|--- FAIL: [A-Za-z0-9_]+/|\[build failed\]|cannot find|undefined:' "$log"; then
		echo harness
		return
	fi
	echo unattributed
}

# run_target runs one target once and prints its classification. Sets the
# globals rc, elapsed, verdict and new_crashers.
run_target() {
	local pkg="$1" target="$2" attempt="$3"
	local dir="${pkg_dir[$pkg]:-}"
	local crashdir="${dir}/testdata/fuzz/${target}"
	local log="${tmpdir}/${target}.${attempt}.log"

	corpus_snapshot "$crashdir" >"${tmpdir}/before"

	local start=$SECONDS
	# -run '^$' skips the ordinary unit tests: the seed corpus is executed by
	# the fuzzing engine anyway, and re-running the package's whole test suite
	# per target would multiply the job's cost by the number of targets.
	go test ${tagflags+"${tagflags[@]}"} "$pkg" \
		-run '^$' \
		-fuzz "^${target}\$" \
		-fuzztime "$FUZZTIME" \
		-fuzzminimizetime "$FUZZMINIMIZETIME" \
		-timeout "${hard_timeout}s" 2>&1 | tee "$log"
	rc="${PIPESTATUS[0]}"
	elapsed=$((SECONDS - start))

	corpus_snapshot "$crashdir" >"${tmpdir}/after"
	mapfile -t new_crashers < <(LC_ALL=C comm -13 "${tmpdir}/before" "${tmpdir}/after")

	if [ "$rc" -eq 0 ]; then
		verdict=ok
		return
	fi
	verdict="$(classify_failure "$log" "${#new_crashers[@]}")"

	# Collect the genuinely-new inputs where CI can upload just those.
	if [ "${#new_crashers[@]}" -gt 0 ]; then
		local out="${FUZZ_CRASH_DIR}/${pkg//\//_}/${target}"
		mkdir -p "$out"
		local f
		for f in "${new_crashers[@]}"; do
			cp "${crashdir}/${f}" "${out}/${f}"
			printf '%s\t%s\t%s\tgo test %s -run %s/%s\n' \
				"$pkg" "$target" "$f" "$pkg" "$target" "$f" \
				>>"${FUZZ_CRASH_DIR}/MANIFEST.tsv"
		done
	fi
}

total=0
failed=0
retries_left="$FUZZ_MAX_RETRIES"
declare -a failures=()
declare -a flakes=()
declare -a new_crashers=()
rc=0
elapsed=0
verdict=ok

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
		run_target "$pkg" "$target" 1

		# The harness race found nothing and blamed no input, so re-running is
		# not "hoping for green": it spends another full budget on the same
		# target. Bounded per sweep, and only ever reached when the run
		# produced no crasher at all.
		if [ "$verdict" = harness ] && [ "$retries_left" -gt 0 ]; then
			retries_left=$((retries_left - 1))
			echo "--- ${target}: failed with the upstream -fuzztime race and NO crasher (issue #335); retrying once ---"
			first_elapsed="$elapsed"
			run_target "$pkg" "$target" 2
			if [ "$rc" -eq 0 ]; then
				flakes+=("${pkg} ${target} (harness -fuzztime race after ${first_elapsed}s, clean on retry in ${elapsed}s)")
				echo "--- ok ${target} in ${elapsed}s (after one harness flake) ---"
				continue
			fi
		fi

		if [ "$rc" -ne 0 ]; then
			failed=$((failed + 1))
			case "$verdict" in
			crasher)
				failures+=("${pkg} ${target} (exit ${rc}, ${elapsed}s) CRASHER: ${new_crashers[*]}")
				;;
			seed)
				failures+=("${pkg} ${target} (exit ${rc}, ${elapsed}s) SEED CORPUS entry failed; no new crasher (the input is already committed)")
				;;
			harness)
				failures+=("${pkg} ${target} (exit ${rc}, ${elapsed}s) HARNESS: the upstream -fuzztime race (issue #335) recurred; NO crasher was produced")
				;;
			*)
				failures+=("${pkg} ${target} (exit ${rc}, ${elapsed}s) UNATTRIBUTED: failed without producing a crasher; read the log above")
				;;
			esac
			echo "--- FAIL ${target} after ${elapsed}s (${verdict}) ---"
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
echo "fuzz.sh: ${total} target(s), ${failed} failure(s), ${FUZZTIME} each${FUZZ_TAGS:+, -tags ${FUZZ_TAGS}}"

# Reported even on a green sweep: a retried flake is not nothing, and burying
# it is how #318 ended up with an unexplained result carried forward.
if [ "${#flakes[@]}" -gt 0 ]; then
	echo
	echo "fuzz.sh: ${#flakes[@]} target(s) hit the upstream -fuzztime race and passed on retry:"
	for f in "${flakes[@]}"; do
		echo "  FLAKE $f"
	done
	echo "  This is golang/go's fuzzing coordinator, not this repository's code."
	echo "  See issue #335. No input was implicated and no crasher was written."
fi

if [ "$failed" -ne 0 ]; then
	echo
	for f in "${failures[@]}"; do
		echo "  FAIL  $f"
	done
	echo
	if [ -d "$FUZZ_CRASH_DIR" ] && [ -f "${FUZZ_CRASH_DIR}/MANIFEST.tsv" ]; then
		echo "New crashing inputs were written under <package>/testdata/fuzz/<Target>/"
		echo "and collected in ${FUZZ_CRASH_DIR#"${REPO_ROOT}/"}/ :"
		while IFS=$'\t' read -r _pkg _target _file _repro; do
			echo "  ${_target}/${_file}    reproduce with: ${_repro}"
		done <"${FUZZ_CRASH_DIR}/MANIFEST.tsv"
	else
		# The distinction #318 lacked. Saying this plainly is worth as much as
		# any fix: it tells the reader not to go looking for an input.
		echo "NO new crasher was produced by this run."
		echo "Nothing was written under <package>/testdata/fuzz/<Target>/, so no"
		echo "input is implicated -- do not go looking for one. Read the failure"
		echo "classification above: a SEED failure names a committed input, and"
		echo "HARNESS / UNATTRIBUTED mean the run itself fell over."
	fi
	exit 1
fi
echo "fuzz.sh: all targets survived their budget"
