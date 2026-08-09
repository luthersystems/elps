#!/usr/bin/env bash
#
# Self-test for the failure CLASSIFIER in scripts/fuzz.sh.
#
# Why this is a separate suite
# ----------------------------
# scripts/ci-gates-test.sh proves the fuzz gate can FAIL -- it arms
# FuzzGateSelfTest and asserts a non-zero exit.  That is the property that was
# missing while the benchmark gate sat dead for 473 runs, and it is necessary.
# It is not sufficient.  A gate that fails for the WRONG REASON is its own
# failure mode: the FuzzMinifySource red on PR #330 and the FuzzFormat red
# recorded in issue #318 were both the fuzzing harness falling over, not a
# defect in the code under test, and nothing in the output distinguished them
# from a real crash.  So the thing under test here is the distinction itself.
#
# Every scenario runs the REAL scripts/fuzz.sh against a stub `go` on PATH, so
# the classifier, the retry budget, the crasher collection and the summary text
# are exercised end to end without a Go toolchain and without waiting for a
# fuzzing budget.  The stub is the only fake; fuzz.sh is untouched.
#
# The stub honours:
#   STUB_SCRIPT   newline-separated per-attempt behaviours, one per `go test
#                 -fuzz` invocation: `ok`, `fail:<fixture>`, or
#                 `crash:<fixture>` (which also writes a new corpus file).
#   STUB_STATE    file used to count attempts across invocations.
#   STUB_TARGETS  how many fuzz targets the stub package declares (default 1),
#                 so the per-SWEEP retry budget can be exercised.
#
# Usage:  scripts/fuzz-classify-test.sh
# Exit:   0 all scenarios passed, 1 one or more failed.

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
FUZZ="${SCRIPT_DIR}/fuzz.sh"

passed=0
failed=0

ok() {
	passed=$((passed + 1))
	echo "PASS  $1"
}

bad() {
	failed=$((failed + 1))
	echo "FAIL  $1"
}

WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

# ---------------------------------------------------------------------------
# The stub toolchain.
# ---------------------------------------------------------------------------
mkdir -p "${WORK}/bin" "${WORK}/pkg"
cat >"${WORK}/bin/go" <<'STUB'
#!/usr/bin/env bash
# Minimal `go` for scripts/fuzz.sh: list, test -list, test -fuzz.
set -uo pipefail

case "${1:-}" in
list)
	# fuzz.sh asks for `-f '{{.ImportPath}}\t{{.Dir}}'`.
	printf '%s\t%s\n' "example.com/stub" "${STUB_PKG_DIR}"
	exit 0
	;;
test) ;;
*) exit 1 ;;
esac

target=""
prev=""
for arg in "$@"; do
	if [ "$arg" = "-list" ]; then
		i=1
		while [ "$i" -le "${STUB_TARGETS:-1}" ]; do
			echo "FuzzStub${i}"
			i=$((i + 1))
		done
		echo "ok  	example.com/stub	0.001s"
		exit 0
	fi
	if [ "$prev" = "-fuzz" ]; then
		target="${arg#^}"
		target="${target%\$}"
	fi
	prev="$arg"
done
[ -n "$target" ] || target="FuzzStub1"

# A fuzzing invocation. Take the next behaviour off the script.
n=0
[ -f "$STUB_STATE" ] && n="$(cat "$STUB_STATE")"
n=$((n + 1))
echo "$n" >"$STUB_STATE"
behaviour="$(printf '%s\n' "$STUB_SCRIPT" | sed -n "${n}p")"
[ -n "$behaviour" ] || behaviour="ok"

kind="${behaviour%%:*}"
fixture="${behaviour#*:}"

case "$kind" in
ok)
	echo "fuzz: elapsed: 3s, execs: 1000 (333/sec), new interesting: 0 (total: 1)"
	echo "PASS"
	echo "ok  	example.com/stub	3.0s"
	exit 0
	;;
crash)
	# What the engine does when it finds an input: write it to the target's
	# corpus directory and name it.
	dir="${STUB_PKG_DIR}/testdata/fuzz/${target}"
	mkdir -p "$dir"
	printf 'go test fuzz v1\n[]byte("boom")\n' >"${dir}/deadbeefcafe"
	echo "--- FAIL: ${target} (3.00s)"
	echo "    --- FAIL: ${target}/deadbeefcafe (0.00s)"
	echo "        stub_test.go:1: boom"
	echo "Failing input written to testdata/fuzz/${target}/deadbeefcafe"
	echo "FAIL"
	exit 1
	;;
fail)
	case "$fixture" in
	deadline)
		# The upstream -fuzztime race, issue #335: the entire failure output.
		echo "fuzz: elapsed: 30s, execs: 105831 (0/sec), new interesting: 16 (total: 475)"
		echo "--- FAIL: ${target} (31.03s)"
		echo "    context deadline exceeded"
		echo "FAIL"
		;;
	seed)
		echo "failure while testing seed corpus entry: ${target}/seed_input"
		echo "--- FAIL: ${target} (0.01s)"
		echo "    stub_test.go:1: deliberate failure"
		echo "FAIL"
		;;
	panicdeadline)
		# A panic that ALSO happens to print the deadline text. Must not be
		# mistaken for the benign race.
		echo "--- FAIL: ${target} (31.03s)"
		echo "    context deadline exceeded"
		echo "panic: runtime error: index out of range [3]"
		echo "FAIL"
		;;
	signal)
		echo "--- FAIL: ${target} (12.00s)"
		echo "    fuzzing process terminated by unexpected signal; no crash will be recorded: signal: killed"
		echo "FAIL"
		;;
	build)
		echo "# example.com/stub [build failed]"
		echo "FAIL"
		;;
	*)
		echo "--- FAIL: ${target} (1.00s)"
		echo "    something else went wrong"
		echo "FAIL"
		;;
	esac
	exit 1
	;;
esac
exit 1
STUB
chmod +x "${WORK}/bin/go"

# run_scenario <name> <stub-script> [extra env assignments...]
# Prints combined output to $WORK/out and sets $rc.
run_scenario() {
	local STUB_TARGETS="${STUB_TARGETS:-1}"
	local name="$1" script="$2"
	shift 2
	local pkgdir="${WORK}/pkg/${name}"
	rm -rf "$pkgdir" "${WORK}/crashers"
	mkdir -p "$pkgdir"
	# Pre-existing committed corpus: the classifier must not count these as
	# new, which is the exact bug in uploading `**/testdata/fuzz/**`.
	local i=1
	while [ "$i" -le "${STUB_TARGETS:-1}" ]; do
		mkdir -p "${pkgdir}/testdata/fuzz/FuzzStub${i}"
		printf 'go test fuzz v1\n[]byte("already here")\n' \
			>"${pkgdir}/testdata/fuzz/FuzzStub${i}/0000committed"
		i=$((i + 1))
	done
	: >"${WORK}/state"
	PATH="${WORK}/bin:${PATH}" \
		STUB_PKG_DIR="$pkgdir" \
		STUB_STATE="${WORK}/state" \
		STUB_SCRIPT="$script" \
		STUB_TARGETS="${STUB_TARGETS:-1}" \
		FUZZ_CRASH_DIR="${WORK}/crashers" \
		FUZZTIME=1s \
		env "$@" bash "$FUZZ" ./stub/... >"${WORK}/out" 2>&1
	rc=$?
}

expect_rc() {
	if [ "$rc" -eq "$1" ]; then
		ok "$2"
	else
		bad "$2 (exit ${rc}, want $1)"
		sed 's/^/      | /' "${WORK}/out"
	fi
}

expect_out() {
	if grep -qF "$1" "${WORK}/out"; then
		ok "$2"
	else
		bad "$2 — output did not contain: $1"
		sed 's/^/      | /' "${WORK}/out"
	fi
}

expect_not_out() {
	if grep -qF "$1" "${WORK}/out"; then
		bad "$2 — output unexpectedly contained: $1"
		sed 's/^/      | /' "${WORK}/out"
	else
		ok "$2"
	fi
}

echo
echo "== a real crasher is still a failure, and is named ======================="

run_scenario crasher 'crash:x'
expect_rc 1 "a target that writes a crasher fails the sweep"
expect_out "CRASHER" "the failure is classified as a crasher"
expect_out "deadbeefcafe" "the new input is named"
expect_out "reproduce with" "a reproduce command is printed"
expect_not_out "retrying once" "a crasher is NEVER retried"
if [ -f "${WORK}/crashers/example.com_stub/FuzzStub1/deadbeefcafe" ]; then
	ok "the new crasher is collected for upload"
else
	bad "the new crasher was not collected into FUZZ_CRASH_DIR"
fi
if [ -f "${WORK}/crashers/example.com_stub/FuzzStub1/0000committed" ]; then
	bad "the pre-existing committed corpus was collected as if it were new"
else
	ok "the pre-existing committed corpus is NOT collected (only new files are)"
fi

echo
echo "== a seed-corpus failure writes no new file and must still be red ========"

run_scenario seed 'fail:seed'
expect_rc 1 "a seed-corpus failure fails the sweep"
expect_out "SEED CORPUS" "it is classified as a seed failure, not as a flake"
expect_not_out "retrying once" "a seed failure is NEVER retried"
expect_out "NO new crasher was produced" "the reader is told not to hunt for an input"

echo
echo "== the upstream -fuzztime race (issue #335) =============================="

run_scenario flake 'fail:deadline
ok'
expect_rc 0 "the race, clean on retry, does not redden the board"
expect_out "retrying once" "the retry is announced"
expect_out "FLAKE" "the flake is reported rather than buried"
expect_out "335" "the report cites the issue"

run_scenario recur 'fail:deadline
fail:deadline'
expect_rc 1 "the race RECURRING on the retry is a failure"
expect_out "HARNESS" "a recurring harness failure is labelled, not silently retried forever"

# Two targets, each hitting the race. The first consumes the sweep's single
# retry; the second must NOT get one, or a shard could retry every target and
# overrun the job timeout that fuzz-budget-check.sh sizes.
STUB_TARGETS=2 run_scenario budget 'fail:deadline
ok
fail:deadline'
expect_rc 1 "the retry budget is per SWEEP, not per target"
expect_out "HARNESS" "the second target's race is reported once the budget is spent"
expect_out "FLAKE" "the first target's retried flake is still reported"

echo
echo "== the race signature must not swallow a real failure ===================="

run_scenario panicky 'fail:panicdeadline'
expect_rc 1 "a panic that also prints the deadline text is NOT treated as the race"
expect_not_out "retrying once" "a panic is never retried as a flake"
expect_out "UNATTRIBUTED" "it is reported as unattributed, not as the known race"

run_scenario killed 'fail:signal'
expect_rc 1 "a worker killed by a signal is a failure"
expect_not_out "retrying once" "a signal death is never retried as a flake"

run_scenario broken 'fail:build'
expect_rc 1 "a build failure is a failure"
expect_not_out "retrying once" "a build failure is never retried as a flake"

run_scenario other 'fail:other'
expect_rc 1 "an unrecognised failure is a failure"
expect_out "UNATTRIBUTED" "an unrecognised failure is labelled unattributed"

echo
echo "== opting out ============================================================"

run_scenario noretry 'fail:deadline
ok' FUZZ_MAX_RETRIES=0
expect_rc 1 "FUZZ_MAX_RETRIES=0 makes the race fail immediately"

echo
echo "== a clean sweep stays clean and quiet ==================================="

run_scenario clean 'ok'
expect_rc 0 "a passing target passes"
expect_not_out "FLAKE" "a clean run reports no flake"
expect_not_out "NO new crasher" "a clean run does not print failure guidance"

echo
echo "=========================================================================="
echo "fuzz-classify-test: ${passed} passed, ${failed} failed"
[ "$failed" -eq 0 ] || exit 1
exit 0
