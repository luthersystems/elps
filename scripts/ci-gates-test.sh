#!/usr/bin/env bash
#
# Self-test for the CI gate logic in scripts/ and .github/workflows/.
#
# The benchmark regression gate was dead for 473 workflow runs because nothing
# ever exercised it: an inline `grep -E '^\S.*\+$'` in benchmark.yml could not
# match any benchstat line, and a gate that can only report success looks
# exactly like a gate that works.  Everything here exists so that class of
# silent death fails a PR instead:
#
#   * benchstat-gate  -- fed known-regression AND known-clean fixtures, in both
#                        benchstat table formats, plus a REAL comparison of elps
#                        against itself.  It must fire on the first and stay
#                        quiet on the second.
#   * metric direction -- elps emits a B/s throughput column (b.SetBytes); the
#                        gate must not read a throughput GAIN as a regression.
#   * workflow shape  -- the gate must be invoked from the workflow as a script
#                        (not reinlined), every `uses:` must be SHA-pinned, and
#                        the dead `grep` pattern must not come back.
#
# Run locally:  make ci-gates-test      (or: bash scripts/ci-gates-test.sh)
# Run in CI:    the `gates` job in .github/workflows/benchmark.yml

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
TESTDATA="${SCRIPT_DIR}/testdata"

pass=0
fail=0

ok() {
	pass=$((pass + 1))
	echo "PASS  $1"
}

bad() {
	fail=$((fail + 1))
	echo "FAIL  $1"
}

# assert_exit <expected-code> <description> <command...>
assert_exit() {
	local want="$1" desc="$2"
	shift 2
	local out rc
	out=$("$@" 2>&1)
	rc=$?
	if [ "$rc" -eq "$want" ]; then
		ok "$desc (exit $rc)"
	else
		bad "$desc — want exit $want, got $rc"
		echo "$out" | sed 's/^/        | /'
	fi
}

# assert_contains <needle> <description> <command...>
assert_contains() {
	local needle="$1" desc="$2"
	shift 2
	local out
	out=$("$@" 2>&1)
	if echo "$out" | grep -qF -- "$needle"; then
		ok "$desc"
	else
		bad "$desc — output did not contain '$needle'"
		echo "$out" | sed 's/^/        | /'
	fi
}

# assert_not_contains <needle> <description> <command...>
assert_not_contains() {
	local needle="$1" desc="$2"
	shift 2
	local out
	out=$("$@" 2>&1)
	if echo "$out" | grep -qF -- "$needle"; then
		bad "$desc — output unexpectedly contained '$needle'"
		echo "$out" | sed 's/^/        | /'
	else
		ok "$desc"
	fi
}

GATE="${SCRIPT_DIR}/benchstat-gate.sh"

echo "== benchstat-gate: fires on regressions =================================="

assert_exit 1 "new-format table with a +83.31% significant timing regression" \
	"$GATE" "${TESTDATA}/benchstat-regression-new.txt"
assert_contains "REGRESSION" "regression report names the offending rows" \
	"$GATE" "${TESTDATA}/benchstat-regression-new.txt"
assert_exit 1 "old-format table with a +83.31% significant timing regression" \
	"$GATE" "${TESTDATA}/benchstat-regression-old.txt"

# The exact sample from the bug report: a 50% regression at p=0.000 that the
# old inline grep waved through. This is the headline assertion of this suite.
assert_exit 1 "the reported +50.00% (p=0.000 n=10) sample FIRES the gate" \
	"$GATE" "${TESTDATA}/benchstat-task-sample.txt"
assert_contains "+50.00%" "the reported sample is named in the report" \
	"$GATE" "${TESTDATA}/benchstat-task-sample.txt"

echo
echo "== benchstat-gate: stays quiet on clean comparisons ======================"

assert_exit 0 "improvements (negative timing deltas) never fire" \
	"$GATE" "${TESTDATA}/benchstat-improvement-new.txt"
assert_exit 0 "large deltas with p above alpha never fire" \
	"$GATE" "${TESTDATA}/benchstat-insignificant-new.txt"
assert_exit 0 "old-format table whose deltas are all under the gate" \
	"$GATE" "${TESTDATA}/benchstat-clean-old.txt"

# benchstat-clean-ci.txt is the REAL CI comparison from the commit that added
# this gate. That commit changed no Go code, so it is a genuine null comparison
# on the real infrastructure -- every delta in it is noise, and the gate must not
# fire, while still parsing the whole table rather than silently understanding
# none of it. This is the fixture that keeps the DEFAULT thresholds honest: if a
# future retune drops them below the real CI noise floor, this assertion flips.
assert_exit 0 "REAL CI null comparison does not fire at the default gates" \
	"$GATE" "${TESTDATA}/benchstat-clean-ci.txt"
assert_contains "interpreted 22 delta row(s) + 148 no-change row(s)" \
	"the real CI comparison is fully parsed, not silently skipped" \
	"$GATE" "${TESTDATA}/benchstat-clean-ci.txt"
assert_contains "3 significant move(s) in the bad direction" \
	"the real CI noise IS seen; only the threshold holds it back" \
	"$GATE" "${TESTDATA}/benchstat-clean-ci.txt"

# The same command on a CONTENDED machine is an order of magnitude noisier
# (worst bad-direction move +33.83% sec/op vs +1.52% on CI). Pinned so nobody
# re-derives the thresholds from a local run and concludes the gate is broken:
# it fires here, and that is the machine's fault, not the gate's.
assert_exit 1 "the same comparison on a CONTENDED machine DOES fire (noise, not a bug)" \
	"$GATE" "${TESTDATA}/benchstat-noisy-sandbox.txt"

echo
echo "== benchstat-gate: metric DIRECTION (elps emits B/s via b.SetBytes) ======"

# The adaptation elps needs and the upstream reference did not. B/s is
# higher-is-better: a +178% delta is a 2.8x throughput GAIN. A gate that reads
# the raw sign fails an improving PR.
assert_exit 0 "B/s throughput GAINS are not regressions, even at a 0% gate" \
	env BENCH_REGRESSION_THRESHOLD_PCT=0 BENCH_ALLOC_THRESHOLD_PCT=0 \
	"$GATE" "${TESTDATA}/benchstat-bps-improvement.txt"
assert_not_contains "REGRESSION" "no B/s gain is reported as a regression" \
	env BENCH_REGRESSION_THRESHOLD_PCT=0 BENCH_ALLOC_THRESHOLD_PCT=0 \
	"$GATE" "${TESTDATA}/benchstat-bps-improvement.txt"

# ...and the mirror: throughput COLLAPSING is a real regression and must fire.
assert_exit 1 "B/s throughput COLLAPSE does fire the gate" \
	"$GATE" "${TESTDATA}/benchstat-bps-regression.txt"
assert_contains "higher is better" "the report labels the metric direction" \
	"$GATE" "${TESTDATA}/benchstat-bps-regression.txt"

# A B/s DIP of a few percent is the case between those two extremes, and the one
# that got mistaken for a gate bug on PR #310: it is the only column in that run
# with magnitudes above BENCH_ALLOC_THRESHOLD_PCT, which invites the conclusion
# that B/s is being judged against the allocation gate. It is not -- B/s has no
# "/op" suffix, so is_alloc_metric() returns 0 and it falls to the timing gate.
# These assertions pin that, so the question does not have to be re-litigated
# from the numbers.
assert_exit 0 "a several-percent B/s DIP with flat B/op and allocs/op does NOT fire" \
	"$GATE" "${TESTDATA}/benchstat-bps-dip-only.txt"
assert_contains "gate 15%" "B/s rows are judged against the TIMING gate, not the allocation gate" \
	"$GATE" "${TESTDATA}/benchstat-bps-dip-only.txt"
assert_not_contains "REGRESSION" "no B/s dip below the timing gate is called a regression" \
	"$GATE" "${TESTDATA}/benchstat-bps-dip-only.txt"
# Belt and braces: even if the allocation gate were tightened to zero, the B/s
# rows must be unaffected by it. If this ever fails, B/s has been mis-classified
# into the allocation bucket -- which is exactly the bug that was suspected.
assert_exit 0 "B/s dip is untouched by the ALLOCATION gate, even at 0%" \
	env BENCH_ALLOC_THRESHOLD_PCT=0 "$GATE" "${TESTDATA}/benchstat-bps-dip-only.txt"

echo
echo "== benchstat-gate: per-metric-class thresholds ==========================="

# elps' allocation metrics are deterministic (measured worst-case noise on
# identical code: 0.19% for B/op, 0.00% for allocs/op) while sec/op noise
# reaches 33.83%. A single threshold cannot serve both. This fixture holds an
# +8% allocation regression: below the loose timing gate, above the tight
# allocation one.
assert_exit 1 "an +8% ALLOCATION regression fires the tight allocation gate" \
	"$GATE" "${TESTDATA}/benchstat-alloc-regression.txt"
assert_contains "allocs/op" "the allocation regression is the row reported" \
	"$GATE" "${TESTDATA}/benchstat-alloc-regression.txt"
# Proof the two thresholds are genuinely independent: raise only the allocation
# gate and the same table passes.
assert_exit 0 "the same table passes once the allocation gate is raised to 20%" \
	env BENCH_ALLOC_THRESHOLD_PCT=20 "$GATE" "${TESTDATA}/benchstat-alloc-regression.txt"
# And the timing gate alone would never have caught it.
assert_exit 0 "a single 50% gate would have missed it entirely" \
	env BENCH_ALLOC_THRESHOLD_PCT=50 "$GATE" "${TESTDATA}/benchstat-alloc-regression.txt"

# The live case: the gate's first real firing, on PR #310. A +8.44% B/op
# regression with an IDENTICAL allocation count -- the same allocations, made
# bigger by a field added to CallFrame. This is the signal the allocation gate
# exists for, and it must keep firing.
assert_exit 1 "the LIVE PR #310 allocation regression fires" \
	"$GATE" "${TESTDATA}/benchstat-alloc-regression-live.txt"
assert_contains "REGRESSION" "the live allocation regression is reported" \
	"$GATE" "${TESTDATA}/benchstat-alloc-regression-live.txt"
assert_contains "EnvFunCallRecursion-4" "the offending row is named" \
	"$GATE" "${TESTDATA}/benchstat-alloc-regression-live.txt"
# Its timing deltas (worst +13.48%) are all under the 15% timing gate, so the
# allocation column is genuinely the only thing that fired. Raising just the
# allocation gate makes the whole table pass -- proof that no timing row was
# responsible, and the knob a maintainer would reach for to accept the cost.
assert_exit 0 "with only the ALLOCATION gate raised, the live table passes" \
	env BENCH_ALLOC_THRESHOLD_PCT=10 "$GATE" "${TESTDATA}/benchstat-alloc-regression-live.txt"

echo
echo "== benchstat-gate: the threshold is the only thing holding it back ======="

# Proves the parser genuinely SEES the real comparison's significant deltas and
# is silent because of the threshold, not because it failed to parse. If the
# table format ever changes out from under the parser, this assertion flips.
assert_exit 1 "the REAL clean fixture DOES fire once the gates are lowered to 0%" \
	env BENCH_REGRESSION_THRESHOLD_PCT=0 BENCH_ALLOC_THRESHOLD_PCT=0 \
	"$GATE" "${TESTDATA}/benchstat-clean-ci.txt"
assert_exit 1 "old-format clean fixture DOES fire at 0%" \
	env BENCH_REGRESSION_THRESHOLD_PCT=0 BENCH_ALLOC_THRESHOLD_PCT=0 \
	"$GATE" "${TESTDATA}/benchstat-clean-old.txt"
assert_exit 0 "improvements still do not fire at a 0% gate" \
	env BENCH_REGRESSION_THRESHOLD_PCT=0 BENCH_ALLOC_THRESHOLD_PCT=0 \
	"$GATE" "${TESTDATA}/benchstat-improvement-new.txt"
assert_exit 0 "p-insignificant rows still do not fire at a 0% gate" \
	env BENCH_REGRESSION_THRESHOLD_PCT=0 BENCH_ALLOC_THRESHOLD_PCT=0 \
	"$GATE" "${TESTDATA}/benchstat-insignificant-new.txt"

echo
echo "== benchstat-gate: uninterpretable input fails loudly ===================="

assert_exit 2 "benchstat crash output (no comparison rows) is an error, not 'clean'" \
	"$GATE" "${TESTDATA}/benchstat-crash.txt"

# A p-value this parser cannot read must fail closed. Truncating at the first
# non-digit turns p=1.5e-05 into 1.5 and drops a +99% regression as
# insignificant -- the one parse path that would fail OPEN.
assert_exit 1 "scientific-notation p-value is read as significant, not dropped" \
	"$GATE" "${TESTDATA}/benchstat-sci-pvalue.txt"
assert_contains "+99.00%" "the sci-notation row is the one reported" \
	"$GATE" "${TESTDATA}/benchstat-sci-pvalue.txt"
assert_exit 2 "a p-value that is not a number at all fails closed" \
	"$GATE" "${TESTDATA}/benchstat-badpvalue.txt"

# A table where nothing moved is a SUCCESSFUL comparison, not an unreadable one.
assert_exit 0 "an all-'~' table with no geomean row is clean, not an error" \
	"$GATE" "${TESTDATA}/benchstat-tilde-only.txt"
assert_contains "no-change row" "the all-'~' table reports interpreted rows" \
	"$GATE" "${TESTDATA}/benchstat-tilde-only.txt"

# Old-format "(all equal)" rows are DATA, not footnotes, but they contain the
# words "all equal"/"samples". A footnote filter written as a substring match
# discards them, the table parses to zero rows, and a perfectly clean comparison
# reports a spurious exit 2. Anchoring on the leading superscript is the fix;
# this fixture is what proves it, since every row in it is an all-equal row.
assert_exit 0 "old-format all-'(all equal)' table is clean, not an exit-2 error" \
	"$GATE" "${TESTDATA}/benchstat-allequal-old.txt"
assert_contains "3 no-change row(s)" "every (all equal) row survives the footnote filter" \
	"$GATE" "${TESTDATA}/benchstat-allequal-old.txt"

empty_file="$(mktemp)"
assert_exit 2 "empty benchstat output is an error, not 'clean'" \
	"$GATE" "$empty_file"
rm -f "$empty_file"

assert_exit 2 "missing input file is an error, not 'clean'" \
	"$GATE" "${TESTDATA}/does-not-exist.txt"
assert_exit 2 "missing argument is a usage error" "$GATE"

echo
echo "== benchstat-gate: regression proof for the original broken pattern ======"

# The gate this replaced. Documented here so nobody reintroduces it: it does not
# match ANY real benchstat output, which is the entire bug.
old_pattern_fired=0
for fx in benchstat-regression-new benchstat-task-sample benchstat-regression-old \
	benchstat-alloc-regression benchstat-bps-regression; do
	if grep -E '^\S.*\+$' "${TESTDATA}/${fx}.txt" 2>/dev/null | grep -qv '^name'; then
		old_pattern_fired=1
		bad "the old inline grep unexpectedly matched ${fx} — fixture no longer reproduces the bug"
	fi
done
if [ "$old_pattern_fired" -eq 0 ]; then
	ok "old inline grep -E '^\\S.*\\+\$' matches NONE of the 5 regression fixtures"
fi

echo
echo "== bench-arms-check: the two arms must be comparable ====================="

# The failure this guards is silent by construction: when the arms cannot pair,
# benchstat emits a normal-looking table with no comparison rows. Downstream the
# gate does go red -- but only as "could not be interpreted", which names the
# symptom and not the cause. These cases assert the cause is named.
ARMS="${SCRIPT_DIR}/bench-arms-check.sh"

ARMS_TMP="$(mktemp -d)"
trap 'rm -rf "$ARMS_TMP"' EXIT

# Six samples so the "need >= 6" advisory does not fire in the clean case.
arms_fixture() { # <file> <cpu> <suffix> [extra-benchmark-name]
	{
		echo "goos: linux"
		echo "goarch: arm64"
		echo "pkg: github.com/luthersystems/elps/lisp"
		echo "cpu: $2"
		for _ in 1 2 3 4 5 6; do
			printf 'BenchmarkEnvGet-%s\t  138022\t      8104 ns/op\t    1808 B/op\t      27 allocs/op\n' "$3"
			printf 'BenchmarkEnvFunCall-%s\t   58022\t     18104 ns/op\t    3808 B/op\t      57 allocs/op\n' "$3"
			if [ -n "${4:-}" ]; then
				printf 'Benchmark%s-%s\t   58022\t     18104 ns/op\t    3808 B/op\t      57 allocs/op\n' "$4" "$3"
			fi
		done
		echo "PASS"
		echo "ok  	github.com/luthersystems/elps/lisp	1.5s"
	} >"$1"
}

arms_fixture "${ARMS_TMP}/base.txt" "Neoverse-N1" 2
arms_fixture "${ARMS_TMP}/pr.txt" "Neoverse-N1" 2
assert_exit 0 "identical configuration and benchmark names -> comparable" \
	"$ARMS" "${ARMS_TMP}/base.txt" "${ARMS_TMP}/pr.txt"

# The footgun. Editing `runs-on` from a 4-core to a 2-core runner renames
# EVERY benchmark, so the intersection is empty and benchstat pairs nothing --
# with no error anywhere that says so.
arms_fixture "${ARMS_TMP}/gomaxprocs4.txt" "Neoverse-N1" 4
assert_exit 2 "GOMAXPROCS suffix mismatch (-4 vs -2) -> not comparable" \
	"$ARMS" "${ARMS_TMP}/gomaxprocs4.txt" "${ARMS_TMP}/pr.txt"
assert_contains "GOMAXPROCS suffix" "GOMAXPROCS mismatch is DIAGNOSED, not reported as generic unpairability" \
	"$ARMS" "${ARMS_TMP}/gomaxprocs4.txt" "${ARMS_TMP}/pr.txt"

# The heterogeneous-pool failure: benchstat keys its configuration off the
# cpu: header, so two different CPU models pair nothing even at the same arch.
arms_fixture "${ARMS_TMP}/othercpu.txt" "AMD EPYC 7763" 2
assert_exit 2 "cpu-model mismatch between arms -> not comparable" \
	"$ARMS" "${ARMS_TMP}/othercpu.txt" "${ARMS_TMP}/pr.txt"
assert_contains "cpu:" "cpu mismatch names the header benchstat keys on" \
	"$ARMS" "${ARMS_TMP}/othercpu.txt" "${ARMS_TMP}/pr.txt"

# Adding or removing a benchmark is legitimate and must NOT fail the build --
# only reported, so a benchmark silently dropping out of the comparison set is
# visible rather than inferred.
arms_fixture "${ARMS_TMP}/extra.txt" "Neoverse-N1" 2 "NewlyAdded"
assert_exit 0 "a PR that ADDS a benchmark is still comparable" \
	"$ARMS" "${ARMS_TMP}/base.txt" "${ARMS_TMP}/extra.txt"
assert_contains "only in pr" "an unpaired benchmark is reported by name" \
	"$ARMS" "${ARMS_TMP}/base.txt" "${ARMS_TMP}/extra.txt"

: >"${ARMS_TMP}/empty.txt"
assert_exit 2 "an empty arm fails rather than degrading to a one-sided report" \
	"$ARMS" "${ARMS_TMP}/empty.txt" "${ARMS_TMP}/pr.txt"

# Benchmarks that failed to build produce output with headers but no result
# lines. That must not read as "nothing changed".
{
	echo "goos: linux"
	echo "goarch: arm64"
	echo "cpu: Neoverse-N1"
	echo "FAIL	github.com/luthersystems/elps/lisp [build failed]"
} >"${ARMS_TMP}/nobench.txt"
assert_exit 2 "an arm with headers but no benchmark results fails" \
	"$ARMS" "${ARMS_TMP}/nobench.txt" "${ARMS_TMP}/pr.txt"

assert_exit 2 "missing file fails with the same 'unusable' code as the gate" \
	"$ARMS" "${ARMS_TMP}/does-not-exist.txt" "${ARMS_TMP}/pr.txt"

echo
echo "== workflow shape guards ================================================="

BENCH_WF="${REPO_ROOT}/.github/workflows/benchmark.yml"

# Both of the next two guards must anchor on an INVOCATION, not the substring:
# benchmark.yml names both scripts in explanatory comments, so a bare `grep -q`
# stays green even after the actual `run:` line is deleted. The negative-control
# matrix caught exactly that — both guards were reported as passing while the
# fix they guard had been reverted.
# invoked_in <file> <script-path> -> prints matching non-comment line numbers
invoked_in() {
	python3 - "$1" "$2" <<'PY'
import sys
path, needle = sys.argv[1], sys.argv[2]
hits = []
for i, line in enumerate(open(path), 1):
    if line.lstrip().startswith("#"):
        continue
    code = line.split("#", 1)[0]      # strip trailing comment
    if needle in code:
        hits.append(str(i))
print(" ".join(hits))
PY
}

if [ -n "$(invoked_in "$BENCH_WF" 'scripts/benchstat-gate.sh')" ]; then
	ok "benchmark.yml INVOKES scripts/benchstat-gate.sh (not just mentions it)"
else
	bad "benchmark.yml no longer invokes scripts/benchstat-gate.sh — logic reinlined or removed?"
fi

# The dead pattern must never come back, in any workflow. Match an INVOCATION,
# not the word: the workflow (and this file) deliberately quote the broken
# pattern in explanatory comments, and a bare substring grep flags those too --
# which would make the guard fail on the very change that documents the fix.
dead_hits="$(python3 - "$REPO_ROOT" <<'PY'
import glob, os, sys
root = sys.argv[1]
hits = []
for f in sorted(glob.glob(os.path.join(root, ".github", "workflows", "*.y*ml"))):
    for i, line in enumerate(open(f), 1):
        stripped = line.lstrip()
        if stripped.startswith("#"):      # a comment ABOUT the pattern is fine
            continue
        if r"^\S.*\+$" in line and "grep" in line:
            hits.append(f"{os.path.basename(f)}:{i}")
print("\n".join(hits))
PY
)"
if [ -n "$dead_hits" ]; then
	bad "the dead '^\\S.*\\+\$' grep pattern reappeared as a live invocation: $dead_hits"
else
	ok "the dead '^\\S.*\\+\$' grep is absent from all workflows (comments excepted)"
fi

# The self-test must itself run in CI, or it protects nothing.
if [ -n "$(invoked_in "$BENCH_WF" 'scripts/ci-gates-test.sh')" ]; then
	ok "benchmark.yml RUNS this self-test in CI (not just mentions it)"
else
	bad "benchmark.yml does not run ci-gates-test.sh — the gate would be unguarded again"
fi

# The benchmark comparison must measure BOTH arms in this job, on one runner.
#
# The gate previously compared the PR against a baseline restored from
# actions/cache -- results recorded on a different machine at a different
# time, with the hardware delta attributed to the code delta. GitHub's hosted
# pool is heterogeneous: consecutive runs of IDENTICAL code landed on an EPYC
# 7763 and an EPYC 9V74, and because benchstat keys its configuration off the
# goos/goarch/cpu headers `go test` emits, it refused to pair them and emitted
# zero comparison rows -- which benchstat-gate.sh correctly calls exit 2 and
# the workflow turns into a hard failure. A gate that flaps on which machine
# it drew teaches people to re-run it, which is how a real regression gets
# waved through.
#
# Two halves, both required, both able to fail independently:
#   1. the base revision is checked out for measurement, and
#   2. no cached benchmark baseline is restored.
# Guard 2 alone would pass on a workflow that compares against nothing;
# guard 1 alone would pass on one that checks out the base and then ignores it
# in favour of the cache.
if [ -n "$(invoked_in "$BENCH_WF" 'pull_request.base.sha')" ]; then
	ok "benchmark.yml checks out the PR base revision to measure it in-job"
else
	bad "benchmark.yml does not check out the PR base — the baseline is not measured on this runner"
fi

cached_baseline="$(python3 - "$BENCH_WF" <<'PY_INNER'
import re, sys
path = sys.argv[1]
hits = []
for i, line in enumerate(open(path), 1):
    if line.lstrip().startswith("#"):     # comments ABOUT the old design are fine
        continue
    code = line.split("#", 1)[0]
    if re.search(r"(key|restore-keys):", code) and "benchmark-baseline" in code:
        hits.append(f"{i}: {line.strip()}")
print("\n".join(hits))
PY_INNER
)"
if [ -n "$cached_baseline" ]; then
	bad "benchmark.yml restores a CACHED benchmark baseline — that compares across machines and is what made this gate flap: $cached_baseline"
else
	ok "benchmark.yml restores no cached baseline (both arms measured in-job)"
fi

# GOMAXPROCS must be PINNED, not inherited from the runner's core count.
# `go test` appends it to every benchmark name, and benchstat pairs rows by
# the full name -- so arms measured at different core counts share no names
# and pair nothing, from a table that looks entirely normal. The trap is that
# it is sprung by editing `runs-on`, which does not look like touching this
# gate. Prevention is the pin; detection is bench-arms-check.sh.
if grep -qE '^\s*GOMAXPROCS:\s*"?[0-9]+"?\s*$' "$BENCH_WF"; then
	ok "benchmark.yml pins GOMAXPROCS (the benchmark-name suffix cannot follow the runner)"
else
	bad "benchmark.yml does not pin GOMAXPROCS — a runner size change would silently unpair every benchmark"
fi

if [ -n "$(invoked_in "$BENCH_WF" 'scripts/bench-arms-check.sh')" ]; then
	ok "benchmark.yml INVOKES scripts/bench-arms-check.sh before comparing"
else
	bad "benchmark.yml does not run the arm comparability pre-flight — an unpairable comparison would report only its symptom"
fi

# A job that relocates its checkout with `path:` has NOTHING from the
# repository at the workspace root. Referring to a repo script as
# `./scripts/foo.sh` there exits 127 "No such file or directory" -- which this
# workflow then reported as "Benchmark regressions detected", a real-looking
# failure with an entirely fictional cause. That is the class this guard
# closes: a relocated checkout plus a root-relative repo path.
relocated="$(python3 - "$BENCH_WF" <<'PY_INNER'
import re, sys

path = sys.argv[1]
try:
    import yaml
except ImportError:
    print("__SKIP__ pyyaml unavailable")
    sys.exit(0)


def strip_comments(run):
    """Drop shell comments so a `run:` block that DOCUMENTS a path is not
    mistaken for one that invokes it. Heuristic (a '#' opening a comment is
    at line start or preceded by whitespace); adequate here because these
    blocks contain no '#' inside string literals."""
    out = []
    for line in run.splitlines():
        m = re.search(r"(?:^|\s)#", line)
        out.append(line[: m.start()] if m else line)
    return "\n".join(out)


# A path REFERENCE starts at a token boundary: start of line, whitespace, or a
# shell separator. `${PR_TREE}/scripts/foo.sh` is preceded by '/', so it is a
# suffix of a longer path and not root-relative -- exactly the fixed form.
REF = re.compile(r"(?:^|[\s;&|(])(?:\./)?(scripts/[\w./-]+)", re.M)

doc = yaml.safe_load(open(path))
hits = []
for job_id, job in (doc.get("jobs") or {}).items():
    steps = job.get("steps") or []
    paths = []
    for st in steps:
        if "actions/checkout" in (st.get("uses") or ""):
            p = (st.get("with") or {}).get("path")
            paths.append(p.strip("/") if p else "")
    # Only jobs where EVERY checkout is relocated are affected; a job that also
    # checks out at the root still has the repo there.
    if not paths or any(p == "" for p in paths):
        continue
    for st in steps:
        run = st.get("run")
        if not isinstance(run, str):
            continue
        for m in REF.finditer(strip_comments(run)):
            hits.append(f"{job_id}: {m.group(1)}")
print("\n".join(sorted(set(hits))))
PY_INNER
)"
case "$relocated" in
	__SKIP__*) echo "SKIP  relocated-checkout path guard ($relocated)" ;;
	"") ok "no job with a relocated checkout refers to repo scripts as if they were at the workspace root" ;;
	*)
		bad "root-relative repo paths inside a job whose checkout is relocated (these exit 127, not a regression):"
		echo "$relocated" | sed 's/^/        | /'
		;;
esac

# Any job on a runner label that is not a standard GitHub-hosted one must be
# watched by the queue watchdog. An unreachable label does not fail: the job
# QUEUES, publishing no status at all, so the board shows the remaining checks
# all green and reads as a pass. elps lost five consecutive pushes to exactly
# that (`ubuntu-arm-4core-150gb`, real but shared with substrate, not elps).
# `timeout-minutes` cannot cover it -- that clock starts when a job STARTS.
watch_out="$(python3 - "$REPO_ROOT" <<'PY_INNER'
import glob, os, re, sys

root = sys.argv[1]
try:
    import yaml
except ImportError:
    print("__SKIP__ pyyaml unavailable")
    sys.exit(0)

# Labels GitHub always provides. Anything else -- a larger-runner label, a
# self-hosted label, or an expression whose value cannot be read here -- is
# only as reachable as the repo's runner settings happen to make it.
STANDARD = re.compile(r"^(ubuntu|windows|macos)-[0-9a-z.]+$")

failures, passes = [], []
for f in sorted(glob.glob(os.path.join(root, ".github", "workflows", "*.y*ml"))):
    base = os.path.basename(f)
    try:
        doc = yaml.safe_load(open(f))
    except Exception:  # noqa: BLE001 -- the YAML-parse guard above owns this
        continue
    if not isinstance(doc, dict):
        continue
    jobs = doc.get("jobs") or {}

    watched = set()
    for job in jobs.values():
        for st in (job.get("steps") or []):
            name = ((st.get("env") or {}).get("WATCH_JOB"))
            if name:
                watched.add(str(name))

    for job_id, job in jobs.items():
        ro = job.get("runs-on")
        labels = []
        if isinstance(ro, str):
            labels = [ro]
        elif isinstance(ro, list):
            labels = [str(x) for x in ro]
        elif isinstance(ro, dict):
            labels = [str(x) for x in (ro.get("labels") or [])] or ["<group>"]
        if not labels:
            continue
        if all(STANDARD.match(x) for x in labels):
            continue
        display = job.get("name") or job_id
        if display in watched:
            passes.append(f"{base}: job {display!r} on non-standard runner {labels} is queue-watched")
        else:
            failures.append(
                f"{base}: job {display!r} runs on {labels}, which is not a standard "
                f"GitHub-hosted label, and no job in that workflow sets "
                f"WATCH_JOB: {display}. An unreachable label QUEUES silently and "
                f"publishes no status."
            )

for p in passes:
    print(f"PASS  {p}")
for f_ in failures:
    print(f"FAIL  {f_}")
print(f"__COUNTS__ {len(passes)} {len(failures)}")
PY_INNER
)"
case "$watch_out" in
	__SKIP__*) echo "SKIP  runner-reachability guard ($watch_out)" ;;
	*)
		echo "$watch_out" | grep -v '^__COUNTS__' || true
		watch_counts="$(echo "$watch_out" | sed -n 's/^__COUNTS__ //p')"
		if [ -n "$watch_counts" ]; then
			read -r w_pass w_fail <<<"$watch_counts"
			pass=$((pass + w_pass))
			fail=$((fail + w_fail))
		else
			bad "runner-reachability guard did not run"
		fi
		;;
esac

# The watchdog must exist and must be wired to a job that is actually in the
# workflow -- WATCH_JOB is matched against the job's `name:` EXACTLY, so a
# rename on one side silently un-watches it.
WATCHDOG="${SCRIPT_DIR}/ci-queue-watchdog.cjs"
if [ -f "$WATCHDOG" ]; then
	ok "scripts/ci-queue-watchdog.cjs exists"
	if [ -n "$(invoked_in "$BENCH_WF" 'ci-queue-watchdog.cjs')" ]; then
		ok "benchmark.yml INVOKES the queue watchdog"
	else
		bad "benchmark.yml does not invoke ci-queue-watchdog.cjs"
	fi
	if command -v node >/dev/null 2>&1; then
		if node --check "$WATCHDOG" 2>/dev/null; then
			ok "node --check ci-queue-watchdog.cjs"
		else
			bad "node --check ci-queue-watchdog.cjs"
			node --check "$WATCHDOG" 2>&1 | sed 's/^/        | /'
		fi
	else
		echo "SKIP  node not installed; cannot syntax-check the watchdog"
	fi
else
	bad "scripts/ci-queue-watchdog.cjs is missing"
fi

wf_out="$(python3 - "$REPO_ROOT" <<'PY'
import glob, os, re, sys

root = sys.argv[1]
failures, passes = [], []

files = sorted(glob.glob(os.path.join(root, ".github", "workflows", "*.yml")) +
               glob.glob(os.path.join(root, ".github", "workflows", "*.yaml")))
if not files:
    failures.append("no workflow files found")

try:
    import yaml
    docs = {}
    for f in files:
        try:
            docs[f] = yaml.safe_load(open(f))
        except Exception as e:  # noqa: BLE001
            failures.append(f"{os.path.basename(f)} does not parse: {e}")
    if not failures:
        passes.append(f"all {len(files)} workflow files parse as YAML")

    # No two workflows may publish the same top-level `name:` -- duplicate names
    # produce indistinguishable check contexts, which lets a trivially-green
    # workflow impersonate a real gate.
    names = {}
    for f, doc in docs.items():
        if isinstance(doc, dict) and doc.get("name"):
            names.setdefault(doc["name"], []).append(os.path.basename(f))
    dupes = {n: v for n, v in names.items() if len(v) > 1}
    for n, v in dupes.items():
        failures.append(f"duplicate workflow name {n!r} in {', '.join(v)}")
    if not dupes:
        passes.append("no two workflows declare the same top-level name")
except ImportError:
    passes.append("(pyyaml unavailable; YAML-parse guards skipped)")

# Every third-party `uses:` should be SHA-pinned (local ./ actions are exempt).
# CONTRACT.md for this check: the repo pinned its actions in f5b12e3 ("chore: pin
# GitHub Actions to SHAs and add Dependabot config"), but two workflows added
# later drifted back to floating tags. Those are pre-existing and out of scope
# for the benchmark-gate fix, so they are ALLOWLISTED rather than silently
# ignored -- they print as WARN and the list may only ever shrink. Any NEW
# unpinned action, or any unpinned action in a workflow this change owns, is a
# hard failure.
UNPINNED_ALLOWLIST = {
    # file: reason (delete the row once the workflow is pinned)
    "govulncheck.yml": "pre-existing, added in #303; not touched by this change",
    "release-tag.yml": "pre-existing, added in #302; not touched by this change",
}
sha = re.compile(r"^[0-9a-f]{40}$")
unpinned, warned = [], []
for f in files:
    base = os.path.basename(f)
    for i, line in enumerate(open(f), 1):
        m = re.search(r"uses:\s*(\S+)", line)
        if not m:
            continue
        ref = m.group(1)
        if ref.startswith("./"):
            continue
        if "@" not in ref or not sha.match(ref.rsplit("@", 1)[1]):
            (warned if base in UNPINNED_ALLOWLIST else unpinned).append(f"{base}:{i} {ref}")
for u in unpinned:
    failures.append(f"unpinned action: {u}")
if not unpinned:
    passes.append(f"every non-allowlisted `uses:` is pinned to a 40-char commit SHA "
                  f"({len(warned)} allowlisted)")
for w in warned:
    print(f"WARN  unpinned action (allowlisted, should be pinned): {w}")

for p in passes:
    print(f"PASS  {p}")
for f_ in failures:
    print(f"FAIL  {f_}")
print(f"__COUNTS__ {len(passes)} {len(failures)}")
PY
)"

echo "$wf_out" | grep -v '^__COUNTS__' || true
wf_counts="$(echo "$wf_out" | sed -n 's/^__COUNTS__ //p')"
if [ -n "$wf_counts" ]; then
	read -r wf_pass wf_fail <<<"$wf_counts"
	pass=$((pass + wf_pass))
	fail=$((fail + wf_fail))
else
	bad "workflow shape guards did not run"
fi

echo
echo "== fuzz gate: time bounding =============================================="

FUZZ="${SCRIPT_DIR}/fuzz.sh"
FUZZ_WF="${REPO_ROOT}/.github/workflows/fuzz.yml"

# `go test -fuzz` has no default limit: without -fuzztime it runs until
# something kills it. Every one of these guards exists to keep that from
# reaching CI.

assert_exit 2 "an unparsable FUZZTIME is refused rather than guessed" \
	env FUZZTIME=forever "$FUZZ" --list
assert_contains "refusing to run unbounded" \
	"the refusal says why" \
	env FUZZTIME=forever "$FUZZ" --list
assert_exit 2 "a bare number (no unit) is refused" \
	env FUZZTIME=60 "$FUZZ" --list
assert_exit 2 "an empty FUZZTIME is refused" \
	env FUZZTIME= "$FUZZ" --list

# Every LIVE `go test -fuzz` in this repository must carry -fuzztime. Logical
# lines, not physical ones: the invocation in fuzz.sh spreads its flags over a
# backslash continuation, and a per-line scan would read the `-fuzz` line as
# unbounded. Anchored on `go test` so that prose, this checker's own source,
# and workflow comments are not mistaken for invocations.
unbounded="$(python3 - "$REPO_ROOT" <<'PY'
import glob, os, re, sys
root = sys.argv[1]
files = sorted(
    glob.glob(os.path.join(root, ".github", "workflows", "*.y*ml"))
    + glob.glob(os.path.join(root, "scripts", "*.sh"))
    + [os.path.join(root, "Makefile")]
)
hits = []
for f in files:
    if not os.path.exists(f):
        continue
    logical, buf, start = [], "", 1
    for i, line in enumerate(open(f), 1):
        if line.lstrip().startswith("#"):
            continue
        code = line.split("#", 1)[0].rstrip("\n")
        if not buf:
            start = i
        if code.rstrip().endswith("\\"):
            buf += code.rstrip()[:-1] + " "
            continue
        logical.append((start, buf + code))
        buf = ""
    if buf:
        logical.append((start, buf))
    for lineno, code in logical:
        if not re.search(r"\bgo\s+test\b", code):
            continue
        if not re.search(r"(^|\s)-fuzz(\s|=|$)", code):
            continue
        if "-fuzztime" not in code:
            hits.append(f"{os.path.relpath(f, root)}:{lineno}")
print("\n".join(hits))
PY
)"
if [ -n "$unbounded" ]; then
	bad "a 'go test -fuzz' invocation has no -fuzztime bound: $unbounded"
else
	ok "every live 'go test -fuzz' invocation is bounded by -fuzztime"
fi

echo
echo "== fuzz gate: discovery and proof it can FAIL ============================"

# Everything from here needs a Go toolchain matching go.mod. The `gates` job in
# benchmark.yml deliberately has none -- it is the fast, build-free gate that
# must report even when the benchmark job is cancelled -- so it sets
# CI_GATES_SKIP_GO and these run in the `fuzz` workflow instead, which already
# has Go set up and its module cache warm.
if [ "${CI_GATES_SKIP_GO:-0}" = "1" ] || ! command -v go >/dev/null 2>&1; then
	echo "SKIP  Go-backed fuzz-gate checks (CI_GATES_SKIP_GO=${CI_GATES_SKIP_GO:-0}," \
		"go present: $(command -v go >/dev/null 2>&1 && echo yes || echo no))"
	echo "SKIP  they run in .github/workflows/fuzz.yml, which sets up Go"
else
	# The derived budget gate. It reads real discovery plus the workflow, so
	# it needs Go; the negative controls below drive it at a scratch copy of
	# the workflow via FUZZ_WORKFLOW.
	BUDGET="${SCRIPT_DIR}/fuzz-budget-check.sh"
	assert_exit 0 "the nightly sweep fits its timeout as configured today" \
		"$BUDGET"

	budget_tmp="$(mktemp -d)"
	budget_wf="${budget_tmp}/fuzz.yml"

	# Each control reverts ONE property of the real workflow. A budget gate
	# that has never been watched failing is worth nothing -- the value it
	# guards went stale twice before this existed (120 sized for 10 targets
	# when there were 12; 140 vs 165 when two branches each counted only their
	# own additions).
	cp "$FUZZ_WF" "$budget_wf"
	sed -i 's/^    timeout-minutes: [0-9]*$/    timeout-minutes: 5/' "$budget_wf"
	assert_exit 1 "a timeout too small for the sweep FAILS" \
		env FUZZ_WORKFLOW="$budget_wf" "$BUDGET"

	cp "$FUZZ_WF" "$budget_wf"
	sed -i 's/^        shard: \[.*\]$/        shard: [1]/' "$budget_wf"
	assert_exit 1 "unsharding (one shard, whole sweep serial) FAILS" \
		env FUZZ_WORKFLOW="$budget_wf" "$BUDGET"

	cp "$FUZZ_WF" "$budget_wf"
	sed -i '/^    timeout-minutes: [0-9]*$/d' "$budget_wf"
	assert_exit 2 "a fuzz job with NO timeout-minutes is unreadable, not fine" \
		env FUZZ_WORKFLOW="$budget_wf" "$BUDGET"

	cp "$FUZZ_WF" "$budget_wf"
	sed -i "s/'schedule' && '[0-9]*[smh]'/'schedule' \&\& 'BOGUS'/" "$budget_wf"
	assert_exit 2 "an unparsable scheduled FUZZTIME is refused, not guessed" \
		env FUZZ_WORKFLOW="$budget_wf" "$BUDGET"

	assert_exit 2 "a missing workflow is an error" \
		env FUZZ_WORKFLOW="${budget_tmp}/nope.yml" "$BUDGET"

	rm -rf "$budget_tmp"

	# Sharding must be a PARTITION: every target in exactly one shard. A shard
	# assignment that drops a target loses coverage silently, and one that
	# duplicates a target just wastes budget.
	shard_tmp="$(mktemp -d)"
	"${SCRIPT_DIR}/fuzz.sh" --list 2>/dev/null | LC_ALL=C sort >"${shard_tmp}/full"
	: >"${shard_tmp}/union"
	for i in 1 2 3 4; do
		"${SCRIPT_DIR}/fuzz.sh" --list --shard "${i}/4" 2>/dev/null >>"${shard_tmp}/union"
	done
	LC_ALL=C sort "${shard_tmp}/union" >"${shard_tmp}/union.sorted"
	if diff -q "${shard_tmp}/full" "${shard_tmp}/union.sorted" >/dev/null; then
		ok "the 4 shards are a partition of the full target list (no target lost)"
	else
		bad "sharding is not a partition — targets are lost or duplicated"
		diff "${shard_tmp}/full" "${shard_tmp}/union.sorted" | sed 's/^/        | /'
	fi
	dupes=$(($(wc -l <"${shard_tmp}/union.sorted") - $(sort -u "${shard_tmp}/union.sorted" | wc -l)))
	if [ "$dupes" -eq 0 ]; then
		ok "no target is claimed by more than one shard"
	else
		bad "${dupes} target(s) appear in more than one shard"
	fi
	rm -rf "$shard_tmp"

	# A shard spec that cannot be read must not silently run a SUBSET and exit
	# 0 -- indistinguishable from a clean full sweep.
	for bad_spec in "0/4" "5/4" "abc" "1/0"; do
		assert_exit 2 "--shard ${bad_spec} is refused rather than guessed" \
			"${SCRIPT_DIR}/fuzz.sh" --list --shard "$bad_spec"
	done
	assert_exit 2 "more shards than targets is an error, not an empty green run" \
		"${SCRIPT_DIR}/fuzz.sh" --shard 99/99

	# Discovery is dynamic, so it can silently discover nothing -- which would
	# look exactly like a clean run.
	assert_exit 2 "discovering ZERO targets is an error, not a clean run" \
		env FUZZTIME=1s "$FUZZ" ./lint/...
	assert_contains "cannot fail" "the empty-discovery error explains itself" \
		env FUZZTIME=1s "$FUZZ" ./lint/...

	# The targets really are found. Listed once and asserted against, so the
	# package set is only compiled a single extra time.
	discovered="$(env FUZZTIME=1s "$FUZZ" --list 2>&1)"
	for want in FuzzParseProgram FuzzLexer FuzzScanner FuzzFormat FuzzMinifySource FuzzLoadJSON; do
		if echo "$discovered" | grep -q "$want"; then
			ok "discovery finds ${want}"
		else
			bad "discovery no longer finds ${want}"
		fi
	done

	# The headline assertion, and the reason FuzzGateSelfTest exists. A gate
	# that has only ever reported success is indistinguishable from a gate that
	# cannot fail -- precisely how the benchmark gate above stayed dead for 473
	# runs. FuzzGateSelfTest is a deliberately-failing target, inert unless
	# ELPS_FUZZ_GATE_SELFTEST is set, so this exercises the whole path end to
	# end: discovery, invocation, and the failure reaching the exit status.
	assert_exit 1 "an armed failing target makes fuzz.sh exit non-zero" \
		env ELPS_FUZZ_GATE_SELFTEST=1 FUZZTIME=5s FUZZMINIMIZETIME=1s \
		"$FUZZ" ./internal/fuzzseed/...
	assert_contains "FuzzGateSelfTest" "the failing target is named in the summary" \
		env ELPS_FUZZ_GATE_SELFTEST=1 FUZZTIME=5s FUZZMINIMIZETIME=1s \
		"$FUZZ" ./internal/fuzzseed/...
	# ...and the mirror: disarmed, the same package is clean. Without this, an
	# always-failing script would satisfy the assertion above for the wrong
	# reason.
	assert_exit 0 "the same target is inert when NOT armed" \
		env FUZZTIME=5s "$FUZZ" ./internal/fuzzseed/...

	# An armed run can leave a generated crasher in the source tree; it is
	# meaningless outside that run, so do not let it linger.
	rm -rf "${REPO_ROOT}/internal/fuzzseed/testdata/fuzz/FuzzGateSelfTest"
fi

echo
echo "== fuzz gate: workflow shape ============================================"

if [ -n "$(invoked_in "$FUZZ_WF" 'scripts/fuzz.sh')" ]; then
	ok "fuzz.yml INVOKES scripts/fuzz.sh (not just mentions it)"
else
	bad "fuzz.yml no longer invokes scripts/fuzz.sh — logic reinlined or removed?"
fi

if grep -qE '^\s*timeout-minutes:' "$FUZZ_WF"; then
	ok "fuzz.yml sets a job-level timeout-minutes backstop"
else
	bad "fuzz.yml has no timeout-minutes — the job has no outer bound"
fi

if grep -q 'FUZZTIME' "$FUZZ_WF"; then
	ok "fuzz.yml sets an explicit FUZZTIME budget"
else
	bad "fuzz.yml does not set FUZZTIME — the PR path would take the default"
fi

if [ -n "$(invoked_in "$FUZZ_WF" 'scripts/fuzz-budget-check.sh')" ]; then
	ok "fuzz.yml RUNS the derived budget check (timeout is not a remembered number)"
else
	bad "fuzz.yml does not run fuzz-budget-check.sh — timeout-minutes is unguarded again"
fi

# ONE REQUIRED CHECK PER WORKFLOW, AND IT MUST COVER EVERYTHING.
#
# Branch protection matches a required status check by NAME. Requiring the
# individual jobs means editing repo settings whenever a job is added, renamed
# or resharded -- and forgetting to leaves the new job unguarded while the board
# still reads fully green. Two concrete traps this repo has been one edit away
# from:
#
#   * sharding renamed "Fuzz targets" to "Fuzz shard i/4". Had the old name
#     been required, every PR would have become unmergeable, because a required
#     check that never reports blocks forever.
#   * adding a job to a workflow silently leaves it outside the required set.
#
# So every pull_request-triggered workflow carries exactly one fixed-name
# `Required: <area>` job, and that name is the only thing in branch protection.
# This guard enforces the two properties that make that safe:
#
#   1. the aggregate exists and its name is FIXED (no ${{ }} interpolation), and
#   2. it `needs` EVERY other job in its workflow -- otherwise the aggregate
#      goes green while an uncovered job is red, which is strictly worse than
#      having no aggregate at all, because it looks like coverage.
req_out="$(python3 - "$REPO_ROOT" <<'PY_INNER'
import glob, os, sys

root = sys.argv[1]
try:
    import yaml
except ImportError:
    print("__SKIP__ pyyaml unavailable")
    sys.exit(0)

MARKER = "Required:"
failures, passes = [], []

for f in sorted(glob.glob(os.path.join(root, ".github", "workflows", "*.y*ml"))):
    base = os.path.basename(f)
    try:
        doc = yaml.safe_load(open(f))
    except Exception:  # noqa: BLE001 -- the YAML-parse guard owns this
        continue
    if not isinstance(doc, dict):
        continue

    # Only a pull_request check can be a REQUIRED check, so release/tag
    # workflows are out of scope. `on` parses as boolean True in YAML 1.1.
    triggers = doc.get("on", doc.get(True)) or {}
    if isinstance(triggers, str):
        triggers = {triggers: None}
    if isinstance(triggers, list):
        triggers = {t: None for t in triggers}
    if "pull_request" not in triggers:
        continue

    jobs = {k: v for k, v in (doc.get("jobs") or {}).items() if isinstance(v, dict)}
    if not jobs:
        continue

    def jobname(jid):
        return str(jobs[jid].get("name") or jid)

    aggs = [j for j in jobs if jobname(j).startswith(MARKER)]
    if not aggs:
        failures.append(
            f"{base}: no fixed-name '{MARKER} ...' aggregate job. Every job in it would "
            f"have to be listed in branch protection by hand, and a job added later "
            f"would be unguarded while the board still looks green."
        )
        continue
    if len(aggs) > 1:
        failures.append(f"{base}: more than one '{MARKER} ...' job ({aggs}); exactly one is the check to require")
        continue

    agg = aggs[0]
    if "${{" in jobname(agg):
        failures.append(f"{base}: aggregate {agg!r} has an interpolated name ({jobname(agg)!r}); a required check's name must be fixed")
        continue

    needs = jobs[agg].get("needs")
    needs = [needs] if isinstance(needs, str) else list(needs or [])
    missing = sorted(set(jobs) - set(needs) - {agg})
    if missing:
        failures.append(
            f"{base}: aggregate {jobname(agg)!r} does not depend on {missing}. "
            f"Those jobs can fail while the one required check goes green."
        )
    else:
        passes.append(f"{base}: '{jobname(agg)}' covers all {len(jobs) - 1} other job(s)")

    # `needs` alone SKIPS the aggregate when an upstream job fails, and a
    # skipped required check reads as green, so always() is load-bearing.
    if str(jobs[agg].get("if", "")).find("always()") < 0:
        failures.append(f"{base}: aggregate {jobname(agg)!r} lacks `if: always()`; it would be SKIPPED on upstream failure, and a skipped required check reads as green")

for p_ in passes:
    print(f"PASS  {p_}")
for f_ in failures:
    print(f"FAIL  {f_}")
print(f"__COUNTS__ {len(passes)} {len(failures)}")
PY_INNER
)"
case "$req_out" in
	__SKIP__*) echo "SKIP  required-check aggregate guard ($req_out)" ;;
	*)
		echo "$req_out" | grep -v '^__COUNTS__' || true
		req_counts="$(echo "$req_out" | sed -n 's/^__COUNTS__ //p')"
		if [ -n "$req_counts" ]; then
			read -r rq_pass rq_fail <<<"$req_counts"
			pass=$((pass + rq_pass))
			fail=$((fail + rq_fail))
		else
			bad "required-check aggregate guard did not run"
		fi
		;;
esac

wf_out="$(python3 - "$REPO_ROOT" <<'PY'
import glob, os, re, sys

root = sys.argv[1]
failures, passes = [], []

files = sorted(glob.glob(os.path.join(root, ".github", "workflows", "*.yml")) +
               glob.glob(os.path.join(root, ".github", "workflows", "*.yaml")))
if not files:
    failures.append("no workflow files found")

try:
    import yaml
    docs = {}
    for f in files:
        try:
            docs[f] = yaml.safe_load(open(f))
        except Exception as e:  # noqa: BLE001
            failures.append(f"{os.path.basename(f)} does not parse: {e}")
    if not failures:
        passes.append(f"all {len(files)} workflow files parse as YAML")

    # No two workflows may publish the same top-level `name:` -- duplicate names
    # produce indistinguishable check contexts, which lets a trivially-green
    # workflow impersonate a real gate.
    names = {}
    for f, doc in docs.items():
        if isinstance(doc, dict) and doc.get("name"):
            names.setdefault(doc["name"], []).append(os.path.basename(f))
    dupes = {n: v for n, v in names.items() if len(v) > 1}
    for n, v in dupes.items():
        failures.append(f"duplicate workflow name {n!r} in {', '.join(v)}")
    if not dupes:
        passes.append("no two workflows declare the same top-level name")
except ImportError:
    passes.append("(pyyaml unavailable; YAML-parse guards skipped)")

# Every third-party `uses:` should be SHA-pinned (local ./ actions are exempt).
# CONTRACT.md for this check: the repo pinned its actions in f5b12e3 ("chore: pin
# GitHub Actions to SHAs and add Dependabot config"), but two workflows added
# later drifted back to floating tags. Those are pre-existing and out of scope
# for the benchmark-gate fix, so they are ALLOWLISTED rather than silently
# ignored -- they print as WARN and the list may only ever shrink. Any NEW
# unpinned action, or any unpinned action in a workflow this change owns, is a
# hard failure.
UNPINNED_ALLOWLIST = {
    # file: reason (delete the row once the workflow is pinned)
    "govulncheck.yml": "pre-existing, added in #303; not touched by this change",
    "release-tag.yml": "pre-existing, added in #302; not touched by this change",
}
sha = re.compile(r"^[0-9a-f]{40}$")
unpinned, warned = [], []
for f in files:
    base = os.path.basename(f)
    for i, line in enumerate(open(f), 1):
        m = re.search(r"uses:\s*(\S+)", line)
        if not m:
            continue
        ref = m.group(1)
        if ref.startswith("./"):
            continue
        if "@" not in ref or not sha.match(ref.rsplit("@", 1)[1]):
            (warned if base in UNPINNED_ALLOWLIST else unpinned).append(f"{base}:{i} {ref}")
for u in unpinned:
    failures.append(f"unpinned action: {u}")
if not unpinned:
    passes.append(f"every non-allowlisted `uses:` is pinned to a 40-char commit SHA "
                  f"({len(warned)} allowlisted)")
for w in warned:
    print(f"WARN  unpinned action (allowlisted, should be pinned): {w}")

for p in passes:
    print(f"PASS  {p}")
for f_ in failures:
    print(f"FAIL  {f_}")
print(f"__COUNTS__ {len(passes)} {len(failures)}")
PY
)"

echo "$wf_out" | grep -v '^__COUNTS__' || true
wf_counts="$(echo "$wf_out" | sed -n 's/^__COUNTS__ //p')"
if [ -n "$wf_counts" ]; then
	read -r wf_pass wf_fail <<<"$wf_counts"
	pass=$((pass + wf_pass))
	fail=$((fail + wf_fail))
else
	bad "workflow shape guards did not run"
fi

echo
echo "== fuzz gate: time bounding =============================================="

FUZZ="${SCRIPT_DIR}/fuzz.sh"
FUZZ_WF="${REPO_ROOT}/.github/workflows/fuzz.yml"

# `go test -fuzz` has no default limit: without -fuzztime it runs until
# something kills it. Every one of these guards exists to keep that from
# reaching CI.

assert_exit 2 "an unparsable FUZZTIME is refused rather than guessed" \
	env FUZZTIME=forever "$FUZZ" --list
assert_contains "refusing to run unbounded" \
	"the refusal says why" \
	env FUZZTIME=forever "$FUZZ" --list
assert_exit 2 "a bare number (no unit) is refused" \
	env FUZZTIME=60 "$FUZZ" --list
assert_exit 2 "an empty FUZZTIME is refused" \
	env FUZZTIME= "$FUZZ" --list

# Every LIVE `go test -fuzz` in this repository must carry -fuzztime. Logical
# lines, not physical ones: the invocation in fuzz.sh spreads its flags over a
# backslash continuation, and a per-line scan would read the `-fuzz` line as
# unbounded. Anchored on `go test` so that prose, this checker's own source,
# and workflow comments are not mistaken for invocations.
unbounded="$(python3 - "$REPO_ROOT" <<'PY'
import glob, os, re, sys
root = sys.argv[1]
files = sorted(
    glob.glob(os.path.join(root, ".github", "workflows", "*.y*ml"))
    + glob.glob(os.path.join(root, "scripts", "*.sh"))
    + [os.path.join(root, "Makefile")]
)
hits = []
for f in files:
    if not os.path.exists(f):
        continue
    logical, buf, start = [], "", 1
    for i, line in enumerate(open(f), 1):
        if line.lstrip().startswith("#"):
            continue
        code = line.split("#", 1)[0].rstrip("\n")
        if not buf:
            start = i
        if code.rstrip().endswith("\\"):
            buf += code.rstrip()[:-1] + " "
            continue
        logical.append((start, buf + code))
        buf = ""
    if buf:
        logical.append((start, buf))
    for lineno, code in logical:
        if not re.search(r"\bgo\s+test\b", code):
            continue
        if not re.search(r"(^|\s)-fuzz(\s|=|$)", code):
            continue
        if "-fuzztime" not in code:
            hits.append(f"{os.path.relpath(f, root)}:{lineno}")
print("\n".join(hits))
PY
)"
if [ -n "$unbounded" ]; then
	bad "a 'go test -fuzz' invocation has no -fuzztime bound: $unbounded"
else
	ok "every live 'go test -fuzz' invocation is bounded by -fuzztime"
fi

echo
echo "== fuzz gate: discovery and proof it can FAIL ============================"

# Everything from here needs a Go toolchain matching go.mod. The `gates` job in
# benchmark.yml deliberately has none -- it is the fast, build-free gate that
# must report even when the benchmark job is cancelled -- so it sets
# CI_GATES_SKIP_GO and these run in the `fuzz` workflow instead, which already
# has Go set up and its module cache warm.
if [ "${CI_GATES_SKIP_GO:-0}" = "1" ] || ! command -v go >/dev/null 2>&1; then
	echo "SKIP  Go-backed fuzz-gate checks (CI_GATES_SKIP_GO=${CI_GATES_SKIP_GO:-0}," \
		"go present: $(command -v go >/dev/null 2>&1 && echo yes || echo no))"
	echo "SKIP  they run in .github/workflows/fuzz.yml, which sets up Go"
else
	# The derived budget gate. It reads real discovery plus the workflow, so
	# it needs Go; the negative controls below drive it at a scratch copy of
	# the workflow via FUZZ_WORKFLOW.
	BUDGET="${SCRIPT_DIR}/fuzz-budget-check.sh"
	assert_exit 0 "the nightly sweep fits its timeout as configured today" \
		"$BUDGET"

	budget_tmp="$(mktemp -d)"
	budget_wf="${budget_tmp}/fuzz.yml"

	# Each control reverts ONE property of the real workflow. A budget gate
	# that has never been watched failing is worth nothing -- the value it
	# guards went stale twice before this existed (120 sized for 10 targets
	# when there were 12; 140 vs 165 when two branches each counted only their
	# own additions).
	cp "$FUZZ_WF" "$budget_wf"
	sed -i 's/^    timeout-minutes: [0-9]*$/    timeout-minutes: 5/' "$budget_wf"
	assert_exit 1 "a timeout too small for the sweep FAILS" \
		env FUZZ_WORKFLOW="$budget_wf" "$BUDGET"

	cp "$FUZZ_WF" "$budget_wf"
	sed -i 's/^        shard: \[.*\]$/        shard: [1]/' "$budget_wf"
	assert_exit 1 "unsharding (one shard, whole sweep serial) FAILS" \
		env FUZZ_WORKFLOW="$budget_wf" "$BUDGET"

	cp "$FUZZ_WF" "$budget_wf"
	sed -i '/^    timeout-minutes: [0-9]*$/d' "$budget_wf"
	assert_exit 2 "a fuzz job with NO timeout-minutes is unreadable, not fine" \
		env FUZZ_WORKFLOW="$budget_wf" "$BUDGET"

	cp "$FUZZ_WF" "$budget_wf"
	sed -i "s/'schedule' && '[0-9]*[smh]'/'schedule' \&\& 'BOGUS'/" "$budget_wf"
	assert_exit 2 "an unparsable scheduled FUZZTIME is refused, not guessed" \
		env FUZZ_WORKFLOW="$budget_wf" "$BUDGET"

	assert_exit 2 "a missing workflow is an error" \
		env FUZZ_WORKFLOW="${budget_tmp}/nope.yml" "$BUDGET"

	rm -rf "$budget_tmp"

	# Sharding must be a PARTITION: every target in exactly one shard. A shard
	# assignment that drops a target loses coverage silently, and one that
	# duplicates a target just wastes budget.
	shard_tmp="$(mktemp -d)"
	"${SCRIPT_DIR}/fuzz.sh" --list 2>/dev/null | LC_ALL=C sort >"${shard_tmp}/full"
	: >"${shard_tmp}/union"
	for i in 1 2 3 4; do
		"${SCRIPT_DIR}/fuzz.sh" --list --shard "${i}/4" 2>/dev/null >>"${shard_tmp}/union"
	done
	LC_ALL=C sort "${shard_tmp}/union" >"${shard_tmp}/union.sorted"
	if diff -q "${shard_tmp}/full" "${shard_tmp}/union.sorted" >/dev/null; then
		ok "the 4 shards are a partition of the full target list (no target lost)"
	else
		bad "sharding is not a partition — targets are lost or duplicated"
		diff "${shard_tmp}/full" "${shard_tmp}/union.sorted" | sed 's/^/        | /'
	fi
	dupes=$(($(wc -l <"${shard_tmp}/union.sorted") - $(sort -u "${shard_tmp}/union.sorted" | wc -l)))
	if [ "$dupes" -eq 0 ]; then
		ok "no target is claimed by more than one shard"
	else
		bad "${dupes} target(s) appear in more than one shard"
	fi
	rm -rf "$shard_tmp"

	# A shard spec that cannot be read must not silently run a SUBSET and exit
	# 0 -- indistinguishable from a clean full sweep.
	for bad_spec in "0/4" "5/4" "abc" "1/0"; do
		assert_exit 2 "--shard ${bad_spec} is refused rather than guessed" \
			"${SCRIPT_DIR}/fuzz.sh" --list --shard "$bad_spec"
	done
	assert_exit 2 "more shards than targets is an error, not an empty green run" \
		"${SCRIPT_DIR}/fuzz.sh" --shard 99/99

	# Discovery is dynamic, so it can silently discover nothing -- which would
	# look exactly like a clean run.
	assert_exit 2 "discovering ZERO targets is an error, not a clean run" \
		env FUZZTIME=1s "$FUZZ" ./lint/...
	assert_contains "cannot fail" "the empty-discovery error explains itself" \
		env FUZZTIME=1s "$FUZZ" ./lint/...

	# The targets really are found. Listed once and asserted against, so the
	# package set is only compiled a single extra time.
	discovered="$(env FUZZTIME=1s "$FUZZ" --list 2>&1)"
	for want in FuzzParseProgram FuzzLexer FuzzScanner FuzzFormat FuzzMinifySource FuzzLoadJSON; do
		if echo "$discovered" | grep -q "$want"; then
			ok "discovery finds ${want}"
		else
			bad "discovery no longer finds ${want}"
		fi
	done

	# The headline assertion, and the reason FuzzGateSelfTest exists. A gate
	# that has only ever reported success is indistinguishable from a gate that
	# cannot fail -- precisely how the benchmark gate above stayed dead for 473
	# runs. FuzzGateSelfTest is a deliberately-failing target, inert unless
	# ELPS_FUZZ_GATE_SELFTEST is set, so this exercises the whole path end to
	# end: discovery, invocation, and the failure reaching the exit status.
	assert_exit 1 "an armed failing target makes fuzz.sh exit non-zero" \
		env ELPS_FUZZ_GATE_SELFTEST=1 FUZZTIME=5s FUZZMINIMIZETIME=1s \
		"$FUZZ" ./internal/fuzzseed/...
	assert_contains "FuzzGateSelfTest" "the failing target is named in the summary" \
		env ELPS_FUZZ_GATE_SELFTEST=1 FUZZTIME=5s FUZZMINIMIZETIME=1s \
		"$FUZZ" ./internal/fuzzseed/...
	# ...and the mirror: disarmed, the same package is clean. Without this, an
	# always-failing script would satisfy the assertion above for the wrong
	# reason.
	assert_exit 0 "the same target is inert when NOT armed" \
		env FUZZTIME=5s "$FUZZ" ./internal/fuzzseed/...

	# An armed run can leave a generated crasher in the source tree; it is
	# meaningless outside that run, so do not let it linger.
	rm -rf "${REPO_ROOT}/internal/fuzzseed/testdata/fuzz/FuzzGateSelfTest"
fi

echo
echo "== fuzz gate: workflow shape ============================================"

if [ -n "$(invoked_in "$FUZZ_WF" 'scripts/fuzz.sh')" ]; then
	ok "fuzz.yml INVOKES scripts/fuzz.sh (not just mentions it)"
else
	bad "fuzz.yml no longer invokes scripts/fuzz.sh — logic reinlined or removed?"
fi

if grep -qE '^\s*timeout-minutes:' "$FUZZ_WF"; then
	ok "fuzz.yml sets a job-level timeout-minutes backstop"
else
	bad "fuzz.yml has no timeout-minutes — the job has no outer bound"
fi

if grep -q 'FUZZTIME' "$FUZZ_WF"; then
	ok "fuzz.yml sets an explicit FUZZTIME budget"
else
	bad "fuzz.yml does not set FUZZTIME — the PR path would take the default"
fi

if [ -n "$(invoked_in "$FUZZ_WF" 'scripts/fuzz-budget-check.sh')" ]; then
	ok "fuzz.yml RUNS the derived budget check (timeout is not a remembered number)"
else
	bad "fuzz.yml does not run fuzz-budget-check.sh — timeout-minutes is unguarded again"
fi

# A required status check is matched by NAME. A job whose name interpolates a
# matrix value changes name when the matrix is resized, so if such a name were
# ever added to branch protection, resizing the matrix would leave a required
# check that can never report and every PR would be unmergeable until someone
# edited repo settings. Every matrix job therefore needs a fixed-name
# aggregate job downstream of it, and that is the name to require.
stable_out="$(python3 - "$REPO_ROOT" <<'PY_INNER'
import glob, os, sys

root = sys.argv[1]
try:
    import yaml
except ImportError:
    print("__SKIP__ pyyaml unavailable")
    sys.exit(0)

failures, passes = [], []
for f in sorted(glob.glob(os.path.join(root, ".github", "workflows", "*.y*ml"))):
    base = os.path.basename(f)
    try:
        doc = yaml.safe_load(open(f))
    except Exception:  # noqa: BLE001
        continue
    if not isinstance(doc, dict):
        continue
    # Only a check that runs on pull_request can be listed as a REQUIRED
    # status check, so a tag/release workflow's matrix names cannot cause the
    # rename trap. `on` parses as the boolean True in YAML 1.1.
    triggers = doc.get("on", doc.get(True)) or {}
    if isinstance(triggers, str):
        triggers = {triggers: None}
    if isinstance(triggers, list):
        triggers = {t: None for t in triggers}
    if "pull_request" not in triggers:
        continue

    jobs = doc.get("jobs") or {}
    for jid, job in jobs.items():
        if not isinstance(job, dict):
            continue
        if not (job.get("strategy") or {}).get("matrix"):
            continue
        name = str(job.get("name") or jid)
        if "${{" not in name:
            passes.append(f"{base}: matrix job {jid!r} has a fixed name; safe to require directly")
            continue
        # find a fixed-name job that depends on this one
        def needs_of(j):
            n = j.get("needs")
            if isinstance(n, str):
                return [n]
            return list(n or [])
        agg = [f"{k}" for k, v in jobs.items()
               if isinstance(v, dict) and jid in needs_of(v)
               and "${{" not in str(v.get("name") or k)]
        if agg:
            passes.append(f"{base}: matrix job {jid!r} is fronted by fixed-name job(s) {agg}")
        else:
            failures.append(
                f"{base}: matrix job {jid!r} has an interpolated name ({name!r}) and no "
                f"fixed-name aggregate job depends on it. Resizing the matrix renames the "
                f"check, and a renamed REQUIRED check can never report."
            )

for p in passes:
    print(f"PASS  {p}")
for f_ in failures:
    print(f"FAIL  {f_}")
print(f"__COUNTS__ {len(passes)} {len(failures)}")
PY_INNER
)"
case "$stable_out" in
	__SKIP__*) echo "SKIP  stable-check-name guard ($stable_out)" ;;
	*)
		echo "$stable_out" | grep -v '^__COUNTS__' || true
		stable_counts="$(echo "$stable_out" | sed -n 's/^__COUNTS__ //p')"
		if [ -n "$stable_counts" ]; then
			read -r st_pass st_fail <<<"$stable_counts"
			pass=$((pass + st_pass))
			fail=$((fail + st_fail))
		else
			bad "stable-check-name guard did not run"
		fi
		;;
esac

echo
echo "== shell lint on the scripts this suite owns ============================="

OWNED_SCRIPTS=(
	"${SCRIPT_DIR}/bench-arms-check.sh"
	"${SCRIPT_DIR}/benchstat-gate.sh"
	"${SCRIPT_DIR}/ci-gates-test.sh"
	"${SCRIPT_DIR}/fuzz-budget-check.sh"
	"${SCRIPT_DIR}/fuzz.sh"
)

for s in "${OWNED_SCRIPTS[@]}"; do
	if bash -n "$s" 2>/dev/null; then
		ok "bash -n $(basename "$s")"
	else
		bad "bash -n $(basename "$s")"
		bash -n "$s" 2>&1 | sed 's/^/        | /'
	fi
done

if command -v shellcheck >/dev/null 2>&1; then
	if sc_out="$(shellcheck -S warning "${OWNED_SCRIPTS[@]}" 2>&1)"; then
		ok "shellcheck -S warning clean on all ${#OWNED_SCRIPTS[@]} scripts"
	else
		bad "shellcheck -S warning reported findings"
		echo "$sc_out" | sed 's/^/        | /'
	fi
else
	echo "SKIP  shellcheck not installed"
fi

echo
echo "=========================================================================="
echo "ci-gates-test: ${pass} passed, ${fail} failed"
if [ "$fail" -gt 0 ]; then
	exit 1
fi
exit 0
