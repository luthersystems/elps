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

# Every benchmark-baseline cache key must be namespaced by runner.arch.
#
# benchstat keys its configuration off the goos/goarch/cpu headers `go test`
# emits. Restore a baseline recorded on different silicon and it does not
# produce a caveated comparison -- it produces two standalone one-column
# tables with ZERO comparison rows, which benchstat-gate.sh reports as exit 2
# and the workflow turns into a hard failure. An un-namespaced restore-keys
# prefix is enough on its own: it matches the most recent baseline of ANY
# architecture.
#
# This is not theoretical. The keys were bare `benchmark-baseline-` when the
# benchmark job moved from amd64 ubuntu-latest to the ARM pool, and every PR
# would have failed that way until a push to main happened to regenerate a
# baseline. Guarding the whole class rather than that one instance: any key or
# restore-keys line naming a benchmark baseline must carry runner.arch.
bad_keys="$(python3 - "$BENCH_WF" <<'PY'
import re, sys
path = sys.argv[1]
bad = []
for i, line in enumerate(open(path), 1):
    stripped = line.lstrip()
    if stripped.startswith("#"):          # comments ABOUT the keys are fine
        continue
    m = re.match(r"(key|restore-keys):\s*(\S.*)$", stripped)
    if not m:
        continue
    value = m.group(2).strip()
    if "benchmark-baseline-" not in value:
        continue
    if "runner.arch" not in value:
        bad.append(f"{i}: {stripped.rstrip()}")
print("\n".join(bad))
PY
)"
if [ -n "$bad_keys" ]; then
	bad "benchmark baseline cache key is not namespaced by runner.arch — a cross-architecture baseline would be restored and fail the gate as 'cannot interpret': $bad_keys"
else
	ok "every benchmark-baseline cache key is namespaced by runner.arch"
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
echo "== shell lint on the scripts this suite owns ============================="

OWNED_SCRIPTS=(
	"${SCRIPT_DIR}/benchstat-gate.sh"
	"${SCRIPT_DIR}/ci-gates-test.sh"
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
