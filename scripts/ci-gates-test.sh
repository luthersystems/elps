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
echo "== benchstat-gate: reviewed waivers ======================================"

# A waiver is a per-row exception, declared in scripts/benchstat-waivers.txt and
# reviewed in the diff that needs it. It is the one construct in this gate whose
# whole job is to make something PASS, so it is the one that most needs proving
# it cannot make the wrong thing pass. Everything here is driven against
# benchstat-libjson-encode-411.txt -- the REAL comparison from PR #411, verbatim
# from the workflow's own PR comment -- rather than a synthetic table, because a
# waiver that only works on a fixture written to suit it proves nothing.
WAIVED_FIXTURE="${TESTDATA}/benchstat-libjson-encode-411.txt"

# The before half of the round trip. With waivers switched off, the real run has
# TWO rows at or above a gate. Note that this is not the "one failing row" the
# change was described as: B/op is an ALLOCATION metric, judged against the 5%
# allocation gate rather than the 15% timing one, and +12.45% is over it.
assert_exit 1 "PR #411's REAL benchstat output fires the gate with no waivers" \
	env BENCH_WAIVERS= "$GATE" "$WAIVED_FIXTURE"
assert_contains "+7.94%" "the allocs/op row is named when unwaived" \
	env BENCH_WAIVERS= "$GATE" "$WAIVED_FIXTURE"
assert_contains "+12.45%" "the B/op row is named too — it is over the allocation gate as well" \
	env BENCH_WAIVERS= "$GATE" "$WAIVED_FIXTURE"

# The after half: the waivers this repository actually ships, with no env
# override at all, make that same comparison pass. This is the assertion that
# keeps scripts/benchstat-waivers.txt honest -- it fails if the file is deleted,
# emptied, malformed, or edited so it no longer covers what it claims to.
assert_exit 0 "the SHIPPED waiver file makes PR #411's real comparison pass" \
	"$GATE" "$WAIVED_FIXTURE"

# ...and it passes because it was WAIVED, not because the gate stopped looking.
# The row must still appear, with its delta, its ceiling and its issue.
# Anchored on the per-ROW marker, not the bare word: the summary line already
# says "row(s) WAIVED", so a substring check for "WAIVED" stays green even after
# the per-row lines are deleted -- which is exactly the regression that matters.
assert_contains "WAIVED      github.com/luthersystems/elps/lisp/lisplib/libjson" \
	"a waived row is still reported by name, not silently dropped" \
	"$GATE" "$WAIVED_FIXTURE"
assert_contains "+7.94%" "the waived row still carries its measured delta" \
	"$GATE" "$WAIVED_FIXTURE"
assert_contains "elps#412" "the waived row names its tracking issue in the report" \
	"$GATE" "$WAIVED_FIXTURE"
assert_contains "row(s) WAIVED" "the summary line counts the waivers" \
	"$GATE" "$WAIVED_FIXTURE"

# NARROWNESS. A waiver covers one package, one benchmark, one metric column.
# Waiving allocs/op alone must leave B/op of the SAME benchmark failing --
# otherwise "per-row" is a description rather than a property.
assert_exit 1 "waiving allocs/op does NOT waive B/op of the same benchmark" \
	env BENCH_WAIVERS="${TESTDATA}/waivers-libjson-allocs.txt" "$GATE" "$WAIVED_FIXTURE"
assert_contains "B/op" "the un-waived neighbouring metric is the row that fails" \
	env BENCH_WAIVERS="${TESTDATA}/waivers-libjson-allocs.txt" "$GATE" "$WAIVED_FIXTURE"
# Right benchmark, right metric, WRONG package: must not reach across packages.
assert_exit 1 "a waiver for another package does not reach libjson" \
	env BENCH_WAIVERS="${TESTDATA}/waivers-wrong-pkg.txt" "$GATE" "$WAIVED_FIXTURE"
# And the control for the two above: with both columns waived it does pass, so
# the failures above are the narrowness and not some unrelated breakage.
assert_exit 0 "with both allocation columns waived, the same table passes" \
	env BENCH_WAIVERS="${TESTDATA}/waivers-libjson-both.txt" "$GATE" "$WAIVED_FIXTURE"

# BOUNDEDNESS. The ceiling is what makes a waiver an accepted COST rather than a
# blessed benchmark: the moment the regression grows past what was reviewed, it
# fails again.
assert_exit 1 "a regression that EXCEEDS its waiver ceiling fails" \
	env BENCH_WAIVERS="${TESTDATA}/waivers-libjson-tight-ceiling.txt" "$GATE" "$WAIVED_FIXTURE"
assert_contains "EXCEEDS its waiver ceiling" \
	"outgrowing a waiver says so, rather than reading as a fresh regression" \
	env BENCH_WAIVERS="${TESTDATA}/waivers-libjson-tight-ceiling.txt" "$GATE" "$WAIVED_FIXTURE"

# EXPIRY. Past its date a waiver stops suppressing and the row is judged
# normally again, so the decision is re-made rather than inherited.
assert_exit 1 "an EXPIRED waiver no longer suppresses its row" \
	env BENCH_WAIVERS="${TESTDATA}/waivers-expired.txt" "$GATE" "$WAIVED_FIXTURE"
assert_contains "WAIVER EXPIRED" "an expired waiver says why the row came back" \
	env BENCH_WAIVERS="${TESTDATA}/waivers-expired.txt" "$GATE" "$WAIVED_FIXTURE"
# The same proof against the waivers this repo actually ships: wind the clock
# past their expiry and PR #411's comparison reds again. Without this, "expires"
# could be a field nothing reads.
assert_exit 1 "the SHIPPED waivers genuinely expire (clock wound past the date)" \
	env BENCH_WAIVER_TODAY=2099-01-01 "$GATE" "$WAIVED_FIXTURE"

# JUSTIFICATION. A waiver with no tracking reference is a threshold increase
# with better manners; the gate must refuse to run rather than honour it. Note
# the exit code: 2, the same "cannot be interpreted" hard failure as an
# unreadable benchstat table, because a waiver list that does not parse must
# never be treated as an empty one.
assert_exit 2 "a waiver with NO issue reference is rejected" \
	env BENCH_WAIVERS="${TESTDATA}/waivers-no-issue.txt" "$GATE" "$WAIVED_FIXTURE"
assert_contains "not a tracking reference" "the rejection says what is missing" \
	env BENCH_WAIVERS="${TESTDATA}/waivers-no-issue.txt" "$GATE" "$WAIVED_FIXTURE"

# Every other malformation is the same hard failure, and each is NAMED with its
# line number -- a waiver that silently fails to parse is a regression nobody is
# told about.
assert_exit 2 "malformed waiver entries are refused, not skipped" \
	env BENCH_WAIVERS="${TESTDATA}/waivers-malformed.txt" "$GATE" "$WAIVED_FIXTURE"
for want in "expected 7 |-separated fields" "is not a positive percentage" \
	"is not a YYYY-MM-DD date" "reason is missing or too short" \
	"empty pkg field"; do
	assert_contains "$want" "malformed waiver diagnosed: ${want}" \
		env BENCH_WAIVERS="${TESTDATA}/waivers-malformed.txt" "$GATE" "$WAIVED_FIXTURE"
done

# `go test` appends -<GOMAXPROCS> to every benchmark name, so a waiver written
# with the suffix would silently unbind the day `runs-on` or the GOMAXPROCS pin
# changes -- the single failure mode this repository has been bitten by most
# (see the GOMAXPROCS notes in benchmark.yml and bench-arms-check.sh). Rejected
# at parse time rather than left to fail open years later.
assert_exit 2 "a waiver naming Encode-2 (with the GOMAXPROCS suffix) is rejected" \
	env BENCH_WAIVERS="${TESTDATA}/waivers-gomaxprocs-suffix.txt" "$GATE" "$WAIVED_FIXTURE"
assert_contains "GOMAXPROCS" "the suffix rejection explains the trap" \
	env BENCH_WAIVERS="${TESTDATA}/waivers-gomaxprocs-suffix.txt" "$GATE" "$WAIVED_FIXTURE"

# An explicitly-named waiver file that is not there is an error. Silently
# adjudicating with no waivers would be the strict direction, but it would also
# mean a typo'd path reads as "no waivers configured".
assert_exit 2 "BENCH_WAIVERS pointing at a missing file is an error" \
	env BENCH_WAIVERS="${TESTDATA}/waivers-does-not-exist.txt" "$GATE" "$WAIVED_FIXTURE"

# STALENESS. A waiver that protects nothing must not rot quietly: the benchmark
# it names was renamed or removed, so it is dead weight that still looks like
# coverage.
assert_contains "WAIVER-STALE" "a waiver whose benchmark no longer exists is REPORTED" \
	env BENCH_WAIVERS="${TESTDATA}/waivers-stale.txt" "$GATE" "$WAIVED_FIXTURE"
assert_contains "WAIVER-STALE" "a waiver aimed at the wrong package is reported as stale too" \
	env BENCH_WAIVERS="${TESTDATA}/waivers-wrong-pkg.txt" "$GATE" "$WAIVED_FIXTURE"
# ...and the softer half: the row is there and simply is not regressing, which
# is the signal to delete the entry rather than carry it forever.
assert_contains "waiver-unused" "a waiver whose row is no longer regressing is REPORTED" \
	env BENCH_WAIVERS="${TESTDATA}/waivers-unused.txt" "$GATE" "$WAIVED_FIXTURE"
# Reporting a stale waiver must not, by itself, turn a clean comparison red --
# otherwise a renamed benchmark reds every PR until someone edits a file, and
# the pressure is to delete the mechanism rather than the entry.
assert_exit 0 "a stale waiver is reported but does not fail an otherwise clean run" \
	env BENCH_WAIVERS="${TESTDATA}/waivers-stale.txt" "$GATE" "${TESTDATA}/benchstat-clean-ci.txt"

# THE HOLE THIS COULD HAVE BEEN. The shipped waiver file must not rescue any of
# the fixtures the gate is supposed to fail on. If a waiver ever widens into
# something that matches broadly, this is where it shows up.
for fx in benchstat-regression-new benchstat-regression-old benchstat-task-sample \
	benchstat-alloc-regression benchstat-alloc-regression-live \
	benchstat-bps-regression benchstat-noisy-sandbox; do
	assert_exit 1 "the shipped waivers do NOT rescue ${fx}" \
		"$GATE" "${TESTDATA}/${fx}.txt"
done
assert_exit 2 "the shipped waivers do NOT turn an uninterpretable table green" \
	"$GATE" "${TESTDATA}/benchstat-crash.txt"

echo
echo "== benchstat-gate: the resolution check (#443) ==========================="

# A threshold is one number for a whole metric class, and it is only as good as
# the assumption that rows in that class have comparable noise. On elps' timing
# rows they do not: BenchmarkPackageGetFunParallel is a sub-100ns map lookup
# under RunParallel, measured at -benchtime=100ms, and it has a ±24% spread on
# IDENTICAL code -- above the 15% gate. It red PR #442, a parser-only change
# that cannot reach it, and a re-run with no code change turned it green.
#
# So a timing row at or above its gate is called a regression only when the move
# is bigger than the spread benchstat measured for that row, on those samples.
# Everything below is the pair that has to hold together: the fix must silence
# the noise AND still fire on a real move, or it is just the gate switched off.
NOISE_FIXTURE="${TESTDATA}/benchstat-parallel-noise-443.txt"
TRUE_FIXTURE="${TESTDATA}/benchstat-parallel-true-regression.txt"

# The noise-only half: #443's row, +15.96% p=0.035 over a 15% gate, with the
# ±24%/±25% spread the null comparison measured.
assert_exit 0 "a timing move INSIDE the row's own measured spread is not a regression" \
	env BENCH_WAIVERS= "$GATE" "$NOISE_FIXTURE"
# ...and it is NOT silence. A benchmark that cannot be adjudicated is a standing
# problem with the benchmark, and a gate that quietly drops rows is the exact
# defect this script exists to prevent.
assert_contains "NOISE-FLOOR" "the unresolvable row is REPORTED, not dropped" \
	env BENCH_WAIVERS= "$GATE" "$NOISE_FIXTURE"
assert_contains "+15.96%" "the unresolvable row still carries its measured delta" \
	env BENCH_WAIVERS= "$GATE" "$NOISE_FIXTURE"
assert_contains "spread ±25%" "the report names the spread it was judged against" \
	env BENCH_WAIVERS= "$GATE" "$NOISE_FIXTURE"
assert_contains "cannot resolve them" "the summary line counts unresolvable rows" \
	env BENCH_WAIVERS= "$GATE" "$NOISE_FIXTURE"

# The true-regression half. Same benchmark, same spread, same flat allocation
# columns; only the size of the move differs. If this ever stops failing, the
# resolution check has become an off switch.
assert_exit 1 "a timing move LARGER than the row's spread is still a regression" \
	env BENCH_WAIVERS= "$GATE" "$TRUE_FIXTURE"
assert_contains "REGRESSION" "the real move is reported as a regression" \
	env BENCH_WAIVERS= "$GATE" "$TRUE_FIXTURE"
assert_contains "+48.00%" "the real move is reported with its delta" \
	env BENCH_WAIVERS= "$GATE" "$TRUE_FIXTURE"

# The check is about RESOLUTION, not about size: the same +48% row is a
# regression at any threshold below it, and the +15.96% row is suppressed only
# because its spread is larger than the move -- not because 16% moves are now
# allowed anywhere.
assert_exit 0 "the noise row stays unresolvable even with the gate lowered to 1%" \
	env BENCH_WAIVERS= BENCH_REGRESSION_THRESHOLD_PCT=1 "$GATE" "$NOISE_FIXTURE"
assert_contains "NOISE-FLOOR" "...and says so, rather than passing silently" \
	env BENCH_WAIVERS= BENCH_REGRESSION_THRESHOLD_PCT=1 "$GATE" "$NOISE_FIXTURE"

# CLASS BOUNDARY. Allocation metrics are exempt, explicitly. They are exact
# rather than sampled and they have caught every real regression this gate has
# caught; the exemption must not depend on their spread happening to be 0%.
# One fixture, three rows, same delta and same spread, differing only in class.
ALLOC_SPREAD_FIXTURE="${TESTDATA}/benchstat-alloc-with-spread.txt"
assert_exit 1 "an ALLOCATION row is judged on its threshold even with a large spread" \
	env BENCH_WAIVERS= "$GATE" "$ALLOC_SPREAD_FIXTURE"
assert_contains "REGRESSION  github.com/luthersystems/elps/lisp             B/op" \
	"the B/op row with a ±30% spread is still a regression" \
	env BENCH_WAIVERS= "$GATE" "$ALLOC_SPREAD_FIXTURE"
assert_contains "REGRESSION  github.com/luthersystems/elps/lisp             allocs/op" \
	"the allocs/op row with a ±30% spread is still a regression" \
	env BENCH_WAIVERS= "$GATE" "$ALLOC_SPREAD_FIXTURE"
assert_contains "NOISE-FLOOR github.com/luthersystems/elps/lisp             sec/op" \
	"...while the sec/op row with the SAME delta and spread is not" \
	env BENCH_WAIVERS= "$GATE" "$ALLOC_SPREAD_FIXTURE"

# WHEN THERE IS NO INTERVAL. benchstat prints "± ∞ ¹" below 6 samples, so there
# is no resolution to check against. Those rows fall back to the threshold alone
# and must SAY SO -- a check that did not run must never look like one that ran
# and passed. (CI uses n=10; this is the n=5 fixtures' case.)
assert_exit 1 "a row with no computable interval is still gated on the threshold" \
	env BENCH_WAIVERS= "$GATE" "${TESTDATA}/benchstat-regression-new.txt"
assert_contains "resolution check did not run" \
	"a regression judged without an interval says the check did not run" \
	env BENCH_WAIVERS= "$GATE" "${TESTDATA}/benchstat-regression-new.txt"

# THE MEASUREMENT ITSELF. A real null comparison -- one tree, two interleaved
# runs, CI's sampling parameters -- kept as evidence for the spreads quoted
# above. Nothing in it is significant, so it must be clean, which also shows the
# NOISE-FLOOR verdicts come from the resolution check rather than from these
# benchmarks being odd in some other way.
assert_exit 0 "a measured null comparison on identical code is clean" \
	env BENCH_WAIVERS= "$GATE" "${TESTDATA}/benchstat-null-parallel-sandbox.txt"

# AND THE ONE THAT ACTUALLY FIRED. Same procedure, one tree, both arms; on 2 of
# 15 such comparisons the pre-#443 gate reported a REGRESSION. This is trial 8
# verbatim: +18.48% p=0.009 over a 15% gate, on code that did not change, with
# the offending arm measuring itself at ±19%. It is the live counterpart of the
# CI failure in #443, and the reason this check is not a matter of taste.
SPURIOUS_FIXTURE="${TESTDATA}/benchstat-null-spurious-firing.txt"
assert_exit 0 "a NULL comparison that fired the old gate no longer reds the build" \
	env BENCH_WAIVERS= "$GATE" "$SPURIOUS_FIXTURE"
# NOISE-FLOOR is only ever printed for a row that reached the threshold, so this
# is also the proof that the row genuinely WAS over the gate -- i.e. that the
# assertion above passes because the row was adjudicated and found unresolvable,
# not because it was quietly under the bar all along.
assert_contains "NOISE-FLOOR" "...and says so: the row DID cross the gate and could not be resolved" \
	env BENCH_WAIVERS= "$GATE" "$SPURIOUS_FIXTURE"
assert_contains "+18.48%" "the false regression keeps its number in the report" \
	env BENCH_WAIVERS= "$GATE" "$SPURIOUS_FIXTURE"
# The allocation columns of that same run: exact, "all samples are equal". The
# contrast is the argument for leaving the 5% allocation gate alone.
assert_contains "no-change row" "the allocation rows of the same run are unmoved" \
	env BENCH_WAIVERS= "$GATE" "$SPURIOUS_FIXTURE"

# The shipped waivers must not be what makes any of this pass, and must not
# rescue the true regression.
assert_exit 0 "the noise-floor fixture passes with the SHIPPED waivers too" \
	"$GATE" "$NOISE_FIXTURE"
assert_exit 1 "the shipped waivers do NOT rescue the true regression" \
	"$GATE" "$TRUE_FIXTURE"

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

# Set by the empty-discovery control far below, which synthesises a throwaway
# Go package INSIDE the module (it has to be inside for `go list` to match it).
# Cleaned up on every exit path so an interrupted run cannot leave a stray
# package sitting in the tree.
EMPTY_PKG_DIR=""

trap 'rm -rf "$ARMS_TMP" ${EMPTY_PKG_DIR:+"$EMPTY_PKG_DIR"}' EXIT

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
echo "== extracted workflow bodies: bench-gate-fail ============================"

# These four scripts were `run: |` blocks in .github/workflows/benchmark.yml.
# Inline bash is not syntax-checked, not shellchecked and not testable -- which
# is exactly how a gate stayed dead for 473 runs. Now that they are files, the
# discovery loop at the bottom lints them; this section exercises the LOGIC.

GATE_FAIL="${SCRIPT_DIR}/bench-gate-fail.sh"

# The three-way split is the whole point of this script, and the third branch is
# the subtle one: an exit code the gate never produces means it reached NO
# verdict, and calling that "regressions detected" (which this branch used to
# do) sends the reader hunting a performance problem that does not exist.
assert_exit 1 "bench-gate-fail: always fails the build" \
	env GATE_STATUS=1 bash "$GATE_FAIL"
assert_contains "::error::Benchmark regressions detected (gate exit 1)" \
	"bench-gate-fail: exit 1 is reported as a measured regression" \
	env GATE_STATUS=1 bash "$GATE_FAIL"
assert_contains "::error::The benchmark comparison could not be interpreted (gate exit 2)" \
	"bench-gate-fail: exit 2 is reported as uninterpretable, not as a regression" \
	env GATE_STATUS=2 bash "$GATE_FAIL"
assert_exit 1 "bench-gate-fail: an unrecognised code still fails" \
	env GATE_STATUS=127 bash "$GATE_FAIL"
assert_contains "did not run to completion (exit 127)" \
	"bench-gate-fail: exit 127 is reported as 'no verdict reached'" \
	env GATE_STATUS=127 bash "$GATE_FAIL"
assert_not_contains "regressions detected" \
	"bench-gate-fail: the no-verdict branch does NOT claim a regression" \
	env GATE_STATUS=127 bash "$GATE_FAIL"
# An empty or absent verdict must still be legible in the log rather than
# rendering as an empty pair of parentheses.
assert_contains "exit <unset>" \
	"bench-gate-fail: an empty verdict prints <unset>" \
	env GATE_STATUS= bash "$GATE_FAIL"
assert_contains "exit <unset>" \
	"bench-gate-fail: an UNSET verdict prints <unset> (not an unbound-variable crash)" \
	env -u GATE_STATUS bash "$GATE_FAIL"

echo "== extracted workflow bodies: require-jobs-succeeded ====================="

REQ_JOBS="${SCRIPT_DIR}/require-jobs-succeeded.sh"

# `success` is the ONLY pass. This is the body of the single job branch
# protection requires, so each of the non-success results has to fail it --
# a skipped or cancelled required check otherwise reads as green.
assert_exit 0 "require-jobs: all success passes" \
	env RESULTS='success success success' bash "$REQ_JOBS"
assert_exit 1 "require-jobs: a failed job fails the aggregate" \
	env RESULTS='success failure success' bash "$REQ_JOBS"
assert_exit 1 "require-jobs: a SKIPPED job fails the aggregate" \
	env RESULTS='success skipped success' bash "$REQ_JOBS"
assert_exit 1 "require-jobs: a CANCELLED job fails the aggregate" \
	env RESULTS='success cancelled' bash "$REQ_JOBS"
assert_contains "::error::A job in this workflow did not succeed" \
	"require-jobs: emits the ::error:: annotation naming the results" \
	env RESULTS='success failure' bash "$REQ_JOBS"

# NO RESULTS IS NOT A PASS (issue #485).
#
# `for r in ${RESULTS}` never enters its body when RESULTS is empty, so rc
# stayed 0 and this -- the aggregate whose NAME is in branch protection -- said
# "All jobs in this workflow succeeded" and exited 0 having checked nothing.
# Every non-success result was already covered above; the one input meaning
# "nothing reported at all" was the one that passed.
#
# RESULTS is `${{ join(needs.*.result, ' ') }}`. It goes empty if the `needs:`
# list is emptied or restructured, or if the expression is mistyped --
# `needs.*.results` silently yields "" rather than erroring.
#
# All three spellings of "no results" are pinned, because they arrive by
# different routes: unset (env var never set), empty (join over no jobs), and
# separators-only (join over empty results).
assert_exit 1 "require-jobs: EMPTY results fails — no upstream reported is not a pass (#485)" \
	env RESULTS='' bash "$REQ_JOBS"
assert_exit 1 "require-jobs: UNSET results fails (#485)" \
	env -u RESULTS bash "$REQ_JOBS"
assert_exit 1 "require-jobs: whitespace-only results fails (#485)" \
	env RESULTS='   ' bash "$REQ_JOBS"
assert_contains "::error::No upstream job results were reported" \
	"require-jobs: the empty case SAYS nothing was verified rather than failing mutely (#485)" \
	env RESULTS='' bash "$REQ_JOBS"
# The old wording must not survive on the empty path: "All jobs in this workflow
# succeeded" over zero jobs is the precise false statement this fixes.
if env RESULTS='' bash "$REQ_JOBS" 2>&1 | grep -q "All jobs in this workflow succeeded"; then
	bad "require-jobs still claims every job succeeded on empty input (#485)"
else
	ok "require-jobs no longer claims every job succeeded when none reported (#485)"
fi

# NEGATIVE CONTROL for the guard above (the bar #480 set).
#
# Strip the emptiness guard back out of a COPY of the script and require the
# vacuous pass to come back. Without this, the guard could be deleted or
# weakened in a refactor and every assertion above would still pass -- the
# check would quietly return to being unable to fail, which is the whole defect
# class this file exists to catch.
#
# The copy is produced by DELETING the guard from the real script rather than
# by keeping a fixture, so the control cannot drift away from what it guards.
# If the block can no longer be located the control fails loudly instead of
# silently testing nothing -- a negative control that stops finding its target
# is itself a check that cannot fail.
reqjobs_tmp="$(mktemp -d)"
reqjobs_strip_rc=0
python3 - "$REQ_JOBS" "${reqjobs_tmp}/stripped.sh" <<'PY' || reqjobs_strip_rc=$?
import re, sys

src = open(sys.argv[1]).read()
# Remove the emptiness guard, and only that: the `if [ -z "${RESULTS// /}" ]`
# block up to its closing `fi`.
out, n = re.subn(
    r'\nif \[ -z "\$\{RESULTS// /\}" \]; then\n.*?\nfi\n',
    "\n",
    src,
    flags=re.S,
)
if n != 1:
    sys.stderr.write("expected exactly 1 guard block, removed %d\n" % n)
    sys.exit(3)
open(sys.argv[2], "w").write(out)
PY
if [ "$reqjobs_strip_rc" -ne 0 ]; then
	bad "negative control could not strip the empty-RESULTS guard — has it been renamed or restructured? (#485)"
else
	# The stripped copy must still behave normally on real input, or the control
	# is testing a script it accidentally broke rather than the missing guard.
	assert_exit 0 "negative-control rig: the stripped copy still passes on all-success (#485)" \
		env RESULTS='success success' bash "${reqjobs_tmp}/stripped.sh"
	assert_exit 1 "negative-control rig: the stripped copy still fails on a failure (#485)" \
		env RESULTS='success failure' bash "${reqjobs_tmp}/stripped.sh"
	# And now the point: without the guard, empty input goes green again.
	if env RESULTS='' bash "${reqjobs_tmp}/stripped.sh" >/dev/null 2>&1; then
		ok "negative control: REMOVING the guard restores the vacuous pass, so the assertions above are load-bearing (#485)"
	else
		bad "negative control did not reproduce the #485 vacuous pass — the assertions above may be passing for the wrong reason"
	fi
fi
rm -rf "$reqjobs_tmp"

echo "== extracted workflow bodies: bench-compare =============================="

BENCH_COMPARE_SH="${SCRIPT_DIR}/bench-compare.sh"

# A sandbox mimicking the workflow's two-tree layout: $GITHUB_WORKSPACE/pr
# holding stub gate scripts, and a working directory holding the two arms.
# Stubs, not the real gate -- this section tests bench-compare.sh's plumbing
# (branching, $GITHUB_OUTPUT, annotations), and benchstat-gate.sh's own verdict
# logic is already covered by the fixture sections above.
make_compare_sandbox() {
	local dir="$1" arms_rc="$2" gate_rc="$3"
	mkdir -p "${dir}/pr/scripts" "${dir}/work" "${dir}/bin"
	{
		echo '#!/usr/bin/env bash'
		echo "echo 'stub arms-check output'"
		echo "exit ${arms_rc}"
	} > "${dir}/pr/scripts/bench-arms-check.sh"
	{
		echo '#!/usr/bin/env bash'
		# Two leading spaces: the waiver extraction greps '^  (WAIVED|...)'.
		echo "echo '  WAIVED      pkg B/op Encode-2 delta=+12.45% accepted: ceiling 14%'"
		echo "echo 'stub gate report'"
		echo "exit ${gate_rc}"
	} > "${dir}/pr/scripts/benchstat-gate.sh"
	{
		echo '#!/usr/bin/env bash'
		echo "echo 'stub benchstat table'"
	} > "${dir}/bin/benchstat"
	chmod +x "${dir}/pr/scripts/bench-arms-check.sh" \
		"${dir}/pr/scripts/benchstat-gate.sh" "${dir}/bin/benchstat"
	printf 'baseline rows\n' > "${dir}/work/bench-baseline.txt"
	printf 'current rows\n' > "${dir}/work/bench-current.txt"
}

# compare_case <arms-rc> <gate-rc> [mode] -> prints the script's stdout, then
# its exit status, then whatever it wrote to $GITHUB_OUTPUT, so a single
# assertion can inspect any of the three.
compare_case() {
	local arms_rc="$1" gate_rc="$2" mode="${3:-normal}"
	local dir rc
	dir="$(mktemp -d)"
	make_compare_sandbox "$dir" "$arms_rc" "$gate_rc"
	case "$mode" in
		missing-scripts) rm -f "${dir}/pr/scripts/benchstat-gate.sh" \
			"${dir}/pr/scripts/bench-arms-check.sh" ;;
		empty-baseline) : > "${dir}/work/bench-baseline.txt" ;;
	esac
	(
		cd "${dir}/work" || exit 99
		PATH="${dir}/bin:${PATH}" \
			GITHUB_WORKSPACE="$dir" \
			GITHUB_OUTPUT="${dir}/gh-output.txt" \
			BENCH_COUNT=10 \
			bash "$BENCH_COMPARE_SH"
	)
	rc=$?
	echo "__EXIT__ ${rc}"
	echo "__GITHUB_OUTPUT__"
	cat "${dir}/gh-output.txt" 2>/dev/null
	rm -rf "$dir"
}

# Every branch must exit 0: this script REPORTS a verdict via gate_status, and
# bench-gate-fail.sh is the only thing allowed to turn that into a red build.
# A non-zero exit here would abort the step before the PR comment is assembled.
for case_args in "0 0 normal" "0 1 normal" "0 2 normal" "1 0 normal" \
	"0 0 missing-scripts" "0 0 empty-baseline"; do
	# Deliberate word splitting of the case tuple into three arguments.
	# shellcheck disable=SC2086
	assert_contains "__EXIT__ 0" \
		"bench-compare: exits 0 so the PR comment is always assembled (${case_args})" \
		compare_case $case_args
done

# The gate's verdict must reach $GITHUB_OUTPUT verbatim -- that value is what
# the workflow's `if:` and bench-gate-fail.sh both key on.
assert_contains "gate_status=0" "bench-compare: a clean gate reports gate_status=0" \
	compare_case 0 0
assert_contains "gate_status=1" "bench-compare: a regression reports gate_status=1" \
	compare_case 0 1
assert_contains "gate_status=2" "bench-compare: an uninterpretable gate reports gate_status=2" \
	compare_case 0 2
assert_contains "gate_status=127" \
	"bench-compare: an unrecognised gate exit is passed through, not flattened" \
	compare_case 0 127
assert_contains "it did not run to completion" \
	"bench-compare: warns when the gate exits an unrecognised code" \
	compare_case 0 127

# The three "could not run" branches each name their own cause. All report
# gate_status=2, so only the annotation distinguishes them.
assert_contains "::error::benchmark gate scripts missing from the PR checkout" \
	"bench-compare: a missing gate script is named, not reported as a regression" \
	compare_case 0 0 missing-scripts
assert_contains "gate_status=2" \
	"bench-compare: a missing gate script reports gate_status=2" \
	compare_case 0 0 missing-scripts
assert_contains "::error::bench-baseline.txt is empty" \
	"bench-compare: an empty base arm fails loudly rather than degrading" \
	compare_case 0 0 empty-baseline
assert_not_contains "gate_status=0" \
	"bench-compare: an empty base arm never reports a pass" \
	compare_case 0 0 empty-baseline
assert_contains "::error::The two benchmark arms are not comparable" \
	"bench-compare: incomparable arms are named by the pre-flight" \
	compare_case 1 0
assert_not_contains "gate_status=0" \
	"bench-compare: incomparable arms never report a pass" \
	compare_case 1 0

# A waived row that is only visible to whoever expands a <details> block is an
# accepted regression nobody reviews. It must be surfaced ABOVE the fold.
assert_contains "### Reviewed waivers" \
	"bench-compare: a WAIVED row is lifted out of the collapsed section" \
	compare_case 0 0
assert_contains "n=10 each." \
	"bench-compare: BENCH_COUNT reaches the comment footer from the environment" \
	compare_case 0 0

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

# invoked_in_any <script-path> <file>... -> non-comment hits across all files
#
# The benchmark job's step bodies were moved out of `run: |` blocks and into
# scripts/bench-*.sh, so the invocations these guards anchor on no longer live
# in the YAML. The guards follow them into the extracted scripts rather than
# being deleted: the property being protected is "this logic is still wired
# into CI", not "this string appears in a .yml file". BENCH_PLUMBING is the
# workflow plus every script it calls, so a reinline, a deletion or an orphaned
# script all still fail.
invoked_in_any() {
	local needle="$1"
	shift
	local f hits all=""
	for f in "$@"; do
		[ -f "$f" ] || continue
		hits="$(invoked_in "$f" "$needle")"
		[ -n "$hits" ] && all="${all} ${hits}"
	done
	echo "$all"
}

BENCH_COMPARE="${SCRIPT_DIR}/bench-compare.sh"
BENCH_RUN_ARMS="${SCRIPT_DIR}/bench-run-arms.sh"
BENCH_GATE_FAIL="${SCRIPT_DIR}/bench-gate-fail.sh"
REQUIRE_JOBS="${SCRIPT_DIR}/require-jobs-succeeded.sh"
BENCH_PLUMBING=("$BENCH_WF" "$BENCH_COMPARE" "$BENCH_RUN_ARMS" "$BENCH_GATE_FAIL" "$REQUIRE_JOBS")

# Each extracted script must still be CALLED from the workflow. Without this,
# deleting the `run:` line would leave a perfectly clean, perfectly linted
# script that nothing executes -- the same "green because it stopped looking"
# shape as the dead grep, one level up.
for s in bench-run-arms.sh bench-compare.sh bench-gate-fail.sh require-jobs-succeeded.sh; do
	if [ ! -f "${SCRIPT_DIR}/${s}" ]; then
		bad "scripts/${s} is missing — the benchmark workflow calls it"
	elif [ -n "$(invoked_in "$BENCH_WF" "scripts/${s}")" ]; then
		ok "benchmark.yml INVOKES scripts/${s} (extracted body still wired in)"
	else
		bad "benchmark.yml does not invoke scripts/${s} — extracted body orphaned or reinlined"
	fi
done

if [ -n "$(invoked_in_any 'scripts/benchstat-gate.sh' "${BENCH_PLUMBING[@]}")" ]; then
	ok "the benchmark plumbing INVOKES scripts/benchstat-gate.sh (not just mentions it)"
else
	bad "nothing in the benchmark plumbing invokes scripts/benchstat-gate.sh — logic reinlined or removed?"
fi

# The waiver file the gate reads by default must exist in the repository, or
# every waiver silently stops applying. That direction is the strict one, so it
# does not fail a build -- which is precisely why nothing else would notice.
if [ -f "${SCRIPT_DIR}/benchstat-waivers.txt" ]; then
	ok "scripts/benchstat-waivers.txt exists (the gate's default waiver list)"
else
	bad "scripts/benchstat-waivers.txt is missing — waivers would silently stop applying"
fi

# A waived row that only ever appears in the job log is an accepted regression
# nobody reviews. The gate's report must reach the PR comment, which means the
# workflow has to capture it and put it in the comment body -- two halves, both
# checkable, and the failure of either is invisible from the outside.
gate_report_hits="$(invoked_in_any 'gate-report.txt' "${BENCH_PLUMBING[@]}")"
if [ "$(echo "$gate_report_hits" | wc -w)" -ge 2 ]; then
	ok "the benchmark plumbing captures the gate report AND feeds it into the PR comment"
else
	bad "the benchmark plumbing does not carry the gate report into the PR comment — a WAIVED row would be visible only to whoever opens the job log"
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

if [ -n "$(invoked_in_any 'scripts/bench-arms-check.sh' "${BENCH_PLUMBING[@]}")" ]; then
	ok "the benchmark plumbing INVOKES scripts/bench-arms-check.sh before comparing"
else
	bad "the benchmark plumbing does not run the arm comparability pre-flight — an unpairable comparison would report only its symptom"
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

# A job that RUNS a script from scripts/ must CHECK THE REPOSITORY OUT, or the
# file is simply not there and the step exits 127 -- which this workflow has
# already once reported as "Benchmark regressions detected", a real-looking
# failure with an entirely fictional cause.
#
# This is not hypothetical bookkeeping: the `required` job carried its assertion
# as inline bash and so needed no checkout at all. Moving that body into
# scripts/require-jobs-succeeded.sh made a checkout mandatory, and nothing else
# in this file would have noticed it missing -- the aggregate required check
# would simply have gone red on every PR with a 127.
nocheckout="$(python3 - "$REPO_ROOT" <<'PY_INNER'
import glob, os, re, sys

root = sys.argv[1]
try:
    import yaml
except ImportError:
    print("__SKIP__ pyyaml unavailable")
    sys.exit(0)

# Any reference to a repo script, however it is spelled: root-relative
# (`scripts/x.sh`, `./scripts/x.sh`) or via a relocated checkout
# (`${GITHUB_WORKSPACE}/pr/scripts/x.sh`). All of them need the repo present.
REF = re.compile(r"(?:^|[\s;&|(\"'/])(?:\./)?scripts/[\w.-]+\.(?:sh|cjs)", re.M)

hits = []
for f in sorted(glob.glob(os.path.join(root, ".github", "workflows", "*.y*ml"))):
    base = os.path.basename(f)
    try:
        doc = yaml.safe_load(open(f))
    except Exception:  # noqa: BLE001 -- the YAML-parse guard owns this
        continue
    if not isinstance(doc, dict):
        continue
    for job_id, job in (doc.get("jobs") or {}).items():
        steps = job.get("steps") or []
        has_checkout = any("actions/checkout" in (st.get("uses") or "") for st in steps)
        if has_checkout:
            continue
        for st in steps:
            run = st.get("run")
            if not isinstance(run, str):
                continue
            # Drop shell comments so a block that DOCUMENTS a path is not
            # mistaken for one that invokes it.
            code = "\n".join(
                line[: m.start()] if (m := re.search(r"(?:^|\s)#", line)) else line
                for line in run.splitlines()
            )
            for m in REF.finditer(code):
                hits.append(f"{base}: job {job_id!r} runs {m.group(0).strip()} with no actions/checkout")
print("\n".join(sorted(set(hits))))
PY_INNER
)"
case "$nocheckout" in
	__SKIP__*) echo "SKIP  script-without-checkout guard ($nocheckout)" ;;
	"") ok "every job that runs a scripts/ helper also checks the repository out" ;;
	*)
		bad "a job runs a repo script without checking the repo out (the step exits 127, not a verdict):"
		echo "$nocheckout" | sed 's/^/        | /'
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
echo "== dependabot covers every dependency manifest ==========================="

# Dependabot only looks where it is told, and says nothing about where it does
# not. A manifest with no matching `directory:` gets no version bumps and no
# security updates, and the only symptom is an absence -- the dashboard is
# green because it is not looking, which reads exactly like green because
# there is nothing to find.
#
# Triggering example: tree-sitter-elps/ is a separate Go module AND an npm
# package with a committed lockfile, built and tested on every PR by
# .github/workflows/tree-sitter.yml, and it had no entry at all. Its
# go-tree-sitter / node-addon-api / tree-sitter-cli pins were frozen from the
# day they were written.
#
# This walks the tree rather than reading a list, so a module added later is
# covered by construction.
dep_out="$(python3 - "$REPO_ROOT" <<'PY'
import os, sys
try:
    import yaml
except ImportError:
    print("__SKIP__ PyYAML not installed")
    sys.exit(0)

root = sys.argv[1]
passes, failures = [], []

cfg_path = os.path.join(root, ".github", "dependabot.yml")
if not os.path.exists(cfg_path):
    print("FAIL  .github/dependabot.yml is missing")
    print("__COUNTS__ 0 1")
    sys.exit(0)

with open(cfg_path) as fh:
    cfg = yaml.safe_load(fh)

# (ecosystem, normalised directory) pairs the config declares. `directories:`
# is the newer plural spelling; accept both so switching to it does not read
# as a regression.
def norm(d):
    d = "/" + str(d).strip().strip("/")
    return "/" if d == "/" else d

declared = set()
for u in (cfg.get("updates") or []):
    eco = u.get("package-ecosystem")
    dirs = u.get("directories") or ([u.get("directory")] if u.get("directory") else [])
    for d in dirs:
        declared.add((eco, norm(d)))

# Manifests actually present, excluding vendored and installed trees.
IGNORE = {"node_modules", "vendor", ".git", "testdata", "build"}
MANIFESTS = {"go.mod": "gomod", "package.json": "npm"}
found = set()
for dirpath, dirnames, filenames in os.walk(root):
    dirnames[:] = [d for d in dirnames if d not in IGNORE]
    for fn in filenames:
        eco = MANIFESTS.get(fn)
        if eco is None:
            continue
        rel = os.path.relpath(dirpath, root)
        found.add((eco, "/" if rel == "." else "/" + rel.replace(os.sep, "/")))

missing = sorted(m for m in found if m not in declared)
for eco, d in missing:
    failures.append(
        f"{eco} manifest at {d} has no dependabot entry — it gets no version "
        f"bumps and no security updates, silently"
    )
if not missing:
    passes.append(
        f"every dependency manifest has a dependabot entry "
        f"({len(found)} manifests: " +
        ", ".join(f"{e}{d}" for e, d in sorted(found)) + ")"
    )

# A `directory:` pointing at nothing is the same failure seen from the other
# side: the entry looks like coverage and provides none.
stale = sorted(
    (e, d) for (e, d) in declared
    if e in MANIFESTS.values() and (e, d) not in found
)
for eco, d in stale:
    failures.append(f"dependabot declares {eco} at {d}, but no such manifest exists")
if not stale:
    passes.append("no dependabot entry points at a manifest that does not exist")

for p in passes:
    print(f"PASS  {p}")
for f_ in failures:
    print(f"FAIL  {f_}")
print(f"__COUNTS__ {len(passes)} {len(failures)}")
PY
)"

if echo "$dep_out" | grep -q '^__SKIP__'; then
	echo "SKIP  $(echo "$dep_out" | sed -n 's/^__SKIP__ //p')"
else
	echo "$dep_out" | grep -v '^__COUNTS__' || true
	dep_counts="$(echo "$dep_out" | sed -n 's/^__COUNTS__ //p')"
	if [ -n "$dep_counts" ]; then
		read -r dep_pass dep_fail <<<"$dep_counts"
		pass=$((pass + dep_pass))
		fail=$((fail + dep_fail))
	else
		bad "dependabot coverage guard did not run"
	fi
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

	# Issue #458: the zero-headroom case. The condition used to be
	# `needed > timeout`, so a sweep sized at EXACTLY timeout-minutes printed
	# "the sweep fits" and exited 0 -- while being one slow shard away from
	# the silently-truncated run this whole gate exists to prevent (observed
	# shard durations vary 4m25s to 5m42s). It must now fail.
	#
	# The timeout is DERIVED from what the check itself reports as `required`
	# rather than hardcoded, so this control cannot go stale as targets are
	# added. Hardcoding the number here would reproduce the exact
	# hand-maintained-constant defect the gate was built to kill.
	#
	# One invocation, both numbers: the check re-runs full target discovery
	# (`go test -list` over every package), which is tens of seconds.
	budget_report="$("$BUDGET" 2>/dev/null)"
	budget_required="$(awk '$1 == "required" && $2 ~ /^[0-9]+$/ { print $2 }' <<<"$budget_report")"
	budget_margin="$(awk '$1 == "required" && $2 == "margin" { print $3 }' <<<"$budget_report")"
	if [ -z "$budget_required" ] || [ -z "$budget_margin" ]; then
		bad "could not read 'required' / 'required margin' from the budget check's own output"
	else
		cp "$FUZZ_WF" "$budget_wf"
		sed -i "s/^    timeout-minutes: [0-9]*\$/    timeout-minutes: ${budget_required}/" "$budget_wf"
		assert_exit 1 "a sweep sized at EXACTLY timeout-minutes FAILS -- zero headroom is not a fit (#458)" \
			env FUZZ_WORKFLOW="$budget_wf" "$BUDGET"

		# ...and exactly one FUZZTIME above that passes. The PAIR is the
		# point: it pins the boundary at one FUZZTIME of margin, so neither
		# dropping the margin nor inflating it past the derived quantum can
		# pass this file.
		cp "$FUZZ_WF" "$budget_wf"
		sed -i "s/^    timeout-minutes: [0-9]*\$/    timeout-minutes: $((budget_required + budget_margin))/" "$budget_wf"
		assert_exit 0 "one FUZZTIME above 'required' is the smallest timeout that passes (#458)" \
			env FUZZ_WORKFLOW="$budget_wf" "$BUDGET"

		# One minute below that boundary must still fail, which is what makes
		# the assertion above a boundary and not just "a big number passes".
		cp "$FUZZ_WF" "$budget_wf"
		sed -i "s/^    timeout-minutes: [0-9]*\$/    timeout-minutes: $((budget_required + budget_margin - 1))/" "$budget_wf"
		assert_exit 1 "one minute short of a full FUZZTIME of margin still FAILS (#458)" \
			env FUZZ_WORKFLOW="$budget_wf" "$BUDGET"
	fi

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

	# FUZZ_TAGS must reach BOTH discovery and execution. Reaching only one
	# would enumerate one build and fuzz the other -- reporting success over
	# targets it never ran. Same blind spot golangci-lint has (it analyses one
	# build), which is why `make static-checks` makes a second tagged pass.
	if FUZZ_TAGS=elpscheck "${SCRIPT_DIR}/fuzz.sh" --list >/dev/null 2>&1; then
		ok "FUZZ_TAGS is accepted by target discovery"
	else
		bad "FUZZ_TAGS=elpscheck breaks target discovery"
	fi
	assert_contains "-tags elpscheck" \
		"a tagged sweep SAYS so (a tagged run must not read as a default one)" \
		env FUZZTIME=1s FUZZ_TAGS=elpscheck "${SCRIPT_DIR}/fuzz.sh" --shard 1/15
	tagged_go="$(grep -c 'go test ${tagflags' "${SCRIPT_DIR}/fuzz.sh")"
	if [ "$tagged_go" -ge 2 ]; then
		ok "both go test invocations (discovery and fuzzing) carry FUZZ_TAGS"
	else
		bad "only ${tagged_go} go test invocation(s) carry FUZZ_TAGS — discovery and execution can disagree about the build"
	fi

	# Sharding must be a PARTITION: every target in exactly one shard. A shard
	# assignment that drops a target loses coverage silently, and one that
	# duplicates a target just wastes budget.
	#
	# The shard count is READ FROM THE WORKFLOW rather than written here. It was
	# hardcoded to 4, so resharding CI would have left this proving the
	# partition for a shard count CI no longer uses -- the same
	# derive-don't-remember failure fuzz-budget-check.sh exists to prevent.
	shard_n="$(grep -oE '^ +shard: \[[0-9, ]+\]' "$FUZZ_WF" | grep -oE '[0-9]+' | wc -l | tr -d ' ')"
	if [ "${shard_n:-0}" -lt 1 ]; then
		bad "cannot read strategy.matrix.shard out of ${FUZZ_WF} — the partition check below would be testing a made-up shard count"
		shard_n=1
	fi
	# "NOTHING TO CHECK" IS NOT "THE CHECK PASSED" (issue #484)
	# ---------------------------------------------------------
	# This block used to discard the exit status AND the stderr of every
	# fuzz.sh invocation:
	#
	#   "${SCRIPT_DIR}/fuzz.sh" --list 2>/dev/null | LC_ALL=C sort >full
	#
	# Two things hid the failure. The `2>/dev/null` threw away the reason, and
	# putting fuzz.sh in a PIPELINE meant `$?` reported `sort`'s status, never
	# fuzz.sh's -- the same mechanism as #479, where `mapfile < <(...)` buried
	# the status in a subshell. So when discovery broke, `full` and `union` were
	# both empty, `diff` called two empty files equal, and BOTH assertions
	# printed PASS. The duplicate count computed 0 - 0 = 0 and passed too.
	#
	# That is the worst possible coupling, and #482 made it live: fuzz.sh now
	# exits non-zero when a package fails to build, so the vacuous-pass branch
	# is taken PRECISELY when something is genuinely broken.
	#
	# There are two distinct ways to end up comparing two empty lists, and the
	# fix has to close both -- checking the exit status alone is NOT enough:
	#
	#   1. discovery FAILS      -- fuzz.sh exits non-zero (a package did not
	#                              build). Caught by testing the status.
	#   2. discovery SUCCEEDS   -- fuzz.sh exits 0 having found nothing.
	#      but finds NOTHING       Verified by running: `fuzz.sh --list` over a
	#                              module that compiles cleanly and defines no
	#                              fuzz target exits 0, by design (#482 asserts
	#                              exactly that, so that ordinary target-free
	#                              packages are not mistaken for broken ones).
	#                              Only an explicit non-empty assertion catches
	#                              this one.
	#
	# Case 2 is why the emptiness test below is load-bearing rather than
	# belt-and-braces: a partition of nothing is not a partition, it is an
	# absence of evidence, and this file exists to prove the other gates can
	# actually fail.
	#
	# The computation lives in a function so the NEGATIVE CONTROL further down
	# can run THE VERY SAME CODE against a tree where discovery is genuinely
	# broken and require it to come back "cannot-run". A control that
	# re-implemented the comparison would only prove the copy works.
	#
	# Prints a one-line verdict on stdout whose first token is one of
	# `partition:`, `cannot-run:`, `not-a-partition:` or `duplicated:`, and
	# returns 0 only for `partition:`.
	shard_partition_probe() {
		local sdir="$1" n="$2"
		local tmp rc=0 i full dupes

		tmp="$(mktemp -d)"

		# Command substitution, NOT a pipeline and NOT `< <(...)`: both hide the
		# status of the command that actually matters. `x="$(cmd)" || rc=$?`
		# evaluates it here, in this shell, where it can be acted on.
		full="$("${sdir}/fuzz.sh" --list 2>"${tmp}/full.err")" || rc=$?
		if [ "$rc" -ne 0 ]; then
			printf 'cannot-run: full target discovery exited %s\n' "$rc"
			sed 's/^/          | /' "${tmp}/full.err"
			rm -rf "$tmp"
			return 1
		fi

		# Case 2 above. An empty target list means the partition is UNKNOWN, and
		# unknown must not read as proven. This matches how the rest of the repo
		# already treats a zero count: fuzz.sh exits 2 rather than sweeping
		# nothing, and fuzz-budget-check.sh exits 2 rather than reporting that a
		# zero-target sweep "fits".
		if [ -z "${full//[[:space:]]/}" ]; then
			printf 'cannot-run: full target discovery returned ZERO targets (exit 0), so there is no partition to verify\n'
			rm -rf "$tmp"
			return 1
		fi

		: >"${tmp}/union"
		for ((i = 1; i <= n; i++)); do
			rc=0
			"${sdir}/fuzz.sh" --list --shard "${i}/${n}" \
				>>"${tmp}/union" 2>"${tmp}/shard.err" || rc=$?
			if [ "$rc" -ne 0 ]; then
				printf 'cannot-run: shard %s/%s discovery exited %s\n' "$i" "$n" "$rc"
				sed 's/^/          | /' "${tmp}/shard.err"
				rm -rf "$tmp"
				return 1
			fi
		done

		printf '%s\n' "$full" | LC_ALL=C sort >"${tmp}/full.sorted"
		LC_ALL=C sort "${tmp}/union" >"${tmp}/union.sorted"

		if ! diff -q "${tmp}/full.sorted" "${tmp}/union.sorted" >/dev/null; then
			printf 'not-a-partition: targets are lost or duplicated\n'
			diff "${tmp}/full.sorted" "${tmp}/union.sorted" | sed 's/^/          | /'
			rm -rf "$tmp"
			return 1
		fi

		dupes=$(($(wc -l <"${tmp}/union.sorted") - $(sort -u "${tmp}/union.sorted" | wc -l)))
		if [ "$dupes" -ne 0 ]; then
			printf 'duplicated: %s target(s) appear in more than one shard\n' "$dupes"
			rm -rf "$tmp"
			return 1
		fi

		printf 'partition: %s targets across %s shards\n' \
			"$(printf '%s\n' "$full" | grep -c .)" "$n"
		rm -rf "$tmp"
		return 0
	}

	# verdict_is <token> <verdict-line> -- true when the verdict carries <token>.
	# A helper rather than inline `case` patterns because `|` is the pattern
	# separator in `case`, which makes "match the token AND the return code" in
	# one pattern quietly error-prone.
	verdict_is() {
		case "$2" in
		"$1":*) return 0 ;;
		*) return 1 ;;
		esac
	}

	shard_verdict="$(shard_partition_probe "$SCRIPT_DIR" "$shard_n" 2>&1)" || true
	case "$shard_verdict" in
	partition:*)
		ok "the ${shard_n} shards are a partition of the full target list (no target lost) — ${shard_verdict#partition: }"
		ok "no target is claimed by more than one shard"
		;;
	cannot-run:*)
		# The #484 path. Discovery could not be trusted, so the partition is
		# unverified -- which is a FAILURE of this check, not a pass.
		bad "the shard-partition check could not run, so the partition is UNVERIFIED (#484)"
		printf '%s\n' "$shard_verdict" | sed 's/^/        | /'
		;;
	*)
		bad "sharding is not a partition — targets are lost or duplicated"
		printf '%s\n' "$shard_verdict" | sed 's/^/        | /'
		;;
	esac

	# NEGATIVE CONTROL for the two assertions above (issue #484).
	#
	# The bar #480 set: a rule that has been fixed must ship something which
	# STRIPS the condition and requires the rule to report it, so the rule keeps
	# proving it can still go red. Before this fix the partition assertions
	# could not fail at all under broken discovery -- they printed two PASS
	# lines over zero targets -- and nothing in this file would have noticed.
	#
	# Run against a THROWAWAY MODULE, not a mock and not this repo: the control
	# needs a genuinely uncompilable package, and breaking one in-tree would
	# break every other check in this file. fuzz.sh derives REPO_ROOT from its
	# own location, so a copy inside the scratch module operates entirely on
	# that module (the technique #482 introduced for the same reason).
	#
	# All THREE cases are asserted, and only the set is meaningful. "Broken
	# discovery is reported" alone is equally satisfied by a probe that fails on
	# everything -- which would be a gate nobody can keep green -- so the
	# healthy case is pinned in the same breath.
	part_tmp="$(mktemp -d)"
	mkdir -p "${part_tmp}/scripts" "${part_tmp}/hastarget" "${part_tmp}/notargets"
	cp "${SCRIPT_DIR}/fuzz.sh" "${part_tmp}/scripts/fuzz.sh"
	cat >"${part_tmp}/go.mod" <<-EOF
		module example.invalid/shardpart

		go $(awk '/^go /{print $2; exit}' "${REPO_ROOT}/go.mod")
	EOF
	cat >"${part_tmp}/hastarget/hastarget_test.go" <<-'EOF'
		package hastarget

		import "testing"

		func FuzzShardAlpha(f *testing.F) {
			f.Add("seed")
			f.Fuzz(func(t *testing.T, s string) { _ = s })
		}

		func FuzzShardBeta(f *testing.F) {
			f.Add("seed")
			f.Fuzz(func(t *testing.T, s string) { _ = s })
		}
	EOF

	# (a) HEALTHY: real targets, everything compiles. The probe must say
	#     `partition:`. Without this the control below proves nothing.
	part_rc=0
	part_verdict="$(shard_partition_probe "${part_tmp}/scripts" "$shard_n" 2>&1)" || part_rc=$?
	if [ "$part_rc" -eq 0 ] && verdict_is partition "$part_verdict"; then
		ok "negative-control rig: a HEALTHY tree still reports a partition (the control is not a gate that fails on everything) (#484)"
	else
		bad "negative-control rig is broken — a healthy scratch module did not report a partition (#484): rc=${part_rc} ${part_verdict}"
	fi

	# (b) DISCOVERY RETURNS NOTHING, exit 0. A module that compiles cleanly and
	#     defines no fuzz target: `fuzz.sh --list` exits 0 with an empty list, so
	#     ONLY the emptiness assertion catches this. This is the case the
	#     exit-status check does not cover.
	mv "${part_tmp}/hastarget" "${part_tmp}/.hastarget.off"
	cat >"${part_tmp}/notargets/notargets.go" <<-'EOF'
		package notargets

		// Ordinary code, no fuzz target anywhere near it.
		func Greet() string { return "hello" }
	EOF
	part_rc=0
	part_verdict="$(shard_partition_probe "${part_tmp}/scripts" "$shard_n" 2>&1)" || part_rc=$?
	if [ "$part_rc" -ne 0 ] && verdict_is cannot-run "$part_verdict"; then
		ok "ZERO discovered targets is reported as unverified, not as a partition of nothing (#484)"
	else
		bad "zero-target discovery still reads as a passing partition (#484): rc=${part_rc} ${part_verdict}"
	fi

	# (c) DISCOVERY FAILS. A real type error, not a mock -- the exact condition
	#     #482 made fatal, which is what turned this latent hole into a live one.
	mv "${part_tmp}/.hastarget.off" "${part_tmp}/hastarget"
	mkdir -p "${part_tmp}/broken"
	cat >"${part_tmp}/broken/broken.go" <<-'EOF'
		package broken

		// Deliberately uncompilable: the point of the assertion below.
		func Broken() int { return "not an int" }
	EOF
	part_rc=0
	part_verdict="$(shard_partition_probe "${part_tmp}/scripts" "$shard_n" 2>&1)" || part_rc=$?
	if [ "$part_rc" -ne 0 ] && verdict_is cannot-run "$part_verdict"; then
		ok "a package that FAILS TO BUILD makes the partition check report UNVERIFIED instead of PASS (#484)"
	else
		bad "broken discovery still reads as a passing partition — #484 has regressed: rc=${part_rc} ${part_verdict}"
	fi
	# The compiler's own error must survive to the operator, not be swallowed by
	# a `2>/dev/null` the way it was before (#479's lesson, applied on this side).
	case "$part_verdict" in
	*"not an int"*)
		ok "the underlying build error is replayed, not swallowed (#484)"
		;;
	*)
		bad "the partition check hid WHY discovery failed (#484): ${part_verdict}"
		;;
	esac
	rm -rf "$part_tmp"

	# Issue #479: a package that FAILS TO BUILD must not look like a package
	# with no fuzz targets.
	#
	# Discovery used to be `go test -list ... 2>/dev/null | grep`, discarding
	# both stderr and exit status, so an uncompilable package contributed zero
	# targets exactly as a target-free one does -- silently. Observed for real:
	# 8 of 30 targets vanished on a cold cache and the sweep still exited 0.
	#
	# Tested against a THROWAWAY MODULE rather than by breaking this repo,
	# because the assertion needs a genuinely uncompilable package and doing
	# that in-tree would break every other check in this file. fuzz.sh derives
	# its REPO_ROOT from its own location, so a copy inside the scratch module
	# operates entirely on that module.
	#
	# Both directions are asserted. Only the pair is meaningful: "broken
	# fails" alone is also satisfied by a discovery step that fails on
	# everything, which would be a gate nobody can keep green.
	disc_tmp="$(mktemp -d)"
	mkdir -p "${disc_tmp}/scripts" "${disc_tmp}/notargets" "${disc_tmp}/hastarget"
	cp "${SCRIPT_DIR}/fuzz.sh" "${disc_tmp}/scripts/fuzz.sh"
	cat >"${disc_tmp}/go.mod" <<-EOF
		module example.invalid/fuzzdisc

		go $(awk '/^go /{print $2; exit}' "${REPO_ROOT}/go.mod")
	EOF
	# Compiles, and has no fuzz targets at all. Must be accepted in silence:
	# most packages in any repo look like this, so a discovery step that
	# complained here would be unusable.
	cat >"${disc_tmp}/notargets/notargets.go" <<-'EOF'
		package notargets

		// Greet is ordinary code with no fuzz target anywhere near it.
		func Greet() string { return "hello" }
	EOF
	# Compiles and defines a target, so the green case has something to find
	# (fuzz.sh treats a repo with zero targets as an error in its own right).
	cat >"${disc_tmp}/hastarget/hastarget_test.go" <<-'EOF'
		package hastarget

		import "testing"

		func FuzzScratch(f *testing.F) {
			f.Add("seed")
			f.Fuzz(func(t *testing.T, s string) { _ = s })
		}
	EOF

	assert_exit 0 "a package with NO fuzz targets is not mistaken for a broken one (#479)" \
		"${disc_tmp}/scripts/fuzz.sh" --list
	assert_contains "FuzzScratch" \
		"the scratch module's one real target IS discovered (#479)" \
		"${disc_tmp}/scripts/fuzz.sh" --list

	# Now break a package for real -- a type error, not a mock.
	mkdir -p "${disc_tmp}/broken"
	cat >"${disc_tmp}/broken/broken.go" <<-'EOF'
		package broken

		// Deliberately uncompilable: the point of the assertion below.
		func Broken() int { return "not an int" }
	EOF
	assert_exit 2 "a package that does NOT COMPILE fails discovery instead of contributing zero targets (#479)" \
		"${disc_tmp}/scripts/fuzz.sh" --list
	assert_contains "FAILED TO BUILD" \
		"the build failure SAYS it was a build failure (#479)" \
		"${disc_tmp}/scripts/fuzz.sh" --list
	assert_contains "not an int" \
		"the compiler's own error is replayed, not swallowed (#479)" \
		"${disc_tmp}/scripts/fuzz.sh" --list
	# The sweep itself, not just --list: the discovery path is shared, and it
	# is the sweep that would otherwise go green having fuzzed nothing.
	assert_exit 2 "the SWEEP also refuses to run over a partially-built tree (#479)" \
		env FUZZTIME=1s "${disc_tmp}/scripts/fuzz.sh" --shard 1/1
	rm -rf "$disc_tmp"

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
	#
	# This control needs a package set that MATCHES at least one package (so it
	# reaches fuzz.sh's zero-TARGET error rather than its "no packages matched"
	# one) while defining no fuzz target. It used to name ./lint/... -- a real
	# package that merely happened to have none -- and the moment FuzzLintSource
	# landed in lint/ the control quietly stopped being a control: the assertion
	# still ran, it just no longer exercised the empty path. A self-test whose
	# entire purpose is proving the gate CAN fail is the last place that should
	# depend on a real package staying empty by luck.
	#
	# So the empty package is synthesised here, fresh, on every run. It is empty
	# by construction: nobody can add a fuzz target to a directory that exists
	# only for the length of these three assertions, and it stays empty no
	# matter what lands anywhere in the real tree. It carries an ordinary Test
	# function so this also proves discovery selects on the Fuzz* prefix rather
	# than merely finding a package with no tests at all.
	EMPTY_PKG_DIR="$(mktemp -d "${REPO_ROOT}/internal/citest-emptyfuzz.XXXXXX")"
	cat >"${EMPTY_PKG_DIR}/empty.go" <<-'EOF'
		// Package emptyfuzz is synthesised by scripts/ci-gates-test.sh and
		// deleted again as soon as the empty-discovery control has run. It
		// deliberately defines no fuzz target.
		package emptyfuzz
	EOF
	cat >"${EMPTY_PKG_DIR}/empty_test.go" <<-'EOF'
		package emptyfuzz

		import "testing"

		// A test, but NOT a fuzz target: discovery must select on the Fuzz
		// prefix, so an ordinary Test must not read as something to fuzz.
		func TestNotAFuzzTarget(t *testing.T) {}
	EOF
	empty_pkg_pat="./internal/$(basename "$EMPTY_PKG_DIR")/..."
	assert_exit 2 "discovering ZERO targets is an error, not a clean run" \
		env FUZZTIME=1s "$FUZZ" "$empty_pkg_pat"
	assert_contains "cannot fail" "the empty-discovery error explains itself" \
		env FUZZTIME=1s "$FUZZ" "$empty_pkg_pat"
	# ...and the control is honestly a control. A synthetic package introduces a
	# second way to exit 2 -- the pattern matching nothing at all -- which would
	# satisfy the assertion above for entirely the wrong reason, leaving the
	# zero-target path as unexercised as ./lint/... left it.
	assert_not_contains "no packages matched" \
		"the empty case reaches zero-target discovery, not an unmatched pattern" \
		env FUZZTIME=1s "$FUZZ" "$empty_pkg_pat"
	rm -rf "$EMPTY_PKG_DIR"
	EMPTY_PKG_DIR=""

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
# still reads fully green. Two traps this repo has been one edit away from:
#
#   * sharding renamed "Fuzz targets" to "Fuzz shard i/4". Had the old name
#     been required, every PR would have become unmergeable, because a required
#     check that never reports blocks forever.
#   * adding a job to a workflow silently leaves it outside the required set.
#
# So every pull_request-triggered workflow carries exactly one fixed-name
# `Required: <area>` job, and that name is the only thing in branch protection.
# This guard enforces the properties that make that safe: the aggregate exists,
# its name is FIXED, it carries if: always() (since `needs` alone SKIPS it on
# upstream failure and a skipped required check reads as green), and it `needs`
# EVERY other job -- an aggregate that has stopped covering a job is worse than
# none, because it looks like coverage.
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

    def jobname(jid, _jobs=jobs):
        return str(_jobs[jid].get("name") or jid)

    # A path-filtered workflow does not run at all on a PR that touches nothing
    # it matches, so its checks never report -- and a REQUIRED check that never
    # reports blocks the PR forever, clearable only by editing repo settings.
    # Since a `Required:` aggregate exists precisely to be required, its
    # workflow must fire on every PR.
    #
    # Caught for real: tree-sitter.yml was filtered to tree-sitter-elps/**, so
    # "Required: tree-sitter" did not report on the PR that introduced it, and
    # adding it to branch protection would have wedged every PR that does not
    # touch the grammar.
    pr_cfg = triggers.get("pull_request") or {}
    filtered = isinstance(pr_cfg, dict) and (pr_cfg.get("paths") or pr_cfg.get("paths-ignore"))

    aggs = [j for j in jobs if jobname(j).startswith(MARKER)]
    if aggs and filtered:
        failures.append(
            f"{base}: has a '{MARKER} ...' aggregate but its pull_request trigger is "
            f"path-filtered, so the check does not report on PRs that miss the filter. "
            f"Required + never-reports = permanently unmergeable. Drop the paths filter, "
            f"or do not make this workflow's aggregate a required check."
        )
        continue
    if not aggs:
        failures.append(
            f"{base}: no fixed-name '{MARKER} ...' aggregate job. Every job would have to "
            f"be listed in branch protection by hand, and a job added later would be "
            f"unguarded while the board still looks green."
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

    if "always()" not in str(jobs[agg].get("if", "")):
        failures.append(
            f"{base}: aggregate {jobname(agg)!r} lacks `if: always()`; it would be SKIPPED "
            f"on upstream failure, and a skipped required check reads as green"
        )

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

echo
echo "== confidentiality guard: it must be able to FAIL ========================"

# scripts/confidentiality-guard.sh had no behavioural coverage here at all --
# only `bash -n` and shellcheck, which prove it parses, not that it works.
#
# It reported "clean" and exited 0 in four distinct situations where it had
# scanned nothing (issue #486): outside a repository, on a corrupt index, on a
# dangling .git gitdir pointer, and over a tree whose tracked files are absent
# from the working directory. The first three come back from `git grep` as exit
# >=2, which the old truthiness test (`if matches="$(git grep ...)"`) routed
# into the clean branch; the fourth comes back as exit 1, which is
# indistinguishable from a genuinely clean tree by status alone.
#
# THE FORBIDDEN TERM IS NEVER WRITTEN LITERALLY, here or anywhere else. Every
# fixture below constructs it at runtime from the same octal codes the guard
# itself uses, and every one of them lives in a throwaway repository under
# $TMPDIR -- never in this worktree, so nothing can ever be committed.
GUARD_SH="${SCRIPT_DIR}/confidentiality-guard.sh"

if [ ! -x "$GUARD_SH" ]; then
	bad "scripts/confidentiality-guard.sh is missing or not executable"
elif ! command -v git >/dev/null 2>&1; then
	echo "SKIP  git unavailable — confidentiality guard assertions not run"
else
	guard_tmp="$(mktemp -d)"

	# new_repo <dir> -- a throwaway git repo with one ordinary tracked file, so
	# the guard has something real to scan.
	new_repo() {
		mkdir -p "$1"
		git -C "$1" init -q
		git -C "$1" config user.email guard@example.invalid
		git -C "$1" config user.name "guard test"
		printf 'package main\n\nfunc main() {}\n' >"$1/main.go"
		git -C "$1" add -A
		git -C "$1" -c commit.gpgsign=false commit -qm fixture
	}

	# (1) POSITIVE CONTROL -- the guard must still pass on a clean tree.
	# Without this the assertions below are equally satisfied by a guard that
	# fails on everything, which nobody could keep green.
	new_repo "${guard_tmp}/clean"
	assert_exit 0 "confidentiality guard: a clean tree passes (#486)" \
		env -C "${guard_tmp}/clean" bash "$GUARD_SH"

	# (2) NEGATIVE CONTROL -- the guard must still CATCH a real violation.
	# This is the assertion that proves the exit-2 paths added for #486 did not
	# turn the guard into something that merely never says "found". The term is
	# assembled at runtime, exactly as the guard assembles it.
	new_repo "${guard_tmp}/dirty"
	guard_term="$(printf '\141\143\162\145')"
	printf '// see the %s-handler for details\n' "$guard_term" \
		>"${guard_tmp}/dirty/violation.go"
	git -C "${guard_tmp}/dirty" add -A
	git -C "${guard_tmp}/dirty" -c commit.gpgsign=false commit -qm violation
	assert_exit 1 "confidentiality guard: a real bounded occurrence is still CAUGHT (#486)" \
		env -C "${guard_tmp}/dirty" bash "$GUARD_SH"
	assert_contains "violation.go" \
		"confidentiality guard: the hit names the offending file (#486)" \
		env -C "${guard_tmp}/dirty" bash "$GUARD_SH"

	# (3) A substring word must still NOT trip it -- the boundary behaviour the
	# guard's own self-test asserts, pinned end-to-end over a real tree so a
	# future widening of the pattern fails here rather than in someone's PR.
	new_repo "${guard_tmp}/substr"
	printf 'const w = "massacre wiseacre acreage"\n' >"${guard_tmp}/substr/words.go"
	git -C "${guard_tmp}/substr" add -A
	git -C "${guard_tmp}/substr" -c commit.gpgsign=false commit -qm words
	assert_exit 0 "confidentiality guard: substring words do not false-positive (#486)" \
		env -C "${guard_tmp}/substr" bash "$GUARD_SH"

	# (4) THE #486 PATHS. Each of these made the guard print "clean" and exit 0.
	# Exit 2 (not 1) is asserted deliberately: "could not run" is a different
	# outcome from "found the term", and conflating them would leave CI unable
	# to tell a broken guard from a real violation.

	# 4a. Not a repository at all.
	mkdir -p "${guard_tmp}/norepo"
	assert_exit 2 "confidentiality guard: OUTSIDE a repository refuses to report clean (#486)" \
		env -C "${guard_tmp}/norepo" bash "$GUARD_SH"

	# 4b. Corrupt index -- git grep exits 128.
	new_repo "${guard_tmp}/badindex"
	printf 'GARBAGE' >"${guard_tmp}/badindex/.git/index"
	assert_exit 2 "confidentiality guard: a CORRUPT INDEX refuses to report clean (#486)" \
		env -C "${guard_tmp}/badindex" bash "$GUARD_SH"

	# 4c. Dangling gitdir pointer -- the shape a broken worktree/submodule has.
	new_repo "${guard_tmp}/badgitdir"
	rm -rf "${guard_tmp}/badgitdir/.git"
	printf 'gitdir: /nonexistent/gitdir\n' >"${guard_tmp}/badgitdir/.git"
	assert_exit 2 "confidentiality guard: a DANGLING gitdir refuses to report clean (#486)" \
		env -C "${guard_tmp}/badgitdir" bash "$GUARD_SH"

	# 4d. Tracked files absent from the working tree. git grep reads the WORKING
	# TREE and skips missing files silently, so this returns exit 1 -- byte-for-
	# byte the "clean" answer. Only the coverage check distinguishes it, which
	# is why that check is load-bearing rather than belt-and-braces.
	new_repo "${guard_tmp}/nocheckout"
	rm -f "${guard_tmp}/nocheckout/main.go"
	assert_exit 2 "confidentiality guard: an UNPOPULATED working tree refuses to report clean (#486)" \
		env -C "${guard_tmp}/nocheckout" bash "$GUARD_SH"
	assert_contains "ZERO readable files" \
		"confidentiality guard: the empty scan SAYS nothing was looked at (#486)" \
		env -C "${guard_tmp}/nocheckout" bash "$GUARD_SH"

	# (5) The clean message must state the scan's extent. "clean" on its own is
	# the string that was printed over zero files; a count makes an empty scan
	# visible in the log even if some future path slips past the checks above.
	assert_contains "files scanned" \
		"confidentiality guard: a clean result reports HOW MUCH was scanned (#486)" \
		env -C "${guard_tmp}/clean" bash "$GUARD_SH"

	rm -rf "$guard_tmp"
fi

echo
echo "== every job declares timeout-minutes ===================================="

# A job with no `timeout-minutes` inherits GitHub's 360-minute default, which
# for a REQUIRED check is indistinguishable from no bound at all: a check stuck
# `in_progress` never goes red. The PR sits at mergeable_state=blocked behind a
# spinner, with no failure to notice, no notification, and nothing to tell it
# apart from "CI is still warming up" except watching the clock.
#
# Observed for real (#468): `CI Gate Self-Test` wedged in `apt-get update` for
# 50 minutes on PR #459 -- a job whose normal runtime is 17-22 seconds -- while
# every other check on the board sat green. Cancelling and re-running cleared
# it in 17s, so nothing about the change under test was involved.
#
# The `queue-watchdog` in benchmark.yml does NOT cover this. It watches for a
# job that never STARTS; timeout-minutes is the only instrument that bounds a
# job which started and then wedged mid-step. The two are complements, and the
# repository had only one of them.
#
# This is a LIVENESS rule, not a performance budget. The values in the
# workflows are deliberately several times the observed runtime; the question
# each one answers is "has this wedged?", never "is this fast enough?". Raising
# one because a job legitimately got slower is fine. Deleting one is not.
timeout_probe() { # <root> -- prints PASS/FAIL lines plus __COUNTS__
	python3 - "$1" <<'PY_INNER'
import glob, os, sys

root = sys.argv[1]
try:
    import yaml
except ImportError:
    print("__SKIP__ pyyaml unavailable")
    sys.exit(0)

# GitHub's implicit default. Declaring it explicitly buys nothing, so a job
# that "declares" 360 is treated as undeclared rather than waved through on a
# technicality.
GITHUB_DEFAULT_MINUTES = 360

failures, passes = [], []
files = sorted(glob.glob(os.path.join(root, ".github", "workflows", "*.y*ml")))
if not files:
    print("__SKIP__ no workflow files found under {}".format(root))
    sys.exit(0)

for f in files:
    base = os.path.basename(f)
    try:
        doc = yaml.safe_load(open(f))
    except Exception:  # noqa: BLE001 -- the YAML-parse guard above owns this
        continue
    if not isinstance(doc, dict):
        continue

    jobs = {k: v for k, v in (doc.get("jobs") or {}).items() if isinstance(v, dict)}
    if not jobs:
        continue

    bounded = 0
    for jid, spec in sorted(jobs.items()):
        name = str(spec.get("name") or jid)
        # A reusable-workflow call cannot carry timeout-minutes; the bound has
        # to live in the called workflow's own jobs, which this same guard
        # covers when that workflow is in this repository.
        if "uses" in spec and "steps" not in spec:
            bounded += 1
            continue

        raw = spec.get("timeout-minutes")
        if raw is None:
            failures.append(
                f"{base}: job {name!r} declares no timeout-minutes, so it inherits "
                f"GitHub's {GITHUB_DEFAULT_MINUTES}-minute default. A wedged step leaves the "
                f"check pending, and a check that is pending is never red -- if this job "
                f"feeds a required check it blocks the PR silently (#468)."
            )
            continue

        # `timeout-minutes: ${{ ... }}` is legal YAML and legal Actions, but it
        # is not checkable here, so it is accepted and named rather than
        # silently counted as a pass.
        if isinstance(raw, str) and "${{" in raw:
            passes.append(f"{base}: job {name!r} bounds itself with an expression ({raw})")
            bounded += 1
            continue

        try:
            mins = int(raw)
        except (TypeError, ValueError):
            failures.append(f"{base}: job {name!r} has a non-numeric timeout-minutes ({raw!r})")
            continue

        if mins <= 0:
            failures.append(f"{base}: job {name!r} has timeout-minutes: {mins}, which is not a bound")
        elif mins >= GITHUB_DEFAULT_MINUTES:
            failures.append(
                f"{base}: job {name!r} has timeout-minutes: {mins}, at or above GitHub's "
                f"{GITHUB_DEFAULT_MINUTES}-minute default -- that is the same as declaring nothing."
            )
        else:
            bounded += 1

    if bounded and not [x for x in failures if x.startswith(base + ":")]:
        passes.append(f"{base}: all {bounded} job(s) declare a timeout-minutes bound")

for p_ in passes:
    print(f"PASS  {p_}")
for f_ in failures:
    print(f"FAIL  {f_}")
print(f"__COUNTS__ {len(passes)} {len(failures)}")
PY_INNER
}

to_out="$(timeout_probe "$REPO_ROOT")"
case "$to_out" in
	__SKIP__*) echo "SKIP  job timeout guard ($to_out)" ;;
	*)
		echo "$to_out" | grep -v '^__COUNTS__' || true
		to_counts="$(echo "$to_out" | sed -n 's/^__COUNTS__ //p')"
		if [ -n "$to_counts" ]; then
			read -r to_pass to_fail <<<"$to_counts"
			pass=$((pass + to_pass))
			fail=$((fail + to_fail))
		else
			bad "job timeout guard did not run"
		fi

		# NEGATIVE CONTROL. The rule above is the kind that passes forever
		# because it is looking at the wrong thing -- which is precisely the
		# failure this whole script exists to catch, and precisely how the
		# original benchstat grep stayed dead for 473 runs. So prove it fires:
		# strip the bound from one real job in a throwaway copy of the tree and
		# require the guard to report it. A green run below means the guard
		# reported the tree clean AND demonstrated it can report otherwise.
		TO_SANDBOX="$(mktemp -d)"
		mkdir -p "${TO_SANDBOX}/.github/workflows"
		cp "$REPO_ROOT"/.github/workflows/*.y*ml "${TO_SANDBOX}/.github/workflows/" 2>/dev/null || true
		# benchmark.yml carries the job from #468 itself. Strip the FIRST
		# job-level bound in it, whatever its value -- keyed on shape rather
		# than on a literal, so re-sizing a timeout cannot quietly disarm the
		# control.
		neg_wf="${TO_SANDBOX}/.github/workflows/benchmark.yml"
		if [ -f "$neg_wf" ] && grep -qE '^    timeout-minutes: [0-9]+$' "$neg_wf" &&
			sed -i '0,/^    timeout-minutes: [0-9]\+$/{/^    timeout-minutes: [0-9]\+$/d}' "$neg_wf"; then
			neg_out="$(timeout_probe "$TO_SANDBOX")"
			neg_counts="$(echo "$neg_out" | sed -n 's/^__COUNTS__ //p')"
			read -r _ neg_fail <<<"${neg_counts:-0 0}"
			if [ "${neg_fail:-0}" -ge 1 ]; then
				ok "negative control: a job whose timeout-minutes is deleted IS reported (${neg_fail} finding(s))"
			else
				bad "negative control: deleting a job's timeout-minutes was NOT reported — this guard cannot fail"
			fi
			if echo "$neg_out" | grep -q 'declares no timeout-minutes'; then
				ok "negative control: the finding names the unbounded job and says why it matters"
			else
				bad "negative control: the guard fired but not with the unbounded-job diagnosis"
				echo "$neg_out" | sed 's/^/        | /'
			fi
		else
			bad "negative control: could not construct an unbounded-job fixture"
		fi
		rm -rf "$TO_SANDBOX"
		;;
esac

echo
echo "== shell lint on every script in scripts/ ================================"

# DISCOVERED, not enumerated. This was a hardcoded five-entry array, and the
# consequence was exactly what a hardcoded list always produces: scripts landed
# in scripts/ and were never linted by anything. scripts/fuzz-classify-test.sh
# sat unlinted from the day it was added, and the four govulncheck-* scripts
# would have joined it. A lint list that has to be edited by hand is a lint
# list that silently shrinks in relative terms every time the directory grows.
#
# The floor below is the other half of the guard: a glob that matches nothing
# (wrong SCRIPT_DIR, a `set -f` somewhere above, a rename of the directory)
# would otherwise report "clean on all 0 scripts" and pass.
OWNED_SCRIPTS=()
while IFS= read -r s; do
	OWNED_SCRIPTS+=("$s")
done < <(find "$SCRIPT_DIR" -maxdepth 1 -name '*.sh' -type f | LC_ALL=C sort)

SCRIPT_FLOOR=5
if [ "${#OWNED_SCRIPTS[@]}" -ge "$SCRIPT_FLOOR" ]; then
	ok "discovered ${#OWNED_SCRIPTS[@]} shell scripts in scripts/ (floor ${SCRIPT_FLOOR})"
else
	bad "discovered only ${#OWNED_SCRIPTS[@]} shell scripts in scripts/, expected >= ${SCRIPT_FLOOR} — the glob is broken, not the tree"
fi

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
elif [ -n "${CI_GATES_REQUIRE_SHELLCHECK:-}" ]; then
	# In CI the tool is expected to be present (it ships in the ubuntu-latest
	# image), so absence is a broken environment, not a reason to shrug. Left
	# as a SKIP this would silently downgrade the shell lint to nothing at all
	# the day the image drops the package, and the board would stay green --
	# the same "a gate that cannot fail" shape this whole script exists to
	# prevent. The benchmark.yml `gates` job sets this variable; the install
	# step it replaced is gone deliberately (#468), so this IS the backstop.
	bad "shellcheck not installed, but CI_GATES_REQUIRE_SHELLCHECK is set — ${#OWNED_SCRIPTS[@]} scripts went unlinted"
else
	echo "SKIP  shellcheck not installed (set CI_GATES_REQUIRE_SHELLCHECK=1 to make this fatal)"
fi

# Same treatment for the .cjs helpers. ci-queue-watchdog.cjs already had a
# node --check above as part of its own workflow-wiring assertions; this covers
# every .cjs in the directory, including any added later with no wiring test.
OWNED_CJS=()
while IFS= read -r s; do
	OWNED_CJS+=("$s")
done < <(find "$SCRIPT_DIR" -maxdepth 1 -name '*.cjs' -type f | LC_ALL=C sort)

if [ "${#OWNED_CJS[@]}" -eq 0 ]; then
	bad "no .cjs helpers discovered in scripts/ — the glob is broken, not the tree"
elif command -v node >/dev/null 2>&1; then
	for s in "${OWNED_CJS[@]}"; do
		if node --check "$s" 2>/dev/null; then
			ok "node --check $(basename "$s")"
		else
			bad "node --check $(basename "$s")"
			node --check "$s" 2>&1 | sed 's/^/        | /'
		fi
	done
else
	echo "SKIP  node not installed (${#OWNED_CJS[@]} .cjs helpers unchecked)"
fi

echo
echo "=========================================================================="
echo "ci-gates-test: ${pass} passed, ${fail} failed"
if [ "$fail" -gt 0 ]; then
	exit 1
fi
exit 0
