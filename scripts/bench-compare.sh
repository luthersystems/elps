#!/usr/bin/env bash
#
# Adjudicate the two benchmark arms: pre-flight their comparability, run
# benchstat over them, hand the table to cmd/benchgate, and
# assemble the PR comment body.
#
# Extracted verbatim from the "Compare with benchstat" step of
# .github/workflows/benchmark.yml so the logic is syntax-checked, shellchecked
# and testable by scripts/ci-gates-test.sh. Bash inside a `run: |` block is none
# of those things, and this repo has already lost 473 runs to a CI gate that
# could never fire. This script makes no decisions of its own -- the verdict is
# cmd/benchgate's, and the pass/fail is scripts/bench-gate-fail.sh's.
#
# `set -euo pipefail` reproduces the original step's effective flags exactly:
# GitHub runs `run:` bodies under `bash -e {0}`, and the step then added
# `set -uo pipefail` on top of that. The `-e` is load-bearing here only in the
# sense that every command whose failure is EXPECTED already carries an
# explicit `|| var=$?` or `|| true`; do not remove those.
#
# Inputs (env):
#   GITHUB_WORKSPACE  root holding the two checkouts (pr/ and base/)
#   BENCH_COUNT       samples per arm, for the comment footer
#   GITHUB_OUTPUT     step-output file; `gate_status` and `result` are written
#   BENCHGATE         path to the benchgate binary; defaults to
#                     $GITHUB_WORKSPACE/bin/benchgate, which the workflow's
#                     "Build the benchmark gate" step produces FROM THE PR TREE
#
# Reads bench-baseline.txt / bench-current.txt from $PWD (written by
# scripts/bench-run-arms.sh) and writes arms-check.txt, benchstat-output.txt and
# gate-report.txt beside them.
#
# Exits 0 in every branch: this script REPORTS the verdict via
# `gate_status` on $GITHUB_OUTPUT, and the workflow's "Fail on regressions" step
# is the only thing that turns a verdict into a red build.
#
# Run locally as:
#   GITHUB_WORKSPACE=/path/to/ws GITHUB_OUTPUT=/dev/stdout BENCH_COUNT=10 \
#     scripts/bench-compare.sh
set -euo pipefail

# BOTH trees live in subdirectories (pr/ and base/), so nothing from
# the repository is at the workspace root -- including these scripts.
# A bare `./scripts/bench-arms-check.sh` here exits 127 "No such file",
# which the final step then reported as "Benchmark regressions
# detected", i.e. a real-looking failure with a fictional cause. The
# existence check below turns that class into a named error, and the
# final step no longer attributes unknown exit codes to regressions.
#
# Both the scripts and the gate BINARY come from the PR arm
# deliberately: a PR that changes the gate must be adjudicated by its
# own version of it, not by main's.
PR_TREE="${GITHUB_WORKSPACE}/pr"
GATE="${BENCHGATE:-${GITHUB_WORKSPACE}/bin/benchgate}"
ARMS="${PR_TREE}/scripts/bench-arms-check.sh"
# The gate's shipped waiver list. The Go binary has no built-in default --
# elps and substrate keep theirs in different places, and a tool that guessed
# would silently adjudicate with the wrong list -- so the caller names it.
# BENCH_WAIVERS still overrides this, and setting it EMPTY switches waivers off
# entirely, which can only make the gate stricter.
WAIVERS="${PR_TREE}/scripts/benchstat-waivers.txt"

missing=""
for s in "$GATE" "$ARMS"; do
  [ -f "$s" ] || missing="${missing} ${s}"
done
if [ -n "$missing" ]; then
  echo "::error::benchmark gate scripts missing from the PR checkout:${missing}. The workflow expects the repository at \$GITHUB_WORKSPACE/pr and the benchgate binary at \$GITHUB_WORKSPACE/bin/benchgate (see the two-tree checkout and the \"Build the benchmark gate\" step above); if the checkout layout changed, these paths must change with it."
  echo "gate_status=2" >> "$GITHUB_OUTPUT"
  {
    echo 'result<<BENCHSTAT_EOF'
    echo '## Benchmark gate could not run'
    echo ''
    echo 'The gate scripts were not found in the PR checkout:'
    echo '```'
    echo "${missing}"
    echo '```'
    echo 'BENCHSTAT_EOF'
  } >> "$GITHUB_OUTPUT"
  exit 0
fi

# Both arms are produced by the step above, in this job. An empty
# baseline is no longer a benign cache miss -- it means the base
# checkout or its benchmark run failed, and silently degrading to
# "PR results only" would hide that. Fail loudly.
if [ ! -s bench-baseline.txt ]; then
  echo "::error::bench-baseline.txt is empty — the base arm did not produce results."
  echo "gate_status=2" >> "$GITHUB_OUTPUT"
  {
    echo 'result<<BENCHSTAT_EOF'
    echo '## Benchmark Results (base arm FAILED — no comparison)'
    echo ''
    echo 'The base checkout or its benchmark run produced nothing.'
    echo 'These are the PR-side numbers only; they are NOT a comparison.'
    echo ''
    echo '```'
    cat bench-current.txt
    echo '```'
    echo 'BENCHSTAT_EOF'
  } >> "$GITHUB_OUTPUT"
  exit 0
fi

# Comparability BEFORE comparison. benchstat does not complain when
# the arms cannot pair -- it emits a normal-looking table with no
# comparison rows, and the gate downstream can only report the
# symptom ("could not be interpreted"). This names the cause: a
# goos/goarch/cpu mismatch, or a GOMAXPROCS suffix mismatch that
# makes EnvGet-4 and EnvGet-2 two unrelated benchmarks.
arms_rc=0
"$ARMS" bench-baseline.txt bench-current.txt > arms-check.txt 2>&1 || arms_rc=$?
cat arms-check.txt
if [ "$arms_rc" -ne 0 ]; then
  echo "::error::The two benchmark arms are not comparable; see the pre-flight report above."
  echo "gate_status=2" >> "$GITHUB_OUTPUT"
  {
    echo 'result<<BENCHSTAT_EOF'
    echo '## Benchmark arms are NOT comparable — no comparison was made'
    echo ''
    echo '```'
    cat arms-check.txt
    echo '```'
    echo 'BENCHSTAT_EOF'
  } >> "$GITHUB_OUTPUT"
  exit 0
fi

# benchstat's own exit code is captured rather than discarded with
# `|| true`: if it crashed, its error text is all that lands in
# benchstat-output.txt, and benchgate turns that into a hard
# exit 2 ("cannot interpret") instead of a silent pass. Swallowing it
# here is what made a benchstat failure indistinguishable from a clean
# comparison.
bench_rc=0
benchstat base=bench-baseline.txt pr=bench-current.txt \
  > benchstat-output.txt 2>&1 || bench_rc=$?
if [ "$bench_rc" -ne 0 ]; then
  echo "::warning::benchstat exited ${bench_rc}; the gate will adjudicate its output."
fi

# The gate is cmd/benchgate, built from the PR tree by the step above --
# deliberately, so a PR that changes the gate is adjudicated by its own version
# of it rather than by main's. It is unit-tested (go test ./cmd/benchgate),
# fixture-tested (scripts/ci-gates-test.sh) and runnable locally
# (make bench-gate). It exits 0 = clean, 1 = regression, 2 = could not
# interpret.
#
# Run BEFORE the comment is assembled, and its report captured, so the
# comment can carry the verdict rather than only the raw table. That
# matters most for a WAIVED row: an accepted regression that is only
# visible to whoever opens the job log is an accepted regression
# nobody reviews, which is the same "green because it stopped
# looking" shape this gate was written to fix.
#
# Redirected rather than piped through tee: this step runs under the
# default `bash -e`, and a pipeline whose first element exits 1 (the
# gate's normal way of saying "regression") would abort the step
# before the PR comment is ever assembled. `|| gate_status=$?` is the
# idiom the rest of this step already uses for exactly that reason.
gate_status=0
"$GATE" -waivers-default "$WAIVERS" benchstat-output.txt > gate-report.txt 2>&1 || gate_status=$?
cat gate-report.txt
echo "gate_status=${gate_status}" >> "$GITHUB_OUTPUT"
case "$gate_status" in
  0) echo "No benchmark regression at or above the configured gates." ;;
  1) echo "::warning::Benchstat detected a significant benchmark regression." ;;
  2) echo "::warning::The benchmark comparison could not be interpreted." ;;
  *) echo "::warning::${GATE} exited ${gate_status} — it did not run to completion." ;;
esac

# Waived and stale-waiver lines are pulled out of the report and shown
# OUTSIDE the collapsed section, so a standing exception is read by
# everyone who reads the PR and not only by whoever expands a details
# block. `|| true` because grep exits 1 on no match, which is the
# normal case and must not fail the step under `set -o pipefail`.
waiver_lines="$(grep -E '^  (WAIVED|WAIVER-|waiver-)' gate-report.txt || true)"

# NOISE-FLOOR lines get the same treatment, and for the same reason. A row whose
# own measured spread is larger than the move it just made cannot be adjudicated
# by this comparison at all (see the resolution-check note in the cmd/benchgate
# package doc). That is a standing problem with the benchmark, not
# a clean result, and it is only ever fixed by someone who sees it -- so it goes
# above the fold rather than inside a collapsed block nobody expands.
noise_lines="$(grep -E '^  NOISE-FLOOR' gate-report.txt || true)"

{
  echo 'result<<BENCHSTAT_EOF'
  echo '## Benchmark Comparison (main baseline vs PR)'
  echo ''
  if [ -n "$noise_lines" ]; then
    echo '### Rows this comparison could not resolve'
    echo ''
    echo 'These timing rows moved past the gate by LESS than their own measured'
    echo 'spread, so this comparison cannot tell the move from noise. They are'
    echo 'NOT counted as regressions and they are NOT suppressed — they are'
    echo 'unmeasurable as sampled. A row that keeps appearing here needs a longer'
    echo '`-benchtime`, or to be kept out of the comparison set; see the'
    echo 'resolution-check note in the `cmd/benchgate` package doc.'
    echo ''
    echo '```'
    echo "$noise_lines"
    echo '```'
    echo ''
  fi
  if [ -n "$waiver_lines" ]; then
    echo '### Reviewed waivers'
    echo ''
    echo 'Declared in `scripts/benchstat-waivers.txt`. A `WAIVED` row was measured,'
    echo 'DID move, and was accepted within the ceiling recorded there — it is not'
    echo 'suppressed. `waiver-unused` means that row is no longer regressing and the'
    echo 'entry can be deleted; `WAIVER-STALE` means it protects nothing at all.'
    echo ''
    echo '```'
    echo "$waiver_lines"
    echo '```'
    echo ''
  fi
  echo '<details>'
  echo '<summary>Click to expand benchstat output</summary>'
  echo ''
  echo '```'
  cat benchstat-output.txt
  echo '```'
  echo ''
  echo '</details>'
  echo ''
  echo '<details>'
  echo '<summary>Click to expand the gate report</summary>'
  echo ''
  echo '```'
  cat gate-report.txt
  echo '```'
  echo ''
  echo '</details>'
  echo ''
  echo "_Both arms measured on the same runner in the same job, interleaved, n=${BENCH_COUNT} each._"
  echo 'BENCHSTAT_EOF'
} >> "$GITHUB_OUTPUT"
