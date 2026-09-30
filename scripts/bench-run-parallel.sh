#!/usr/bin/env bash
#
# Measure the RunParallel benchmarks -- the ones named *Parallel, which the
# gated job skips because GOMAXPROCS=1 cannot express contention (#767) -- at
# the GOMAXPROCS the `benchmark-parallel` job pins (4), both arms interleaved
# exactly as bench-run-arms.sh does, then report benchgate's verdict.
#
# INFORMATIONAL, NOT A GATE, and the reason is measured: on identical code at
# GOMAXPROCS=2, BenchmarkPackageGetFunParallel (an ~11 ns/op lookup) spread
# ~30% within one arm and its median moved -1%, -16% and -28% across three CI
# runs (#763 twice, a dependabot PR) -- at or past the 30% fitness ceiling, so
# a gate would mostly answer "re-measure". The verdict is printed so a real
# contention regression is visible; it does not fail the job. The job DOES
# fail if the benchmarks do not build or run, or if nothing matched.
#
# Inputs (env): BENCH_COUNT, GOMAXPROCS, BENCHGATE (default
# $GITHUB_WORKSPACE/bin/benchgate). Expects ./base and ./pr.
set -euo pipefail

: > bench-parallel-base.txt
: > bench-parallel-pr.txt
for round in $(seq 1 "${BENCH_COUNT}"); do
  echo "::group::round ${round}/${BENCH_COUNT}"
  (cd base && go test -bench='Parallel$' -benchmem -benchtime=100ms -count=1 \
    -run='^$' -timeout=10m ./...) | tee -a bench-parallel-base.txt
  (cd pr && go test -bench='Parallel$' -benchmem -benchtime=100ms -count=1 \
    -run='^$' -timeout=10m ./...) | tee -a bench-parallel-pr.txt
  echo "::endgroup::"
done

for f in bench-parallel-base.txt bench-parallel-pr.txt; do
  if ! grep -q '^Benchmark.*Parallel' "$f"; then
    echo "::error::no *Parallel benchmark ran in ${f}; the -bench pattern or the naming convention broke" >&2
    exit 2
  fi
done

GATE="${BENCHGATE:-${GITHUB_WORKSPACE}/bin/benchgate}"
rc=0
BENCH_GOMAXPROCS="${GOMAXPROCS:-0}" BENCH_WAIVERS='' "$GATE" \
  -base bench-parallel-base.txt -head bench-parallel-pr.txt | tee bench-parallel-verdict.txt || rc=$?
echo "benchgate exit ${rc} (informational: 0 clean, 1 regression, 3 runner-unfit)"
if [ -n "${GITHUB_STEP_SUMMARY:-}" ]; then
  {
    echo "## Parallel benchmarks (GOMAXPROCS=${GOMAXPROCS:-?}, informational)"
    echo '```'
    cat bench-parallel-verdict.txt
    echo '```'
  } >>"$GITHUB_STEP_SUMMARY"
fi
case "$rc" in
  0|1|3) exit 0 ;;
  *) echo "::error::benchgate could not adjudicate the parallel arms (exit ${rc})" >&2; exit "$rc" ;;
esac
