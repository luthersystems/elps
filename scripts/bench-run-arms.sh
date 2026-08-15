#!/usr/bin/env bash
#
# Measure BOTH benchmark arms -- PR head and PR base -- on this runner, in this
# job, interleaved.
#
# Extracted verbatim from the "Run benchmarks (both arms, interleaved)" step of
# .github/workflows/benchmark.yml so the logic is syntax-checked, shellchecked
# and testable by scripts/ci-gates-test.sh. Bash inside a `run: |` block is none
# of those things, and this repo has already lost 473 runs to a CI gate that
# could never fire.
#
# Interleaved, alternating arms every round rather than running all of one arm
# then all of the other. A runner that slows down partway through -- thermal
# throttling, a noisy neighbour -- biases a block-ordered run entirely into
# whichever arm ran second. Alternating spreads that drift across both arms
# instead of pooling it into one. Each round contributes one sample per arm, so
# BENCH_COUNT rounds give BENCH_COUNT samples each.
#
# `pipefail` is load-bearing: without it the exit status of `go test ... | tee`
# is tee's, which is 0 even when the benchmarks fail to compile or panic. The
# step would go green, the output files would hold error text, and the
# comparison downstream would be meaningless. GitHub's default shell is
# `bash -e` WITHOUT pipefail, so the original step had to set it explicitly;
# here it is part of the `set -euo pipefail` line below, which reproduces that
# shell's `-e` too.
#
# Inputs (env):
#   BENCH_COUNT   number of interleaved rounds (workflow-level env in
#                 benchmark.yml; see the long note there for why it is 10)
#
# Expects the two working trees at ./base and ./pr, per the two-checkout layout
# in benchmark.yml. Writes bench-baseline.txt and bench-current.txt in $PWD.
#
# Run locally as:  BENCH_COUNT=3 scripts/bench-run-arms.sh
set -euo pipefail

: > bench-baseline.txt
: > bench-current.txt
for round in $(seq 1 "${BENCH_COUNT}"); do
  echo "::group::round ${round}/${BENCH_COUNT}"
  (cd base && go test -bench=. -benchmem -benchtime=100ms -count=1 \
    -run='^$' -timeout=10m ./...) | tee -a bench-baseline.txt
  (cd pr && go test -bench=. -benchmem -benchtime=100ms -count=1 \
    -run='^$' -timeout=10m ./...) | tee -a bench-current.txt
  echo "::endgroup::"
done
