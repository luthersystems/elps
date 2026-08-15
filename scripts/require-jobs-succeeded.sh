#!/usr/bin/env bash
#
# Aggregate gate: assert that EVERY upstream job in the workflow succeeded.
#
# Extracted verbatim from the "Assert every job in this workflow succeeded" step
# of the `required` job in .github/workflows/benchmark.yml so the logic is
# syntax-checked, shellchecked and testable by scripts/ci-gates-test.sh.
#
# Branch protection matches a required status check by NAME, so the workflow
# has exactly one aggregate job whose name never changes and which `needs:`
# every other job. This is the body of that job. `success` is the ONLY pass:
# `failure`, `cancelled` and `skipped` all mean the work was not demonstrably
# done, and a skipped required check reads as green rather than red.
#
# Inputs (env):
#   RESULTS   space-separated job results, from `${{ join(needs.*.result, ' ') }}`
#
# Exits 0 if every result is `success`, 1 otherwise.
#
# Run locally as:  RESULTS='success failure' scripts/require-jobs-succeeded.sh
set -euo pipefail

# Sourced from the environment rather than being interpolated. `-` (not `:-`)
# so an unset RESULTS stays an empty string under `set -u`, preserving the
# original step's behaviour exactly.
RESULTS="${RESULTS-}"

echo "upstream job results: ${RESULTS}"
rc=0
# Unquoted on purpose: this is the one place word splitting is the point --
# RESULTS is a space-separated list of job results.
# shellcheck disable=SC2086
for r in ${RESULTS}; do
  # success is the ONLY pass. failure, cancelled and skipped all mean
  # the work was not demonstrably done.
  [ "$r" = "success" ] || rc=1
done
if [ "$rc" -ne 0 ]; then
  echo "::error::A job in this workflow did not succeed (results: ${RESULTS}). Open the individual jobs above."
  exit 1
fi
echo "All jobs in this workflow succeeded."
