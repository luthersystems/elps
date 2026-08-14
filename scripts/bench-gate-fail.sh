#!/usr/bin/env bash
#
# Turn the benchmark gate's verdict into a red build, and say WHICH verdict it
# was.
#
# Extracted verbatim from the "Fail on regressions" step of
# .github/workflows/benchmark.yml so the logic is syntax-checked, shellchecked
# and testable by scripts/ci-gates-test.sh.
#
# This script decides nothing. The workflow's `if:` already established that the
# gate did not pass; all this does is name the reason before exiting 1. The
# three-way split is deliberate and must be preserved:
#
#   1  regressions found      -- a real, measured performance problem
#   2  could not interpret    -- a hard failure BY DESIGN; a gate that cannot
#                               read its input must never report success
#   *  no verdict reached     -- e.g. exit 127, the script not being found.
#                               This branch used to say "regressions detected",
#                               which sent the reader hunting a performance
#                               problem that did not exist.
#
# Inputs (env):
#   GATE_STATUS   steps.benchstat.outputs.gate_status; may be empty, which the
#                 `*` branch reports as <unset> exactly as the original did
#
# Always exits 1 -- it only runs when the gate did not pass.
#
# Run locally as:  GATE_STATUS=2 scripts/bench-gate-fail.sh
set -euo pipefail

# Sourced from the environment rather than a `${{ }}` template expansion. The
# `-` (not `:-`) keeps an UNSET variable distinct from nothing at all under
# `set -u`, so the `${status:-<unset>}` below still behaves as it did when the
# expansion produced an empty string.
status="${GATE_STATUS-}"
case "$status" in
  1)
    echo "::error::Benchmark regressions detected (gate exit 1). Review the benchstat comparison in the PR comment and the gate report in the job log."
    ;;
  2)
    echo "::error::The benchmark comparison could not be interpreted (gate exit 2). This is a hard failure by design — a gate that cannot read its input must never report success. See the job log above."
    ;;
  *)
    # Anything else means the gate did not reach a verdict at all --
    # e.g. exit 127, the script not being found. Reporting that as
    # "regressions detected" (which this branch used to do) sends the
    # reader looking for a performance problem that does not exist.
    echo "::error::The benchmark gate did not run to completion (exit ${status:-<unset>}); it reached no verdict, so this is neither a pass nor a measured regression. See the job log above for the failing command."
    ;;
esac
exit 1
