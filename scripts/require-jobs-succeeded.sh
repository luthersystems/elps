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
#   RESULTS     space-separated job results, from `${{ join(needs.*.result, ' ') }}`
#   DOCS_ONLY   optional; `${{ needs.changes.outputs.docs_only }}`
#   EVENT_NAME  optional; `${{ github.event_name }}`
#
# Exits 0 if every result is `success`, 1 otherwise -- including when there are
# no results at all (see below). The ONE exception is the docs-only skip below.
#
# Run locally as:  RESULTS='success failure' scripts/require-jobs-succeeded.sh
#
# THE DOCS-ONLY SKIP (the one deliberate widening of "success is the only pass")
# -----------------------------------------------------------------------------
# Every workflow with a `Required: *` aggregate has a cheap `changes` job
# (scripts/docs-only-changes.sh) and conditions its heavy jobs on
# `needs.changes.outputs.docs_only != 'true'`. On a docs-only PR those jobs
# report `skipped`, and this aggregate must still go green -- a workflow-level
# `paths-ignore` would instead leave the required check pending forever.
#
# `skipped` is accepted ONLY when ALL of these hold, and otherwise it fails
# exactly as before:
#   * DOCS_ONLY is the exact string `true` -- set only by a `changes` job that
#     ran to success and positively classified every changed path. A failed or
#     skipped `changes` job leaves it empty, so its dependants' `skipped` fails;
#   * EVENT_NAME is `pull_request` -- a push to main or a scheduled run never
#     skips, whatever DOCS_ONLY says;
#   * at least one upstream result is `success` (the `changes` job itself is
#     always upstream of the aggregate), so an all-skipped run is not a pass.
# `failure` and `cancelled` are never accepted, docs-only or not.
set -euo pipefail

# Sourced from the environment rather than being interpolated. `-` (not `:-`)
# so an unset RESULTS stays an empty string under `set -u`.
RESULTS="${RESULTS-}"

echo "upstream job results: ${RESULTS}"

# NO RESULTS IS NOT A PASS (issue #485)
# -------------------------------------
# The loop below is `for r in ${RESULTS}`. With RESULTS empty the body never
# executes, `rc` stays 0, and this -- the aggregate whose name sits in branch
# protection, the check that gates merging -- printed "All jobs in this
# workflow succeeded" and exited 0 having verified nothing.
#
# Demonstrated by running, before this guard existed:
#
#     $ RESULTS='' bash scripts/require-jobs-succeeded.sh
#     upstream job results:
#     All jobs in this workflow succeeded.
#     >>> EXIT=0
#
# RESULTS comes from `${{ join(needs.*.result, ' ') }}`. Empty means NO UPSTREAM
# JOB REPORTED, which is reachable without anyone noticing:
#
#   * the `needs:` list is emptied or restructured during a refactor;
#   * the expression is mistyped -- `needs.*.results` (plural) is not an error
#     in GitHub Actions expression syntax, it silently evaluates to the empty
#     string;
#   * every upstream job is renamed and `needs:` is not updated to match.
#
# In each case the required check goes green over an empty set. That is the
# header's own argument about skipped jobs, one level up: `failure`, `cancelled`
# and `skipped` all mean the work was not demonstrably done -- and neither does
# "nothing reported at all". Zero results joins that list.
#
# The earlier comment here noted that the empty-string behaviour was preserved
# deliberately, to match the original inline step. It is not preserved any more:
# matching the original exactly is worth less than the gate being able to fail.
#
# For an aggregate that exists solely to cover other jobs there is no legitimate
# "nothing to check" case -- if this job is running at all, it has upstreams by
# construction. So empty input can only mean the check could not be performed,
# and that must be loud rather than green.
#
# `${RESULTS// /}` strips spaces, so a value that is only separators is caught
# alongside the empty string.
if [ -z "${RESULTS// /}" ]; then
  echo "::error::No upstream job results were reported, so nothing was verified — refusing to report success. This is the required aggregate: an empty 'needs.*.result' means no job's outcome was checked, not that every job passed. Check the 'needs:' list and the RESULTS expression in the workflow."
  exit 1
fi

allow_skip=0
if [ "${DOCS_ONLY-}" = "true" ] && [ "${EVENT_NAME-}" = "pull_request" ]; then
  allow_skip=1
  echo "docs-only pull request: a 'skipped' heavy job counts as intentionally not run."
fi

rc=0
saw_success=0
# Unquoted on purpose: this is the one place word splitting is the point --
# RESULTS is a space-separated list of job results.
# shellcheck disable=SC2086
for r in ${RESULTS}; do
  # success is the ONLY pass. failure, cancelled and skipped all mean
  # the work was not demonstrably done -- except `skipped` on a docs-only PR,
  # where the skip IS the decision (see the header).
  case "$r" in
    success) saw_success=1 ;;
    skipped) [ "$allow_skip" -eq 1 ] || rc=1 ;;
    *) rc=1 ;;
  esac
done
# Only reachable in docs-only mode: otherwise every result had to be `success`,
# and the emptiness guard above already refused a run with no results.
if [ "$allow_skip" -eq 1 ] && [ "$rc" -eq 0 ] && [ "$saw_success" -eq 0 ]; then
  echo "::error::No upstream job succeeded (results: ${RESULTS}). Even on a docs-only PR the 'changes' job must succeed; an all-skipped run verified nothing."
  exit 1
fi
if [ "$rc" -ne 0 ]; then
  echo "::error::A job in this workflow did not succeed (results: ${RESULTS}). Open the individual jobs above."
  exit 1
fi
if [ "$allow_skip" -eq 1 ] && [[ " ${RESULTS} " == *" skipped "* ]]; then
  echo "Every job that ran succeeded; the rest were skipped because this PR changes only documentation."
else
  echo "All jobs in this workflow succeeded."
fi
