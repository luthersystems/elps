#!/usr/bin/env bash
# Final gate step for the govulncheck workflows.
#
# The job goes red for more than one reason and the check name ("govulncheck")
# reads as "security finding" for all of them. This step exists so the LAST
# thing in the log, and the top of the job summary, names which condition
# actually tripped. It never decides anything and never softens anything: it
# fails whenever any condition is set.
#
# Inputs (env, all optional — an unset/empty value counts as "did not trip"):
#   SOURCE_SCAN_EXIT      steps.scan.outputs.exit      (source-mode pass)
#   BINARY_BUILD_FAILED   steps.binscan.outputs.build_failed
#   BINARY_VULNS_FOUND    steps.binscan.outputs.vulns_found
#
# Run locally as:
#   BINARY_BUILD_FAILED=1 scripts/govulncheck-fail-summary.sh
set -uo pipefail

SOURCE_SCAN_EXIT="${SOURCE_SCAN_EXIT:-0}"
BINARY_BUILD_FAILED="${BINARY_BUILD_FAILED:-0}"
BINARY_VULNS_FOUND="${BINARY_VULNS_FOUND:-0}"
SUMMARY_FILE="${GITHUB_STEP_SUMMARY:-/dev/null}"

reasons=()
if [ "$SOURCE_SCAN_EXIT" != "0" ] && [ -n "$SOURCE_SCAN_EXIT" ]; then
  reasons+=("VULNERABILITY: the source-mode scan reported a finding.")
fi
if [ "$BINARY_BUILD_FAILED" != "0" ] && [ -n "$BINARY_BUILD_FAILED" ]; then
  reasons+=("BUILD FAILURE: a main package did not compile, so the binary-mode pass could not scan it. This is NOT a vulnerability finding.")
fi
if [ "$BINARY_VULNS_FOUND" != "0" ] && [ -n "$BINARY_VULNS_FOUND" ]; then
  reasons+=("VULNERABILITY: the binary-mode pass reported a finding against a binary that built fine.")
fi

if [ "${#reasons[@]}" -eq 0 ]; then
  echo "No govulncheck condition tripped — nothing to fail."
  exit 0
fi

{
  echo "## govulncheck failed"
  echo
  for r in "${reasons[@]}"; do
    echo "- ${r}"
  done
  echo
} >>"$SUMMARY_FILE"

echo "govulncheck failed for the following reason(s):"
for r in "${reasons[@]}"; do
  echo "  - ${r}"
done

exit 1
