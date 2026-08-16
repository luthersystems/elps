#!/usr/bin/env bash
# Final gate step for the govulncheck workflows.
#
# The job goes red for more than one reason and the check name ("govulncheck")
# reads as "security finding" for all of them. This step exists so the LAST
# thing in the log, and the top of the job summary, names which condition
# actually tripped. It never decides anything and never softens anything: it
# fails whenever any condition is set.
#
# ---------------------------------------------------------------------------
# "DID NOT TRIP" IS NOT "WAS NOT TOLD"  (issue #495)
# ---------------------------------------------------------------------------
# Every input here is a step output the caller reads back out of $GITHUB_OUTPUT.
# An EMPTY or UNSET input therefore does not mean "that condition was clear"; it
# means the scan's status never reached this step at all. Those are different
# facts and they must not collapse to the same branch.
#
# They used to. The inputs were defaulted with `${VAR:-0}`, so an empty value
# was read as an explicit zero, every reason test fell through, and the script
# printed "No govulncheck condition tripped — nothing to fail" and exited 0.
# The caller's `if:` gates this step on `steps.scan.outputs.exit != '0'`, and in
# Actions expression syntax `'' != '0'` is TRUE -- so the step ran BECAUSE the
# outputs were missing, and then reported that nothing was wrong. The caller was
# fail-safe and the script un-failed it.
#
# Now an unreported input exits 2, "could not determine", matching the
# convention scripts/fuzz.sh, scripts/fuzz-budget-check.sh and
# scripts/confidentiality-guard.sh already use and keeping it distinguishable
# from 1, "a real condition tripped". Only an input that is explicitly `0` is
# read as "this condition was clear".
#
# Inputs (env, all REQUIRED -- see above):
#   SOURCE_SCAN_EXIT      steps.scan.outputs.exit             (source-mode pass)
#   BINARY_SCAN_EXIT      steps.binscan.outputs.exit          (binary-mode aggregate)
#   BINARY_BUILD_FAILED   steps.binscan.outputs.build_failed
#   BINARY_VULNS_FOUND    steps.binscan.outputs.vulns_found
#
# BINARY_SCAN_EXIT is the other half of the same disagreement. The caller's
# `if:` also fires on `steps.binscan.outputs.exit != '0'`, but that output was
# never passed in, so a binary pass reporting a failure this script has no
# specific reason for would again be summarised as "nothing to fail". It is an
# input now, and an aggregate failure that no specific condition accounts for is
# reported as UNATTRIBUTED rather than as clear.
#
# Run locally as (all four, or it will tell you what it was not told):
#   SOURCE_SCAN_EXIT=0 BINARY_SCAN_EXIT=1 BINARY_BUILD_FAILED=1 \
#     BINARY_VULNS_FOUND=0 scripts/govulncheck-fail-summary.sh
set -uo pipefail

SUMMARY_FILE="${GITHUB_STEP_SUMMARY:-/dev/null}"

# 2 == "the gate could not determine its inputs", as distinct from 1 == "a
# condition tripped". Conflating them would leave CI unable to tell a broken
# gate from a real finding.
EXIT_UNDETERMINED=2

reasons=()
unreported=()

# record <NAME> <value> <reason-if-non-zero>
#
# Empty is never "clear": it is recorded as unreported and handled below.
record() {
  local name="$1" value="$2" reason="$3"
  if [ -z "$value" ]; then
    unreported+=("$name")
    return
  fi
  if [ "$value" != "0" ]; then
    reasons+=("$reason")
  fi
}

record SOURCE_SCAN_EXIT "${SOURCE_SCAN_EXIT-}" \
  "VULNERABILITY: the source-mode scan reported a finding."
record BINARY_BUILD_FAILED "${BINARY_BUILD_FAILED-}" \
  "BUILD FAILURE: a main package did not compile, so the binary-mode pass could not scan it. This is NOT a vulnerability finding."
record BINARY_VULNS_FOUND "${BINARY_VULNS_FOUND-}" \
  "VULNERABILITY: the binary-mode pass reported a finding against a binary that built fine."

# The binary-mode aggregate is not a fourth independent condition: it is 1
# exactly when build_failed or vulns_found is. So it is only interesting when it
# DISAGREES with them -- an aggregate failure that neither specific output
# accounts for means the binary pass failed for a reason this summary cannot
# name, and "cannot name it" is not "nothing happened".
if [ -z "${BINARY_SCAN_EXIT-}" ]; then
  unreported+=("BINARY_SCAN_EXIT")
elif [ "${BINARY_SCAN_EXIT}" != "0" ] &&
  [ "${BINARY_BUILD_FAILED-}" = "0" ] && [ "${BINARY_VULNS_FOUND-}" = "0" ]; then
  reasons+=("UNATTRIBUTED FAILURE: the binary-mode pass reported exit=${BINARY_SCAN_EXIT}, but neither build_failed nor vulns_found is set. It failed for a reason this summary cannot name — read the step log.")
fi

# An input that never arrived is reported first and loudest: it means the gate
# could not run, which is a worse state than any single condition it might have
# found.
if [ "${#unreported[@]}" -ne 0 ]; then
  {
    echo "## govulncheck: the gate could not determine its inputs"
    echo
    echo "These scan outputs were empty or unset, so their conditions were never checked:"
    echo
    for u in "${unreported[@]}"; do
      echo "- \`${u}\`"
    done
    echo
    echo "An empty step output means the scan's status never reached this step —"
    echo "not that the scan was clean. Check that the scan steps ran, and that"
    echo "their \`id:\` and output names still match this step's \`env:\` block."
    echo
  } >>"$SUMMARY_FILE"

  echo "::error title=govulncheck gate could not run::Scan outputs were empty or unset (${unreported[*]}), so nothing was verified. An unreported scan status is not a clean one — refusing to summarise this run as passing. See #495."
  echo "govulncheck gate could not determine its inputs; unreported:"
  for u in "${unreported[@]}"; do
    echo "  - ${u}"
  done
  if [ "${#reasons[@]}" -ne 0 ]; then
    echo "conditions that DID trip among the inputs that were reported:"
    for r in "${reasons[@]}"; do
      echo "  - ${r}"
    done
  fi
  exit "$EXIT_UNDETERMINED"
fi

if [ "${#reasons[@]}" -eq 0 ]; then
  echo "No govulncheck condition tripped — nothing to fail (all four scan outputs reported, all clear)."
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
