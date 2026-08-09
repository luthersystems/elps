#!/usr/bin/env bash
# Source-mode govulncheck scan for the scheduled workflow.
#
# Extracted from .github/workflows/govulncheck-scheduled.yml so the logic lives
# in a real, lintable, runnable file and the workflow step stays a thin shim.
# Run it locally exactly as CI does:
#
#   GITHUB_OUTPUT=/dev/stdout scripts/govulncheck-scan.sh
#
# Writes the full report to $REPORT_PATH for the drift-issue body, and the scan's
# exit status to $GITHUB_OUTPUT as `exit=N`. It deliberately does NOT fail the
# step itself -- the workflow decides, after the binary pass has also run, so a
# finding in one mode cannot mask the other.
set -uo pipefail

REPORT_PATH="${REPORT_PATH:-/tmp/govulncheck.txt}"

# `set +e` around the pipeline, then read PIPESTATUS immediately: the pipeline's
# own status is tee's, which succeeds regardless, so reading it would silently
# swallow every finding.
set +e
govulncheck ./... 2>&1 | tee "$REPORT_PATH"
status="${PIPESTATUS[0]}"
set -e

echo "exit=${status}" >> "${GITHUB_OUTPUT:-/dev/null}"
exit 0
