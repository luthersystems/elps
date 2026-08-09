#!/usr/bin/env bash
# Binary-mode govulncheck pass for the scheduled workflow.
#
# Extracted from .github/workflows/govulncheck-scheduled.yml. Run locally as:
#
#   GITHUB_OUTPUT=/dev/stdout scripts/govulncheck-binary-scan.sh
#
# ADDITIVE to the source scan, never a replacement: source mode reports
# vulnerabilities in imported packages even when the linker dropped the code,
# which binary mode cannot see. What binary mode adds is reflection-reached code
# (the govulncheck docs state such calls are not reported in source scan mode)
# and the standard library at the version the binary was BUILT with rather than
# the version the scanner runs under.
#
# Library modules have no main packages; there this is a no-op, not an error.
#
# ---------------------------------------------------------------------------
# BUILD FAILURE vs VULNERABILITY FINDING
# ---------------------------------------------------------------------------
# This pass has to `go build` every main package before it has an artifact to
# scan, so there are two independent ways for it to go red and they call for
# completely different responses:
#
#   build_failed=1   the tree did not compile. NOT a security finding.
#   vulns_found=1    the binary built, and govulncheck reported against it.
#
# BOTH still fail, and that is deliberate: a package that does not compile has
# not been scanned, so calling it clean would open a hole in the gate. What
# changes here is only diagnosability -- each condition gets its own loud
# ::error::, its own line in the job summary, and its own step output, so a
# reader (and scripts/govulncheck-drift.cjs, which files the tracking issue)
# can tell them apart.
#
# The triggering example: ui-core run 31232762542 was red under the check name
# "govulncheck" with ZERO vulnerabilities in every tree -- argo-workflows
# v4.0.8 had stopped compiling against a bumped k8s.io/api. Every reader of
# that check concluded "security finding" and was wrong.
#
# `set -o pipefail` below is load-bearing and really is set (it is the `o` in
# `set -uo pipefail`): without it the `govulncheck | tee` pipeline would report
# tee's status, which always succeeds, and every finding would be swallowed.
# Statuses are captured with `|| status=$?` on the command/pipeline itself,
# never via ${PIPESTATUS[0]} -- PIPESTATUS is clobbered by ANY subsequent
# command, including `|| true`, and reading it late silently passes the gate.
set -uo pipefail

REPORT_PATH="${REPORT_PATH:-/tmp/govulncheck.txt}"
BIN_DIR="${BIN_DIR:-/tmp/gv-bins}"
OUT_FILE="${GITHUB_OUTPUT:-/dev/null}"
SUMMARY_FILE="${GITHUB_STEP_SUMMARY:-/dev/null}"

# emit_outputs <build_failed> <vulns_found>
#
# `exit` is the pre-existing aggregate output the workflows already gate on;
# build_failed / vulns_found are the new, separately-readable conditions.
emit_outputs() {
  local aggregate=0
  if [ "$1" != "0" ] || [ "$2" != "0" ]; then
    aggregate=1
  fi
  {
    echo "build_failed=$1"
    echo "vulns_found=$2"
    echo "exit=${aggregate}"
  } >>"$OUT_FILE"
}

# `go list` failing is itself a build-level failure (a syntax error anywhere in
# the module makes it non-zero), and its stdout is empty in that case -- so it
# must NOT be mistaken for "this module has no main packages".
#
# `gt (len .GoFiles) 0` excludes TEST-ONLY main packages -- a directory whose
# only Go files are `*_test.go` declaring `package main`. `go list` reports
# .Name=main for those, but `go build` cannot produce a binary from one and
# fails with "no non-test Go files in ...". That is not a build break to
# report; there is simply no binary to scan. This is NOT a softening of the
# build-failure rule: any package that has non-test Go files is still built,
# and any failure to build it still fails the check.
# Triggering example: luthersystems/substrate cmd/substratehcp/stresstest
# (mockstress_test.go, package main, no non-test sources).
if ! mains_raw="$(go list -f '{{if and (eq .Name "main") (gt (len .GoFiles) 0)}}{{.ImportPath}}{{end}}' ./...)"; then
  echo "::error title=govulncheck binary pass: BUILD FAILURE (not a vulnerability)::\`go list ./...\` failed, so no package could be built or scanned. This is a build/module error, NOT a govulncheck finding."
  {
    echo "=== BUILD FAILURE: go list ./... failed ==="
    echo "No package could be enumerated, so binary-mode govulncheck scanned"
    echo "nothing. This is a build error, NOT a vulnerability finding."
  } >>"$REPORT_PATH"
  {
    echo "### govulncheck binary pass: BUILD FAILURE (not a vulnerability)"
    echo
    echo '`go list ./...` failed — nothing was built, nothing was scanned.'
  } >>"$SUMMARY_FILE"
  emit_outputs 1 0
  exit 1
fi

mains="$(printf '%s\n' "$mains_raw" | grep -v '^$' || true)"
if [ -z "$mains" ]; then
  echo "No main packages in this module — binary mode does not apply."
  emit_outputs 0 0
  exit 0
fi

mkdir -p "$BIN_DIR"
build_failed=0
vulns_found=0
failed_builds=""
found_pkgs=""

for pkg in $mains; do
  out="${BIN_DIR}/$(echo "$pkg" | tr '/' '_')"

  echo "::group::go build ${pkg}"
  build_status=0
  go build -o "$out" "$pkg" || build_status=$?
  echo "::endgroup::"

  if [ "$build_status" -ne 0 ]; then
    build_failed=1
    failed_builds="${failed_builds}${failed_builds:+ }${pkg}"
    rm -f "$out"
    echo "::error title=govulncheck binary pass: BUILD FAILURE (not a vulnerability)::\`go build ${pkg}\` exited ${build_status}. This is a COMPILE ERROR in the tree, NOT a govulncheck finding — the package was not scanned at all. Fix the build, then re-read the scan."
    {
      echo "=== BUILD FAILURE: ${pkg} (go build exit ${build_status}) ==="
      echo "This package could not be compiled, so binary-mode govulncheck did"
      echo "not scan it. This is a build error, NOT a vulnerability finding."
    } >>"$REPORT_PATH"
    continue
  fi

  echo "::group::govulncheck -mode=binary ${pkg}"
  gv_status=0
  govulncheck -mode=binary "$out" 2>&1 | tee -a "$REPORT_PATH" || gv_status=$?
  echo "::endgroup::"
  rm -f "$out"

  if [ "$gv_status" -ne 0 ]; then
    vulns_found=1
    found_pkgs="${found_pkgs}${found_pkgs:+ }${pkg}"
    if [ "$gv_status" -eq 3 ]; then
      # 3 is govulncheck's documented "vulnerabilities found" exit code.
      echo "::error title=govulncheck binary pass: vulnerability found::govulncheck -mode=binary reported a finding in the built binary for ${pkg}. The binary compiled fine — this IS a security finding."
    else
      echo "::error title=govulncheck binary pass: scanner failure::govulncheck -mode=binary exited ${gv_status} for ${pkg}. The binary compiled fine, but ${gv_status} is not govulncheck's vulnerabilities-found code (3), so this is more likely a scanner error than a finding. Failing either way — an unscanned binary is not a clean binary."
    fi
  fi
done

{
  if [ "$build_failed" -ne 0 ]; then
    echo "### govulncheck binary pass: BUILD FAILURE (not a vulnerability)"
    echo
    echo "These \`main\` packages did not compile, so they were **not scanned**:"
    echo
    for p in $failed_builds; do
      echo "- \`${p}\`"
    done
    echo
    echo "Fix the compile error. Do not read this as a security finding."
    echo
  fi
  if [ "$vulns_found" -ne 0 ]; then
    echo "### govulncheck binary pass: findings in built binaries"
    echo
    for p in $found_pkgs; do
      echo "- \`${p}\`"
    done
    echo
  fi
} >>"$SUMMARY_FILE"

emit_outputs "$build_failed" "$vulns_found"

if [ "$build_failed" -ne 0 ] || [ "$vulns_found" -ne 0 ]; then
  exit 1
fi
exit 0
