#!/usr/bin/env bash
#
# SessionStart hook for Claude Code on the web.
#
# This repairs two pieces of version skew between the web container image and
# what this repository targets.  Neither is a defect in this repository, and
# neither reproduces on a developer laptop, so the whole script is a no-op
# outside a remote session.
#
# 1. `make test` (`go test -cover ./...`) dies with:
#
#        go: no such tool "covdata"
#
#    Go 1.25 changed cmd/distpack to ship only the tools a build action needs
#    (asm cgo compile cover link preprofile vet).  covdata is no longer
#    prebuilt; Go >= 1.25's `go tool` builds it on demand from GOROOT/src, and
#    Go 1.24's `go tool` has no such fallback -- it only stats
#    $GOROOT/pkg/tool/$GOOS_$GOARCH.
#
#    The container's PATH `go` has been older than this module's `go`
#    directive (go1.24.7 vs go 1.25.0), so GOTOOLCHAIN=auto makes it exec the
#    newer toolchain *and* export GOROOT pointing at that toolchain's tree.
#    `go test -cover` then shells out to `go tool covdata percent` for every
#    package that has no test files, and that child resolves `go` from PATH --
#    finding the OLD binary again, now aimed at a GOROOT whose pkg/tool has no
#    covdata.  Hence the error, and hence `go test ./...` (no -cover) working
#    fine, which is what makes this look like a phantom.
#
#    Pinning GOTOOLCHAIN makes parent and child select the same toolchain, so
#    the skew cannot arise.  It also makes the session use exactly the Go that
#    CI uses, rather than whatever the image happens to ship.
#
# 2. golangci-lint findings are not stable across its minor versions, and an
#    unpinned binary silently disagrees with CI.  Measured on this tree: the
#    version below reports 0 issues, v2.11.4 reports 27.  A golangci-lint
#    built with a Go older than the module's `go` directive additionally
#    refuses to start at all ("the Go language version ... is lower than the
#    targeted Go version"), which is how this bites a repository that has
#    moved its `go` directive forward.
#
# Both pins below must be kept in sync with .github/workflows/elps.yml.
set -euo pipefail

if [ "${CLAUDE_CODE_REMOTE:-}" != "true" ]; then
	exit 0
fi

# Keep in sync with `go-version:` in .github/workflows/elps.yml.
GO_TOOLCHAIN="go1.25.13"
# Keep in sync with the golangci-lint-action `version:` in the same file.
GOLANGCI_VERSION="2.6.2"

# Version-scoped so a container reused across repositories does not have two
# pins fighting over one path.
TOOLS_BIN="${HOME}/.cache/claude-code-tools/golangci-lint-${GOLANGCI_VERSION}"
ENV_FILE="${CLAUDE_ENV_FILE:-/dev/null}"

mkdir -p "$TOOLS_BIN"

# --- Go toolchain ---------------------------------------------------------
# Fetching it here means the first build of the session is not also a
# download, and the export makes every `go` invocation agree with CI.
if GOTOOLCHAIN="$GO_TOOLCHAIN" go version >/dev/null 2>&1; then
	printf 'export GOTOOLCHAIN=%s\n' "$GO_TOOLCHAIN" >>"$ENV_FILE"
	echo "session-start: GOTOOLCHAIN pinned to ${GO_TOOLCHAIN}"
else
	echo "session-start: WARNING could not fetch ${GO_TOOLCHAIN}; GOTOOLCHAIN left alone" >&2
fi

# --- golangci-lint --------------------------------------------------------
install_golangci() {
	arch="$(uname -m)"
	case "$arch" in
	x86_64) arch=amd64 ;;
	aarch64 | arm64) arch=arm64 ;;
	*)
		echo "session-start: WARNING unsupported arch ${arch}" >&2
		return 1
		;;
	esac

	url="https://github.com/golangci/golangci-lint/releases/download/v${GOLANGCI_VERSION}/golangci-lint-${GOLANGCI_VERSION}-linux-${arch}.tar.gz"
	tmp="$(mktemp -d)"
	rc=0
	if curl -fsSL --retry 3 --max-time 300 "$url" | tar -xz -C "$tmp" --strip-components=1; then
		install -m 0755 "${tmp}/golangci-lint" "${TOOLS_BIN}/golangci-lint" || rc=1
	else
		rc=1
	fi
	rm -rf "$tmp"
	return "$rc"
}

if "${TOOLS_BIN}/golangci-lint" version 2>/dev/null | grep -q "version ${GOLANGCI_VERSION} "; then
	echo "session-start: golangci-lint ${GOLANGCI_VERSION} already cached"
elif install_golangci; then
	echo "session-start: golangci-lint ${GOLANGCI_VERSION} installed"
else
	echo "session-start: WARNING golangci-lint ${GOLANGCI_VERSION} install failed" >&2
fi

if [ -x "${TOOLS_BIN}/golangci-lint" ]; then
	printf 'export PATH=%s:$PATH\n' "$TOOLS_BIN" >>"$ENV_FILE"
fi
