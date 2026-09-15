#!/usr/bin/env bash
#
# Claude Code on the web -- environment "Setup script" for the elps repository.
#
# Copy the full contents of this file into the "Setup script" field of the environment:
#     claude.ai/code -> New session -> cloud icon -> Add/Edit environment -> Setup script.
#
# It runs ONCE as root on a fresh Ubuntu 24.04 VM and its filesystem output is CACHED
# (~7-day expiry), so it only re-runs when you edit it. Keep it to repo-independent
# system tools only -- the repo is NOT reliably present here. The per-session repair of
# version skew (GOTOOLCHAIN pin, golangci-lint on PATH) lives in
# .claude/hooks/session-start.sh (a SessionStart hook); this script makes that hook a
# fast no-op by pre-installing the same pins system-wide.
#
# elps is a pure Go project (an embedded Lisp interpreter). There is no cloud infra, no
# Docker, no Playwright. What the Makefile and CI actually need:
#
#   go 1.26.8             .github/workflows/*.yml `go-version`, hook GOTOOLCHAIN pin
#   golangci-lint v2.13.x elps.yml golangci-lint-action `version: v2.13`; hook pins 2.13.2
#   betteralign v0.14.3   Makefile `fieldalign-fix` (via `go run ...@v0.14.3`)
#   govulncheck v1.8.0    govulncheck.yml / govulncheck-scheduled.yml
#   benchstat @latest     benchmark.yml (needs GOTOOLCHAIN=auto; see install_benchstat)
#   (shellcheck)          scripts/ci-gates-test.sh (CI asserts it is present);
#                         parenthesised so this line is not read as a directive
#   gh                    Makefile release / release-notes targets
#   node 20, python3      scripts/*.cjs, scripts/*.py, editors/vscode (both preinstalled)
#
# Preinstalled in the sandbox (do NOT install): Node, Go (may be OLDER than go.mod -- see
# ensure_go), Python.
#
# Optional codex support: the `codex` CLI + the openai/codex-plugin-cc Claude plugin
# (=> /codex:review, and the scripts/codex-delegate.sh wrapper) and sam-at-luther/claude-config
# are baked into the snapshot so codex can be used for selected coding tasks. NO secret is
# handled here: codex auth is done per session with `codex login --device-auth` (prints a URL
# and a one-time code). The environment carries no long-lived credential.
#
set -Eeuo pipefail

# Persistent setup log. The cached setup phase's stdout is not easily inspectable after the
# fact, so mirror every log line (UTC-stamped) to a file that survives into the session
# snapshot. Inspect from any session with:  cat /var/log/cloud-web-setup.log
# Override the path with CLOUD_WEB_SETUP_LOG; falls back to /tmp if /var/log isn't writable.
LOG_FILE="${CLOUD_WEB_SETUP_LOG:-/var/log/cloud-web-setup.log}"
mkdir -p "$(dirname "$LOG_FILE")" 2>/dev/null || true
: > "$LOG_FILE" 2>/dev/null || LOG_FILE=/tmp/cloud-web-setup.log
: > "$LOG_FILE" 2>/dev/null || true

# Keep these four in sync with .github/workflows/elps.yml and .claude/hooks/session-start.sh.
GO_VERSION=1.26.8               # CI `go-version`; hook pins GOTOOLCHAIN=go1.26.8
GOLANGCI_VERSION=v2.13.2        # CI pins v2.13 (golangci-lint-action); hook pins 2.13.2
BETTERALIGN_VERSION=v0.14.3     # Makefile `fieldalign-fix`
GOVULNCHECK_VERSION=v1.8.0      # govulncheck.yml / govulncheck-scheduled.yml
ARCH="$(dpkg --print-architecture)"   # auto-detect: amd64 or arm64

log() {
  local line="[cloud-web-setup] $*"
  echo "$line"
  # O_APPEND keeps these short lines atomic across the parallel-phase subshells.
  printf '%s %s\n' "$(date -u +%FT%TZ)" "$line" >> "$LOG_FILE" 2>/dev/null || true
}

# Record any unhandled failure (a `set -e` abort) with its line number, so a step that dies
# in the cached phase leaves a breadcrumb in the log instead of vanishing silently. The
# best-effort steps are already guarded with `|| log ...` / `|| true`, so they won't trip this.
on_err() {
  # $? on entry is the status of the command that tripped `set -e`; $1 is
  # the caller's LINENO, passed in because the trap fires in this function.
  local rc=$?
  log "ERROR: setup aborted at line $1 (exit ${rc})"
  exit "${rc}"
}
trap 'on_err "${LINENO}"' ERR

ensure_go() {
  # The sandbox's PATH `go` has been OLDER than go.mod (go1.24.7 vs go 1.25.0). With
  # GOTOOLCHAIN=auto that skew makes `go test -cover` die with `no such tool "covdata"`
  # (the session hook's header explains why). Installing the exact CI version as the PATH
  # `go` removes the skew at the root: the hook's GOTOOLCHAIN pin then selects the toolchain
  # that is already on disk and nothing is downloaded per session.
  if command -v go >/dev/null 2>&1; then
    have="$(go version | awk '{print $3}' | sed 's/^go//')"
    if [ "$have" = "$GO_VERSION" ]; then
      log "preinstalled go ${have} matches CI pin; skipping"
      return 0
    fi
    log "preinstalled go ${have} != CI pin ${GO_VERSION}; installing ${GO_VERSION}"
  else
    log "go not found; installing ${GO_VERSION}"
  fi
  curl -fsSL "https://go.dev/dl/go${GO_VERSION}.linux-${ARCH}.tar.gz" -o /tmp/go.tgz
  rm -rf /usr/local/go && tar -C /usr/local -xzf /tmp/go.tgz
  rm -f /tmp/go.tgz
  ln -sf /usr/local/go/bin/go /usr/local/bin/go
  ln -sf /usr/local/go/bin/gofmt /usr/local/bin/gofmt
  go version
}

install_golangci() {
  # Findings are not stable across golangci-lint minor versions (this tree: 2.6.2 -> 0
  # issues, 2.11.4 -> 27), and `make static-checks` warns when the PATH version differs
  # from CI's. Install CI's pin on /usr/local/bin; the session hook then finds its
  # version-scoped copy missing and installs the same version to ~/.cache -- harmless.
  # Direct release-tarball download (same as the hook), NOT the upstream install.sh: that
  # script resolves the tag through api.github.com, which the sandbox's egress proxy
  # answers with 403.
  log "installing golangci-lint ${GOLANGCI_VERSION} (.golangci.yml is v2)"
  local ver="${GOLANGCI_VERSION#v}" tmp
  tmp="$(mktemp -d)"
  curl -fsSL --retry 3 --max-time 300 \
    "https://github.com/golangci/golangci-lint/releases/download/v${ver}/golangci-lint-${ver}-linux-${ARCH}.tar.gz" \
    | tar -xz -C "$tmp" --strip-components=1
  install -m 0755 "${tmp}/golangci-lint" /usr/local/bin/golangci-lint
  rm -rf "$tmp"
  golangci-lint --version
}

install_gopls() {
  # Go language server for Claude Code's LSP (diagnostics, hover, go-to-def). Not part of
  # the Go toolchain, so it must be installed separately. GOBIN puts it on /usr/local/bin
  # (on PATH). gopls@latest has required a Go newer than the CI pin before (v0.23 needed
  # >= 1.26 while the pin was 1.25), so it is built under GOTOOLCHAIN=auto, same as
  # benchstat. Best-effort: a gopls hiccup shouldn't poison the whole cache.
  log "installing gopls (Go LSP)"
  GOTOOLCHAIN=auto GOBIN=/usr/local/bin go install golang.org/x/tools/gopls@latest && gopls version \
    || log "gopls install failed (non-fatal; Go LSP unavailable)"
}

install_govulncheck() {
  # Same pin as govulncheck.yml so a local `govulncheck ./...` agrees with CI. Best-effort.
  log "installing govulncheck ${GOVULNCHECK_VERSION}"
  GOBIN=/usr/local/bin go install "golang.org/x/vuln/cmd/govulncheck@${GOVULNCHECK_VERSION}" \
    && govulncheck -version >/dev/null \
    || log "govulncheck install failed (non-fatal)"
}

install_benchstat() {
  # scripts/bench-*.sh and the /benchmark skill call `benchstat`. benchmark.yml installs it
  # with GOTOOLCHAIN=auto because x/perf's go.mod can require a Go newer than the CI pin;
  # mirror that here. Best-effort.
  log "installing benchstat"
  GOTOOLCHAIN=auto GOBIN=/usr/local/bin go install golang.org/x/perf/cmd/benchstat@latest \
    && command -v benchstat >/dev/null \
    || log "benchstat install failed (non-fatal; make bench-* targets need it)"
}

warm_betteralign() {
  # `make fieldalign-fix` runs `go run github.com/dkorunic/betteralign/cmd/betteralign@v0.14.3`.
  # It does NOT read PATH, so this only warms the module cache so the first `go run` in a
  # session is not also a download. betteralign itself needs Go >= 1.26 (Makefile comment),
  # which GO_VERSION now satisfies; GOTOOLCHAIN=auto is kept so a future bump in its own
  # go directive still resolves rather than failing the cached phase.
  # Best-effort: the Makefile target works without this, just slower the first time.
  log "warming module cache for betteralign ${BETTERALIGN_VERSION}"
  GOTOOLCHAIN=auto GOBIN=/usr/local/bin \
    go install "github.com/dkorunic/betteralign/cmd/betteralign@${BETTERALIGN_VERSION}" \
    || log "betteralign warm failed (non-fatal; make fieldalign-fix fetches it on first use)"
}

install_codex_cli() {
  # codex backs the `codex` plugin (/codex:review) and is usable directly from a session for
  # selected coding tasks. node/npm are preinstalled.
  log "installing global npm CLI: @openai/codex"
  npm install -g @openai/codex
  codex --version
}

install_claude_plugins() {
  # Claude Code plugins baked into the cached snapshot: codex (=> /codex:review; its
  # codex-companion.mjs runtime is what scripts/codex-delegate.sh drives) and claude-config
  # (=> qa-professor / pr). Doing it here rather than in the SessionStart hook means the
  # plugin install runs once, not on every session start.
  #
  # Best-effort: `claude` is a harness binary that may not be on PATH during the cached
  # setup phase. When it is, this bakes installed_plugins.json into the snapshot so the
  # plugins load at session start; when it isn't, this is a no-op and `codex` from the CLI
  # still works once logged in. All guarded: this runs under `set -e` and must never poison
  # the cache.
  #
  # Codex AUTH is deliberately NOT done here. Run once per session:
  #     codex login --device-auth
  # It prints a URL + code; approve it in a browser. No token is stored in the snapshot.
  if command -v claude >/dev/null 2>&1; then
    claude plugin marketplace add openai/codex-plugin-cc      >/dev/null 2>&1 || true
    claude plugin marketplace add sam-at-luther/claude-config >/dev/null 2>&1 || true
    for p in codex@openai-codex claude-config@claude-config; do
      if claude plugin install "$p" >/dev/null 2>&1; then
        log "plugin installed: $p"
      else
        log "WARNING: plugin install failed: $p (non-fatal; claude CLI may be unavailable at setup)"
      fi
    done
  else
    log "claude CLI not on PATH at setup -- skipping plugin install (install codex plugin from a session: claude plugin install codex@openai-codex)"
  fi
}

# gh via apt. Kept as its own function so it can run in the parallel phase as the ONLY apt
# user there (phase 1's apt is synchronous and already finished).
install_apt_extras() {
  export DEBIAN_FRONTEND=noninteractive
  log "configuring gh apt repo"
  install -m 0755 -d /etc/apt/keyrings
  curl -fsSL https://cli.github.com/packages/githubcli-archive-keyring.gpg \
    | tee /etc/apt/keyrings/githubcli-archive-keyring.gpg >/dev/null
  chmod go+r /etc/apt/keyrings/githubcli-archive-keyring.gpg
  echo "deb [arch=${ARCH} signed-by=/etc/apt/keyrings/githubcli-archive-keyring.gpg] https://cli.github.com/packages stable main" \
    > /etc/apt/sources.list.d/github-cli.list

  log "installing gh"
  apt-get update -y --allow-releaseinfo-change   # tolerate PPA Release-metadata changes (see phase 1)
  apt-get install -y gh
  gh --version
}

# Phase 0 (apt hardening): on a fresh Ubuntu 24.04 VM the boot-time apt-daily /
# unattended-upgrades timers grab /var/lib/dpkg/lock-frontend a minute or two after boot --
# i.e. right in the gap between phase 1's apt and install_apt_extras' apt. An `apt-get install`
# with no lock timeout then dies instantly with "Could not get lock ... (exit 100)". Two
# defenses, both before any apt-get runs:
#   1. A global apt.conf.d drop-in so EVERY apt-get in this script WAITS up to 10 min for the
#      lock instead of failing immediately.
#   2. Best-effort stop of the background timers so they don't contend at all. No-op (|| true)
#      where systemd isn't the init (e.g. a container sandbox), where the conf drop-in is the
#      real safety net.
log "hardening apt against dpkg-lock contention (DPkg::Lock::Timeout + stop apt-daily timers)"
mkdir -p /etc/apt/apt.conf.d
printf 'DPkg::Lock::Timeout "600";\n' > /etc/apt/apt.conf.d/99-cloud-web-setup
systemctl stop apt-daily.timer apt-daily-upgrade.timer       >/dev/null 2>&1 || true
systemctl stop apt-daily.service apt-daily-upgrade.service   >/dev/null 2>&1 || true

# Phase 1 (sync): base packages the parallel installers depend on.
#   (shellcheck) -- scripts/ci-gates-test.sh lints every owned script with `shellcheck -S
#                  warning`; CI sets CI_GATES_REQUIRE_SHELLCHECK=1 so a missing binary is fatal.
#   bubblewrap  -- codex's sandbox runtime; without it on PATH codex warns on every
#                  invocation and falls back to its bundled copy.
#   util-linux  -- `flock`, required by scripts/codex-delegate.sh (normally present).
#   python3     -- scripts/*.py and the python3 snippets in ci-gates-test.sh (usually
#                  preinstalled; listed so the assumption is explicit).
# Installed in this single synchronous transaction (not the parallel phase) to avoid
# dpkg-lock contention with install_apt_extras.
log "installing base apt packages (incl. shellcheck, bubblewrap, python3)"
export DEBIAN_FRONTEND=noninteractive
# --allow-releaseinfo-change: the base image pre-configures PPAs this repo never installs from
# (deadsnakes, ondrej/php). When one of them changes its Release metadata a bare
# `apt-get update` aborts with exit 100 ("changed its 'Label' value ..."). The flag accepts the
# metadata change and proceeds; safe here because we pull nothing from those PPAs.
apt-get update -y --allow-releaseinfo-change
apt-get install -y --no-install-recommends ca-certificates curl gnupg shellcheck bubblewrap python3 util-linux

# Phase 2 (parallel): non-apt installers PLUS the single apt user (install_apt_extras).
pids=()
ensure_go                & pids+=("$!")
install_golangci         & pids+=("$!")
install_codex_cli        & pids+=("$!")
install_apt_extras       & pids+=("$!")

fail=0
for pid in "${pids[@]}"; do wait "$pid" || fail=1; done
if [ "$fail" -ne 0 ]; then
  log "ERROR: a required installer failed (see above); failing so the cache is not poisoned"
  exit 1
fi

# Phase 3 (sequential): runs after phase 2 so the Go toolchain is guaranteed present.
# All best-effort.
install_gopls
install_govulncheck
install_benchstat
warm_betteralign
install_claude_plugins || log "claude plugin setup failed (non-fatal)"

log "setup complete: go ${GO_VERSION}, golangci-lint ${GOLANGCI_VERSION}, gopls, govulncheck, benchstat, betteralign (cache), shellcheck, gh, codex CLI, bubblewrap installed; claude plugins baked (codex auth: run 'codex login --device-auth' per session)"
# Explicit completion marker. If this line is ABSENT from the log, the script aborted partway
# (look for the "ERROR: setup aborted at line N" breadcrumb above). To check what was skipped
# vs failed, grep the log for: 'skipping plugin install' | 'plugin install failed' | 'WARNING' | 'ERROR'
log "OK: cloud-web-setup finished cleanly -- full log at ${LOG_FILE}"
