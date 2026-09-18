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
# FAILURE POLICY (this is the important part -- read before editing)
#
#   This script ALWAYS EXITS 0. A non-zero setup script fails the environment build, and
#   a failed environment build breaks the whole Claude Code web session -- which is a far
#   worse outcome than a session that is merely missing `benchstat`. So:
#
#     * every installer is a function that returns non-zero on failure and is invoked
#       through `run_step` / `run_step_async`, which record the verdict and CONTINUE;
#     * there is no `set -e` and no `exit 1` anywhere: `set -Euo pipefail` plus explicit
#       `|| { log ...; return 1; }` guards on every command that can fail. The ERR trap
#       logs a breadcrumb (`ERROR: unguarded failure at line N`) and does NOT exit;
#     * an EXIT trap (`finish`) writes the summary and forces status 0, so even a bug in
#       this script (an unbound variable, a syntax-level abort) cannot fail the build;
#     * every network step is bounded: `curl --max-time`, `timeout` around `go install`,
#       `npm`, `apt-get` and the `claude plugin` calls, so a hung proxy cannot stall the
#       environment build until the platform kills it;
#     * the last two log lines are the machine-readable verdict:
#           SUMMARY: ok=...  /  SUMMARY: failed=...
#           OK: cloud-web-setup finished ...
#       A session diagnoses itself with:   cat /var/log/cloud-web-setup.log
#
#   A tool in the `failed=` list means exactly one thing: that tool is missing in the
#   session. Everything else still works.
#
# elps is a pure Go project (an embedded Lisp interpreter). There is no cloud infra, no
# Docker, no Playwright. What the Makefile and CI actually need:
#
#   go 1.26.8             .github/workflows/*.yml `go-version`, hook GOTOOLCHAIN pin
#   golangci-lint v2.13.x elps.yml golangci-lint-action `version: v2.13`; hook pins 2.13.2
#   betteralign v0.14.3   Makefile `fieldalign-fix` (via `go run ...@v0.14.3`)
#   govulncheck v1.8.0    govulncheck.yml / govulncheck-scheduled.yml
#   benchstat @latest     benchmark.yml (needs GOTOOLCHAIN=auto; see go_install)
#   (shellcheck)          scripts/ci-gates-test.sh (CI asserts it is present);
#                         parenthesised so this line is not read as a directive
#   gh                    Makefile release / release-notes targets
#   node 20, python3      scripts/*.cjs, scripts/*.py, editors/vscode (both preinstalled)
#
# Preinstalled in the sandbox (do NOT install): Node, Go (may be OLDER than go.mod -- see
# ensure_go), Python.
#
# GOTOOLCHAIN: the VM exports a pin of its own (observed: GOTOOLCHAIN=go1.25.13). Two
# consequences this script has to handle, both of which were live bugs:
#   * `go version` then reports the SELECTED toolchain, not what is on disk, so the
#     "is the right Go installed?" check must ask `GOTOOLCHAIN=local go version`;
#   * `go install` of a module whose go directive is newer than the pin fails outright
#     ("requires go >= 1.26.0 (running go 1.25.13; GOTOOLCHAIN=go1.25.13)"), so EVERY
#     `go install` here runs under GOTOOLCHAIN=auto via the go_install helper.
#
# Optional codex support: the `codex` CLI + the openai/codex-plugin-cc Claude plugin
# (=> /codex:review, and the scripts/codex-delegate.sh wrapper) and sam-at-luther/claude-config
# are baked into the snapshot so codex can be used for selected coding tasks. NO secret is
# handled here: codex auth is done per session with `codex login --device-auth` (prints a URL
# and a one-time code). The environment carries no long-lived credential.
#
# Deliberately NOT `set -e`: see FAILURE POLICY above.
set -Euo pipefail

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

# Bounded network budgets (seconds). Nothing here may block forever: the environment build
# has its own ceiling, and a step that hangs until that ceiling burns the whole snapshot.
CURL_MAX_TIME=300               # curl --max-time for a single download
GO_INSTALL_TIMEOUT=600          # `timeout` around one `go install` (module downloads + build)
NPM_TIMEOUT=600                 # `timeout` around `npm install -g`
APT_TIMEOUT=900                 # `timeout` around an apt-get (includes the 600s dpkg lock wait)
CLAUDE_PLUGIN_TIMEOUT=120       # `timeout` around one `claude plugin ...` call

log() {
  local line="[cloud-web-setup] $*"
  echo "$line"
  # O_APPEND keeps these short lines atomic across the parallel-phase subshells.
  printf '%s %s\n' "$(date -u +%FT%TZ)" "$line" >> "$LOG_FILE" 2>/dev/null || true
}

# dpkg is the normal answer, but this must not be the line that kills the script, so
# fall back to uname and then to amd64 rather than running with ARCH unset under `-u`.
ARCH="$(dpkg --print-architecture 2>/dev/null || true)"
if [ -z "$ARCH" ]; then
  case "$(uname -m 2>/dev/null || echo unknown)" in
    x86_64) ARCH=amd64 ;;
    aarch64 | arm64) ARCH=arm64 ;;
    *) ARCH=amd64 ;;
  esac
  log "WARNING: dpkg --print-architecture unavailable; guessed ARCH=${ARCH} from uname"
fi

# ---------------------------------------------------------------------------
# Step bookkeeping. Every installer runs through here so the log ends with a
# verdict per tool instead of a shrug.
# ---------------------------------------------------------------------------
STEPS_OK=()
STEPS_FAILED=()
STATUS_DIR="$(mktemp -d 2>/dev/null || true)"
if [ -z "$STATUS_DIR" ]; then
  STATUS_DIR="/tmp/cloud-web-setup-status.$$"
  mkdir -p "$STATUS_DIR" 2>/dev/null || true
fi

record_step() {
  local name="$1" rc="$2"
  if [ "$rc" = "0" ]; then
    STEPS_OK+=("$name")
    log "STEP OK: ${name}"
  else
    STEPS_FAILED+=("$name")
    log "STEP FAILED: ${name} (exit ${rc}) -- continuing; that tool will be MISSING in the session"
  fi
}

# Synchronous step: never propagates a failure.
run_step() {
  local name="$1" rc=0
  shift
  "$@" || rc=$?
  record_step "$name" "$rc"
  return 0
}

ASYNC_NAMES=()
ASYNC_PIDS=()

# Parallel step. A background subshell cannot append to the parent's arrays, so the
# verdict travels through a status file and is recorded by collect_async.
run_step_async() {
  local name="$1"
  shift
  (
    rc=0
    "$@" || rc=$?
    printf '%s\n' "$rc" > "${STATUS_DIR}/${name}.rc" 2>/dev/null || true
    exit 0
  ) &
  ASYNC_NAMES+=("$name")
  ASYNC_PIDS+=("$!")
}

collect_async() {
  local i name rc
  for i in "${!ASYNC_PIDS[@]}"; do
    wait "${ASYNC_PIDS[$i]}" 2>/dev/null || true
    name="${ASYNC_NAMES[$i]}"
    rc="$(cat "${STATUS_DIR}/${name}.rc" 2>/dev/null || true)"
    [ -n "$rc" ] || rc="1 (no verdict written -- step died before reporting)"
    record_step "$name" "$rc"
  done
  ASYNC_NAMES=()
  ASYNC_PIDS=()
}

# Breadcrumb for a failure this script did NOT guard -- a bug in the script rather than a
# flaky installer. It logs and returns; it must never exit, because the ERR trap is
# inherited by the parallel phase's subshells (`set -E`) and because nothing in this file
# is allowed to end the run early.
on_err() {
  local rc=$?
  log "ERROR: unguarded failure at line $1 (exit ${rc}) -- continuing (best-effort setup)"
}
trap 'on_err "${LINENO}"' ERR

# Always-exit-0 gate plus the summary. Runs on every exit path, including one taken by a
# bug in this script, so the log always ends with a verdict and the environment build
# always succeeds.
finish() {
  local rc=$?
  local ok_list="(none)" failed_list="(none)"
  [ "${#STEPS_OK[@]}" -eq 0 ] || ok_list="${STEPS_OK[*]}"
  [ "${#STEPS_FAILED[@]}" -eq 0 ] || failed_list="${STEPS_FAILED[*]}"
  log "SUMMARY: ok=${ok_list}"
  log "SUMMARY: failed=${failed_list}"
  if [ "${#STEPS_FAILED[@]}" -eq 0 ]; then
    log "OK: cloud-web-setup finished cleanly (${#STEPS_OK[@]} steps) -- full log at ${LOG_FILE}"
  else
    log "OK: cloud-web-setup finished DEGRADED: ${#STEPS_FAILED[@]} step(s) failed, ${#STEPS_OK[@]} ok -- full log at ${LOG_FILE}"
    log "     the failed tools are simply absent; grep this log for 'STEP FAILED' / 'ERROR' for the reason"
  fi
  [ "$rc" -eq 0 ] || log "note: last command exited ${rc}; exiting 0 anyway so the environment build is not failed"
  rm -rf "$STATUS_DIR" 2>/dev/null || true
  exit 0
}
trap finish EXIT

# `go install` wrapper: GOTOOLCHAIN=auto (the VM's inherited pin can be OLDER than a
# tool's go directive -- that is what silently lost govulncheck), GOBIN on PATH, and a
# hard timeout so a stalled module fetch cannot hang the build.
go_install() {
  local pkg="$1"
  command -v go >/dev/null 2>&1 || { log "go not on PATH; cannot 'go install ${pkg}'"; return 1; }
  GOTOOLCHAIN=auto GOBIN=/usr/local/bin \
    timeout "$GO_INSTALL_TIMEOUT" go install "$pkg" \
    || { log "go install ${pkg} failed (timeout ${GO_INSTALL_TIMEOUT}s or build error)"; return 1; }
}

ensure_go() {
  # The sandbox's PATH `go` has been OLDER than go.mod (go1.24.7 vs go 1.25.0). With
  # GOTOOLCHAIN=auto that skew makes `go test -cover` die with `no such tool "covdata"`
  # (the session hook's header explains why). Installing the exact CI version as the PATH
  # `go` removes the skew at the root: the hook's GOTOOLCHAIN pin then selects the toolchain
  # that is already on disk and nothing is downloaded per session.
  local have stage
  if command -v go >/dev/null 2>&1; then
    # GOTOOLCHAIN=local, NOT a bare `go version`: the VM exports its own GOTOOLCHAIN pin,
    # under which `go version` reports the toolchain it would SELECT (e.g. go1.25.13 out
    # of the module cache) even when /usr/local/go is already exactly GO_VERSION. Asking
    # the bare question made this check permanently false and re-downloaded the tarball on
    # every cache rebuild, then logged the wrong version as "installed".
    have="$(GOTOOLCHAIN=local go version 2>/dev/null | awk '{print $3}' | sed 's/^go//')"
    if [ "$have" = "$GO_VERSION" ]; then
      log "preinstalled go ${have} matches CI pin; skipping"
      return 0
    fi
    log "preinstalled go ${have:-unknown} != CI pin ${GO_VERSION}; installing ${GO_VERSION}"
  else
    log "go not found; installing ${GO_VERSION}"
  fi

  curl -fsSL --retry 3 --max-time "$CURL_MAX_TIME" \
    "https://go.dev/dl/go${GO_VERSION}.linux-${ARCH}.tar.gz" -o /tmp/go.tgz \
    || { log "download of go${GO_VERSION}.linux-${ARCH}.tar.gz failed; keeping the preinstalled toolchain"; rm -f /tmp/go.tgz; return 1; }

  # Unpack into a staging dir and SWAP. The old `rm -rf /usr/local/go && tar ...` deleted
  # the working toolchain first, so a truncated tarball left the VM with no Go at all --
  # the one failure here that really would break every session.
  stage="$(mktemp -d)" || { log "mktemp failed; skipping go install"; rm -f /tmp/go.tgz; return 1; }
  if ! tar -C "$stage" -xzf /tmp/go.tgz || [ ! -x "${stage}/go/bin/go" ]; then
    log "go tarball did not unpack to a usable toolchain; keeping the preinstalled one"
    rm -rf "$stage"; rm -f /tmp/go.tgz
    return 1
  fi
  rm -f /tmp/go.tgz
  rm -rf /usr/local/go.old
  if [ -d /usr/local/go ] && ! mv /usr/local/go /usr/local/go.old; then
    log "could not move the existing /usr/local/go aside; keeping it"
    rm -rf "$stage"
    return 1
  fi
  if ! mv "${stage}/go" /usr/local/go; then
    log "could not install the new toolchain at /usr/local/go; restoring the previous one"
    [ -d /usr/local/go.old ] && mv /usr/local/go.old /usr/local/go
    rm -rf "$stage"
    return 1
  fi
  rm -rf "$stage" /usr/local/go.old
  ln -sf /usr/local/go/bin/go /usr/local/bin/go || log "WARNING: could not link /usr/local/bin/go"
  ln -sf /usr/local/go/bin/gofmt /usr/local/bin/gofmt || log "WARNING: could not link /usr/local/bin/gofmt"
  GOTOOLCHAIN=local go version || { log "installed go does not run"; return 1; }
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
  local ver="${GOLANGCI_VERSION#v}" tmp rc=0
  tmp="$(mktemp -d)" || { log "mktemp failed; skipping golangci-lint"; return 1; }
  if curl -fsSL --retry 3 --max-time "$CURL_MAX_TIME" \
    "https://github.com/golangci/golangci-lint/releases/download/v${ver}/golangci-lint-${ver}-linux-${ARCH}.tar.gz" \
    | tar -xz -C "$tmp" --strip-components=1; then
    install -m 0755 "${tmp}/golangci-lint" /usr/local/bin/golangci-lint || rc=1
  else
    log "golangci-lint ${GOLANGCI_VERSION} download/unpack failed"
    rc=1
  fi
  rm -rf "$tmp"
  [ "$rc" -eq 0 ] || return 1
  /usr/local/bin/golangci-lint --version || return 1
}

install_gopls() {
  # Go language server for Claude Code's LSP (diagnostics, hover, go-to-def). Not part of
  # the Go toolchain, so it must be installed separately. GOBIN puts it on /usr/local/bin
  # (on PATH). gopls@latest has required a Go newer than the CI pin before (v0.23 needed
  # >= 1.26 while the pin was 1.25), which is why go_install uses GOTOOLCHAIN=auto.
  log "installing gopls (Go LSP)"
  go_install golang.org/x/tools/gopls@latest || return 1
  gopls version >/dev/null 2>&1 || { log "gopls installed but does not run"; return 1; }
}

install_govulncheck() {
  # Same pin as govulncheck.yml so a local `govulncheck ./...` agrees with CI.
  # This step was FAILING every run: it was the only `go install` without
  # GOTOOLCHAIN=auto, and x/vuln@v1.8.0's go directive (>= 1.26.0) is newer than the
  # GOTOOLCHAIN pin the VM exports, so the module fetch refused before building.
  log "installing govulncheck ${GOVULNCHECK_VERSION}"
  go_install "golang.org/x/vuln/cmd/govulncheck@${GOVULNCHECK_VERSION}" || return 1
  govulncheck -version >/dev/null 2>&1 || { log "govulncheck installed but does not run"; return 1; }
}

install_benchstat() {
  # scripts/bench-*.sh and the /benchmark skill call `benchstat`. benchmark.yml installs it
  # with GOTOOLCHAIN=auto because x/perf's go.mod can require a Go newer than the CI pin;
  # go_install mirrors that.
  log "installing benchstat"
  go_install golang.org/x/perf/cmd/benchstat@latest || return 1
  command -v benchstat >/dev/null 2>&1 || { log "benchstat not on PATH after install"; return 1; }
}

warm_betteralign() {
  # `make fieldalign-fix` runs `go run github.com/dkorunic/betteralign/cmd/betteralign@v0.14.3`.
  # It does NOT read PATH, so this only warms the module cache so the first `go run` in a
  # session is not also a download. betteralign itself needs Go >= 1.26 (Makefile comment),
  # hence GOTOOLCHAIN=auto in go_install. The Makefile target works without this, just
  # slower the first time.
  log "warming module cache for betteralign ${BETTERALIGN_VERSION}"
  go_install "github.com/dkorunic/betteralign/cmd/betteralign@${BETTERALIGN_VERSION}" || return 1
}

install_codex_cli() {
  # codex backs the `codex` plugin (/codex:review) and is usable directly from a session for
  # selected coding tasks. node/npm are preinstalled. Bounded: npm behind the egress proxy
  # can sit on a stalled socket much longer than the environment build can afford.
  log "installing global npm CLI: @openai/codex"
  command -v npm >/dev/null 2>&1 || { log "npm not on PATH; skipping codex CLI"; return 1; }
  timeout "$NPM_TIMEOUT" npm install -g @openai/codex \
    || { log "npm install -g @openai/codex failed (timeout ${NPM_TIMEOUT}s or registry/proxy error)"; return 1; }
  codex --version || { log "codex installed but does not run"; return 1; }
}

install_claude_plugins() {
  # Claude Code plugins baked into the cached snapshot: codex (=> /codex:review; its
  # codex-companion.mjs runtime is what scripts/codex-delegate.sh drives) and claude-config
  # (=> qa-professor / pr). Doing it here rather than in the SessionStart hook means the
  # plugin install runs once, not on every session start.
  #
  # `claude` is a harness binary that may not be on PATH during the cached setup phase.
  # When it is, this bakes installed_plugins.json into the snapshot so the plugins load at
  # session start; when it isn't, this is a no-op and `codex` from the CLI still works once
  # logged in. A missing plugin is reported as a failed step, never as a failed setup.
  #
  # Codex AUTH is deliberately NOT done here. Run once per session:
  #     codex login --device-auth
  # It prints a URL + code; approve it in a browser. No token is stored in the snapshot.
  local p rc=0
  if ! command -v claude >/dev/null 2>&1; then
    log "claude CLI not on PATH at setup -- skipping plugin install (install from a session: claude plugin install codex@openai-codex)"
    return 0
  fi
  timeout "$CLAUDE_PLUGIN_TIMEOUT" claude plugin marketplace add openai/codex-plugin-cc      >/dev/null 2>&1 || true
  timeout "$CLAUDE_PLUGIN_TIMEOUT" claude plugin marketplace add sam-at-luther/claude-config >/dev/null 2>&1 || true
  for p in codex@openai-codex claude-config@claude-config; do
    if timeout "$CLAUDE_PLUGIN_TIMEOUT" claude plugin install "$p" >/dev/null 2>&1; then
      log "plugin installed: $p"
    else
      log "WARNING: plugin install failed: $p (non-fatal; install it from a session)"
      rc=1
    fi
  done
  [ "$rc" -eq 0 ] || return 1
}

install_base_packages() {
  # Base packages the parallel installers depend on.
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
  timeout "$APT_TIMEOUT" apt-get update -y --allow-releaseinfo-change \
    || log "WARNING: apt-get update failed; trying the install against whatever lists exist"
  timeout "$APT_TIMEOUT" apt-get install -y --no-install-recommends \
    ca-certificates curl gnupg shellcheck bubblewrap python3 util-linux \
    || { log "apt-get install of the base packages failed"; return 1; }
}

# gh via apt. Kept as its own function so it can run in the parallel phase as the ONLY apt
# user there (phase 1's apt is synchronous and already finished).
install_apt_extras() {
  export DEBIAN_FRONTEND=noninteractive
  log "configuring gh apt repo"
  install -m 0755 -d /etc/apt/keyrings || { log "could not create /etc/apt/keyrings"; return 1; }
  curl -fsSL --retry 3 --max-time "$CURL_MAX_TIME" https://cli.github.com/packages/githubcli-archive-keyring.gpg \
    -o /etc/apt/keyrings/githubcli-archive-keyring.gpg \
    || { log "gh keyring download failed; skipping gh"; rm -f /etc/apt/keyrings/githubcli-archive-keyring.gpg; return 1; }
  chmod go+r /etc/apt/keyrings/githubcli-archive-keyring.gpg || true
  echo "deb [arch=${ARCH} signed-by=/etc/apt/keyrings/githubcli-archive-keyring.gpg] https://cli.github.com/packages stable main" \
    > /etc/apt/sources.list.d/github-cli.list \
    || { log "could not write the gh apt source"; return 1; }

  log "installing gh"
  timeout "$APT_TIMEOUT" apt-get update -y --allow-releaseinfo-change \
    || log "WARNING: apt-get update (gh repo) failed; trying the install anyway"
  timeout "$APT_TIMEOUT" apt-get install -y gh || { log "apt-get install gh failed"; return 1; }
  gh --version || { log "gh installed but does not run"; return 1; }
}

harden_apt() {
  # On a fresh Ubuntu 24.04 VM the boot-time apt-daily / unattended-upgrades timers grab
  # /var/lib/dpkg/lock-frontend a minute or two after boot -- i.e. right in the gap between
  # the base apt transaction and install_apt_extras' apt. An `apt-get install` with no lock
  # timeout then dies instantly with "Could not get lock ... (exit 100)". Two defenses, both
  # before any apt-get runs:
  #   1. A global apt.conf.d drop-in so EVERY apt-get in this script WAITS up to 10 min for
  #      the lock instead of failing immediately.
  #   2. Best-effort stop of the background timers so they don't contend at all. No-op
  #      where systemd isn't the init (e.g. a container sandbox), where the conf drop-in is
  #      the real safety net.
  log "hardening apt against dpkg-lock contention (DPkg::Lock::Timeout + stop apt-daily timers)"
  mkdir -p /etc/apt/apt.conf.d || { log "could not create /etc/apt/apt.conf.d"; return 1; }
  printf 'DPkg::Lock::Timeout "600";\n' > /etc/apt/apt.conf.d/99-cloud-web-setup \
    || { log "could not write the dpkg lock-timeout drop-in"; return 1; }
  systemctl stop apt-daily.timer apt-daily-upgrade.timer       >/dev/null 2>&1 || true
  systemctl stop apt-daily.service apt-daily-upgrade.service   >/dev/null 2>&1 || true
}

# Phase 0: apt hardening.
run_step apt-hardening harden_apt

# Phase 1 (sync): base packages the parallel installers depend on.
run_step apt-base install_base_packages

# Phase 2 (parallel): non-apt installers PLUS the single apt user (install_apt_extras).
# Nothing here is "required" any more: a failure is a missing tool, recorded in the
# summary, not a failed environment build.
run_step_async go             ensure_go
run_step_async golangci-lint  install_golangci
run_step_async codex-cli      install_codex_cli
run_step_async gh             install_apt_extras
collect_async

# Phase 3 (sequential): runs after phase 2 so the Go toolchain is guaranteed present.
run_step gopls            install_gopls
run_step govulncheck      install_govulncheck
run_step benchstat        install_benchstat
run_step betteralign-warm warm_betteralign
run_step claude-plugins   install_claude_plugins

log "setup complete: pins go ${GO_VERSION}, golangci-lint ${GOLANGCI_VERSION}, govulncheck ${GOVULNCHECK_VERSION}, betteralign ${BETTERALIGN_VERSION} (codex auth: run 'codex login --device-auth' per session)"
# The `finish` EXIT trap writes the two SUMMARY lines and the completion marker, then
# exits 0. If the marker is ABSENT from the log the script was killed from outside (the
# platform's build timeout, OOM); if it says DEGRADED, read the 'STEP FAILED' lines.
