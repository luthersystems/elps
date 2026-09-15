#!/usr/bin/env bash
# Delegate a task to OpenAI Codex through the installed Claude Code codex plugin.
#
# This script calls the plugin's own runtime (codex-companion.mjs). It does not
# copy or replace the plugin logic.
#
# Why this script exists: elps sends many Codex tasks at the same time, each
# from a different subagent. Three things must be correct every time, and this
# script makes them correct:
#   1. Disk. Each worktree costs disk. This container has a fixed allowance.
#   2. Broker isolation. The plugin keys its Codex broker on the workspace root.
#      Two tasks in the SAME directory share one broker and can collide. Two
#      tasks in DIFFERENT worktrees each get their own broker.
#   3. Retry. A busy broker is a start-up race, not an error. Retry it.
#
# WARNING: Codex CANNOT COMMIT here (.git is read-only). Until a snapshot is
# published, the worktree is the ONLY copy. git checkout/restore/reset/clean and
# worktree remove --force can destroy it; Git cannot recover unstaged edits.
# Never tidy an active unit. Use sweep for retired worktrees: it locks, captures,
# and archives outside the removed tree before deletion. Copy bundles off-host
# for host/disk loss; ignored files are never captured.
#
# Usage:
#   codex-delegate.sh preflight
#   codex-delegate.sh run [options] "<prompt>"
#   codex-delegate.sh worktree <name>
#   codex-delegate.sh sweep [name]
#
# Options for "run":
#   --worktree <name>   Run in a dedicated git worktree. Made if it is absent.
#                       Use one name per subagent to keep brokers separate.
#   --dir <path>        Run in this directory. Default: the current directory.
#   --read-only         Codex cannot change files. Default: Codex CAN change files.
#   --background        Return a job id immediately. Collect it later.
#   --model <name>      Codex model. "spark" maps to gpt-5.3-codex-spark.
#   --effort <level>    none | minimal | low | medium | high | xhigh
#   --resume            Continue the last Codex thread for this workspace.
#   --allow-busy        Explicitly allow an occupied workspace (unsafe).
#
# Runs: <workspace>/.codex-delegate/runs/<unique-id>/transcript.log
# Results: tracked.patch, untracked.list, untracked.paths0, untracked/,
#          final-message.txt, job.json (when available), exit-status.
# current points atomically to a complete immutable snapshots/snapshot-* bundle.
# Capture runs before launch, every second in the monitor, and around teardown.
# Earlier snapshots survive later deletion/reset; base.commit identifies the base.
# latest is a symlink to the newest run; ignored files are not captured.
#
# Exit codes:
#   0 success   1 usage error   2 preflight failed   3 Codex run failed

set -euo pipefail

PLUGIN_ROOT="${CLAUDE_PLUGIN_ROOT:-/root/.claude/plugins/cache/openai-codex/codex/1.0.6}"
COMPANION="${PLUGIN_ROOT}/scripts/codex-companion.mjs"
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
RUNTIME="${REPO_ROOT}/scripts/codex-delegate-runtime.mjs"
WORKTREE_BASE="${REPO_ROOT}/.claude/worktrees"

# Disk floor. Below this, a worktree or an npm install can die with no error.
MIN_FREE_GB="${CODEX_DELEGATE_MIN_FREE_GB:-8}"
# A busy broker is a race. Retry it this many times.
BROKER_RETRIES="${CODEX_DELEGATE_BROKER_RETRIES:-3}"

log()  { printf '[codex-delegate] %s\n' "$*" >&2; }
die()  { printf '[codex-delegate] ERROR: %s\n' "$*" >&2; exit "${2:-1}"; }

free_gb() {
  df -BG --output=avail / 2>/dev/null | tail -1 | tr -dc '0-9'
}

check_disk() {
  local avail
  avail="$(free_gb)"
  if [ -z "$avail" ]; then
    die "Cannot read free disk space on /. Check the container." 2
  fi
  if [ "$avail" -lt "$MIN_FREE_GB" ]; then
    die "Only ${avail}GB free on /, need ${MIN_FREE_GB}GB. Sweep worktrees before you start more work: $0 sweep" 2
  fi
  printf '%s' "$avail"
}

cmd_preflight() {
  local avail ok=0
  avail="$(check_disk)"
  log "disk: ${avail}GB free (floor ${MIN_FREE_GB}GB) OK"

  [ -f "$COMPANION" ] || die "Plugin runtime not found at ${COMPANION}. The codex plugin is not installed in this container." 2
  log "plugin: ${PLUGIN_ROOT} OK"

  command -v codex >/dev/null 2>&1 || die "codex CLI not on PATH. Run: npm install -g @openai/codex" 2
  log "codex: $(codex --version 2>&1 | head -1) OK"

  # Ask the plugin itself. It is the authority on whether Codex can run.
  local out
  if ! out="$(cd "$REPO_ROOT" && node "$COMPANION" setup --json 2>&1)"; then
    printf '%s\n' "$out" >&2
    die "Plugin health check failed. See the output above." 2
  fi
  printf '%s\n' "$out"

  if printf '%s' "$out" | grep -q '"loggedIn": *true'; then
    log "auth: OK"
  elif printf '%s' "$out" | grep -qi 'broker is busy'; then
    log "auth: broker was busy, this is a start-up race, not a failure. Run preflight again."
    ok=1
  else
    die "Codex is not logged in. Run: codex login --device-auth" 2
  fi
  return $ok
}

validate_name() {
  [[ "$1" =~ ^[a-zA-Z0-9][a-zA-Z0-9._-]*$ ]] || die "Invalid worktree name: $1"
}

shipped_base() {
  local base
  base="$(git -C "$REPO_ROOT" symbolic-ref --quiet refs/remotes/origin/HEAD)" || base="refs/remotes/origin/main"
  git -C "$REPO_ROOT" rev-parse --verify "${base}^{commit}" >/dev/null 2>&1 || die "No local origin default branch or origin/main ref. Fetch the remote outside this offline wrapper before creating a worktree." 2
  printf '%s\n' "$base"
}

check_base() {
  local candidate="$1" shipped="$2" ref
  for ref in "$shipped" refs/remotes/origin/main; do
    git -C "$REPO_ROOT" show-ref --verify --quiet "$ref" || continue
    git -C "$REPO_ROOT" merge-base --is-ancestor "$ref" "$candidate" || die "Refusing stale or diverged base $candidate: it does not contain $ref. Update/rebase it before delegation; no files were reset." 2
  done
}

cmd_worktree() {
  local name="${1:-}"
  [ -n "$name" ] || die "worktree needs a name. Example: $0 worktree review-a"
  validate_name "$name"
  check_disk >/dev/null
  local base
  base="$(shipped_base)" || return $?

  local path="${WORKTREE_BASE}/${name}"
  if [ -d "$path" ]; then
    # Do not trust a directory by its name. Confirm it is a live worktree.
    if git -C "$path" rev-parse --show-toplevel >/dev/null 2>&1; then
      [ "$(git -C "$path" rev-parse --show-toplevel)" = "$path" ] || die "$path is not a worktree root" 2
      check_base "$(git -C "$path" rev-parse HEAD)" "$base"
      printf '%s\n' "$path"
      return 0
    fi
    die "${path} exists but is not a git worktree. Look at it before you remove it." 1
  fi

  mkdir -p "$WORKTREE_BASE"
  local branch="codex/${name}"
  log "making worktree ${name} from local ${base} (offline; remote freshness is not verified)"
  if git -C "$REPO_ROOT" show-ref --verify --quiet "refs/heads/${branch}"; then
    check_base "$branch" "$base"
    git -C "$REPO_ROOT" worktree add --quiet "$path" "$branch" >&2
  else
    check_base "$base" "$base"
    git -C "$REPO_ROOT" worktree add --quiet -b "$branch" "$path" "$base" >&2
  fi
  printf '%s\n' "$path"
}

sweep_one() (
  # Subshell releases this workspace's lock only after archive and removal.
  local name="$1" path="$WORKTREE_BASE/$1" archive
  validate_name "$name"
  [ -d "$path" ] || die "No worktree named ${name}" 1
  path="$(cd "$path" && pwd -P)"
  [ "$(git -C "$path" rev-parse --show-toplevel)" = "$path" ] || die "$path is not a worktree root" 2
  mkdir -p "$path/.codex-delegate" "$REPO_ROOT/.codex-delegate/swept"
  exec 9>"$path/.codex-delegate/run.lock"
  flock -n 9 || die "Workspace run.lock is owned; refusing to sweep $name" 2
  archive="$(mktemp -d "$REPO_ROOT/.codex-delegate/swept/${name}.XXXXXXXX")"
  node "$RUNTIME" snapshot "$path" "$archive" || die "Capture failed; refusing to remove $path. Inspect $archive." 2
  node "$RUNTIME" reap "$path" || die "Teardown failed; refusing to remove $path" 2
  node "$RUNTIME" guard "$path" || die "Workspace occupied; refusing to remove $path" 2
  node "$RUNTIME" snapshot "$path" "$archive" || die "Final capture failed; refusing to remove $path" 2
  cp -a "$path/.codex-delegate" "$archive/workspace-bundles" || die "Bundle archive failed; refusing to remove $path" 2
  log "Recovery archive: $archive (includes previous runs and final snapshot)"
  git -C "$REPO_ROOT" worktree remove --force "$path" >&2
  log "removed $name. $(free_gb)GB free"
)

cmd_sweep() {
  local name="${1:-}" path failed=0
  if [ -n "$name" ]; then
    sweep_one "$name"
    return
  fi
  [ -d "$WORKTREE_BASE" ] || { log "no worktrees to sweep"; return 0; }
  for path in "$WORKTREE_BASE"/*; do
    [ -d "$path" ] || continue
    # Invoke separately: do not suppress errexit inside the destructive operation.
    "$0" sweep "$(basename "$path")" || failed=1
  done
  return "$failed"
}

cmd_run() {
  local workdir="" worktree="" write="--write" background="" model="" effort="" resume="" allow_busy=0
  local -a prompt=()

  while [ $# -gt 0 ]; do
    case "$1" in
      --worktree)   worktree="${2:-}"; [ -n "$worktree" ] || die "--worktree needs a name"; shift 2 ;;
      --dir)        workdir="${2:-}";  [ -n "$workdir" ]  || die "--dir needs a path";  shift 2 ;;
      --read-only)  write="";          shift ;;
      --background) background="--background"; shift ;;
      --model)      model="${2:-}";    [ -n "$model" ]    || die "--model needs a value"; shift 2 ;;
      --effort)     effort="${2:-}";   [ -n "$effort" ]   || die "--effort needs a value"; shift 2 ;;
      --allow-busy) allow_busy=1; shift ;;
      --resume)     resume="--resume-last"; shift ;;
      --)           shift; prompt+=("$@"); break ;;
      -*)           die "Unknown option: $1" ;;
      *)            prompt+=("$1"); shift ;;
    esac
  done

  [ "${#prompt[@]}" -gt 0 ] || die "run needs a prompt"
  [ -f "$COMPANION" ] || die "Plugin runtime not found at ${COMPANION}" 2
  check_disk >/dev/null

  if [ -n "$worktree" ]; then
    [ -z "$workdir" ] || die "Use --worktree or --dir, not both"
    workdir="$(cmd_worktree "$worktree")"
  fi
  workdir="${workdir:-$PWD}"
  [ -d "$workdir" ] || die "Directory does not exist: ${workdir}"

  workdir="$(cd "$workdir" && pwd -P)"
  workdir="$(git -C "$workdir" rev-parse --show-toplevel)" || die "Cannot resolve git workspace. Run in a git repository." 2
  workdir="$(cd "$workdir" && pwd -P)"
  command -v flock >/dev/null || die "flock is required; install util-linux." 2
  [ -f "$RUNTIME" ] || die "Missing $RUNTIME; restore the delegate runtime adapter." 2
  local root="$workdir/.codex-delegate" output
  mkdir -p "$root/runs" || die "Cannot create $root/runs; check workspace permissions and disk." 2
  exec 9>"$root/run.lock" || die "Cannot open workspace lock; check $root permissions." 2
  if [ "$allow_busy" -eq 0 ]; then
    flock -n 9 || die "Another codex-delegate owns $root/run.lock. Inspect $root/latest/transcript.log and stop its owner (fuser $root/run.lock), or use --allow-busy." 2
    # The guard only ever runs AFTER the flock was won: no other delegate owns
    # this worktree, so remaining Codex processes are leftovers, not live peers.
    local reap_status=0 rescue
    rescue="$(mktemp -d "$root/runs/recovery.XXXXXXXX")"
    node "$RUNTIME" snapshot "$workdir" "$rescue" || die "Cannot preserve workspace before reap; inspect $rescue" 2
    log "Before-reap recovery: $rescue"
    node "$RUNTIME" reap "$workdir" || reap_status=$?
    node "$RUNTIME" guard "$workdir" || die "Workspace process check failed. Follow the PID-specific instructions above." 2
    [ "$reap_status" -eq 0 ] || die "Workspace teardown failed. Follow the PID-specific instructions above." 2
  else
    log "WARNING: --allow-busy permits concurrent writers in $workdir"
  fi
  output="$(mktemp -d "$root/runs/run.XXXXXXXX")" || die "Cannot create result directory; check disk and permissions." 2
  ln -sfn "runs/$(basename "$output")" "$root/latest" || die "Cannot update $root/latest; inspect permissions." 2
  printf 'Transcript: %s/transcript.log\n' "$output" | tee "$output/transcript.log"
  log "Transcript: $output/transcript.log"

  # "spark" is the plugin's own shorthand. Keep it working here too.
  [ "$model" = "spark" ] && model="gpt-5.3-codex-spark"

  local -a args=(task)
  args+=(--background --json)
  [ -n "$write" ]      && args+=("$write")
  [ -n "$resume" ]     && args+=("$resume")
  [ -n "$model" ]      && args+=(--model "$model")
  [ -n "$effort" ]     && args+=(--effort "$effort")
  args+=("${prompt[*]}")

  log "workspace: ${workdir}"
  log "mode: $([ -n "$write" ] && echo 'write-capable' || echo 'read-only')$([ -n "$background" ] && echo ', background')"

  local attempt=1 status=0

  launch() {
    : >"$output/monitor-error.txt"
    rm -f "$output/job.json" "$output/final-message.txt" || { log "Cannot clear prior attempt results; inspect $output permissions."; return 3; }
    (cd "$workdir" && node "$COMPANION" "${args[@]}" </dev/null) >"$output/launch.json" 2>"$output/launch-error.txt"
  }

  capture() {
    node "$RUNTIME" snapshot "$workdir" "$output" || { log "Capture failed; previous snapshots retained in $output/snapshots"; status=3; }
    if [ ! -f "$output/final-message.txt" ]; then
      : >"$output/final-message.txt"
      log "No final message available; inspect launch-error.txt and transcript.log."
    fi
    printf '%s\n' "$status" >"$output/exit-status"
    printf 'Results: %s\n' "$output" | tee -a "$output/transcript.log"
  }

  teardown() {
    node "$RUNTIME" snapshot "$workdir" "$output" || { log "Before-teardown capture failed; previous snapshots retained."; status=3; }
    # --allow-busy owns no lock and may share these processes with a live peer.
    [ "$allow_busy" -eq 0 ] || return 0
    if ! node "$RUNTIME" reap "$workdir" 2>&1 | tee -a "$output/transcript.log" >&2; then
      log "Delegate teardown failed; inspect $output/transcript.log before retrying."
      status=3
    fi
  }

  finish_exit() {
    # $? on entry is the status that triggered the EXIT trap.
    local code=$?
    if [ "$code" -ne 0 ]; then
      status=3
    fi
    teardown
    capture
    exit "$status"
  }

  finish() {
    # The detached background finisher inherits fd 9 and this EXIT trap, keeping
    # the lock through teardown and capture on success, failure or interruption.
    # SIGKILL cannot be finalized; already published monitor snapshots survive.
    trap 'status=3; log "Delegate interrupted; inspect remaining workers before retrying."; exit 3' INT TERM
    trap finish_exit EXIT
    while :; do
      cat "$output/launch-error.txt" | tee -a "$output/transcript.log" >&2
      if [ "$status" -eq 0 ]; then
        node "$RUNTIME" monitor "$workdir" "$output" "$COMPANION" 2>"$output/monitor-error.txt" || status=$?
        cat "$output/monitor-error.txt" | tee -a "$output/transcript.log" >&2
      fi
      [ "$status" -ne 0 ] || break
      if grep -qi 'broker is busy' "$output/launch-error.txt" "$output/monitor-error.txt" 2>"$output/retry-check-error.txt"; then
        if [ "$attempt" -lt "$BROKER_RETRIES" ]; then
          log "broker busy, retry ${attempt}/${BROKER_RETRIES}"
          attempt=$((attempt + 1))
          sleep $((attempt * 2))
          status=0
          launch || status=$?
          continue
        fi
      elif [ -s "$output/retry-check-error.txt" ]; then
        log "Retry check could not read job diagnostics; see $output/retry-check-error.txt. Inspect the launch failure before retrying."
      fi
      log "Codex run failed after ${attempt} attempt(s). Inspect $output/transcript.log before retrying."
      status=3
      break
    done
    exit "$status"
  }

  node "$RUNTIME" snapshot "$workdir" "$output" || die "Initial capture failed; refusing to launch." 3
  launch || status=$?
  if [ -n "$background" ] && [ "$status" -eq 0 ]; then
    if ! node -e 'const j=require(process.argv[1]); if (!j.jobId || !j.logFile) throw Error("Plugin returned no jobId/logFile; inspect launch.json and plugin wiring"); console.log("Background job: " + j.jobId)' "$output/launch.json"; then
      status=3
      (finish)
      return 3
    fi
    (trap '' HUP; finish) </dev/null >/dev/null 2>>"$output/transcript.log" &
    disown "$!"
    printf 'Results (pending): %s\n' "$output"
  else
    (finish)
  fi
}

main() {
  local sub="${1:-}"
  [ $# -gt 0 ] && shift || true
  case "$sub" in
    preflight) cmd_preflight "$@" ;;
    run)       cmd_run "$@" ;;
    worktree)  cmd_worktree "$@" ;;
    sweep)     cmd_sweep "$@" ;;
    ""|help|-h|--help)
      sed -n '2,/^set -euo pipefail/p' "${BASH_SOURCE[0]}" | sed '$d; s/^# \{0,1\}//'
      ;;
    *) die "Unknown subcommand: ${sub}. Try: preflight | run | worktree | sweep" ;;
  esac
}

main "$@"
