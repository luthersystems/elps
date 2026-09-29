# /codex-delegate — Codex Delegation Skill

Hands a bounded coding unit to OpenAI Codex in its own git worktree, with write
access to the files, and takes the work back verified on the host.

## Trigger

Use when asked to delegate work to Codex, to get a second opinion or an
independent diagnosis from Codex, to fan out several Codex tasks at the same
time, or when a Codex run says the broker is busy. Also covers the set-up this
container needs, because the Codex login and the plugin do not survive a new
session.

Use `scripts/codex-delegate.sh`. It calls the Codex plugin's own runtime
(`codex-companion.mjs`). It does not copy the plugin logic.

## WARNING: the working tree may be the only copy

**Codex CANNOT COMMIT in this sandbox: `.git` is read-only. Git cannot recover
unstaged edits destroyed by `git checkout HEAD -- .`, `git restore`,
`git reset --hard`, `git clean`, or `git worktree remove --force`.** Never tidy
an active or uncollected unit's worktree. A transcript is not a backup.

The wrapper captures before launch, every second while monitoring, and before
and after teardown. Each run's `current` symlink points to a complete immutable
`snapshots/snapshot-*` directory. `tracked.patch` includes staged and unstaged
changes and binary payloads relative to `base.commit`; `untracked/` preserves
nonignored new files and symlinks. `untracked.paths0` is a NUL-delimited manifest.
Earlier snapshots are retained even if a later command removes the changes.
A failed capture leaves the previous snapshot available and prints an error.

To recover, select a snapshot containing the work, create a separate clean
worktree at the commit in its `base.commit`, apply its `tracked.patch` with
`git apply`, then copy `untracked/.` into that worktree with `cp -a`. Inspect
and commit the recovered work from an environment that can write `.git`.
After SIGKILL, `exit-status` and the final message may be absent: look in
`snapshots/` and in `runs/recovery.*` before touching the source worktree.

Snapshots are best-effort reads during editing, not a transaction across all
files. Edits since the last successful capture, ignored files, and files in
submodule worktrees are not protected. Copy bundles off-host for host/disk loss.
Retained snapshots cost disk; remove them only after accepting and securing the
unit's work. Use `sweep` for retired worktrees: it takes the same workspace lock,
captures before cleanup, and archives both work and previous run bundles under
the wrapper repository's `.codex-delegate/swept/` before removing the worktree.
Capture, archive, or teardown failure refuses removal.

All cleanup must use the wrapper or its runtime `reap` command, which takes the
same `run.lock`. Never kill brokers by hand while launching work into their
workspace. The lock cannot coordinate arbitrary external `kill`/Git commands
or plugin launches that bypass the wrapper. `--allow-busy` explicitly bypasses
this protection and must not be used to work around cleanup in progress.

Worktrees start from the locally recorded remote default (`origin/HEAD`, or
`origin/main` if the symbolic ref is absent). Existing branches/worktrees must
contain that tip and `origin/main` when present; stale/diverged bases are refused.
The wrapper does not fetch: update remote-tracking refs separately before
launching if their freshness is unknown. Missing remote refs cause refusal.

## First, prove the container is ready

The Codex login and the plugin live in the container. **A new session loses
both.** Always start here:

```bash
./scripts/codex-delegate.sh preflight
```

If it says the plugin is missing, or Codex is not logged in, do the set-up in
"Set up a fresh container" below before anything else.

## Send one task

```bash
./scripts/codex-delegate.sh run --worktree <name> "<the task>"
```

- `--worktree <name>` makes (or re-uses) a git worktree in
  `.claude/worktrees/<name>` on a branch `codex/<name>`.
- Codex **can change files** by default. For reviews, see "Reviews" below.
- Add `--background` for long work. The run directory it prints holds
  `exit-status` and `final-message.txt`; collect them as in "The loop, as run".

Other options: `--dir <path>`, `--model <name|spark>` (see "Models"),
`--effort none|minimal|low|medium|high|xhigh`, `--resume`.

## The loop, as run

1. **One worktree per unit.** `scripts/codex-delegate.sh worktree <name>` cuts
   `codex/<name>` from `origin/main` into `.claude/worktrees/<name>`. To build on
   a feature branch, cut it by hand
   (`git worktree add .claude/worktrees/<name> -b codex/<name> <branch>`) and
   pass `--dir`.
2. **Launch in the background.** `run --dir <worktree> --background
   <model flags from "Models"> "$(cat brief.md)"` returns a job id and the run
   directory at once.
3. **Watch, don't poll by hand.** A `Monitor` loop reads the newest
   `.codex-delegate/runs/run.*/exit-status` per worktree and emits a line only on
   change.
4. **Read `final-message.txt`, then verify on the host — always.** The sandbox
   cannot open sockets, write the shared git index (units never commit), or write
   the default Go cache (units export `GOCACHE=/tmp/elps-<name>-go-cache`). A
   unit's "green" is a claim until the host has run the same command.
5. **Commit on the unit's branch yourself**, message in the repo's style — a
   plain sentence saying what changes and why, never a model identifier.
6. **Sweep** the moment the round is accepted: `sweep <name>`, then delete
   `codex/<name>` and `/tmp/elps-<name>-go-cache`.

## Models

- Default: `--model gpt-6.1-sol --effort xhigh`.
- Hard work (deep diagnosis, a subtle interpreter change): `--model gpt-6-astra
  --effort high`.
- Never a `max` or `ultra` tier.

Other sections refer here rather than repeating model names.

## Reviews

A review runs in **normal write mode**, in its own worktree, with its own Go
cache. For a review of `main`, let the wrapper make the worktree; it refuses a
stale or diverged base:

```bash
./scripts/codex-delegate.sh run --worktree <name> --background \
  <model flags from "Models"> "$(cat brief.md)"
# brief says: export GOCACHE=/tmp/elps-<name>-go-cache before any go command
```

To review a PR branch, make the worktree by hand: `git fetch origin` first,
pick a unique `<name>` (`-b` fails if `codex/<name>` already exists), then
`git worktree add .claude/worktrees/<name> -b codex/<name> origin/<pr-branch>`
and pass `--dir .claude/worktrees/<name>`. It lives under `.claude/worktrees`,
so `sweep <name>` retires it.

Observed behaviour: under `--read-only` the sandbox could not run tests or
write `/tmp`, so a review there can only read code. Put "do not edit files" in
the brief if edits are unwanted, and verify its findings on the host. Use only
the wrapper; do not build a custom sandbox.

## The brief

Keep it self-contained. A short template:

```text
Read AGENTS.md first, then .claude/skills/<skill>/SKILL.md, and follow them.
Before any go command: export GOCACHE=/tmp/elps-<name>-go-cache

FAILURE (verbatim): <test name / CI lines / gate output>
Start reading at: <file>:<line>

The fix must: <rule it satisfies>
The fix must NOT: <what is off limits, e.g. touch other packages, drop a nolint>

Verify with exactly: go test ./<pkg>/... ; make static-checks ; make elpsvet
If the sandbox cannot run a command, say which and why. Never claim green.
Do not commit; leave the changes in the worktree.

Report: root cause; files changed; each command with its verbatim output;
anything you are unsure of.
```

## Continuing a thread

`run --dir <worktree> --resume "<follow-up>"` continues the last Codex thread
for that workspace: answer a question a unit stopped on, hand back a review
finding, or hand back host results the sandbox could not produce. If it fails
with "Task ... is still running" (the plugin's registry kept a unit killed
earlier), launch a fresh run with the same brief plus the follow-up; the
worktree already holds the code.

## Send several at the same time

Give **every subagent its own `--worktree` name**. This is not only tidiness:

> The plugin keys its Codex broker on the workspace root. Two tasks in the SAME
> directory share one broker and can collide. Two tasks in DIFFERENT worktrees
> each get their own broker.

Keep each subagent a **thin forwarder**, which is how the plugin's own
`codex-rescue` agent works: make one `run` call, give back what Codex said, and
add no analysis of your own. This keeps Codex's answer independent of Claude's.

## Verify on the host

```bash
make test            # Go tests plus the example lisp files
make static-checks   # golangci-lint with gosec; CI's version is the authority
make elpsvet         # elps's own Go analyzers, two passes
go test ./lisp/...   # the interpreter core alone, for a narrow unit
```

## Rules this container taught us

- **Check before a wave.** `uptime` and `df -h /`; `ps aux | grep
  app-server-broker` shows units already running (other sessions count). Keep
  heavy units to about 3 at a time; more drives load high enough that
  everything slows or times out.
- **Watch the disk.** The script refuses to start below 8 GB free. A unit that
  vanishes or a subagent that stops reporting is almost always disk. Clear, in
  order: `.codex-delegate/swept/` archives of accepted work, then the
  `/tmp/elps-<name>-go-cache` and `/tmp/codex-*-cache` directories of units
  that are swept or finished (no owner on their `.codex-delegate/run.lock`,
  checked with `fuser`). Never judge by age, and never delete a live unit's
  cache or worktree. `go clean -cache` goes last, and only when no unit is
  running.
- **Stale brokers.** Retire them with `sweep <name>`, which stops the
  workspace's broker under the lock. Do not kill brokers or delete broker
  temp directories by hand.
- **Never `git stash` in any worktree.** The stash list is shared by every
  worktree of the repo; a pop in one can apply another checkout's stash.
- **One Codex unit per PR or item.** Do not fold unrelated items into a unit.
- **Sweep when an agent retires.** `./scripts/codex-delegate.sh sweep <name>`,
  or `sweep` alone for all of them. Do this as each agent finishes, not later.
- **"Broker is busy" is not an error.** It is a start-up race. The script
  retries it three times. Never read it as a login failure.
- **Never run git in a worktree while another process watches `.git`.** A
  worktree in `.claude/worktrees/` shares the same `.git`.

## What Codex does not know

Do not assume Codex has read `AGENTS.md` or the skills. **Put the rules it must obey into the task
text**, or read its diff before you keep the work. The ones a brief must cite:

- Read the matching `.claude/skills/*/SKILL.md` before starting, and follow it.
- Run `make test && make static-checks && make elpsvet` before handing back (the full gate is `.claude/skills/verify/SKILL.md`).
- Every new builtin, special operator, or macro needs a docstring; CI runs
  `elps doc -m`.
- A new form with structural requirements needs a lint analyzer registered in
  `DefaultAnalyzers()`, and `TestDefaultAnalyzers`' count updated.
- User-facing changes update `docs/lang.md`.
- Never delete the `//nolint:gosec` directive on the last `return` of
  `parser/token/token.go`'s `Type.String`. It is load-bearing under CI's
  golangci-lint; a local lint run reporting it as unused is version skew, not
  a finding. Settle any `//nolint` question against CI's pinned version, not
  against whatever is on PATH.
- Commit messages are plain sentences saying what changes and why.

Codex writes files but it does not commit. Committing and pushing stays a
separate, deliberate step.

## Set up a fresh container

```bash
npm install -g @openai/codex
claude plugin marketplace add openai/codex-plugin-cc
claude plugin install codex@openai-codex
codex login --device-auth      # prints a URL and a code to approve
```

`codex login --device-auth` is the only login that works here. The normal
`codex login` sends the browser to a `localhost` address inside the container,
which nobody outside can reach. There is no long-lived secret in this
environment: the login is per session, and a new container repeats it.
