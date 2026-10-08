# AGENTS.md

Guidance for coding agents working in this repository. Prescriptive,
step-by-step workflows live in `.claude/skills/` (table at the bottom); this
file is the durable overview.

## Project Overview

ELPS is an embedded Lisp interpreter implemented in Go. It is a Lisp-1 dialect
designed to be embedded within Go applications. Module:
`github.com/luthersystems/elps`.

## Build and Test Commands

| Command | Description |
|---------|-------------|
| `make` | Build the `./elps` binary |
| `make test` | Run all tests (Go tests + example lisp files) |
| `make go-test` | Run Go tests only: `go test -cover ./...` |
| `make race` | Full test suite under the race detector (CI runs it) |
| `make test-elpscheck` | Test suite under the `elpscheck` build tag (CI runs it) |
| `make elpsvet` | elps's own Go analyzers, untagged + `elpscheck` passes (CI runs it; see the `elpsvet` skill) |
| `make fuzz` | Run every native Go fuzz target, 30s each (`FUZZTIME=10m make fuzz` for longer; see the `fuzz` skill) |
| `make fuzz-list` | List the discovered fuzz targets without running them |
| `go test ./lisp/...` | Run tests for a specific package |
| `go test -run TestName ./lisp/` | Run a single test |
| `make static-checks` | golangci-lint with gosec, plus an `elpscheck`-tagged pass (warns if your version differs from CI's) |
| `make fieldalign-fix` | Reorder struct fields for the fieldalignment gate (uses betteralign — `fieldalignment -fix` deletes field comments) |
| `make ci-gates-test` | Self-test for the CI gate scripts in `scripts/` |
| `make api-break-gate` | API break gate vs `BASE` (default `origin/main`; CI runs it; see below) |
| `make work-marker-gate` | Work marker gate over the tracked tree (CI runs it; see below) |
| `make repl` | Build and launch the REPL |
| `./elps run file.lisp` | Run a lisp file |
| `./elps doc <query>` | Show function/package documentation |
| `./elps doc -m` | Check for missing docstrings (used in CI) |
| `./elps doc --guide` | Print the full language reference |
| `./elps doc --debug-guide` | Print the debugging guide |
| `./elps doc --lsp-guide` | Print the Language Server Protocol guide |
| `./elps lsp --stdio` | Start the LSP server (stdio transport) |
| `./elps lsp --port 7998` | Start the LSP server (TCP transport) |
| `./elps lint file.lisp` | Run static analysis on a lisp file |
| `./elps fmt file.lisp` | Format a lisp file |
| `./elps minify file.lisp` | Minify sources, emitting a symbol map |
| `./elps analyze file.lisp` | Run performance analysis on lisp sources |
| `./elps debug file.lisp` | Run a file under the DAP debugger |
| `./elps mcp` | Start the ELPS MCP server over stdio |
| `make release-notes` | Preview release notes (commits, PRs, CI status since last tag) |
| `make release VERSION=v1.X.0` | Human fallback only; agents use the `release` skill |

### golangci-lint version skew

`make static-checks` runs whatever `golangci-lint` is on PATH; CI pins the
version in `.github/workflows/elps.yml`. When the two differ the findings
differ **in both directions, silently** (the target prints a warning naming
both versions). Trust CI over a local run.

The trap: the `//nolint:gosec` on the last `return` of
`parser/token/token.go`'s `Type.String` is **load-bearing** under CI's
golangci-lint, but an older gosec does not flag that index, so `nolintlint`
calls it unused locally. Deleting it turns CI red. The skew runs in both
directions, so keep or drop a `//nolint` on the evidence of CI's pinned version
(is `main` green without it?), never on a local run.

### API break gate

CI's `api-break` job (`scripts/api-break-gate.sh`, `cmd/apibreak`) fails a PR
that removes or incompatibly changes elps's public API against its base: the
Go API of every non-internal package (`apidiff -m`, version pinned in the
script) and the Lisp surface from `elps doc --json -l` (packages, exported
symbols, kind, formals; a formals change fails only if it rejects a call the
base accepted). Additions never fail. An intended break needs one reviewed
line in `scripts/api-breaks.txt` -- `surface | symbol | expires | issue |
reason`, validated like `scripts/benchstat-waivers.txt` -- which the gate
prints ready to fill. Entries stay until the next release, whose notes list
them (`scripts/api-breaks-since.sh`), then are deleted.

### Work marker gate

CI's `work-markers` job (`scripts/work-marker-gate.sh`) fails when a tracked
file contains a work marker that `scripts/work-markers.txt` does not allow.
The header of that file lists the five marker words and the entry format,
`path | text | expires | issue | reason`. Do the work before you merge. Deferred
work needs a tracking issue and an expiry date. A marker word that is data (a
log prefix, third-party source) uses `never` and `-`. The gate also fails on an
entry that covers nothing, so delete the entry when the marker goes. Never
spell a marker in the gate script or its tests; assemble it at runtime.

## Architecture

### Packages

- **`lisp/`** — Interpreter core: `LVal` values, `LEnv` evaluator, builtins
  (`builtins.go`), special operators (`op.go`), macros (`macro.go`), packages,
  call stack, errors, Go interop, templates.
- **`parser/`** — Lexer (`lexer/`), tokens (`token/`), and the `rdparser/`
  recursive-descent parser. `parser.NewReader` returns an `rdparser` reader;
  pass `WithFormatPreserving()` for the tooling (formatter/LSP) variant.
- **`lisp/lisplib/`** — Standard library loaded by `LoadLibrary()` (assembled
  in `internal/stdlib`): time, help, golang, math, string, base64, json,
  regexp, testing, schema, elpspath.
- **`cmd/`** — Cobra CLI commands: `run`, `repl`, `doc`, `lint`, `fmt`, `lsp`,
  `minify`, `analyze`, `debug`, `mcp`.
- **`cmd/elpsvet/`** — Go analyzers over elps's *own* Go source (ownership,
  freshness, escape, native payload, builtin shared state, frozen packages,
  lazy reads, own package, exhaustive value switches, new value walkers, marker fields,
  durable natives).
  See the `elpsvet` skill.
- **`elpsvet/ownpkg/`** — The importable `elpsownpkg` analyzer: a library
  builtin runs in its own package (#736), so it must not evaluate code, build
  lambdas, read `Runtime.Package` or resolve a symbol it was handed.
- **`elpsvet/nativepayload/`** — The importable `elpsnativepayload` and
  `elpsdurablenative` analyzers. Each native payload must be safe for a
  template to publish, and each native payload type needs a durable codec
  (`libjson.DurableCodec[T]`) or a transient mark (`TransientNative`).
  Another module builds its own with `New(Config)` and
  `NewDurable(DurableConfig)`.
- **`repl/`** — Interactive REPL using readline.
- **`elpstest/`** — Test framework (`Runner`, `TestSuite`) for executing lisp
  test files as Go subtests.
- **`elpsutil/`** — Helpers for building embedded packages in Go
  (`Function()`, `PackageLoader()`, etc.).
- **`lsp/`** — Language Server (on `tliron/glsp`): diagnostics, hover,
  definition, references, completion, symbols, rename. Embeddable via
  `lsp.WithEnv()` / `lsp.WithRegistry()`.
- **`mcpserver/`** — MCP server behind `elps mcp`.
- **`formatter/`** — Source formatter over the annotated AST
  (`LVal.Meta *SourceMeta`).
- **`analysis/`** — Semantic analysis: scopes, symbol resolution, references.
  `prescan()` is two-phase (definitions, then exports) so `(export 'name)`
  before `(defun name ...)` works.
- **`lint/`** — Static analysis for lisp source, modeled after `go vet`
  (see the `add-linter-check` skill).
- **`diagnostic/`** — Rust-style annotated source snippets; no dependency on
  `lisp/`.
- **`internal/symtext/`** — The single definition of an ELPS symbol as text
  (alphabet, word bounds under a cursor, line lookup; byte columns) shared by
  `lsp/` and `mcpserver/` — never re-implement it locally.
- **`internal/fuzzseed/`** — Seed corpora for the fuzz targets.
- **`lisp/x/profiler/`** — Experimental profiling (callgrind, OpenCensus,
  OpenTelemetry).

Docs: `docs/lang.md` (language reference, embedded in the binary),
`docs/templates.md`, `docs/embed.md`, `docs/minify.md`, `docs/lint-checks.md`,
and design notes under `docs/internals/`.

### Key Types (lisp/)

- **`LVal`** — The universal value type (ints, floats, strings, symbols,
  lists, functions, errors, sorted-maps, arrays, native Go values, tagged
  values).
- **`LEnv`** — Environment/evaluator: eval, scoping, function calls, tail
  recursion, macro expansion, package management. Tree-structured.
- **`Runtime`** — Shared state across the env tree: package registry, call
  stack, reader, library, profiler.
- **`LBuiltin`** — `func(env *LEnv, args *LVal) *LVal`.

### Embedding Pattern

```go
env := lisp.NewEnv(nil)
env.Runtime.Reader = parser.NewReader()
env.Runtime.Library = &lisp.RelativeFileSystemLibrary{}
rc := lisp.InitializeUserEnv(env)
rc = lisplib.LoadLibrary(env)
rc = env.InPackage(lisp.String(lisp.DefaultUserPackage))
```

## Conventions

### Error propagation

Functions return `*LVal`. Errors are LVal values with type `LError` (build
them with `env.Errorf(...)`). Check `v.Type == lisp.LError` (or use
`GoError()`) and return the error immediately — ELPS code does not use Go's
`error` interface for lisp errors.

### Language key points

- **Lisp-1**: one namespace for functions and variables.
- **Booleans**: `true` and `false` are symbols. `()` (nil) is falsey;
  everything else is truthy.
- **Function args**: required, `&optional`, `&rest`, and `&key`.
- **Packages**: `in-package`, `use-package`, `export`. Qualified symbols use
  `:` (`lisp:set`); keywords start with `:`.
- **No backtick reader syntax**: write `(quasiquote ...)` / `(unquote ...)`.
- **Error handling**: `handler-bind` and `ignore-errors` catch; `with-cleanup`
  is `finally`, not `catch` (cleanup-form list first, then body), and never
  masks an `internal-panic`.
- **Tail recursion**: optimized via stack frame analysis.

### `set` vs `set!`

- **`set` creates or overwrites** bindings — the only way to create a new
  top-level binding.
- **`set!` only mutates** existing bindings — it errors on an unbound symbol.
- The `set-usage` linter flags repeated `set` on the same symbol, not every
  `set`.

### Tests

Go unit tests (`testify/assert`), lisp test files run through
`elpstest.Runner` (the `testing` stdlib package: `test`, `assert=`, ...), and
`elpstest.TestSuite` tables of `{expression, expected-result,
expected-output}`. Native fuzz targets live in `fuzz_test.go` files; see the
`fuzz` skill before touching them.

Shared Lisp test helpers: every `*_testhelpers.lisp` next to a `*_test.lisp`
is loaded (sorted by name) into each fresh test env immediately before the test
file (after `SetupFn` when running tests/benchmarks; discovery skips
`SetupFn`), and the package in effect before each helper is restored after it; other runners reuse the rule via
`elpstest.TestHelperFiles` / `elpstest.LoadTestHelpers`. Helpers never match
`*_test.lisp` and are never part of a production load.

### Workflows (see skills)

- New builtin / special op / macro, and **static migration diagnostics** for
  behavior changes → `implement`.
- New lint analyzer → `add-linter-check`.
- Go-side invariants → `elpsvet`: native payloads, LVal ownership and
  freshness, builtins that capture state (method values, closures,
  package-level vars), writes to `Package` tables or `packageBase`, and reads
  of `Package.symbols`, `Package.baseValues` or `sortedmap.m`. Also use it for
  exhaustive value switches (`elpsltypeswitch`) and new value walkers
  (`elpsvalwalker`). Value walkers own their traversal and choose children
  through `lisp.ShapeOf` or exhaustive `LType` switches. Each new walker
  needs an audited traversal contract in `valueWalkerFunctions`. A module
  that runs elpsvet's `elpsvalwalker` over its own code records the audit as
  `//elpsvet:allow-valwalker <reason>` in the walker's doc comment instead;
  elps itself ignores that marker and keeps the table.
- Before committing → `verify` (mirrors CI). Never commit to `main`.
- All writing (docs, docstrings, comments, commits, PRs, issues, release
  notes) → `write-docs`.

## Skills

Prescriptive workflows live in `.claude/skills/`. **Before starting a task,
read the matching skill file and follow its workflow.**

| Skill File | When to Use |
|------------|-------------|
| `implement/SKILL.md` | Any code change — builtins, ops, macros, parser, formatter, CLI, bug fixes, docs |
| `add-linter-check/SKILL.md` | New lint analyzer for lisp source (`lint/`) |
| `add-stdlib-package/SKILL.md` | New stdlib package — create, wire, test |
| `elpsvet/SKILL.md` | Run `make elpsvet`, read its diagnostics, add a native payload type or `//elpsvet:allow-native`, change an analyzer rule |
| `fuzz/SKILL.md` | Run fuzzing, add a fuzz target, triage a crasher in `testdata/fuzz/` |
| `verify/SKILL.md` | CI gate — the same checks `.github/workflows/elps.yml` runs |
| `pr/SKILL.md` | Ship — branch guard, verify, push, create PR |
| `benchmark/SKILL.md` | Performance — before/after benchstat comparison and the CI bench gate |
| `audit/SKILL.md` | Systematic codebase audit — bugs, security, perf, tests, docs, quality |
| `release/SKILL.md` | Create a tagged GitHub release via the release pipeline |
| `codex-delegate/SKILL.md` | Hand a bounded coding unit to Codex in a worktree; verify on host |
| `write-docs/SKILL.md` | House writing style for docs, docstrings, comments, commits, PRs, issues and release notes |

Skills chain: e.g. `implement` for the change, `verify` before committing,
and `pr` to ship.

## GitHub Tooling

Use `gh` for issues (`gh issue view/list/comment`), pull requests
(`gh pr view/checks/create`) and API queries (`gh repo view`, `gh api`). Where
`gh` is unavailable (some agent sandboxes), use the GitHub MCP tools; do not
scrape GitHub with generic web tooling.
