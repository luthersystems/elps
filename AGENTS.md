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
calls it unused locally. Deleting it turns CI red. The skew also runs the other
way — a sibling directive on the constant-index `typeStrings[INVALID]` return
really was dead under the v2.13 pin and was removed. Keep or drop a `//nolint`
on the evidence of CI's pinned version (is `main` green without it?), never on
a local run.

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
  lazy reads). See the `elpsvet` skill.
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

### Workflows (see skills)

- New builtin / special op / macro, and **static migration diagnostics** for
  behavior changes → `implement`.
- New lint analyzer → `add-linter-check`.
- Go-side invariants (native payloads, LVal ownership) → `elpsvet`.
- Before committing → `verify` (mirrors CI). Never commit to `main`.

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
| `pickup-issue/SKILL.md` | Full lifecycle — issue to branch to implementation to PR |
| `benchmark/SKILL.md` | Performance — before/after benchstat comparison and the CI bench gate |
| `audit/SKILL.md` | Systematic codebase audit — bugs, security, perf, tests, docs, quality |
| `release/SKILL.md` | Create a tagged GitHub release via the release pipeline |
| `codex-delegate/SKILL.md` | Hand a bounded coding unit to Codex in a worktree; verify on host |

Skills chain: e.g. `pickup-issue` uses `implement` for the change, `verify`
before committing, and `pr` to ship.

## GitHub Tooling

Use `gh` for issues (`gh issue view/list/comment`), pull requests
(`gh pr view/checks/create`) and API queries (`gh repo view`, `gh api`). Where
`gh` is unavailable (some agent sandboxes), use the GitHub MCP tools; do not
scrape GitHub with generic web tooling.
