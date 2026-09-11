# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

ELPS is an embedded Lisp interpreter implemented in Go. It is a Lisp-1 dialect designed to be embedded within Go applications. Module: `github.com/luthersystems/elps`.

## Build and Test Commands

| Command | Description |
|---------|-------------|
| `make` | Build the `./elps` binary |
| `make test` | Run all tests (Go tests + example lisp files) |
| `make go-test` | Run Go tests only: `go test -cover ./...` |
| `make fuzz` | Run every native go fuzz target, 30s each (`FUZZTIME=10m make fuzz` for longer) |
| `make fuzz-list` | List the discovered fuzz targets without running them |
| `go test ./lisp/...` | Run tests for a specific package |
| `go test -run TestName ./lisp/` | Run a single test |
| `make static-checks` | Run golangci-lint with gosec (warns if your version differs from CI's) |
| `make fieldalign-fix` | Reorder struct fields for the fieldalignment gate (uses betteralign — `fieldalignment -fix` deletes field comments) |
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
| `make release VERSION=v1.X.0` | Create a tagged GitHub release (must be on main, CI must pass) |


### golangci-lint version skew

`make static-checks` runs whatever `golangci-lint` is on PATH; CI pins a
version in `.github/workflows/elps.yml`. When the two differ the results
differ, **in both directions and silently**, so the target now prints a
warning naming both versions. Trust CI over a local run.

The trap worth knowing, because it invites you to break the build: two
`//nolint:gosec` directives in `parser/token/token.go` (lines 113 and 115)
are **load-bearing** under CI's golangci-lint, but an older gosec does not
flag those array indexes, so `nolintlint` reports the directives as unused
and a local run reads as "two issues to clean up". Deleting them turns CI
red. `main` being green in CI is the authority on whether a `//nolint` is
dead — not a local run on a different version.


## Architecture

### Core Packages

- **`lisp/`** — The interpreter core. Contains `LVal` (lisp values), `LEnv` (environment/evaluator), builtins, special operators, macros, package system, call stack, error handling, and Go interop.
- **`parser/`** — Lexer (`lexer/`), tokens (`token/`), and the `rdparser/` recursive-descent parser. `parser.NewReader` returns an `rdparser` reader; pass `WithFormatPreserving()` for the tooling (formatter/LSP) variant.
- **`lisp/lisplib/`** — Standard library packages loaded by `LoadLibrary()`: time, help, golang, math, string, base64, json, regexp, testing, schema, elpspath.
- **`cmd/`** — Cobra CLI commands: `run`, `repl`, `doc`, `lint`, `fmt`, `lsp`, `minify`, `analyze`, `debug`, `mcp`.
- **`repl/`** — Interactive REPL using readline.
- **`elpstest/`** — Test framework (`Runner`) for executing lisp-based test files as Go subtests.
- **`elpsutil/`** — Helpers for building embedded packages in Go (`Function()`, `PackageLoader()`, etc.).
- **`lsp/`** — Language Server Protocol server built on `tliron/glsp`. Provides diagnostics, hover, go-to-definition, references, completion, document symbols, and rename. Embeddable via `lsp.WithEnv()` / `lsp.WithRegistry()` options.
- **`lisp/x/profiler/`** — Experimental profiling (callgrind, OpenCensus, OpenTelemetry).

### Key Types (lisp/)

- **`LVal`** — The universal value type. Everything in ELPS is an LVal: ints, floats, strings, symbols, lists, functions, errors, sorted-maps, arrays, native Go values, tagged values.
- **`LEnv`** — Environment/evaluator. Handles eval, scoping, function calls, tail recursion optimization, macro expansion, and package management. Tree-structured (parent/child scopes).
- **`Runtime`** — Shared state across the env tree: package registry, call stack, reader, library, profiler.
- **`LBuiltin`** — Function signature for Go-implemented builtins: `func(env *LEnv, args *LVal) *LVal`.

### Embedding Pattern

Standard setup for embedding ELPS in Go:
```go
env := lisp.NewEnv(nil)
env.Runtime.Reader = parser.NewReader()
env.Runtime.Library = &lisp.RelativeFileSystemLibrary{}
rc := lisp.InitializeUserEnv(env)
rc = lisplib.LoadLibrary(env)
rc = env.InPackage(lisp.String(lisp.DefaultUserPackage))
```

### Test Infrastructure

Tests exist in three forms:
1. **Go unit tests** — Standard `_test.go` files using `testify/assert`.
2. **Lisp test files** — `.lisp` files executed via `elpstest.Runner`, which loads them and runs as Go subtests. The `libtesting` stdlib package provides `test`, `test-let`, `assert=`, `assert-equal`, `assert-nil`, etc.
3. **Fuzz targets** — native `go test -fuzz` targets in `fuzz_test.go` files. There are over thirty; `make fuzz-list` prints the current set, and that command is the authoritative list rather than any enumeration here. Coverage includes the parser (strict, fault-tolerant and format-preserving), the lexer and byte scanner, the formatter and minifier round-trips, the JSON decoder, `elpspath`, the schema validator, `elpsutil` embedding, sorted-map and higher-order operations, cyclic value walks, shared-program/multi-env evaluation, the sealed load-file cache hook, and the CI gate self-test — plus the three below, whose hazards are load-bearing: the **evaluator** (`lisp.FuzzEval`), the **debugger's** eval path (`lisp/x/debugger.FuzzDebugEval` — attaching a debugger disables tail-call optimisation globally and stamps `MacroExpansionInfo` onto every macro-expanded node, so it is a materially different path from `FuzzEval`'s), and the **language server** as a whole session (`lsp.FuzzLSPSession` — a fuzzer-chosen sequence of LSP requests against fuzzer-chosen documents and *cursor positions*; it is also the only target that reaches `analysis/` and `lint/`). Seeds come from `internal/fuzzseed` (the repository's real `.lisp` sources plus hand-written adversarial input); each package's `testdata/fuzz/<Target>/` holds the crashers found so far, which plain `go test` replays as regression cases. Run with `make fuzz`; CI runs `.github/workflows/fuzz.yml` (30s/target on PRs, 10m/target nightly).

   **Evaluator fuzzing is different** and the differences are load-bearing (see the header comment on `lisp/eval_fuzz_test.go`):
   - Evaluation is Turing-complete and `go test -fuzz` has no per-input deadline, so every evaluation runs under an explicit budget (`WithMaxSteps`, `WithMaxTailIterations`, `WithMaximumPhysicalStackHeight`, `WithMaxAlloc`, plus a context deadline) *and* a 30s watchdog goroutine, so "it terminates" is an assertion rather than an assumption.
   - `env.eval` recovers every Go panic into an ordinary-looking `*LVal`, so "the process survived" proves nothing. The assertion with teeth is **`lisp.IsInternalPanic(result) == false`** — the non-forgeable marker keyed off the recovered Go-stack snapshot.
   - Eval seeds are *executed*, so `internal/fuzzseed/evalseed.go` is hand-written and deliberately does **not** seed from `LispSources()`: a large `.lisp` test suite as a seed throttles every generation descended from it. Seeds are split into `EvalRunaway` (must error) and `EvalTerminating` (must complete without error) — an infinite ELPS loop the budget stops is CORRECT; a bounded program that trips a budget is a defect, and both directions are asserted.

   **LSP fuzzing has the same hazard in a different shape** (see the header on `lsp/lsp_fuzz_test.go`): the server installs four blanket `recover()`s, so "it did not crash" is again worth nothing until they are accounted for. Two are neutralised by running the timer closures inline instead of letting them fire asynchronously; `buildWorkspaceIndex`'s is *detected* (it has no early return, so a nil `analysisCfg` afterwards means it panicked); `EnvMacroExpander.ExpandMacro`'s is not covered and the header says so. Everything else runs with no recover between the handler and the fuzz function, so a plain Go panic is the assertion.

Go test suites typically use `elpstest.TestSuite` with `TestSequence` entries that define `{expression, expected-result, expected-output}` triples.

### Language Key Points

- **Lisp-1**: Single namespace for functions and variables.
- **Booleans**: `true` and `false` are symbols. `()` (nil) is falsey; everything else is truthy.
- **Function args**: Support required, optional (`&optional`), variadic (`&rest`), and keyword (`&key`) arguments.
- **Packages**: Namespaced with `in-package`, `use-package`, `export`. Qualified symbols use `:` (e.g., `lisp:set`). Keywords start with `:`.
- **Error handling**: Condition-based with stack traces. `handler-bind` and `ignore-errors` catch; `with-cleanup` guarantees cleanup on every exit path (it is `finally`, not `catch`, and takes a cleanup-form list then the body) and never masks an `internal-panic`.
- **Tail recursion**: Optimized via stack frame analysis.

### Error Propagation Convention

Functions return `*LVal`. Errors are LVal values with type `LError`. Check with `v.Type == lisp.LError` or use the `GoError()` helper. Errors propagate up the call chain — most code returns errors immediately rather than using Go's `error` interface.

## Additional Packages

- **`formatter/`** — Source code formatter. Uses annotated AST with `LVal.Meta *SourceMeta`. Parser `preserveFormat` mode collects comments, brackets, newlines.
- **`analysis/`** — Semantic analysis: scope building, symbol resolution, reference counting. `prescan()` uses a two-phase approach (definitions first, exports second) so `(export 'name)` before `(defun name ...)` works correctly.
- **`lint/`** — Static analysis modeled after `go vet`. Each check is an `Analyzer` with a `Run func(pass *Pass) error`. Uses `Walk()`/`WalkSExprs()` for AST traversal, plus custom walkers for context-sensitive checks.
- **`diagnostic/`** — Rust-style annotated source snippets for error and lint output. Zero dependencies on `lisp/`.
- **`internal/symtext/`** — The one definition of an ELPS symbol *as text*: the symbol alphabet (`IsSymbolChar`), the word bounds under a cursor (`WordBoundsInLine`, `WordAt`), and the allocation-free walk to a line (`LineAt`). Both cursor servers call it — `lsp/` (hover, definition, references, completion, rename) and `mcpserver/` (hover, definition) — where each previously kept a private copy, and the copies drifted in both of the ways a duplicated helper drifts (#654): the MCP copy left `&` out of the alphabet, so a cursor in `&rest` answered for `rest` and one in `p&q:a&b` (`&` is legal in ELPS package and symbol names) answered for `q:a`, and it ran `strings.Split(content, "\n")` over the whole document to read one line — 1 alloc and 270 KiB per hover on a 112 KiB file, against lsp's 0. Columns are **bytes** throughout; an LSP wire column is converted at lsp's inbound boundary (#464) before it gets here. It is under `internal/` on purpose: an embedder gets the servers, not their cursor arithmetic, so the alphabet stays free to follow the lexer with no exported API to hold still.
- **`cmd/elpsvet/`** — Go static analyzers over elps's *own* Go source, enforcing invariants the compiler cannot: the freshness rule (writes rooted at `lisp.LVal` storage), the slice-alias tracking that catches writes laundered through a local alias first (issues #369, #371), and the native-payload audit (see "Go static analysis over elps's own sources" below).

## Go static analysis over elps's own sources

`cmd/elpsvet` is a `golang.org/x/tools/go/analysis` harness over elps's own
Go code: focused analyzers, audited suppression markers, and testdata-driven
`analysistest` fixtures under
`cmd/elpsvet/testdata/src`. CI runs it as `make elpsvet`, which is two passes
of `go run ./cmd/elpsvet -test=false ./...` (the second under
`GOFLAGS=-tags=elpscheck`, because elpsvet silently ignores `-tags`; see the
Makefile comment). The package list is `./...`, so there is no hand-scoped
list to drift away from where the code lives and no scope-drift check to
maintain.

Its fourth rule, `elpsnativepayload` (`cmd/elpsvet/nativepayload.go`), closes
a class of bug no isolation test can see from the outside. A template
PUBLISHES one value graph and instantiates a VM per request; instantiation
rebuilds every LVal header, cell span, byte buffer and map backing, but an
admitted native payload is **not** rebuilt — `instantiate` stores
`p.natives[value.payload]` straight into the fresh header
(`lisp/template_plan.go`) — so one Go value is shared by every VM the
template mints, for the life of the process. Publication is where that is
decided, and `(*templateInventory).native` (`lisp/template.go`) is the
runtime half of the audit. This rule is the static half: it runs at the
construction sites, where the payload still has a Go type a reader can see,
and it fails a NEW payload type in review rather than in whatever request
first publishes the value. Ported from the substrate repository's
nativepayload analyzer and re-audited against template admission.

A native construction is any of these SPELLINGS, and the list is the first
thing to check when the gate looks suspiciously quiet: `lisp.Native(x)`, the
typed `lisp.NativeOf[T](x)` (inferred or explicitly instantiated — the latter
wraps the callee in an `*ast.IndexExpr`, which `calleeFunc` unwraps), a
`lisp.Value(x)` the compiler can see falling through to Native (a defined
type over `[]byte` DOES fall through; the type switch matches the unnamed
type only), a keyed literal setting the `lisp.LVal.Native` field, and a write
to that field. The field is matched by its **object**, not by the receiver's
spelled type, so `lisp.ErrorVal{Native: x}` (`type ErrorVal LVal` shares the
struct), `(*lisp.ErrorVal)(v).Native = x`, and a promoted `w.Native` through
an embedding struct are all seen. Taking the field's address (`&v.Native`)
is reported unconditionally — whatever is later stored through the pointer
has no type at that site. `NativeOf` needs its own arm even though it is
*implemented* as a call to `Native`: a generic instantiation resolves to the
generic `*types.Func`, whose name is never `Native`. Invisible, and
documented in the analyzer's header: an indirect call through a function
value, a multi-value assignment (`v.Native, ok = g()`), a positional
`LVal{...}` literal (only spellable inside package lisp), and anything done
through `reflect`.

The exemption tiers MIRROR `templateInventory.native`, which is the point —
a rule that exempted more than publication does would let a payload through
review that the runtime then refuses at the first request, and one that
exempted less would make authors annotate what the runtime already admits:

- the payload's static type has a basic underlying type **whose kind is on
  `runtimeScalarKinds`** — the kind-for-kind mirror of the runtime's scalar
  `reflect.Kind` arm. A non-pointer value of scalar type is immutable inside
  an interface. `uintptr` and `unsafe.Pointer` are **not** in the tier
  because they are not in the runtime's either: both are addresses wearing a
  basic type's clothes, and `templateInventory.native` names
  `reflect.Uintptr` and `reflect.UnsafePointer` in the arm it refuses;
- the payload's static type is a **struct VALUE** whose method set carries
  `internal/templatepolicy.Immutable`'s unexported `templateImmutable()`,
  which only embedding `templatepolicy.Marker` can supply. The struct-value
  half is load-bearing and is the runtime's own condition: a pointer's
  method set inherits the marker, but a caller can replace the whole pointee
  however private its fields are, so `*T` is REPORTED even when `T` is
  marked. The three marked types in the tree — `libtime.ownedTime`,
  `libregexp.compiledRegexp`, `libschema.validatorTag` — pass through this
  tier and hold no allowlist row. A pointer form needs the embedder's
  `lisp.TemplateWithNativePolicy` approval instead, which is invisible here
  and so needs a site annotation;
- the type is on the audited allowlist — which after the re-audit holds only
  the kernel's own representation slots, `*funData`, `*[]byte`, `*MapData`,
  each row carrying **the `LType` header its storage belongs to** (`LFun`,
  `LBytes`, `LSortMap`) — **and the site shows that header**. Each row has an
  explicit arm in `templateInventory.val` keyed off the header's `Type`, never
  reaches `templateInventory.native`, and is rebuilt per VM by the planner, so
  a row is a claim about a HEADER rather than about a type. Three conditions,
  all required: **(1)** the site is IN package
  `github.com/luthersystems/elps/lisp`, since the rows describe the kernel's
  own slots; **(2)** the site is not a constructor — `Native`, `NativeOf` and
  a falling-through `Value` always build an `LNative`, whose payload `val`
  hands to `native()`, where all three row types are refused, so
  `b := []byte{1}; lisp.Native(&b)` is REPORTED even though `*[]byte` is a
  row; **(3)** for a keyed literal, the SAME literal's `Type:` key names the
  row's header, resolved through the type checker rather than by source text
  and checked by the constant's **identity** — the object it resolves to must
  be the one package `lisp` declares at package scope under that name, so a
  function-local `const LBytes LType = LNative` inside the kernel (same
  package, same `lisp.LType`, same spelling, different object, and an
  `LNative` header) does not satisfy the row.
  A literal with no `Type:` key shows no header, one naming another header
  shows the wrong one, one whose key is a shadowing local or an alias names
  no header the rule can read, and `LVal{Type: LNative, Native: &b}` names
  precisely the header `val` routes to `native()` — all four are reported. A `.Native`
  FIELD WRITE shows no header at all and cannot, so condition 3 does not apply
  to it and condition 1 carries the whole weight: exempt in package lisp,
  reported everywhere else. That residual is deliberate and named in the
  analyzer header — the seven such writes in the kernel (`lisp/copier.go`,
  `lisp/detach.go`, `lisp/template_plan.go`) are each guarded a few lines up
  by a check of the header's own `Type` that the rule does not model, and
  reporting them would force seven annotations onto code publication already
  routes correctly;
- a `//elpsvet:allow-native <justification>` comment covers the site:
  trailing on the reported line, standalone on the line above, or for a
  multi-line literal on either the opening line or the `Native:` line; or
  the enclosing function's doc carries one. **The justification must be at
  least three words; a bare or shorter marker does not suppress.** One
  justification covers every construction on its line.

`lisp.NativeCloner` is deliberately **not** a tier, though the ported rule
had it as one. `NewTemplate` rejects mutable payloads *including*
NativeCloner implementations, and an approved immutable payload is shared
without `CloneNative` ever being called (`lisp/native.go`,
`lisp/template_plan.go`) — so the method is evidence a payload needs cloning,
which is the opposite of a reason to share it. `time.Time` and
`*regexp.Regexp` are not rows either: `lisp/lisplib/template_natives_test.go`
demonstrates a host mutating both after admission, which is why libtime and
libregexp wrap them in marked owned struct values. A `*lisp.CallStack` gets
its own diagnostic naming `(*templateInventory).checkDiagnosticPayload`,
because publication bans the category outright (#629) and no row, marker or
policy could make one publishable.

The marker is this rule's own: `elpsownership`'s `//elpsvet:allow` is a bare
prefix match that enforces no justification, so sharing it would let one
sentence about sealed formals silence both rules — and the ownership matcher
stops at the marker's word boundary so `allow-native` does not satisfy it
either. An interface-typed payload (or a type parameter) is REPORTED, not
skipped: this module is where `interface{}` enters the system, so the
constructors themselves (`Native`, `NativeOf`, `Value`'s fallthrough), the
detach walker's clone arm, the planner's payload replay, the error-condition
data arm and libgolang's reflected field projection each carry an allow
naming the contract. A NEW payload type fails until a human classifies it;
that is the point, not an oversight.

`cmd/elpsvet/nativepayload_test.go` pins the rule's shape three ways: every
row in `allowedPayloadTypes` must appear in the test's audited inventory with
a justification long enough to read AND with the `LType` header it belongs to
(so adding a row, or moving one to another header, is a two-file change a
reviewer sees), the rows the re-audit dropped must stay dropped, and
`TestRegisteredAnalyzers` pins the four-rule set `make elpsvet` actually
runs. The `analysistest` fixtures live in five packages across three testdata
roots. Under `testdata/src`: `nativepayload` for the spellings, the allowlist
and the marker placements, `github.com/luthersystems/elps/nativemarker` for
the marker tier — under the module path because Go's internal rule lets only
packages there import `internal/templatepolicy`, which is the same reason the
tier is closed to downstream embedders — and
`github.com/luthersystems/elps/nativepaired`. The other two roots,
`testdata/nativelisp` and `testdata/nativepairedkernel`, each hold one package
whose import path IS `github.com/luthersystems/elps/lisp`, because the
allowlist tier's first condition is that the site is inside the kernel and no
fixture at another path can exercise the exempt side of it. They are private
roots rather than files in `testdata/src`'s kernel-path stub because that stub
carries expectations for the ESCAPE rule, and `analysistest` checks every
expectation in a package against the one analyzer it is running.

The paired fixtures exist because `analysistest` can only ask whether the rule
still says what its own fixtures expect, so a rule and its fixtures can drift
away from publication together and stay green — which is how a `uintptr`
payload, a `lisp.Value([]**LVal)`, a `*[]byte` through a constructor, and the
same `*[]byte` stored onto an `LNative` header came to be silently exempt from
a gate that claims to mirror admission.
`TestNativePayloadAnalyzerMirrorsTemplateAdmission`
(`cmd/elpsvet/nativepayload_runtime_test.go`) is the negative control: each
case is one construction spelled twice — once in a paired fixture, which the
real analyzer runs over, and once as a value a real `lisp.NewTemplate` is
asked to publish in-process — and both verdicts must agree. It runs over TWO
paired fixtures kept apart, since each one's diagnostic count is itself an
assertion: `nativepaired` for constructions outside the kernel, and
`testdata/nativepairedkernel`, a package at the kernel's own import path, for
the pair that makes the header condition checkable — two literals in package
lisp differing only in the `LType` constant they name, one exempt and one
reported. Three positive controls (`lisp.Native(int64(1))`, a marked struct
value, and the kernel's own `LSortMap` literal) are what stop "tighten until
nothing passes" from looking like a fix. **Change a tier and this test is
where the claim is checked**, not the fixtures.

## Development Workflow

### Adding New Builtins or Special Operators

When adding a new builtin function, special operator, or macro:

1. **Add the implementation** in `lisp/builtins.go`, `lisp/op.go`, or `lisp/macro.go`.
2. **Include a docstring** in the definition. All builtins must have documentation — CI runs `elps doc -m` to enforce this.
3. **Add a linter check** if the new form has structural requirements (e.g., `rethrow-context` checks that `rethrow` is only used inside `handler-bind`). Add the analyzer to `lint/analyzers.go` and register it in `DefaultAnalyzers()` in `lint/lint.go`.
4. **Update `docs/lang.md`** if the feature is user-facing. This file is embedded in the binary and served via `elps doc --guide`.
5. **Write tests**: Go unit tests in `_test.go` files and/or lisp test files via `elpstest.Runner`.
6. **Verify**: Run `make test && make static-checks` before committing.

### Linter Development

- Each analyzer is an `*Analyzer` struct in `lint/analyzers.go` with `Name`, `Doc`, and `Run` fields.
- Use `WalkSExprs()` for simple checks that inspect each s-expression independently.
- Use custom recursive walkers with depth/context tracking for checks that depend on enclosing forms (see `walkRethrowContext` for an example).
- Register new analyzers in `DefaultAnalyzers()` and update the analyzer count in `TestDefaultAnalyzers`.
- Support `; nolint:analyzer-name` suppression via trailing comments — this is handled automatically by `filterSuppressed()`.
- Every diagnostic now includes a `; nolint:` suppression hint in its notes.

### Static Migration Diagnostics

When a correctness fix changes previously accepted behavior, include a reliable
static migration diagnostic for recognizable affected source patterns. Use
`elps lint` for Lisp source and `elpsvet` for Go embedding or implementation
invariants. Point to the affected expression, explain the replacement, and
test both the broken pattern and valid lookalikes. Document the limits when
dynamic bindings, runtime data, or opaque macros prevent a complete static
check; do not present a partial check as proof that a program is compatible.

### `set` vs `set!` Semantics

- **`set` creates or overwrites** bindings — it is the only way to create new top-level bindings.
- **`set!` only mutates** existing bindings — it errors if the symbol is not already bound.
- The `set-usage` linter flags repeated `set` on the same symbol, not every `set`.

## Skills

Prescriptive workflows live in `.claude/skills/`. **Before starting a task, read the matching skill file and follow its workflow exactly.** Match tasks to skills by description:

| Skill File | When to Use |
|------------|-------------|
| `implement/SKILL.md` | Any code change — builtins, ops, macros, parser, formatter, CLI, bug fixes, docs |
| `add-linter-check/SKILL.md` | New lint analyzer — prescriptive 3-file-touch workflow |
| `add-stdlib-package/SKILL.md` | New stdlib package — create, wire, test |
| `verify/SKILL.md` | CI gate — build, test, golangci-lint, fmt, lint, doc checks |
| `pr/SKILL.md` | Ship — verify, push, create PR with summary and test plan |
| `pickup-issue/SKILL.md` | Full lifecycle — issue to branch to implementation to PR |
| `benchmark/SKILL.md` | Performance — before/after benchstat comparison |
| `audit/SKILL.md` | Systematic codebase audit — bugs, security, perf, tests, docs, quality |
| `release/SKILL.md` | Create a tagged GitHub release with auto-generated notes from merged PRs |

Multiple skills can chain: e.g., a GitHub issue triggers `pickup-issue`, which uses `implement` for the code change, `verify` before committing, and `pr` to ship.

## GitHub Tooling

Use `gh` for GitHub-hosted resources in this repository's workflows:
- issues: `gh issue view`, `gh issue list`, `gh issue comment`
- pull requests: `gh pr view`, `gh pr checks`, `gh pr create`
- repo metadata and API queries: `gh repo view`, `gh api`

Do not use generic web-scraping/search tooling for normal GitHub issue or PR retrieval when the GitHub CLI can provide the data directly.
