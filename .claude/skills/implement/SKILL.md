# /implement — Code Change Skill

Implements any code change in the ELPS codebase. This is the foundation skill — all other skills reference its patterns.

## Trigger

Use when asked to implement a feature, fix a bug, add a builtin, modify the parser, update the formatter, add CLI flags, update docs, or make any other code change.

## Workflow

### 1. Classify the Change

Determine the change type to know which files to touch:

| Type | Primary Files | Test Pattern |
|------|--------------|--------------|
| `builtin` | `lisp/builtins.go` (`langBuiltins`) | `elpstest.TestSuite` in a topical `lisp/*_test.go` (e.g. `math_test.go`) |
| `special-op` | `lisp/op.go` (`langSpecialOps`) | `elpstest.TestSuite` in a topical `lisp/*_test.go` |
| `macro` | `lisp/macro.go` (`langMacros`) | `elpstest.TestSuite` in `lisp/macro_test.go` or a topical file |
| `linter` | `lint/analyzers.go`, `lint/lint.go` | `lint/lint_test.go` (use `/add-linter-check` skill) |
| `elpsvet` | `cmd/elpsvet/` | `go test ./cmd/elpsvet/` (use `/elpsvet` skill) |
| `fuzz` | `<pkg>/fuzz_test.go`, `internal/fuzzseed/` | `make fuzz` (use `/fuzz` skill) |
| `stdlib` | `lisp/lisplib/lib<name>/` | `elpstest.Runner` + `.lisp` test file (use `/add-stdlib-package` skill) |
| `cli` | `cmd/<command>.go` | Manual verification or integration tests |
| `formatter` | `formatter/formatter.go` | `formatter/formatter_test.go` (`TestASTPreservation`, `TestRepoFileRoundTrip`) |
| `parser` | `parser/rdparser/parser.go`, `parser/lexer/` | `parser/rdparser/parser_test.go` |
| `diagnostic` | `diagnostic/diagnostic.go`, `diagnostic/renderer.go` | `diagnostic/renderer_test.go` |
| `bug-fix` | Varies — find the root cause first | Regression test reproducing the bug |
| `docs` | `docs/lang.md` (embedded), `docs/templates.md`, `docs/embed.md`, `docs/minify.md`, `docs/lint-checks.md`, `AGENTS.md`, skills | `./elps doc --guide` to verify rendering of embedded guides |
| `deps` | `go.mod`, `go.sum` | `make test` to verify compatibility |
| `ci` | `.github/workflows/*.yml` | Push and verify Actions run |
| `profiler` | `lisp/x/profiler/` | Profiler-specific tests |

### 2. Read Before Writing

**Always read the files you plan to modify before making changes.** Understand existing patterns:

- For builtins: read nearby entries in `langBuiltins` (`lisp/builtins.go`) and their `builtinXxx` functions
- For ops: read `langSpecialOps` in `lisp/op.go` for the `opXxx` naming convention
- For macros: read `langMacros` in `lisp/macro.go` for the `macroXxx` patterns
- For linter: read existing analyzers in `lint/analyzers.go`
- For stdlib: read an existing package like `lisp/lisplib/libmath/`

### 3. Implement

Follow these rules for all ELPS code:

#### Go Implementation Rules

- **Function signature**: `func builtinXxx(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal`
- **Formals**: `Formals("a", "b")`; modifiers are plain symbols — `Formals("a", OptArgSymbol, "b")` (`&optional`), `VarArgSymbol` (`&rest`), `KeyArgSymbol` (`&key`). Formals are untyped: check `args.Cells[i].Type` in the body and return `env.Errorf(...)` on a mismatch
- **Error propagation**: Return `env.Error("message")` or `env.Errorf("format", args...)`. Errors are `*LVal` values with `Type == LError`, NOT Go `error` interface
- **Check errors**: After any call that returns `*LVal`, check `if v.Type == lisp.LError { return v }` to propagate
- **Docstrings are mandatory**: CI runs `elps doc -m` and fails if any builtin/op/macro lacks a docstring
- **Registration**: Add a `{"name", Formals(...), fn, `docstring`}` entry to `langBuiltins`, `langSpecialOps`, or `langMacros` (returned by `DefaultBuiltins()` / `DefaultSpecialOps()` / `DefaultMacros()`)

#### Lisp Code Rules

- `set` creates bindings. `set!` mutates existing bindings. First binding MUST use `set`
- No backtick reader syntax — write `(quasiquote ...)` / `(unquote ...)`
- `true` and `false` are symbols, not special forms
- `()` is nil/falsey, everything else is truthy
- Keywords start with `:` (e.g., `:key-name`)
- Qualified symbols use `:` separator (e.g., `math:pi`)

#### Adding a builtin, special operator, or macro

1. Implement it in `lisp/builtins.go`, `lisp/op.go`, or `lisp/macro.go`.
2. Give it a docstring in the definition — CI runs `elps doc -m`.
3. If the form has structural requirements (e.g. `rethrow` only inside
   `handler-bind`), add a lint analyzer (`/add-linter-check`).
4. Update `docs/lang.md` if user-facing (it is embedded and served by
   `elps doc --guide`).
5. Write Go and/or lisp tests (below), then `/verify`.

#### Static migration diagnostics

When a correctness fix changes previously accepted behavior, ship a reliable
static diagnostic for recognizable affected source patterns: `elps lint` for
Lisp source (`/add-linter-check`), `elpsvet` for Go embedding or
implementation invariants (`/elpsvet`). Point at the affected expression,
explain the replacement, and test both the broken pattern and valid
lookalikes. Where dynamic bindings, runtime data or opaque macros prevent a
complete check, document the limits — never present a partial check as proof
that a program is compatible.

### 4. Write Tests

#### For builtins/ops/macros — `elpstest.TestSuite`

Each row is a positional `{expression, expected-result, expected-output}`
triple; an expected error is written as its rendered message:

```go
func TestMyBuiltin(t *testing.T) {
	tests := elpstest.TestSuite{
		{"my-builtin", elpstest.TestSequence{
			{`(my-builtin "arg1" 42)`, `"expected-result"`, ""},
			{`(my-builtin)`, "test:1:1: lisp:my-builtin: invalid number of arguments: 0", ""},
		}},
	}
	elpstest.RunTestSuite(t, tests)
}
```

#### For linter checks — See `/add-linter-check` skill

#### For stdlib packages — See `/add-stdlib-package` skill

#### For bug fixes — Write a regression test that:
1. Fails without the fix (verify this first)
2. Passes with the fix

### 5. Branch Naming

Create a branch before starting work:
- Features: `feature/<short-description>`
- Bug fixes: `fix/<short-description>`
- Refactors: `refactor/<short-description>`
- Issue-driven: `issue-<N>/<short-description>`

### 6. Verify

After implementation, run the full verification pipeline. See `/verify` skill.

## Anti-Patterns

- **Never use Go's `error` interface** for ELPS errors — always return `*LVal` with `LError` type
- **Never omit docstrings** — CI will catch it
- **Never use `set!` for first bindings** — it errors on unbound symbols
- **Never use backtick quasiquote** in test inputs — ELPS has no reader syntax for it
- **Never modify files without reading them first** — understand existing patterns
- **Never skip tests** — write them before or alongside implementation
- **Never commit or push to main** — always use a feature branch (the `/pr` skill has the guard)

## Checklist

- [ ] Read target files before modifying
- [ ] Implementation follows existing code style
- [ ] Docstring included (if adding builtin/op/macro)
- [ ] Tests written (regression test for bug fixes)
- [ ] `make test` passes
- [ ] `make static-checks` passes
- [ ] `./elps doc -m` passes (if touching builtins)
- [ ] `docs/lang.md` updated (if user-facing feature)
