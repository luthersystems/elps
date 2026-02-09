# /implement — Code Change Skill

Implements any code change in the ELPS codebase. This is the foundation skill — all other skills reference its patterns.

## Trigger

Use when asked to implement a feature, fix a bug, add a builtin, modify the parser, update the formatter, add CLI flags, update docs, or make any other code change.

## Workflow

### 1. Classify the Change

Determine the change type to know which files to touch:

| Type | Primary Files | Test Pattern |
|------|--------------|--------------|
| `builtin` | `lisp/builtins.go` | `TestSuite` + `TestSequence` in `lisp/lisp_test.go` |
| `special-op` | `lisp/op.go` | `TestSuite` + `TestSequence` in `lisp/lisp_test.go` |
| `macro` | `lisp/macro.go` | `TestSuite` + `TestSequence` in `lisp/lisp_test.go` |
| `linter` | `lint/analyzers.go`, `lint/lint.go` | `lint/lint_test.go` (use `/add-linter-check` skill) |
| `stdlib` | `lisp/lisplib/lib<name>/` | `elpstest.Runner` + `.lisp` test file (use `/add-stdlib-package` skill) |
| `cli` | `cmd/<command>.go` | Manual verification or integration tests |
| `formatter` | `formatter/formatter.go` | `formatter/formatter_test.go` (`TestASTPreservation`, `TestRepoFileRoundTrip`) |
| `parser` | `parser/rdparser/rdparser.go` | `parser/rdparser/rdparser_test.go` |
| `diagnostic` | `diagnostic/diagnostic.go` | `diagnostic/diagnostic_test.go` |
| `bug-fix` | Varies — find the root cause first | Regression test reproducing the bug |
| `docs` | `docs/lang.md`, `CLAUDE.md` | `./elps doc --guide` to verify rendering |
| `deps` | `go.mod`, `go.sum` | `make test` to verify compatibility |
| `ci` | `.github/workflows/*.yml` | Push and verify Actions run |
| `profiler` | `lisp/x/profiler/` | Profiler-specific tests |

### 2. Read Before Writing

**Always read the files you plan to modify before making changes.** Understand existing patterns:

- For builtins: read nearby functions in `lisp/builtins.go` to match style
- For ops: read `lisp/op.go` for the `opXxx` naming convention
- For macros: read `lisp/macro.go` for `opMacroXxx` patterns
- For linter: read existing analyzers in `lint/analyzers.go`
- For stdlib: read an existing package like `lisp/lisplib/libmath/`

### 3. Implement

Follow these rules for all ELPS code:

#### Go Implementation Rules

- **Function signature**: `func builtinXxx(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal`
- **Argument parsing**: Use `Formals("name", lisp.FmtString("arg1"), ...)` for typed args. Available format types: `FmtString`, `FmtInt`, `FmtFloat`, `FmtBool`, `FmtSymbol`, `FmtAny`, `FmtList`, `FmtFun`, `FmtSortedMap`
- **Error propagation**: Return `env.Error("message")` or `env.Errorf("format", args...)`. Errors are `*LVal` values with `Type == LError`, NOT Go `error` interface
- **Check errors**: After any call that returns `*LVal`, check `if v.Type == lisp.LError { return v }` to propagate
- **Docstrings are mandatory**: CI runs `elps doc -m` and fails if any builtin/op/macro lacks a docstring
- **Registration**: Add to `DefaultBuiltins()`, `DefaultSpecialOps()`, or `DefaultMacros()` in the appropriate file

#### Lisp Code Rules

- `set` creates bindings. `set!` mutates existing bindings. First binding MUST use `set`
- No backtick quasiquote — ELPS doesn't have it
- `true` and `false` are symbols, not special forms
- `()` is nil/falsey, everything else is truthy
- Keywords start with `:` (e.g., `:key-name`)
- Qualified symbols use `:` separator (e.g., `math:pi`)

### 4. Write Tests

#### For builtins/ops/macros — Use `TestSuite` + `TestSequence`:

```go
{
    Expr:   `(my-builtin "arg1" 42)`,
    Result: `"expected-result"`,
},
{
    Expr:   `(my-builtin "error-case")`,
    Error:  true, // Expect an error
},
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
- **Never use backtick quasiquote** in test inputs — ELPS doesn't support it
- **Never modify files without reading them first** — understand existing patterns
- **Never skip tests** — write them before or alongside implementation
- **Never push to main** — always use a feature branch

## Checklist

- [ ] Read target files before modifying
- [ ] Implementation follows existing code style
- [ ] Docstring included (if adding builtin/op/macro)
- [ ] Tests written (regression test for bug fixes)
- [ ] `make test` passes
- [ ] `make static-checks` passes
- [ ] `./elps doc -m` passes (if touching builtins)
- [ ] `docs/lang.md` updated (if user-facing feature)
