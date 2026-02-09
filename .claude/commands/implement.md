# Implement a Code Change

Implement any code change in the ELPS codebase: features, bug fixes, builtins, parser changes, CLI updates, etc.

## Arguments

Describe what to implement (e.g., "add a string-reverse builtin", "fix off-by-one in parser").

## Steps

### 1. Classify the Change

Determine the change type to know which files to touch:

| Type | Primary Files | Test Pattern |
|------|--------------|--------------|
| builtin | `lisp/builtins.go` | `TestSuite` + `TestSequence` in `lisp/lisp_test.go` |
| special-op | `lisp/op.go` | `TestSuite` + `TestSequence` in `lisp/lisp_test.go` |
| macro | `lisp/macro.go` | `TestSuite` + `TestSequence` in `lisp/lisp_test.go` |
| linter | `lint/analyzers.go`, `lint/lint.go` | `lint/lint_test.go` (use `/project:add-linter-check`) |
| stdlib | `lisp/lisplib/lib<name>/` | `elpstest.Runner` + `.lisp` test file (use `/project:add-stdlib-package`) |
| cli | `cmd/<command>.go` | Manual verification or integration tests |
| formatter | `formatter/formatter.go` | `formatter/formatter_test.go` |
| parser | `parser/rdparser/rdparser.go` | `parser/rdparser/rdparser_test.go` |
| diagnostic | `diagnostic/diagnostic.go` | `diagnostic/diagnostic_test.go` |
| bug-fix | Varies -- find root cause first | Regression test reproducing the bug |
| docs | `docs/lang.md`, `CLAUDE.md` | `./elps doc --guide` to verify rendering |

### 2. Create a Branch

```bash
git fetch origin main
git checkout -b <type>/<short-description> origin/main
```

Branch prefixes: `feature/`, `fix/`, `refactor/`, `docs/`, `perf/`, `issue-<N>/`

### 3. Read Before Writing

Always read the files you plan to modify before making changes. Understand existing patterns.

### 4. Implement

#### Go Rules
- Function signature: `func builtinXxx(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal`
- Argument parsing: Use `Formals("name", ...)` with format types (`FmtString`, `FmtInt`, `FmtFloat`, `FmtBool`, `FmtSymbol`, `FmtAny`, `FmtList`, `FmtFun`, `FmtSortedMap`)
- Error propagation: Return `env.Errorf(...)` -- errors are `*LVal` with `Type == LError`, NOT Go `error`
- After any call returning `*LVal`, check `if v.Type == lisp.LError { return v }`
- Docstrings are mandatory -- CI runs `elps doc -m`
- Register in `DefaultBuiltins()`, `DefaultSpecialOps()`, or `DefaultMacros()`

#### Lisp Rules
- `set` creates bindings; `set!` mutates existing bindings
- No backtick quasiquote -- ELPS doesn't have it
- `true`/`false` are symbols; `()` is nil/falsey
- Keywords start with `:`, qualified symbols use `:` (e.g., `math:pi`)

### 5. Write Tests

For builtins/ops/macros, use `TestSuite` + `TestSequence`:
```go
{Expr: `(my-builtin "arg")`, Result: `"expected"`},
{Expr: `(my-builtin)`, Error: true},
```

For bug fixes, write a regression test that fails without the fix.

### 6. Verify

Run `/project:verify` (all 6 CI gate steps).

### 7. Update Docs

If user-facing, update `docs/lang.md`. The file is embedded in the binary via `//go:embed`.
