# Codebase Audit

Perform a systematic, multi-category audit of the ELPS codebase. Modeled on the approach used in PR #74.

## Arguments

Optional: focus area (e.g., `security`, `bugs`, `performance`, `tests`, `docs`, `quality`, or a package path like `lisp/`).

## Steps

### 1. Create Branch

```bash
git fetch origin main
git checkout -b audit/<focus-or-date> origin/main
```

### 2. Scan Each Category

Work through categories systematically. For each finding: understand it, fix it, write a test if applicable, commit as a logical unit.

#### Category 1: Bugs
Scan for: dead code, copy-paste errors, off-by-one errors, nil dereference paths, race conditions, silently ignored error `*LVal` returns.
Where: `lisp/`, `parser/`, `formatter/`, `lint/`

#### Category 2: Security
Scan for: input validation gaps, path traversal/symlink following, permission checks, integer overflow, unsafe type assertions.
Run: `golangci-lint run --enable gosec ./...`
Where: `lisp/lisplib/`, `cmd/`, `repl/`

#### Category 3: Performance
Scan for: unnecessary allocations in hot paths, copies vs pointers, unsized maps, repeated computation, string concatenation in loops.
Run: `/project:benchmark` for before/after comparison.
Where: `lisp/eval.go`, `lisp/builtins.go`, `parser/rdparser/`

#### Category 4: Tests
Scan for: coverage gaps, missing edge cases, error path coverage, test isolation issues, flaky tests.
Run: `go test -cover ./...`
Where: All `_test.go` files

#### Category 5: Documentation
Scan for: missing docstrings (`./elps doc -m`), stale `docs/lang.md`, incorrect help text, missing examples.
Where: `docs/lang.md`, builtin/op/macro definitions, `CLAUDE.md`

#### Category 6: Code Quality
Scan for: golangci-lint findings, inconsistent patterns, dead imports, TODO/FIXME/HACK comments, naming inconsistencies.
Where: Entire codebase

### 3. Commit Logically

Group fixes by category:
- `fix: resolve nil dereference in handler-bind error path`
- `security: validate file paths before opening`
- `perf: pre-size maps in parser initialization`
- `test: add coverage for edge cases in builtin arithmetic`

### 4. Verify

Run `/project:verify` after all fixes.

### 5. Create PR

Use `/project:pr`. Structure the PR body with categorized findings and counts per category.
