# /audit — Codebase Audit Skill

Performs a systematic, multi-category audit of the ELPS codebase. Modeled on the approach used in PR #74 (25-commit multi-category audit).

## Trigger

Use when asked to audit the codebase, find bugs, improve security, review code quality, or perform a comprehensive health check.

## Workflow

### 1. Create Branch

```bash
git checkout main && git pull origin main
git checkout -b audit/<focus-or-date>
```

### 2. Scan Each Category

Work through categories systematically. For each finding: understand it, fix it, write a test if applicable, and commit as a logical unit.

#### Category 1: Bugs

Scan for:
- Dead code and unreachable branches
- Copy-paste errors (duplicate logic with wrong variables)
- Off-by-one errors in loops and slices
- Nil dereference paths (unchecked `*LVal` returns)
- Race conditions in concurrent code
- Error values silently ignored (returned `*LVal` with `LError` type not checked)

Where to look: `lisp/`, `parser/`, `formatter/`, `lint/`

#### Category 2: Security

Scan for:
- Input validation gaps (especially in file I/O operations)
- Path traversal / symlink following (check `os.Open`, `os.ReadFile`, `filepath.Walk`)
- Permission checks on file operations
- Integer overflow in numeric operations
- Unsafe type assertions without checking

Run: `golangci-lint run --enable gosec ./...`

Where to look: `lisp/lisplib/`, `cmd/`, `repl/`

#### Category 3: Performance

Scan for:
- Unnecessary allocations in hot paths (eval loop, parser)
- Copies where pointers would suffice
- Maps that could be pre-sized with `make(map, n)`
- Repeated computation that could be cached (singleton pattern)
- String concatenation in loops (use `strings.Builder`)

Run benchmarks before/after: Follow `/benchmark` skill workflow.

Where to look: `lisp/eval.go`, `lisp/builtins.go`, `parser/rdparser/`

#### Category 4: Tests

Scan for:
- Coverage gaps: functions or branches without test coverage
- Missing edge cases: empty inputs, nil values, max values, error paths
- Error path coverage: ensure error branches are tested
- Test isolation: tests depending on shared mutable state
- Flaky tests: time-dependent or order-dependent tests

Run: `go test -cover ./...` and examine coverage percentages.

Where to look: All `_test.go` files, compare against source files

#### Category 5: Documentation

Scan for:
- Missing docstrings: `./elps doc -m`
- Stale docs in `docs/lang.md` (features added but docs not updated)
- Incorrect help text (description doesn't match behavior)
- Missing examples in documentation

Where to look: `docs/lang.md`, builtin/op/macro definitions, `CLAUDE.md`

#### Category 6: Code Quality

Scan for:
- golangci-lint findings: `golangci-lint run ./...`
- Inconsistent patterns (e.g., some builtins check errors, others don't)
- Dead imports
- TODO/FIXME/HACK comments that should be resolved
- Inconsistent naming conventions

Where to look: Entire codebase

### 3. Group Commits Logically

Organize fixes into coherent commits by category and sub-topic:
- `fix: resolve nil dereference in handler-bind error path`
- `security: validate file paths before opening`
- `perf: pre-size maps in parser initialization`
- `test: add coverage for edge cases in builtin arithmetic`
- `docs: update lang.md for new builtins`

### 4. Verify

Run full `/verify` pipeline after all fixes.

### 5. Create PR

Follow `/pr` skill. Structure the PR body with categorized findings:

```markdown
## Summary

Systematic codebase audit covering N categories with M fixes.

### Bugs (N fixes)
- Fix nil dereference in ...
- Fix off-by-one in ...

### Security (N fixes)
- Validate file paths in ...

### Performance (N fixes)
- Pre-size parser maps ...

### Tests (N additions)
- Add edge case coverage for ...

### Documentation (N updates)
- Update lang.md for ...

### Code Quality (N fixes)
- Resolve TODO in ...
```

## Scoped Audits

For targeted audits, focus on a single category or package:
- `/audit security` — Security-focused scan only
- `/audit lisp/` — Audit only the interpreter core
- `/audit tests` — Test coverage and quality only

## Checklist

- [ ] All 6 categories scanned (or scoped subset)
- [ ] Findings fixed with tests where applicable
- [ ] Commits grouped logically by category
- [ ] No regressions introduced (benchmarks stable)
- [ ] Full verify pipeline passes
- [ ] PR created with categorized summary
