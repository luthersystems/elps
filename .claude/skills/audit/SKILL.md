# /audit — Codebase Audit Skill

Performs a systematic, multi-category audit of the ELPS codebase.

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
- Returned `*LVal` errors (`LError`) not checked and propagated
- Walkers that treat values as trees: nested sharing (`(set! x (list x x))`) makes them exponential (`lisp/sharing.go` has the budget and memo pattern)
- Builtins whose work grows with value size but charge no steps or skip `CheckAlloc`

Where to look: `lisp/`, `parser/`, `formatter/`, `lint/`

#### Category 2: Security

Scan for:
- File access in builtins and the CLI: path traversal and symlink following (`os.Open`, `os.ReadFile`, `filepath.Walk`)
- Limits a program can bypass: the step budget, `MaxAlloc`, and context cancellation

Run: `make static-checks` (gosec is enabled in `.golangci.yml`), `make elpsvet` (shared-state and native-payload invariants; `/elpsvet`), and a `make fuzz` pass over the packages in scope (`/fuzz`)

Where to look: `lisp/lisplib/`, `cmd/`, `repl/`

#### Category 3: Performance

Scan the eval loop and parser for allocations per call. Measure on the
whole-program benchmarks (`BenchmarkWorkload`) before and after, per the
`/benchmark` skill.

Where to look: `lisp/env.go` (eval loop), `lisp/builtins.go`, `parser/rdparser/`

#### Category 4: Tests

Scan for coverage gaps in error paths, and for tests that depend on shared
state or wall-clock time.

Run: `go test -cover ./...` and examine coverage percentages.

Where to look: All `_test.go` files, compare against source files

#### Category 5: Documentation

Scan for:
- Missing docstrings: `./elps doc -m`
- Stale docs in `docs/lang.md` (features added but docs not updated)
- Incorrect help text (description doesn't match behavior)
- Missing examples in documentation

Where to look: `docs/lang.md`, `docs/lint-checks.md`, builtin/op/macro definitions, `AGENTS.md`, `.claude/skills/`

#### Category 6: Code Quality

Run `make static-checks` (mind the version-skew note in `AGENTS.md` before
removing a `//nolint`), and look for builtins that handle errors differently
from their neighbours.

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
