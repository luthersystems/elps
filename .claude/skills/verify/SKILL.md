# /verify — CI Gate Skill

Runs the full verification pipeline matching the CI workflow. Use this before committing or creating a PR to catch issues locally.

## Trigger

Use when asked to verify changes, check CI readiness, or as a pre-commit/pre-PR gate.

## Pipeline

Run these 6 steps in order. Stop and report on the first failure.

### Step 1: Build

```bash
go build -o elps .
```

If this fails, there are compilation errors. Fix them before proceeding.

### Step 2: Test

```bash
make test
```

This runs both Go tests (`go test -cover ./...`) and example lisp files. All tests must pass.

### Step 3: Static Analysis

```bash
golangci-lint run ./...
```

Fixes lint issues flagged by golangci-lint (includes gosec for security checks).

### Step 4: Format Check

```bash
./elps fmt -l ./...
```

The `-l` flag lists files that would be reformatted. If any files are listed, run `./elps fmt ./...` to fix them, then re-verify.

### Step 5: Lint Check

```bash
./elps lint ./...
```

Runs all ELPS linter analyzers on the codebase. Fix any diagnostics or add `; nolint:check-name` suppression if the diagnostic is a false positive.

### Step 6: Documentation Check

```bash
./elps doc -m
```

Checks that all builtins, special operators, macros, and library exports have docstrings. If missing, add docstrings to the function definitions.

## On Failure

When a step fails:
1. Report which step failed and the error output
2. Fix the issue
3. Re-run the full pipeline from the beginning (fixes can introduce new issues)
4. Repeat until all 6 steps pass

## Quick Mode

For iterative development, run just the relevant subset:
- Changed Go code: Steps 1-3
- Changed `.lisp` files: Steps 2, 4-5
- Added builtins/ops/macros: Steps 1-3, 6
