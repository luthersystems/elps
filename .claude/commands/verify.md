# Verify CI Readiness

Run the full verification pipeline matching the CI workflow. Use this before committing or creating a PR.

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

Runs Go tests (`go test -cover ./...`) and example lisp files. All must pass.

### Step 3: Static Analysis

```bash
golangci-lint run ./...
```

Fix any lint issues (includes gosec for security checks).

### Step 4: Format Check

```bash
./elps fmt -l ./...
```

Lists files that need reformatting. If any are listed, run `./elps fmt ./...` to fix them, then re-verify.

### Step 5: Lint Check

```bash
./elps lint ./...
```

Fix any ELPS linter diagnostics or suppress false positives with `; nolint:check-name`.

### Step 6: Documentation Check

```bash
./elps doc -m
```

All builtins, special operators, macros, and library exports must have docstrings.

## On Failure

1. Report which step failed and the error output
2. Fix the issue
3. Re-run the full pipeline from the beginning (fixes can introduce new issues)
4. Repeat until all 6 steps pass

## Quick Mode

For iterative development, run just the relevant subset:
- Changed Go code: Steps 1-3
- Changed `.lisp` files: Steps 2, 4-5
- Added builtins/ops/macros: Steps 1-3, 6
