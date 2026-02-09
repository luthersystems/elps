# Run Tests

Run the ELPS test suite -- full or targeted.

## Arguments

- No arguments: run the full test suite
- Package path: run tests for a specific package (e.g., `./lisp/...`, `./lint/...`)
- Test name: run a single test by name

## Full Test Suite

```bash
make test
```

This runs:
1. `go test -cover ./...` (all Go unit tests)
2. Example lisp file tests (via `make -C _examples test`)

## Targeted Tests

### By package:
```bash
go test -v ./lisp/...
go test -v ./parser/rdparser/...
go test -v ./lint/...
go test -v ./formatter/...
go test -v ./diagnostic/...
go test -v ./lisp/lisplib/libmath/...
```

### By test name:
```bash
go test -run TestFunctionName ./lisp/...
go test -run TestMyLinter ./lint/...
```

### With coverage:
```bash
go test -cover ./...
go test -coverprofile=coverage.out ./...
go tool cover -html=coverage.out  # Open coverage report
```

### With verbose output:
```bash
go test -v -run TestSpecificTest ./lisp/...
```

## Test Patterns in This Repo

### Go unit tests (`_test.go`)
Standard Go test files using `testify/assert`. Most tests use `elpstest.TestSuite` with `TestSequence` entries defining `{Expr, Result, Error, Output}` triples.

### Lisp test files (`.lisp`)
Executed via `elpstest.Runner` which loads and runs them as Go subtests. Use `testing` stdlib: `test`, `test-let`, `assert=`, `assert-equal`, `assert-nil`, `assert-error`.

## On Failure

Report which test failed, the full error output, and the file/line where the failure occurred. If the user is working on a change, suggest whether the failure is expected (test needs updating) or indicates a bug.
