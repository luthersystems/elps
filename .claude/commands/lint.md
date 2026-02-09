# Run Linters and Fix Issues

Run all linters on the ELPS codebase and fix any issues found.

## Steps

### 1. Go Static Analysis

```bash
golangci-lint run ./...
```

Fix any issues reported. Common categories:
- `gosec`: security issues (G104 unchecked errors, G304 file inclusion, G306 permissions)
- `staticcheck`: code simplification and correctness
- `govet`: suspicious constructs

### 2. ELPS Source Formatting

```bash
./elps fmt -l ./...
```

The `-l` flag lists files that would be reformatted. If any are listed:
```bash
./elps fmt ./...
```

This reformats all `.lisp` files in-place using ELPS indent rules (align, body, special forms).

### 3. ELPS Static Analysis

```bash
./elps lint ./...
```

Current analyzers:
| Check | Description |
|-------|-------------|
| `set-usage` | Flags repeated `set` on same symbol (should use `set!`) |
| `in-package-toplevel` | Flags `in-package` inside nested forms |
| `if-arity` | Requires exactly 3 args (condition, then, else) |
| `let-bindings` | Validates `let`/`let*` binding list structure |
| `defun-structure` | Validates `defun`/`defmacro` form structure |
| `cond-structure` | Validates `cond` clause structure and `else` placement |
| `builtin-arity` | Checks argument counts for known builtins |
| `quote-call` | Flags unquoted `set`/`defconst` arguments |
| `cond-missing-else` | Flags `cond` with no default clause |
| `rethrow-context` | Flags `rethrow` used outside `handler-bind` |
| `unnecessary-progn` | Flags redundant `progn` wrappers |

Fix findings or suppress false positives with `; nolint:check-name` trailing comments.

### 4. Documentation Completeness

```bash
./elps doc -m
```

If missing docstrings are reported, add them to the function/macro/op definitions. All builtins, special operators, macros, and library exports must have docstrings.

## Build First

If `./elps` doesn't exist or is stale, build it first:
```bash
go build -o elps .
```

## Fixing Patterns

- **gosec G104** (unchecked error): Use `errWriter` pattern or explicitly handle the error
- **set-usage**: Change subsequent `set` to `set!` for re-binding
- **unnecessary-progn**: Remove the wrapping `(progn ...)` since the parent form supports multiple body expressions
- **Missing docstring**: Add documentation string(s) to the function definition
