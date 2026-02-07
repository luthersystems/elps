# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

ELPS is an embedded Lisp interpreter implemented in Go. It is a Lisp-1 dialect designed to be embedded within Go applications. Module: `github.com/luthersystems/elps`.

## Build and Test Commands

| Command | Description |
|---------|-------------|
| `make` | Build the `./elps` binary |
| `make test` | Run all tests (Go tests + example lisp files) |
| `make go-test` | Run Go tests only: `go test -cover ./...` |
| `go test ./lisp/...` | Run tests for a specific package |
| `go test -run TestName ./lisp/` | Run a single test |
| `make static-checks` | Run golangci-lint with gosec |
| `make repl` | Build and launch the REPL |
| `./elps run file.lisp` | Run a lisp file |
| `./elps doc <query>` | Show function/package documentation |

## Architecture

### Core Packages

- **`lisp/`** — The interpreter core. Contains `LVal` (lisp values), `LEnv` (environment/evaluator), builtins, special operators, macros, package system, call stack, error handling, and Go interop.
- **`parser/`** — Lexer (`lexer/`), tokens (`token/`), and two parser implementations: `rdparser/` (recursive descent, primary) and `regexparser/` (regex-based, alternative).
- **`lisp/lisplib/`** — Standard library packages loaded by `LoadLibrary()`: time, help, golang, math, string, base64, json, regexp, testing, schema.
- **`cmd/`** — Cobra CLI commands: `run`, `repl`, `doc`.
- **`repl/`** — Interactive REPL using readline.
- **`elpstest/`** — Test framework (`Runner`) for executing lisp-based test files as Go subtests.
- **`elpsutil/`** — Helpers for building embedded packages in Go (`Function()`, `PackageLoader()`, etc.).
- **`lisp/x/profiler/`** — Experimental profiling (callgrind, OpenCensus, OpenTelemetry).

### Key Types (lisp/)

- **`LVal`** — The universal value type. Everything in ELPS is an LVal: ints, floats, strings, symbols, lists, functions, errors, sorted-maps, arrays, native Go values, tagged values.
- **`LEnv`** — Environment/evaluator. Handles eval, scoping, function calls, tail recursion optimization, macro expansion, and package management. Tree-structured (parent/child scopes).
- **`Runtime`** — Shared state across the env tree: package registry, call stack, reader, library, profiler.
- **`LBuiltin`** — Function signature for Go-implemented builtins: `func(env *LEnv, args *LVal) *LVal`.

### Embedding Pattern

Standard setup for embedding ELPS in Go:
```go
env := lisp.NewEnv(nil)
env.Runtime.Reader = parser.NewReader()
env.Runtime.Library = &lisp.RelativeFileSystemLibrary{}
rc := lisp.InitializeUserEnv(env)
rc = lisplib.LoadLibrary(env)
rc = env.InPackage(lisp.String(lisp.DefaultUserPackage))
```

### Test Infrastructure

Tests exist in two forms:
1. **Go unit tests** — Standard `_test.go` files using `testify/assert`.
2. **Lisp test files** — `.lisp` files executed via `elpstest.Runner`, which loads them and runs as Go subtests. The `libtesting` stdlib package provides `test`, `test-let`, `assert=`, `assert-equal`, `assert-nil`, etc.

Go test suites typically use `elpstest.TestSuite` with `TestSequence` entries that define `{expression, expected-result, expected-output}` triples.

### Language Key Points

- **Lisp-1**: Single namespace for functions and variables.
- **Booleans**: `true` and `false` are symbols. `()` (nil) is falsey; everything else is truthy.
- **Function args**: Support required, optional (`&optional`), variadic (`&rest`), and keyword (`&key`) arguments.
- **Packages**: Namespaced with `in-package`, `use-package`, `export`. Qualified symbols use `:` (e.g., `lisp:set`). Keywords start with `:`.
- **Error handling**: Condition-based with stack traces. `handler-bind` and `ignore-errors`.
- **Tail recursion**: Optimized via stack frame analysis.

### Error Propagation Convention

Functions return `*LVal`. Errors are LVal values with type `LError`. Check with `v.Type == lisp.LError` or use the `GoError()` helper. Errors propagate up the call chain — most code returns errors immediately rather than using Go's `error` interface.
