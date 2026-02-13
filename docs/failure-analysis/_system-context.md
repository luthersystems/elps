# System Context — ELPS Failure Mode Analysis

## Date
2026-02-13

## Scope
Full system analysis of the ELPS embedded Lisp interpreter.

## System Description

ELPS is an embedded Lisp-1 interpreter implemented in Go (module: `github.com/luthersystems/elps`). It is designed to be embedded within Go applications, providing a scripting layer with full Lisp semantics. The system consists of: a recursive-descent parser that tokenizes and parses Lisp source into an AST of `LVal` nodes; an evaluator (`LEnv`) that walks the AST with tree-structured scoping, tail-recursion optimization, macro expansion, and a package system; a condition-based error handling system with stack traces; a standard library of 10 packages (time, help, math, string, json, regexp, testing, etc.); a source code formatter and static analysis linter; a CLI (`elps run/repl/doc/lint/fmt`); and Go interop via native value wrapping and builtin function registration. The universal value type `LVal` represents everything: ints, floats, strings, symbols, lists, functions, errors, sorted-maps, arrays, and native Go values. Resource limits include configurable max stack depth and max allocation. The system has no network services, databases, or external dependencies at runtime — it is a pure interpreter library with a CLI frontend.

## Key Source Files to Read

### Core Interpreter (lisp/)
- `/Users/swood/work/src/github.com/luthersystems/elps/lisp/lisp.go` — LVal type definitions, all 18 LType constants, core value operations
- `/Users/swood/work/src/github.com/luthersystems/elps/lisp/env.go` — LEnv evaluator, scoping, function calls, TRO, macro expansion, package management
- `/Users/swood/work/src/github.com/luthersystems/elps/lisp/runtime.go` — Shared runtime state, package registry, call stack, resource limits (MaxAlloc, MaxHeight)
- `/Users/swood/work/src/github.com/luthersystems/elps/lisp/builtins.go` — 50+ builtin functions, arithmetic, list ops, I/O, type checks
- `/Users/swood/work/src/github.com/luthersystems/elps/lisp/op.go` — Special operators: quote, lambda, if, let, cond, progn, handler-bind, thread macros
- `/Users/swood/work/src/github.com/luthersystems/elps/lisp/macro.go` — Macro system: defmacro, defun, deftype, macro expansion with source location stamping
- `/Users/swood/work/src/github.com/luthersystems/elps/lisp/error.go` — Error/condition system, stack trace capture
- `/Users/swood/work/src/github.com/luthersystems/elps/lisp/conditions.go` — Error condition constants
- `/Users/swood/work/src/github.com/luthersystems/elps/lisp/stack.go` — Call stack, call frames, stack capture on error
- `/Users/swood/work/src/github.com/luthersystems/elps/lisp/package.go` — Package registry, symbol binding/lookup, exports, qualified names
- `/Users/swood/work/src/github.com/luthersystems/elps/lisp/loader.go` — File loading, source library, search paths
- `/Users/swood/work/src/github.com/luthersystems/elps/lisp/lang.go` — Type predicates, conversion, comparison, sorting

### Parser (parser/)
- `/Users/swood/work/src/github.com/luthersystems/elps/parser/rdparser/parser.go` — Recursive descent parser, format-preserving mode, bracket matching
- `/Users/swood/work/src/github.com/luthersystems/elps/parser/lexer/lexer.go` — Lexical analysis, state machine, token emission
- `/Users/swood/work/src/github.com/luthersystems/elps/parser/token/token.go` — Token types, location tracking
- `/Users/swood/work/src/github.com/luthersystems/elps/parser/token/scanner.go` — UTF-8 scanner, line/column tracking

### Standard Library (lisp/lisplib/)
- `/Users/swood/work/src/github.com/luthersystems/elps/lisp/lisplib/lisplib.go` — Stdlib loader, LoadLibrary()
- `/Users/swood/work/src/github.com/luthersystems/elps/lisp/lisplib/libjson/libjson.go` — JSON serialization (security-relevant: untrusted input parsing)
- `/Users/swood/work/src/github.com/luthersystems/elps/lisp/lisplib/libregexp/libregexp.go` — Regex operations (potential ReDoS)

### Formatter & Linter
- `/Users/swood/work/src/github.com/luthersystems/elps/formatter/formatter.go` — Source formatter
- `/Users/swood/work/src/github.com/luthersystems/elps/lint/lint.go` — Static analyzer framework
- `/Users/swood/work/src/github.com/luthersystems/elps/lint/analyzers.go` — Built-in lint checks
- `/Users/swood/work/src/github.com/luthersystems/elps/analysis/analysis.go` — Semantic analysis, prescan

### CLI & Entry Points
- `/Users/swood/work/src/github.com/luthersystems/elps/cmd/run.go` — File execution, env initialization
- `/Users/swood/work/src/github.com/luthersystems/elps/cmd/root.go` — CLI setup
- `/Users/swood/work/src/github.com/luthersystems/elps/main.go` — Entry point

### Build & CI
- `/Users/swood/work/src/github.com/luthersystems/elps/Makefile` — Build tasks
- `/Users/swood/work/src/github.com/luthersystems/elps/.github/workflows/elps.yml` — CI pipeline
