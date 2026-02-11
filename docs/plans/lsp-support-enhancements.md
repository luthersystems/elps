# Design: ELPS Core Enhancements for LSP Support

**Date:** 2026-02-09
**Branch:** `design/lsp-support-enhancements`
**Status:** Draft — awaiting review
**Audience:** ELPS core developers + LSP team

---

## Motivation

The ELPS Language Server team has built comprehensive semantic analysis
capabilities on top of the ELPS parser and AST. Their two gap analysis
documents identify concrete areas where changes to ELPS core would:

1. **Eliminate fragile workarounds** in the LSP (e.g. 261-line `patch_locations.go`)
2. **Enable richer diagnostics** for both CLI and IDE users
3. **Reduce code duplication** between LSP and native tooling
4. **Provide a foundation** for future `elps check`, rename refactoring, etc.

This document proposes specific, backwards-compatible changes to ELPS core,
organized by priority. Each change is designed to be independently shippable.

---

## Design Principles

- **Backwards compatibility is paramount.** No existing public API signatures
  change. New fields use zero-value defaults. New packages are additive.
- **Performance matters.** All new capabilities are opt-in. The default code
  path (interpreter, REPL) pays zero cost.
- **Security.** No new attack surface. The analyzer is read-only static
  analysis — no eval, no file writes, no network.
- **Incremental delivery.** Each change is a separate PR that can be reviewed,
  tested, and merged independently.

---

## Table of Contents

1. [P0: Expose Format-Preserving Parser Mode](#p0-expose-format-preserving-parser-mode)
2. [P1: Add End Positions to token.Location](#p1-add-end-positions-to-tokenlocation)
3. [P2: Add Severity to lint.Diagnostic](#p2-add-severity-to-lintdiagnostic)
4. [P3: Semantic Analyzer Package](#p3-semantic-analyzer-package)
5. [P4: Multi-File Linting Support](#p4-multi-file-linting-support)
6. [P5: CLI Integration (elps check)](#p5-cli-integration-elps-check)
7. [P6: Structured Parse Errors](#p6-structured-parse-errors)
8. [P7: Function Metadata Helper](#p7-function-metadata-helper)
9. [Deferred / Out of Scope](#deferred--out-of-scope)
10. [Migration Guide for LSP Team](#migration-guide-for-lsp-team)

---

## P0: Expose Format-Preserving Parser Mode

**Impact:** HIGH — eliminates LSP's `patch_locations.go` workaround and custom
comment parsing. Unlocks `SourceMeta` for external consumers.

**Problem:** `rdparser.NewFormatting()` exists and populates `LVal.Meta`
(`SourceMeta`) with bracket types, comments, blank lines, and original text.
But the public `parser.NewReader()` API has no way to enable this mode. The
LSP cannot access `SourceMeta` without importing `rdparser` directly and
managing scanners manually.

**Current state:**
- `parser/parser.go:11` — `NewReader()` always returns standard mode
- `parser/rdparser/parser.go:61` — `NewFormatting()` sets `preserveFormat = true`
- `lisp/loader.go:14-27` — `Reader` interface has no options

### Proposed Changes

#### A. Add `ReaderOption` to `parser` package

```go
// parser/parser.go

type ReaderOption func(*readerConfig)

type readerConfig struct {
    preserveFormat bool
}

// WithFormatPreserving enables format-preserving mode. When enabled, the
// parser populates LVal.Meta with SourceMeta containing comments, bracket
// types, blank lines, and original literal text. This has a modest memory
// cost and should only be enabled for tooling (formatter, linter, LSP).
func WithFormatPreserving() ReaderOption {
    return func(c *readerConfig) {
        c.preserveFormat = true
    }
}

// NewReader returns a new lisp.Reader. Options are applied in order.
func NewReader(opts ...ReaderOption) lisp.Reader {
    cfg := &readerConfig{}
    for _, opt := range opts {
        opt(cfg)
    }
    if cfg.preserveFormat {
        return &formattingReader{}
    }
    return rdparser.NewReader()
}
```

The `formattingReader` type implements `lisp.Reader` by creating a scanner
and using `rdparser.NewFormatting()` internally.

#### B. Backwards compatibility

- `parser.NewReader()` with no args behaves identically to today.
- The `Reader` and `LocationReader` interfaces remain unchanged.
- LSP team can switch from direct `rdparser` usage to `parser.NewReader(parser.WithFormatPreserving())`.

#### Files touched
- `parser/parser.go` — add `ReaderOption`, `WithFormatPreserving`, update `NewReader`
- `parser/formatting_reader.go` — new file implementing `formattingReader`

#### Tests
- Existing `TestRepoFileRoundTrip` and `TestASTPreservation` continue passing.
- New test: verify `NewReader(WithFormatPreserving())` populates `Meta` fields.
- New test: verify `NewReader()` (no options) does NOT populate `Meta`.

---

## P1: Add End Positions to token.Location

**Impact:** HIGH — eliminates the LSP's `patcher` struct (200+ lines) that
manually calculates end positions and byte offsets for every AST node.

**Problem:** `token.Location` has `File`, `Path`, `Pos`, `Line`, `Col` but no
end position. The LSP must re-scan source text to compute ranges for every
node, stored in parallel `map[*lisp.LVal]protocol.Range`.

**Current state:**
- `parser/token/token.go:98-117` — `Location` struct
- `Scanner.LocStart()` returns start position
- `Scanner.Loc()` returns current (end) position — but this is not stored

### Proposed Changes

#### A. Extend `token.Location`

```go
// parser/token/token.go

type Location struct {
    File string
    Path string
    Pos  int
    Line int
    Col  int

    // End position fields. Zero values mean "not tracked" for backwards
    // compatibility. Populated when the parser has end-position information.
    EndLine int  // end line (1-based, 0 = not tracked)
    EndCol  int  // end column (1-based, 0 = not tracked)
    EndPos  int  // end byte position (0 = not tracked)
}
```

#### B. Update `Location.String()`

When `EndLine > 0`, the string representation remains `file:line:col` (start
position only). The end position is available programmatically but doesn't
change the display format. This ensures all existing error messages and test
expectations remain unchanged.

#### C. Populate end positions in the parser

In `rdparser`, after parsing each token/expression:
- **Atoms** (symbols, ints, floats, strings): `EndLine`/`EndCol`/`EndPos`
  computed from token text length.
- **S-expressions**: `EndLine`/`EndCol`/`EndPos` set from closing bracket
  position.
- **Mode:** End positions are always populated — the cost is 3 int fields
  (24 bytes) per Location, which is negligible since Location is already
  heap-allocated and shared.

#### D. Backwards compatibility

- All new fields default to zero ("not tracked").
- `String()` format unchanged.
- No existing test output changes.
- `Pos < 0` sentinel for synthetic/native source still works.

#### Files touched
- `parser/token/token.go` — extend `Location` struct
- `parser/rdparser/parser.go` — populate end positions during parsing
- `parser/token/scanner.go` — may need `LocEnd()` helper

#### Tests
- Existing tests pass (zero-value new fields).
- New test: verify end positions for atoms, s-expressions, nested forms.
- New test: verify `EndLine`/`EndCol` for multi-line expressions.

---

## P2: Add Severity to lint.Diagnostic

**Impact:** MEDIUM — enables the LSP to distinguish errors from warnings from
info-level hints without maintaining a separate mapping.

**Problem:** `lint.Diagnostic` has no severity field. The LSP's
`lintDiagToDiagnostic()` in `cmd/diagnostic.go` hardcodes all lint
diagnostics as `SeverityWarning`. But analyzers like `builtin-arity` and
`rethrow-context` report actual errors, while `cond-missing-else` and
`unnecessary-progn` are informational.

**Current state:**
- `lint/lint.go:77-90` — `Diagnostic` has `Pos`, `Message`, `Analyzer`, `Notes`
- `cmd/diagnostic.go:80-95` — all lint diags → `SeverityWarning`
- `diagnostic/diagnostic.go:8-28` — `Severity` type exists (Error, Warning, Note)

### Proposed Changes

#### A. Add `Severity` field to `lint.Diagnostic`

```go
// lint/lint.go

// Severity indicates the importance of a diagnostic finding.
type Severity int

const (
    SeverityError   Severity = iota // Likely bug or incorrect code
    SeverityWarning                 // Suspicious pattern, may be intentional
    SeverityInfo                    // Suggestion or style hint
)

type Diagnostic struct {
    Pos      Position `json:"pos"`
    Message  string   `json:"message"`
    Analyzer string   `json:"analyzer"`
    Severity Severity `json:"severity"`          // NEW
    Notes    []string `json:"notes,omitempty"`
}
```

#### B. Set severity per analyzer

| Analyzer | Severity | Rationale |
|----------|----------|-----------|
| `builtin-arity` | Error | Wrong arg count = runtime crash |
| `rethrow-context` | Error | rethrow outside handler = runtime crash |
| `if-arity` | Error | Wrong if shape = logic bug |
| `let-bindings` | Error | Malformed let = parse/runtime failure |
| `defun-structure` | Error | Malformed defun = parse/runtime failure |
| `cond-structure` | Error | Malformed cond = runtime failure |
| `quote-call` | Warning | Likely mistake but could be intentional |
| `set-usage` | Warning | Style issue, code works either way |
| `in-package-toplevel` | Warning | Will work but behavior may surprise |
| `cond-missing-else` | Info | Best practice suggestion |
| `unnecessary-progn` | Info | Style suggestion |

#### C. Backwards compatibility

- `Severity` zero value is `SeverityError` — matches the most common case
  and ensures any diagnostic without explicit severity is treated conservatively.
- JSON output gains a `"severity"` field. Existing JSON consumers that
  ignore unknown fields are unaffected.
- Text output (`Diagnostic.String()`) can optionally include severity prefix.
- `cmd/diagnostic.go` updated to use `d.Severity` instead of hardcoded warning.

#### D. Extend `Analyzer` struct (optional)

```go
type Analyzer struct {
    Name     string
    Doc      string
    Severity Severity  // Default severity for diagnostics from this analyzer
    Run      func(pass *Pass) error
}
```

Analyzers can still override per-diagnostic via `pass.Report()`. The
`Analyzer.Severity` provides a default that `Report` applies when the
diagnostic's severity is zero-value.

#### Files touched
- `lint/lint.go` — add `Severity` type and field
- `lint/analyzers.go` — set `Severity` on each analyzer
- `cmd/diagnostic.go` — map `lint.Severity` → `diagnostic.Severity`

#### Tests
- Update existing lint tests to verify severity values.
- Verify JSON output includes severity.

---

## P3: Semantic Analyzer Package

**Impact:** HIGH — the core new capability. Provides scope analysis, symbol
resolution, and semantic diagnostics that both CLI and LSP can share.

**Problem:** The LSP has built its own scope tree, symbol tracking, and
semantic analysis in `internal/analysis/`. This is ~1500 lines of code that
duplicates concepts already partially present in ELPS's runtime (`LEnv` scope
chain, package system, formals parsing). A shared package would:
- Give CLI users the same semantic checks
- Let the LSP delete its custom analyzer and use the shared one
- Ensure consistent behavior

### Proposed Design

#### A. New package: `analysis/`

Top-level package (not under `lisp/` to avoid import cycles with `parser/`).

```
analysis/
    analysis.go      // Analyzer, Result, public API
    scope.go         // Scope tree construction
    symbol.go        // Symbol, Signature types
    reference.go     // Reference tracking
    checks.go        // Semantic diagnostic checks
    builtins.go      // Builtin symbol/signature registry
    analysis_test.go
```

#### B. Core types

```go
package analysis

import (
    "github.com/luthersystems/elps/lisp"
    "github.com/luthersystems/elps/parser/token"
)

// ScopeKind classifies the type of lexical scope.
type ScopeKind int

const (
    ScopeGlobal   ScopeKind = iota
    ScopeFunction           // defun, defmacro
    ScopeLambda             // lambda
    ScopeLet                // let, let*
    ScopeFlet               // flet, labels
)

// Scope represents a lexical scope in the source code.
type Scope struct {
    Kind     ScopeKind
    Parent   *Scope
    Children []*Scope
    Symbols  map[string]*Symbol
    Node     *lisp.LVal            // AST node that created this scope
}

// Lookup resolves a symbol name through the scope chain.
func (s *Scope) Lookup(name string) *Symbol { ... }

// SymbolKind classifies what a symbol represents.
type SymbolKind int

const (
    SymVariable  SymbolKind = iota
    SymFunction
    SymMacro
    SymParameter
    SymSpecialOp
    SymKeyword
)

// Symbol represents a named binding in a scope.
type Symbol struct {
    Name       string
    Kind       SymbolKind
    Source     *token.Location     // definition site
    Scope      *Scope             // owning scope
    Signature  *Signature         // non-nil for functions/macros
    DocString  string
    UsageCount int                // incremented on each reference
}

// Signature describes the parameter list of a function or macro.
type Signature struct {
    Required []string
    Optional []string
    Rest     string              // empty if no &rest
    Keys     []string
}

// Reference represents a use of a symbol at a source location.
type Reference struct {
    Symbol   *Symbol
    Source   *token.Location
    Node     *lisp.LVal
}
```

#### C. Analyzer API

```go
// Config controls which checks are enabled and provides external context.
type Config struct {
    // Checks enables specific semantic checks. Nil means all checks.
    Checks []string

    // ExtraGlobals provides additional symbol names that should be
    // considered defined (e.g. from workspace index, loaded libraries).
    // This is how the LSP injects cross-file symbols.
    ExtraGlobals []ExternalSymbol
}

// ExternalSymbol describes a symbol defined outside the current file.
type ExternalSymbol struct {
    Name      string
    Kind      SymbolKind
    Signature *Signature         // nil if unknown
    Source    *token.Location    // nil if unknown
}

// Result holds the complete analysis output for a single file.
type Result struct {
    // Diagnostics are the semantic problems found.
    Diagnostics []Diagnostic

    // Scopes is the root scope tree for the file.
    RootScope *Scope

    // Symbols are all symbol definitions found.
    Symbols []*Symbol

    // References are all resolved symbol references.
    References []*Reference

    // Unresolved are symbol references that could not be resolved.
    Unresolved []*Reference
}

// Diagnostic represents a semantic problem found during analysis.
type Diagnostic struct {
    Source   *token.Location
    EndSource *token.Location    // nil if not available
    Message  string
    Severity Severity           // Error, Warning, Info
    Check    string             // e.g. "undefined-symbol", "unused-variable"
}

type Severity int
const (
    Error   Severity = iota
    Warning
    Info
)

// Analyze performs semantic analysis on parsed expressions from a single file.
func Analyze(exprs []*lisp.LVal, cfg *Config) *Result { ... }
```

#### D. Semantic checks to implement

| Check | Severity | Description |
|-------|----------|-------------|
| `undefined-symbol` | Error | Symbol not found in any scope or ExtraGlobals |
| `unused-variable` | Warning | `let`/`let*` binding, parameter, or `flet` function never referenced |
| `unused-function` | Warning | `defun` at file scope never referenced (excludes exported symbols) |
| `shadowing` | Info | Inner scope symbol hides outer scope symbol |
| `user-arity` | Error | Call to user-defined function with wrong argument count |

**Conventions:**
- Symbols starting with `_` are exempt from unused checks.
- Global scope symbols are exempt from unused checks (may be used externally).
- Builtins, special ops, and macros are pre-populated in the global scope
  via a registry built from `lisp.DefaultBuiltins()`, `lisp.DefaultSpecialOps()`,
  `lisp.DefaultMacros()`.

#### E. Building the builtin registry

```go
// builtins.go

// BuiltinRegistry returns symbols for all ELPS builtins, special ops,
// and macros with their signatures.
func BuiltinRegistry() []*Symbol {
    var syms []*Symbol
    for _, b := range lisp.DefaultBuiltins() {
        syms = append(syms, &Symbol{
            Name:      b.Name(),
            Kind:      SymFunction,
            Signature: parseFormals(b.Formals()),
        })
    }
    // ... same for DefaultSpecialOps() and DefaultMacros()
    return syms
}

// parseFormals extracts a Signature from a formals LVal.
func parseFormals(formals *lisp.LVal) *Signature { ... }
```

This reuses existing infrastructure — `lisp.DefaultBuiltins()` already
returns `[]LBuiltinDef` with `Name()` and `Formals()` methods.

#### F. Scope construction algorithm

Walk the AST depth-first. On entry to scope-creating forms:

1. **`defun`/`defmacro`**: Create `ScopeFunction`. Add parameters as
   `SymParameter`. Walk body in new scope.
2. **`lambda`**: Create `ScopeLambda`. Add parameters. Walk body.
3. **`let`**: Create `ScopeLet`. For each binding `(name value)`, analyze
   `value` in the parent scope, then add `name` as `SymVariable` in the
   new scope.
4. **`let*`**: Like `let` but each binding is visible to subsequent bindings.
5. **`flet`/`labels`**: Create `ScopeFlet`. Add local function bindings.
6. **`set`**: Add `SymVariable` to current scope (or nearest `ScopeGlobal`).
7. **Symbol reference**: `Scope.Lookup()` through chain. Increment `UsageCount`.

#### G. Interaction with existing lint package

The `analysis` package is independent of `lint/`. They can be composed:

```go
// In the LSP or a future `elps check` command:
exprs, _ := parser.Parse(source)
semanticResult := analysis.Analyze(exprs, &analysis.Config{...})
lintResult, _ := linter.LintFile(source, filename)
// Merge diagnostics from both
```

Later (P4), we can optionally pass `analysis.Result` into lint analyzers for
richer checks. But that's additive — the initial version keeps them separate.

#### Files touched
- `analysis/` — new package (6-7 files)
- No changes to existing packages

#### Tests
- Unit tests for scope construction with various nesting patterns.
- Unit tests for each semantic check with positive and negative cases.
- Integration test: analyze every `.lisp` file in the repo, verify no panics.

---

## P4: Multi-File Linting Support

**Impact:** MEDIUM — enables the linter to accept cross-file context,
reducing false positives for undefined symbols in multi-file projects.

**Problem:** `lint.LintFile()` analyzes one file at a time with no knowledge
of symbols defined elsewhere. The `analysis` package (P3) supports
`ExtraGlobals` but there's no convenient way to build that list from a
workspace.

### Proposed Changes

#### A. Add workspace scanning utility

```go
// analysis/workspace.go

// ScanWorkspace discovers all .lisp files under root and extracts
// exported symbols from each. Returns a slice of ExternalSymbol
// suitable for passing to Config.ExtraGlobals.
//
// This is a simple, non-incremental scan. The LSP can use its own
// incremental indexer instead.
func ScanWorkspace(root string, reader lisp.Reader) ([]ExternalSymbol, error) { ... }
```

Implementation:
1. Walk directory tree, find `.lisp` files.
2. Parse each file.
3. Extract top-level `defun`, `defmacro`, `set`, `export` forms.
4. Return symbols with their signatures and source locations.

#### B. Pass semantic context to lint Pass (optional enhancement)

```go
// lint/lint.go

type Pass struct {
    Analyzer    *Analyzer
    Filename    string
    Exprs       []*lisp.LVal
    Semantics   *analysis.Result  // NEW: nil if no semantic analysis available
    diagnostics []Diagnostic
}
```

If `Semantics` is non-nil, analyzers can use scope/symbol information for
more precise checks. If nil, they fall back to current behavior. This is
fully backwards compatible — existing analyzers ignore `Semantics`.

#### C. New `Linter` method

```go
// LintFileWithContext is like LintFile but provides semantic analysis
// results to analyzers that can use them.
func (l *Linter) LintFileWithContext(
    source []byte,
    filename string,
    semantics *analysis.Result,
) ([]Diagnostic, error) { ... }
```

`LintFile()` continues to work exactly as before (calls `LintFileWithContext`
with nil semantics internally).

#### Files touched
- `analysis/workspace.go` — new file
- `lint/lint.go` — add `Semantics` field to `Pass`, add `LintFileWithContext`

#### Tests
- Workspace scanning test with a temp directory of `.lisp` files.
- Verify `LintFile()` unchanged behavior.
- Verify `LintFileWithContext()` with and without semantics.

---

## P5: CLI Integration (elps check)

**Impact:** MEDIUM — gives CLI users access to semantic analysis.

**Problem:** `elps lint` runs structural checks only. There's no CLI command
for semantic analysis (undefined symbols, unused variables, arity errors).

### Proposed Changes

#### A. New `cmd/check.go`

```go
// cmd/check.go

var checkCmd = &cobra.Command{
    Use:   "check [flags] [files...]",
    Short: "Run semantic analysis on ELPS source files",
    Long:  `Check performs scope-aware semantic analysis...`,
}

// Flags:
//   --json          Output diagnostics as JSON
//   --checks        Comma-separated list of checks to run
//   --exclude       Glob patterns for files to skip
//   --workspace     Workspace root for cross-file resolution (default: ".")
//   --no-workspace  Disable workspace scanning
```

#### B. Workflow

1. Discover files via `expandArgs()` (reuse from lint/fmt).
2. If workspace mode: `analysis.ScanWorkspace()` to build `ExtraGlobals`.
3. For each file: parse → `analysis.Analyze()` → collect diagnostics.
4. Optionally: also run `lint.LintFile()` and merge results.
5. Output via `diagnostic.Renderer` or JSON.

#### C. Exit codes

- 0: No problems.
- 1: Problems found.
- 2: Bad invocation.

(Same convention as `elps lint`.)

#### Files touched
- `cmd/check.go` — new file
- `cmd/root.go` — register `checkCmd`

#### Tests
- Integration test with sample files containing undefined symbols, unused vars.

---

## P6: Structured Parse Errors

**Impact:** MEDIUM — enables the LSP to map parse errors to `protocol.Diagnostic`
without string parsing.

**Problem:** Parse errors are `*token.LocationError` with an `Err` field (Go
error) and a `Source` field. The LSP must string-parse the error message to
extract severity, error code, and suggestions.

### Proposed Changes

#### A. Add error code enum

```go
// parser/token/errors.go (new file)

// ErrorCode identifies the kind of parse error for programmatic handling.
type ErrorCode string

const (
    ErrUnexpectedToken  ErrorCode = "unexpected-token"
    ErrUnclosedParen    ErrorCode = "unclosed-paren"
    ErrUnclosedBracket  ErrorCode = "unclosed-bracket"
    ErrUnclosedString   ErrorCode = "unclosed-string"
    ErrBracketMismatch  ErrorCode = "bracket-mismatch"
    ErrInvalidEscape    ErrorCode = "invalid-escape"
    ErrInvalidNumber    ErrorCode = "invalid-number"
    // ... extend as needed
)
```

#### B. Extend LocationError

```go
type LocationError struct {
    Err    error
    Source *Location
    Code   ErrorCode  // NEW: empty string for unclassified errors
}
```

#### C. Backwards compatibility

- `LocationError.Error()` output unchanged.
- `Code` defaults to empty string — existing error handling unaffected.
- Classified errors are populated incrementally (no need to classify all
  errors at once).
- LSP can switch on `Code` when non-empty, fall back to string parsing
  when empty.

#### Files touched
- `parser/token/errors.go` — new file with `ErrorCode` constants
- `parser/token/token.go` — add `Code` field to `LocationError`
- `parser/rdparser/parser.go` — set `Code` on errors where classification
  is straightforward

#### Tests
- Existing parse error tests unchanged (Code field ignored).
- New tests verify specific error codes for known error patterns.

---

## P7: Function Metadata Helper

**Impact:** LOW-MEDIUM — convenience for LSP and tooling, not a core
requirement.

**Problem:** The LSP manually parses `defun` bodies to extract parameter
names, docstrings, and function classification. This is ~40 lines of code
that every consumer must duplicate.

### Proposed Changes

#### A. Add helper function (not AST change)

Rather than adding a `FnMeta` field to `LVal` (which would complicate the
AST and have unclear ownership during parsing), provide a helper function:

```go
// lisp/inspect.go (new file)

// FunctionInfo extracts metadata from a defun/defmacro/lambda AST node.
// Returns nil if the node is not a recognized function-defining form.
type FunctionInfo struct {
    Name      string      // empty for lambda
    Kind      string      // "defun", "defmacro", "lambda"
    Params    []ParamInfo
    DocString string
    Source    *token.Location
}

type ParamInfo struct {
    Name     string
    Kind     ParamKind   // Required, Optional, Rest, Key
}

type ParamKind int
const (
    ParamRequired ParamKind = iota
    ParamOptional
    ParamRest
    ParamKey
)

// InspectFunction analyzes a defun/defmacro/lambda s-expression and
// returns structured metadata. Returns nil if the node doesn't match.
func InspectFunction(node *LVal) *FunctionInfo { ... }
```

This is a pure read-only helper — no AST mutation, no parser changes.

#### B. Reuse in analysis package

The `analysis` package (P3) uses `InspectFunction` internally for building
`Signature` objects, avoiding duplication.

#### Files touched
- `lisp/inspect.go` — new file
- `lisp/inspect_test.go` — new file

#### Tests
- Test with `defun`, `defmacro`, `lambda` AST nodes.
- Test with various parameter patterns (`&optional`, `&rest`, `&key`).
- Test with and without docstrings.
- Test returns nil for non-function nodes.

---

## Deferred / Out of Scope

The following items from the gap analysis documents are intentionally
deferred:

### Custom Macro Configuration
The LSP supports `.elps-ls.yaml` for configuring custom macro scoping rules
(e.g. `def-app-route`). This is LSP-specific configuration and should
remain in the LSP codebase. The `analysis.Config.ExtraGlobals` mechanism
provides the necessary extension point.

### `elps rename` Refactoring Tool
Depends on P3 (semantic analyzer) being stable and well-tested. A future
effort once the foundation is solid.

### Incremental Analysis / File Watching
These are LSP-specific concerns (debouncing, document management, incremental
re-parsing). The `analysis` package provides a stateless, single-invocation
API. The LSP manages incrementality on top.

### Native LSP Protocol
Building a Language Server Protocol implementation into ELPS core is a
separate, larger effort. The changes in this document provide the foundation
that makes a native LSP feasible in the future.

### LVal.Spliced / LVal.Native
Confirmed no action needed — these are runtime concepts irrelevant to static
analysis.

---

## Migration Guide for LSP Team

### After P0 (Format-Preserving Parser)

**Before:**
```go
import "github.com/luthersystems/elps/parser/rdparser"

s := token.NewScanner(name, reader)
p := rdparser.NewFormatting(s)
exprs, err := p.ParseProgram()
```

**After:**
```go
import "github.com/luthersystems/elps/parser"

r := parser.NewReader(parser.WithFormatPreserving())
exprs, err := r.Read(name, reader)
// exprs[i].Meta is populated with SourceMeta
```

### After P1 (End Positions)

**Before (patch_locations.go):**
```go
// 261 lines of manual range calculation
type patcher struct {
    ranges  map[*lisp.LVal]protocol.Range
    offsets map[*lisp.LVal][2]int
}
```

**After:**
```go
// Direct access via Source field
startLine := node.Source.Line
startCol  := node.Source.Col
endLine   := node.Source.EndLine
endCol    := node.Source.EndCol
// Convert to LSP protocol.Range directly
```

The entire `patch_locations.go` file can be deleted.

### After P3 (Semantic Analyzer)

**Before (LSP internal/analysis/):**
```go
// ~1500 lines of custom scope/symbol/analysis code
result := analyzer.Analyze(ctx, doc.AST, doc.Ranges, extraGlobals)
```

**After:**
```go
import "github.com/luthersystems/elps/analysis"

result := analysis.Analyze(exprs, &analysis.Config{
    ExtraGlobals: workspaceSymbols,  // from LSP indexer
})
// result.Diagnostics, result.Symbols, result.References, etc.
```

The LSP retains its workspace indexer and incremental analysis layer but
delegates core semantic analysis to the shared package.

### After P6 (Structured Parse Errors)

**Before:**
```go
// String parsing to extract error details
if strings.Contains(err.Error(), "unclosed") { ... }
```

**After:**
```go
if locErr, ok := err.(*token.LocationError); ok {
    switch locErr.Code {
    case token.ErrUnclosedParen:
        // Handle specifically
    }
}
```

---

## Implementation Order

```
P0 (parser options)  ──┐
P1 (end positions)   ──┼──> P3 (semantic analyzer) ──> P4 (multi-file) ──> P5 (elps check)
P2 (lint severity)   ──┘
P6 (parse errors)    ──────> (independent, can ship anytime)
P7 (function info)   ──────> (independent, feeds into P3)
```

**Suggested PR sequence:**
1. P7 — `lisp/inspect.go` helper (small, self-contained)
2. P0 — parser `WithFormatPreserving()` option
3. P1 — end positions in `token.Location`
4. P2 — severity in `lint.Diagnostic`
5. P6 — structured parse errors
6. P3 — `analysis/` package (largest change, depends on P7)
7. P4 — multi-file support
8. P5 — `elps check` CLI command

---

## Open Questions

1. **Should `analysis/` live at top level or under `lisp/`?** Top level
   avoids import cycles (`analysis` imports `lisp` and `parser/token`).
   Under `lisp/` would need careful dependency management.

2. **Should the semantic analyzer reuse `lint.Diagnostic` or define its own?**
   This document proposes a separate `analysis.Diagnostic` type to keep the
   packages decoupled. A shared type could be introduced later if warranted.

3. **Workspace scanning in P4 — should it respect `.gitignore`?** The LSP
   likely has its own file discovery. The CLI utility could use a simple
   walk or respect excludes via the existing `--exclude` flag pattern.

4. **Should P1 end positions be opt-in (like format mode) or always-on?**
   This document proposes always-on since the cost is minimal (24 bytes per
   Location) and the benefit is universal. But if benchmarks show measurable
   impact, it could be gated behind a parser option.
