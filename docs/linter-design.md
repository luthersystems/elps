# ELPS Linter Design Document

> Issue: [#47 — linter reference implementation](https://github.com/luthersystems/elps/issues/47)
>
> Related: [#46 — documentation parsed from code](https://github.com/luthersystems/elps/issues/46)

## Goals

Provide `elps lint` — a static analysis tool modeled after `go vet`. Like `go vet`,
the linter should:

- Report **likely mistakes**, not style preferences (formatting is `elps fmt`'s job)
- Have a **very low false-positive rate** — everything it reports is worth examining
- Be **composable** — each check is an independent, self-contained analyzer
- Produce **clear diagnostics** in the standard `file:line: message` format
- Serve as a **reference implementation** that embedders can extend with
  domain-specific checks

## Architecture Overview

### The ELPS AST

The entire AST is built from a single type: `LVal` (`lisp/lisp.go`). Every value
in ELPS — integers, strings, symbols, lists, functions, errors — is an `LVal`.

```
LVal
├── Type      LType          // discriminant: LInt, LString, LSymbol, LSExpr, LFun, ...
├── Str       string         // symbol name or string content
├── Int       int            // integer value
├── Float     float64        // float value
├── Cells     []*LVal        // children (list elements, function body, etc.)
├── Quoted    bool           // true for [bracket] lists (QExpr)
├── FunType   LFunType       // LFunNone, LFunMacro, LFunSpecialOp
├── Native    interface{}    // LFunData for functions, MapData for sorted-maps, etc.
├── Source    *token.Location // file, line, col, pos
├── Meta      *SourceMeta    // format-preserving metadata (nil in normal parse mode)
└── Spliced   bool           // splice marker for unquote-splicing
```

There are 16 LType constants, but for static analysis the most important are:

| Type | Represents | Key fields |
|------|-----------|------------|
| `LSymbol` | Variable/function name | `Str` = name |
| `LSExpr` | S-expression (list/call) | `Cells` = elements, `Quoted` = data list |
| `LInt`, `LFloat` | Numeric literals | `Int`/`Float` |
| `LString` | String literal | `Str` |
| `LFun` | Function value | `Cells[0]` = formals, `Cells[1:]` = body, `Native` = `*LFunData` |
| `LQuote` | Quote wrapper | `Cells[0]` = quoted value |
| `LError` | Error value | `Str` = condition, `Cells` = data |

**Source location** is available on every `LVal` via `Source *token.Location` which
carries `File`, `Line`, and optionally `Col`.

**SourceMeta** (the `Meta` field) is **only populated in format-preserving parse mode**
(`rdparser.NewFormatting()`). It carries comments, bracket types, original literal
text, and newline/spacing info. This is nil in normal mode — zero cost.

### How the Parser Builds the AST

The recursive descent parser (`parser/rdparser/`) produces a flat slice of
top-level `*LVal` nodes:

```go
p := rdparser.New(scanner)          // normal mode
p := rdparser.NewFormatting(scanner) // format-preserving mode
exprs, err := p.ParseProgram()       // returns []*LVal
```

In normal mode, the AST is minimal — just structure and source locations. In
format-preserving mode, every node gets a `Meta *SourceMeta` with comments,
bracket types, spacing, and original text.

### How the Formatter Walks the AST

The formatter (`formatter/`) demonstrates the tree-walking pattern a linter
would use:

```
Format(source)
  → rdparser.NewFormatting(scanner)   // parse with metadata
  → printer.writeTopLevel(exprs)       // entry point
      → writeExpr(node, indent)        // recursive dispatch on node.Type
          → writeSExpr()               // for s-expressions: iterate Cells
          → writeAtom()                // for symbols, numbers, strings
          → writeQuote()               // for quote wrappers
```

The printer recurses through `Cells` depth-first. For each s-expression, it
inspects the head symbol to look up an indent rule, then processes each child.
This same pattern — dispatch on type, inspect head symbol, recurse through
children — is exactly what a linter needs.

### Builtins, Special Forms, and Macros

All builtin function signatures are statically known:

| Category | Count | Registration | Arity known? |
|----------|-------|-------------|--------------|
| Builtin functions | ~50 | `langBuiltins` array in `builtins.go` | Yes — `Formals()` |
| Special operators | ~23 | `langSpecialOps` in `op.go` | Yes — `Formals()` |
| Builtin macros | ~6 | `DefaultMacros()` in `macro.go` | Yes — `Formals()` |

Every `LBuiltinDef` has a `Formals() *LVal` method that returns the argument
specification as a quoted list. Argument types:

- **Required**: plain symbol (`"value"`)
- **Optional**: `&optional` marker followed by symbol
- **Variadic**: `&rest` marker followed by symbol (must be penultimate)
- **Keyword**: `&key` marker followed by symbol names

This means **arity checking for all builtins is trivially implementable** — the
linter can introspect the formals list to determine min/max argument counts.

## Linter Framework Design

Modeled after `go vet`'s `golang.org/x/tools/go/analysis` framework.

### Core Types

```go
package lint

// Analyzer defines a single lint check.
type Analyzer struct {
    Name string           // identifier, e.g. "set-usage"
    Doc  string           // first line is short description
    Run  func(*Pass) error
}

// Pass provides context to a running check.
type Pass struct {
    Analyzer *Analyzer
    Filename string
    Source   []byte           // raw source (for suggested fixes)
    Exprs    []*lisp.LVal    // parsed top-level expressions
    Report   func(Diagnostic) // emit a finding
}

// Diagnostic is a single reported problem.
type Diagnostic struct {
    Pos     token.Location  // file:line:col
    End     token.Location  // optional range end
    Message string
    Fix     *SuggestedFix   // optional auto-fix
}

// SuggestedFix describes a machine-applicable fix.
type SuggestedFix struct {
    Message string
    Edits   []TextEdit
}

// TextEdit is a replacement in source text.
type TextEdit struct {
    Pos     int    // byte offset start
    End     int    // byte offset end
    NewText string
}
```

### Key Design Decisions

1. **Analyzers don't control output format.** They call `pass.Report()`. The
   driver handles stderr, JSON, etc.

2. **Analyzers don't assign severity.** The driver decides what's a warning vs
   error (via configuration or `--error` flags). This avoids bikeshedding
   within individual checks.

3. **Each analyzer is a package-level var** in its own file under `lint/`.
   Self-contained, independently testable.

4. **The driver parses each file once** using `rdparser.NewFormatting()` (reusing
   formatter infrastructure to get comments and metadata). All analyzers receive
   the same AST.

5. **No AST modifications required.** The linter is read-only over the existing
   `LVal` tree. The `Meta` field provides comments when available. Source
   locations come from the existing `Source` field.

### Driver (CLI Integration)

```
elps lint [--checks=...] [--json] [--fix] [files...]

Exit codes (matching go vet):
  0 — no problems found
  1 — problems were reported
  2 — bad invocation
```

The `cmd/lint.go` command follows the same pattern as `cmd/fmt.go`:

1. Parse flags and file arguments
2. For each file: parse with `rdparser.NewFormatting()`
3. Create a `Pass` for each enabled analyzer
4. Run analyzers, collecting diagnostics
5. Post-process: sort by file:line, deduplicate
6. Output in requested format
7. If `--fix`: apply `SuggestedFix` edits and rewrite files

### Suppression

Like `golangci-lint`'s `//nolint`, support suppression comments:

```lisp
(set x 42) ; nolint:set-usage
```

The driver strips diagnostics whose source line contains `; nolint:analyzer-name`
or `; nolint` (suppress all). This is handled in post-processing, not by
individual analyzers.

### AST Walking Helpers

Provide a small set of helpers that analyzers can use, so each check doesn't
reimplement tree traversal:

```go
// Walk calls fn for every node in the tree, depth-first.
func Walk(exprs []*lisp.LVal, fn func(node *lisp.LVal, parent *lisp.LVal))

// WalkSExprs calls fn for every s-expression (function call / special form).
func WalkSExprs(exprs []*lisp.LVal, fn func(sexpr *lisp.LVal))

// HeadSymbol returns the head symbol name of an s-expression, or "".
func HeadSymbol(sexpr *lisp.LVal) string

// IsTopLevel returns true if the node is a direct child of the program root.
func IsTopLevel(node *lisp.LVal, parent *lisp.LVal) bool
```

These are thin wrappers over the same recursion pattern the formatter uses.

## Proposed Checks

### Tier 1: Low-Hanging Fruit (Issue #47 scope)

These can be implemented with pure AST pattern-matching — no scope tracking,
no type inference, no evaluation.

#### 1. `set-usage` — Warn on repeated `set` to same symbol

From issue #47: detect when `set` is used to reassign a symbol that was
already bound by a prior `set` in the same file. The first `set` creating a
new binding is fine — ELPS has no `defvar`, so `set` is the standard way to
create top-level bindings. Subsequent mutations should use `set!`.

**Important:** `set` creates OR overwrites bindings. `set!` ONLY mutates
existing bindings and errors if not bound. Blindly replacing all `set` with
`set!` will break code where `set` is creating the initial binding.

```lisp
;; OK — first binding
(set 'x 42)

;; BAD — x already bound, use set! to signal mutation
(set 'x 99)

;; GOOD
(set! 'x 99)
```

**Implementation**: Walk s-expressions tracking seen symbol names per file.
If head symbol is `"set"` and the target symbol was already seen, report.

**False positive rate**: Very low. Only flags repeated `set` on the same
symbol, which is clearly a mutation that should use `set!`.

#### 2. `in-package-toplevel` — `in-package` only at top level

From issue #47: detect `in-package` used inside function bodies or nested
expressions, where it won't behave as expected.

```lisp
;; BAD
(defun foo ()
  (in-package "my-pkg")  ; has no effect outside load context
  ...)

;; GOOD (top-level only)
(in-package "my-pkg")
```

**Implementation**: Walk tree with depth tracking. Report any `in-package` call
where depth > 0 (not a direct child of the program root).

**False positive rate**: Very low. `in-package` inside a function body is almost
certainly a mistake.

#### 3. `if-arity` — `if` requires exactly 3 arguments

```lisp
;; BAD
(if condition then-branch)               ; missing else
(if condition then-branch else-1 else-2) ; too many

;; GOOD
(if condition then-branch else-branch)
```

**Implementation**: Count children of `if` s-expressions. Report if != 4
(head + 3 args).

**False positive rate**: Zero. The runtime will error too, but a linter catches
it before execution.

#### 4. `let-bindings` — Malformed `let`/`let*` binding lists

```lisp
;; BAD
(let (x 42) ...)           ; should be ((x 42))
(let ((x)) ...)            ; missing value
(let ((x 1 2)) ...)       ; too many values

;; GOOD
(let ((x 42) (y 0)) body)
```

**Implementation**: Check that first arg to `let`/`let*` is a list of 2-element
lists. Each binding must be `(symbol value)`.

**False positive rate**: Very low. Malformed bindings are always bugs.

#### 5. `defun-structure` — Malformed `defun`/`defmacro`

```lisp
;; BAD
(defun 42 (x) x)          ; name is not a symbol
(defun foo x x)            ; formals is not a list

;; GOOD
(defun foo (x) x)
```

**Implementation**: Check that second child is a symbol, third child is a list.

**False positive rate**: Zero. These are structural errors.

### Tier 2: Medium Effort

These require some scope tracking or knowledge of builtin signatures.

#### 6. `builtin-arity` — Wrong number of arguments to builtins

```lisp
;; BAD
(car)            ; needs 1 arg
(car a b)        ; too many args
(nth)            ; needs 2 args

;; GOOD
(car my-list)
(nth 0 my-list)
```

**Implementation**: Build a table of `{name → min_args, max_args}` from the
`langBuiltins` array's `Formals()` definitions. When an s-expression's head
matches a known builtin, check argument count.

**Complexity**: Need to parse formals to extract arity. Count symbols before
`&optional`/`&rest`/`&key` for min; check for `&rest` for max=unlimited.

**False positive rate**: Low. Only checks known builtins, and arity rules are
precise. Could false-positive if a user shadows a builtin name in a local
scope, but this is rare and arguably worth flagging anyway.

#### 7. `set-bang-scope` — `set!` with no matching binding in scope

From issue #47: using `set!` when there is no matching variable in scope.

```lisp
;; BAD — no binding for `x` exists
(set! x 42)

;; GOOD — `x` is in scope from let
(let ((x 0))
  (set! x 42))
```

**Implementation**: Track bindings introduced by `let`, `let*`, `defun` formals,
`lambda` formals, `flet`, `labels`. When encountering `set!`, check that the
target symbol exists in the current scope chain.

**Complexity**: Requires a scope stack (push on `let`/`lambda`/`defun`, pop on
exit). Not difficult, but more state than tier 1 checks.

**False positive rate**: Low-medium. Top-level `defun` forms can create
package-level bindings that aren't tracked in a simple scope walk.
Could limit to warning only inside function bodies.

#### 8. `unused-binding` — Unused `let` bindings

```lisp
;; WARN
(let ((x 42))  ; x is never used
  (+ 1 2))
```

**Implementation**: Track symbols bound in `let`/`let*`. Walk the body to see
if each symbol appears. Report unused ones.

**Complexity**: Moderate. Need to handle shadowing correctly. Macros complicate
this since they may reference bindings in non-obvious ways.

**False positive rate**: Medium. Macros and eval-based patterns can reference
bindings invisibly. Could restrict to `let` bindings only (not function formals)
to reduce noise.

### Tier 3: Higher Effort / Future Work

These require more sophisticated analysis or have higher false-positive risk.

#### 9. `unreachable` — Code after unconditional error

```lisp
(defun foo ()
  (error "fail")
  (+ 1 2))        ; unreachable
```

Requires knowing which forms unconditionally diverge (error, signal-error).
Moderate complexity, moderate false-positive risk.

#### 10. `missing-docstring` — Functions without docstrings

From issue #46. The linter could warn when `defun`/`defmacro` lacks a
docstring as the first body expression.

```lisp
;; WARN
(defun foo (x) (+ x 1))

;; GOOD
(defun foo (x) "Add one to x." (+ x 1))
```

Low implementation complexity but **high noise** on existing codebases. Best
as an opt-in check.

#### 11. `cond-exhaustiveness` — `cond` without catch-all

```lisp
;; WARN — no default branch
(cond
  ((= x 1) "one")
  ((= x 2) "two"))

;; GOOD
(cond
  ((= x 1) "one")
  ((= x 2) "two")
  (true "other"))
```

Check that the last `cond` clause has a `true` or `t` test. Low complexity,
but stylistic enough that it might be opt-in.

#### 12. `qualified-symbol` — Referencing unexported symbols

Check that `pkg:symbol` references only symbols in the package's `Externals`
list. Requires building a package export table from `export` calls.

## Feasibility Assessment

### What's Free (no AST changes)

| Capability | Source |
|-----------|--------|
| Source location (file, line) | `LVal.Source` — always populated |
| Node type discrimination | `LVal.Type` — always available |
| Symbol names | `LVal.Str` — always available |
| S-expression children | `LVal.Cells` — always available |
| Quoted/data distinction | `LVal.Quoted` — always available |
| Comment text and position | `LVal.Meta` — available in format-preserving mode |
| Function arity specs | `LBuiltinDef.Formals()` — introspectable |
| Special form identification | Head symbol matching against known list |

### What Would Need New Code (but no AST changes)

| Capability | Approach |
|-----------|----------|
| Scope tracking | Push/pop scope stack during walk |
| Builtin arity table | Generate from `langBuiltins` at init time |
| Package export tracking | Walk `export` calls, build table |
| Suppression comments | Post-process filter on `Meta.TrailingComment` |

### What Would Need AST Changes (out of scope)

| Capability | Why it's hard |
|-----------|--------------|
| Column numbers | `token.Location.Col` is not reliably populated |
| Type inference | No type annotations in the language |
| Macro expansion | Would need to evaluate macros, which is runtime |
| Cross-file analysis | Via `--workspace` flag and `analysis.ScanWorkspace()` |

## Implementation Plan

### Phase 1: Framework + Tier 1 checks

1. Create `lint/` package with `Analyzer`, `Pass`, `Diagnostic` types
2. Implement `Walk()` and `WalkSExprs()` helpers
3. Implement tier 1 checks: `set-usage`, `in-package-toplevel`, `if-arity`,
   `let-bindings`, `defun-structure`
4. Create `cmd/lint.go` (modeled on `cmd/fmt.go`)
5. Add `--json` output format
6. Add `; nolint` suppression

### Phase 2: Builtin arity + scope checks

1. Generate arity table from `langBuiltins`/`langSpecialOps`
2. Implement `builtin-arity` check
3. Implement scope tracker
4. Implement `set-bang-scope` check

### Phase 3: Extensibility

1. Document how embedders can define custom `Analyzer` values
2. Add `--fix` support with `SuggestedFix`
3. Add opt-in checks (`missing-docstring`, `cond-exhaustiveness`)

## Appendix: Comparison with go vet

| Aspect | go vet | elps lint |
|--------|--------|-----------|
| Unit of analysis | Go package | Single .lisp file (with workspace context) |
| AST source | `go/ast` + `go/types` | `*lisp.LVal` from `rdparser` |
| Type information | Full (from `go/types`) | None (untyped language) |
| Cross-file analysis | Yes (via facts) | Yes (via `--workspace` and `analysis/`) |
| Analyzer registration | `analysis.Analyzer` struct | `lint.Analyzer` struct |
| Diagnostic reporting | `pass.Reportf()` | `pass.Report(Diagnostic{})` |
| Output format | text, JSON | text, JSON |
| Exit codes | 0/1/2 | 0/1/2 |
| Suppression | N/A (go vet has no nolint) | `; nolint:check-name` |
| Auto-fix | `-fix` flag | `--fix` flag |

The main simplification vs go vet: ELPS has no type system, so there's no
equivalent of `go/types` analysis. This limits what can be checked but also
dramatically simplifies the framework. Each analyzer just walks `[]*LVal`
trees and pattern-matches.
