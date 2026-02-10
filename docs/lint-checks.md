# ELPS Lint Checks

`elps lint` is a static analysis tool for ELPS lisp source files, modeled
after `go vet`. It reports likely mistakes — not style issues (use `elps fmt`
for formatting).

## Usage

```
elps lint [flags] [files...]
```

| Flag | Description |
|------|-------------|
| `--json` | Output diagnostics as JSON (for editor/CI integration) |
| `--checks=name1,name2` | Run only the specified checks (default: all) |
| `--list` | List available checks and exit |

**Exit codes:**
- `0` — No problems found
- `1` — One or more problems were reported
- `2` — Bad invocation (invalid flags, unreadable files)

**Examples:**
```sh
elps lint file.lisp                       # Lint a single file
elps lint *.lisp                          # Lint multiple files
elps lint --json file.lisp                # JSON output for tooling
elps lint --checks=if-arity,set-usage f.lisp  # Run specific checks only
cat file.lisp | elps lint                 # Lint from stdin
```

## Suppression

To suppress a specific check on a line, add a trailing comment:

```lisp
(set x 42) ; nolint:set-usage
```

To suppress all checks on a line:

```lisp
(set x 42) ; nolint
```

## Checks

### `set-usage`

**Warns when `set` is used to reassign an already-bound symbol.**

The first `set` creating a new binding is fine — ELPS has no `defvar`, so
`set` is the standard way to create top-level bindings. However, subsequent
`set` calls on the same symbol should use `set!` to clearly signal mutation
intent.

**Important:** `set` creates OR overwrites bindings. `set!` ONLY mutates
existing bindings and errors if the symbol is not already bound. You cannot
replace all `set` with `set!` — the first binding must use `set`.

```lisp
;; OK — first binding of x
(set 'x 42)

;; BAD — x is already bound, use set! to signal mutation
(set 'x 99)

;; GOOD — clearly signals mutation intent
(set! 'x 99)
```

### `in-package-toplevel`

**Warns when `in-package` is used inside nested expressions.**

`in-package` only has meaningful effect at the top level of a file (or during
`load`). Using it inside a `defun`, `let`, `lambda`, or other nested form is
almost certainly a mistake — it won't change the package context the way you
expect.

```lisp
;; BAD
(defun setup ()
  (in-package "my-pkg")  ; has no useful effect here
  ...)

;; GOOD
(in-package "my-pkg")
(defun setup () ...)
```

### `if-arity`

**Checks that `if` has exactly 3 arguments: condition, then-branch,
else-branch.**

A missing else branch is a common source of subtle nil-return bugs. Extra
arguments after the else branch are silently ignored by the parser but
indicate a structural error.

```lisp
;; BAD
(if (= x 1) "yes")               ; missing else branch
(if (= x 1) "yes" "no" "extra")  ; too many arguments

;; GOOD
(if (= x 1) "yes" "no")
```

### `let-bindings`

**Checks for malformed `let`/`let*` binding lists.**

The first argument to `let` or `let*` must be a list of `(symbol value)`
pairs. The most common mistake is forgetting the outer parentheses, which
causes unexpected nil bindings.

```lisp
;; BAD — binds x to nil and 42 to nil
(let (x 42) ...)

;; BAD — binding has wrong number of elements
(let ((x 1 2)) ...)
(let ((x)) ...)

;; GOOD
(let ((x 42)) ...)
(let ((x 1) (y 2)) ...)
```

### `defun-structure`

**Checks for malformed `defun`/`defmacro` definitions.**

A `defun` requires a symbol name, a formals list (parenthesized), and at
least one body expression.

```lisp
;; BAD
(defun 42 (x) x)     ; name is not a symbol
(defun foo x x)       ; formals is not a list
(defun foo)           ; missing formals and body

;; GOOD
(defun foo (x) (+ x 1))
(defun foo (x) "Docstring." (+ x 1))
```

### `cond-structure`

**Checks for malformed `cond` clauses.**

Each `cond` clause must be a non-empty list `(test body...)`. The `else`
clause, if present, must be the last clause. Common mistakes include bare
values instead of lists, empty clauses, or misplaced `else`.

```lisp
;; BAD — clause is not a list
(cond true "yes")

;; BAD — empty clause
(cond ())

;; BAD — else not last
(cond
  (else "default")
  ((= x 1) "one"))

;; GOOD
(cond
  ((= x 1) "one")
  ((= x 2) "two")
  (else "other"))
```

### `builtin-arity`

**Checks argument counts for calls to known builtin functions and special
forms.**

ELPS builtin functions have well-defined argument signatures. This check
catches calls with too few or too many arguments before runtime. It covers
all builtins (`car`, `cdr`, `cons`, `=`, `not`, etc.), special operators
(`set!`, `lambda`, `quote`, etc.), and macros (`defun`, `defmacro`, etc.).

```lisp
;; BAD
(car)              ; requires 1 argument
(car a b)          ; accepts at most 1
(cons 1)           ; requires 2
(not x y z)        ; accepts at most 1
(gensym "x")       ; accepts 0

;; GOOD
(car my-list)
(cons 1 my-list)
(not (= x 1))
(gensym)
```

**Note:** User-defined functions that shadow builtin names (via `defun` or
`defmacro`) are automatically detected and excluded from arity checking.
Formals lists and threading macro (`thread-first`, `thread-last`) children
are also excluded, since their static argument count differs from the
runtime count after macro expansion.

### `quote-call`

**Warns when forms like `set` and `defconst` are called with an unquoted
symbol name.**

`set` expects a quoted symbol as its first argument. Passing a bare symbol
causes it to be evaluated, which is usually not intended.

```lisp
;; BAD — x is evaluated, not used as a name
(set x 42)

;; GOOD
(set 'x 42)
```

### `cond-missing-else`

**Suggests adding a default clause to `cond` expressions.** (Severity: info)

A `cond` without an `else` or `true` default clause returns nil when no
branch matches. While sometimes intentional, this is often an oversight.

```lisp
;; INFO — no default branch
(cond
  ((= x 1) "one")
  ((= x 2) "two"))

;; GOOD
(cond
  ((= x 1) "one")
  ((= x 2) "two")
  (else "other"))
```

### `rethrow-context`

**Flags `(rethrow)` calls outside a `handler-bind` handler.**

`rethrow` re-signals the currently active error and only works inside a
`handler-bind` handler. Using it outside causes a runtime error.

```lisp
;; BAD — not inside handler-bind
(defun my-handler (c &rest args)
  (rethrow))

;; GOOD
(handler-bind ((condition (lambda (c &rest args) (rethrow))))
  (error 'test "data"))
```

### `unnecessary-progn`

**Flags redundant `progn` wrappers.** (Severity: info)

Forms like `defun`, `let`, `lambda`, and `handler-bind` already accept
multiple body expressions, so wrapping them in `progn` is unnecessary.

```lisp
;; BAD — progn is redundant
(defun foo (x) (progn (print x) (+ x 1)))

;; GOOD
(defun foo (x) (print x) (+ x 1))
```

### `undefined-symbol`

**Reports symbols that cannot be resolved in any enclosing scope.**

Requires semantic analysis (`--workspace` flag). Keywords, qualified
symbols (e.g., `math:floor`), and builtins are excluded.

```lisp
;; ERROR — unknown-fn is not defined anywhere
(unknown-fn 1 2)

;; OK — builtins, keywords, and qualified names are recognized
(+ 1 2)
(list :key :value)
(math:floor 1.5)
```

**Known limitation — macros that delay evaluation:** The semantic analyzer
operates on the unexpanded AST. Macros that capture an expression in a
thunk (e.g., `stream-cons` wrapping its second argument in `delay`) create
references that are not evaluated until after surrounding bindings exist.
The analyzer cannot see through macro expansions, so self-referential
bindings that rely on delayed evaluation will be flagged as undefined:

```lisp
;; False positive: guesses appears undefined in its own init-form,
;; but stream-cons delays evaluation via (delay ...) so this works
;; at runtime.
(let ([guesses (stream-cons 1.0
                            (stream-map f guesses))]) ; nolint:undefined-symbol
  guesses)
```

Use `; nolint:undefined-symbol` to suppress these cases.

### `unused-variable`

**Warns about variables that are defined but never referenced.**

Requires semantic analysis (`--workspace` flag). Top-level `set` bindings
are excluded (they may be used by other files). Prefix unused variables
with `_` to silence the warning.

```lisp
;; WARNING — x is never used
(let ((x 1)) (+ 1 2))

;; GOOD — underscore prefix signals intent
(let ((_x 1)) (+ 1 2))
```

### `unused-function`

**Warns about local functions that are defined but never called or
exported.**

Requires semantic analysis (`--workspace` flag). Prefix unused functions
with `_` to silence the warning, or `export` them.

```lisp
;; WARNING — helper is never called
(defun helper () 42)

;; GOOD — exported functions are not flagged
(defun helper () 42)
(export 'helper)
```

### `shadowing`

**Reports local bindings that shadow an outer binding.** (Severity: info)

Requires semantic analysis. Flags parameters, `let` bindings, and local
functions (via `labels`/`flet`) that reuse a name from an enclosing scope
or the global scope. Top-level `defun` redefinitions are excluded (they
are intentional overrides, not shadowing).

```lisp
;; INFO — parameter car shadows the builtin
(defun foo (car) (+ car 1))

;; INFO — local x shadows the parameter x
(defun foo (x) (let ((x 2)) (+ x 1)))

;; OK — top-level defun overriding a builtin is not flagged
(defun map (f l) (cons (f (car l)) ()))
```

### `user-arity`

**Checks argument counts for calls to user-defined functions.**

Requires semantic analysis. Inspects `defun` formals to compute minimum
and maximum arity, then flags call sites with wrong argument counts.
Functions with `&rest` are variadic (no maximum). Threading macro children
(`thread-first`, `thread-last`) are excluded.

```lisp
;; ERROR — add requires 2 arguments
(defun add (a b) (+ a b))
(add 1)        ; too few
(add 1 2 3)    ; too many
```

### `unused-nolint`

**Warns about `; nolint` directives that do not suppress any diagnostic.**

A `; nolint:foo` comment that doesn't actually suppress any finding is dead
code — it may reference a misspelled analyzer name, or the underlying issue
may have been fixed. This check also detects unknown analyzer names.

```lisp
;; WARNING — no set-usage finding on this line
(+ 1 2) ; nolint:set-usage

;; WARNING — "nonexistent" is not a known analyzer
(+ 1 2) ; nolint:nonexistent

;; Self-suppression: suppress unused-nolint itself
(+ 1 2) ; nolint:unused-nolint
```

## Extending the Linter

The linter is designed to be extensible. Embedders can define custom
analyzers:

```go
var MyCheck = &lint.Analyzer{
    Name: "my-check",
    Doc:  "Description of what this check does.",
    Run: func(pass *lint.Pass) error {
        lint.WalkSExprs(pass.Exprs, func(sexpr *lisp.LVal, depth int) {
            if lint.HeadSymbol(sexpr) == "dangerous-func" {
                pass.Reportf(sexpr.Source, "avoid dangerous-func")
            }
        })
        return nil
    },
}

// Register with the linter:
l := &lint.Linter{
    Analyzers: append(lint.DefaultAnalyzers(), MyCheck),
}
```
