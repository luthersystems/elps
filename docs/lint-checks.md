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
