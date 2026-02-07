# Suppressing Lint Diagnostics

The ELPS linter supports inline suppression of diagnostics using `; nolint`
comments. This allows you to silence specific findings on lines where the
linter's warning does not apply or where the code is intentional.

## Syntax

Add a trailing comment on the same line as the flagged expression:

```lisp
; Suppress ALL checks on this line:
(set 'x 1) ; nolint

; Suppress a specific check:
(set 'x 1) ; nolint:set-usage

; Suppress multiple specific checks:
(if true 1) ; nolint:if-arity,builtin-arity
```

### Rules

- The `; nolint` directive must be a **trailing comment** (on the same line as
  the expression). Leading comments on the line above do **not** suppress.
- `; nolint` alone suppresses every check on that line.
- `; nolint:name` suppresses only the named check.
- `; nolint:name1,name2` suppresses multiple named checks (comma-separated).
- Check names must match exactly (case-sensitive). Use `elps lint --list` to
  see available check names.

### Examples

```lisp
;; Intentional repeated set — we want set, not set!
(set 'counter 0)
(set 'counter (+ counter 1)) ; nolint:set-usage

;; Two-branch if is intentional here (nil else is desired)
(if (ready?) (run)) ; nolint:if-arity

;; Suppress everything on this line
(set 'x (car)) ; nolint
```

### What does NOT work

```lisp
; nolint:set-usage          <-- leading comment: does NOT suppress the next line
(set 'counter (+ counter 1))
```

## Available checks

Run `elps lint --list` to see all available check names:

```
builtin-arity
cond-structure
defun-structure
if-arity
in-package-toplevel
let-bindings
set-usage
```

## When to use nolint

Use `; nolint` sparingly. It is best reserved for cases where:

- The linter flags code that is intentionally written a certain way.
- A false positive cannot be avoided by restructuring the code.
- You want to acknowledge the finding but keep the current code.

Avoid blanket `; nolint` when a specific `; nolint:check-name` would suffice,
so that other checks remain active on the same line.
