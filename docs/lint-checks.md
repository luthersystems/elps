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

`shadowing` is also a family name: `; nolint:shadowing` suppresses both
[`shadowing`](#shadowing) (local bindings) and
[`builtin-shadowing`](#builtin-shadowing) (top-level bindings).

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

The first argument to `let` or `let*` must be a list of `(name value)`
pairs. A name is a symbol or a list pattern, such as `(list a b)` or
`[s s:string]` (see "Binding patterns" in the language reference). The most
common mistake is forgetting the outer parentheses, which causes unexpected
nil bindings.

A pattern that `let` always refuses is reported with the runtime's message:
a malformed pattern such as `(f x)`, a quoted list that is not
`[name type]`, a pattern that binds a name twice, and a pattern with a
`:when` guard. A pattern in a macro template that holds an `unquote` is not
checked, because its shape is known only after expansion.

A literal name such as `42` is reported. `let` accepts it as a pattern that
binds nothing and only tests the value, but that is more often a mistake.
Use `match` to test a value against a literal.

```lisp
;; BAD — binds x to nil and 42 to nil
(let (x 42) ...)

;; BAD — binding has wrong number of elements
(let ((x 1 2)) ...)
(let ((x)) ...)

;; GOOD
(let ((x 42)) ...)
(let ((x 1) (y 2)) ...)
(let (((list a b) pair)) ...)
```

### `let-recursion`

**Warns about an unresolved self-reference in an initializer-created closure.**

The check runs on a single file without `--workspace`. A `let` initializer
uses the enclosing environment; a `let*` initializer also sees earlier
bindings. Neither can capture the binding it is about to introduce, even
when a lambda defers the reference until the body runs. Use `labels` for
local recursion:

To make these migration warnings fail a CI check, run
`elps lint --checks=let-recursion --fail-on=warning file.lisp`.

```lisp
;; BAD: the lambda cannot capture this f binding
(let* ((f (lambda (n) (if (= n 0) 0 (f (- n 1))))))
  (f 1))

;; GOOD: labels binds the recursive function in its own environment
(labels ((f (n) (if (= n 0) 0 (f (- n 1)))))
  (f 1))
```

The warning points to the unresolved reference and links its binding.
Known outer functions, earlier `let*` bindings, lambda parameters, and
nested local bindings are respected. The check covers explicit lambdas
and prefix lambdas (`#^`), including closures inside initializer expressions.
The `lisp:`-qualified binding and lambda forms work too.

Supply `--workspace` when the reference intentionally names a function
defined in another file. Without that context, the check cannot distinguish
an unavailable outer function from intended local recursion. Quoted data,
quasiquote templates, opaque macro-dependent references, and known shadowed
bare special-form names are skipped. Runtime-generated code and macros
that introduce deferred evaluation are not fully covered; a clean result
does not prove compatibility.

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

A call-shaped list that the code walker classifies as quoted data or as
structure (`(quote (car))`, `'((car) 1)`, a `dotimes` control list) is not a
call and is not checked.

**Note:** User-defined functions that shadow builtin names are automatically
detected and excluded from arity checking. This covers `defun`/`defmacro`
names as well as names bound locally by `let`, `let*`, `flet`, `labels` and
`macrolet`. Formals lists and threading macro (`thread-first`,
`thread-last`) children are also excluded, since their static argument count
differs from the runtime count after macro expansion.

Binding forms are excluded too. A binding entry is shaped like a call —
`(let ((map (sorted-map))) ...)` contains the s-expression
`(map (sorted-map))` — but it binds a name rather than calling `map`, so it
is never arity-checked.

```lisp
;; OK — these bind local names, they are not calls
(let ((map (sorted-map 'a 1))) (get map 'a))
(flet ((first (xs) (car xs))) (first '(1 2 3)))
```

### `quote-call`

**Warns when `set` is called with an unquoted symbol name.**

`set` expects a quoted symbol as its first argument. Passing a bare symbol
causes it to be evaluated, which is usually not intended.

```lisp
;; BAD — x is evaluated, not used as a name
(set x 42)

;; GOOD
(set 'x 42)
```

**Note:** `set!` and `defconst` are not checked. `set!` takes an unquoted
symbol by design, and `defconst` is a macro that quotes its own name
argument — `(defconst x 42)` expands to `(set 'x 42)`, so the unquoted
spelling is the correct one and `(defconst 'x 42)` fails at runtime.

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

**Flags `(rethrow)`, `(error-stack)` and `(error-message)` calls outside a `handler-bind` handler.**

`rethrow` re-signals the currently active error, and `error-stack` and
`error-message` return its call stack and message; all three only work inside
a `handler-bind` handler. Using one outside causes a runtime error. An
`(error-stack)` or `(error-message)` inside a function body (`defun`,
`lambda`, `flet`, ...) is not reported, since a handler may call that
function; `rethrow` is reported there as before.

The check walks code with the shared code walker (`astutil.ExpandAll`), so
quoted data and quasiquote templates are not searched, a local function or
variable named `rethrow` is not the builtin, and the body of a `flet` or
`labels` form (unlike its function bodies) is not a function body. When
semantic analysis runs with a macro expander (`--workspace` with an
environment, the LSP, `elps mcp`), each form is fully expanded first, so a
user macro that expands to `handler-bind` counts as one, however deeply it is
nested. A call a macro synthesizes is not reported at the call site;
instead the quoted and quasiquoted templates inside a `defmacro` body are
searched as code, so a macro that emits a bare `(rethrow)` is reported at its
template.

```lisp
;; BAD — not inside handler-bind
(defun my-handler (c &rest args)
  (rethrow))

;; GOOD
(handler-bind ((condition (lambda (c &rest args) (rethrow))))
  (error 'test "data"))
```

### `loop-variable-capture`

**Warns when a closure stored from a `dotimes` body captures the loop
variable.** (Severity: **warning**)

`dotimes` reuses one binding for every turn, so a closure that captures the
loop variable and outlives its turn sees the variable's final value, not the
value of the turn that created it (see "dotimes and captured loop variables"
in the language reference).

```lisp
;; BAD — every closure returns 3
(dotimes (i 3) (append! fs (lambda () i)))

;; GOOD — a fresh binding per turn
(dotimes (i 3) (let ((i i)) (append! fs (lambda () i))))
```

The check runs on fully expanded code, with the macro expander when semantic
analysis has one, and reads the analysis package's scope resolution to decide
what a closure captures, so a parameter or inner `let` that shadows the
variable is not reported. Only the loop body is checked; the result form runs
once, after the loop. A closure counts as stored only when it is the
direct argument, not shadowed by a local function, of `set`, `set!`, `append!`, `append`, `assoc!`, `assoc`,
`cons`, `list`, `vector`, `concat`, `sorted-map`, `insert-index` or
`insert-sorted`; one passed to `map` or called on the spot runs during its
turn and is not reported. A closure stored some other way (returned from a
helper, passed to a user function that stores it) is not seen.

### `test-outside-test-file`

**Reports tests registered outside a `_test.lisp` file.**
(Severity: **error**)

`test`, `test-let`, `test-let*`, `benchmark` and `benchmark-simple` —
unqualified, `lisp:` or `testing:` — register a test with the running test
suite. Only test runners install one, and they load `*_test.lisp` files; a
production load has no test suite, so a test form in any other file fails the
load with `no test suite` (elps#736). A shared `*_testhelpers.lisp` file is
reported too: it defines helpers for tests and should not register tests of
its own.

```lisp
;; ERROR in main.lisp — move it to main_test.lisp
(test "adds" (testing:assert= 2 (add 1 1)))

;; GOOD — in main_test.lisp
(test "adds" (testing:assert= 2 (add 1 1)))
```

A file that defines its own function or macro under one of these names is not
checked for that name; quoted data and quasiquote templates are ignored, and
input without a `.lisp` file name (stdin) is not checked. Suppress a
deliberate case with `; nolint:test-outside-test-file`.

### `comparator-mutation`

**Flags a mutating call inside a `stable-sort` or `insert-sorted` predicate.**
(Severity: error)

A comparator runs an unspecified number of times, in an unspecified order.
Writes to shared state or input elements can therefore make sorting unreliable.
This is a conservative syntactic rule: it also reports writes to callback-local
scratch values, even when those writes cannot escape the callback. Every known
mutating builtin is reported:
`assoc!`, `dissoc!`, `append!`, `append-bytes!`, `set!`, and `stable-sort`
itself, which sorts in place despite carrying no `!`.

```lisp
;; BAD — the predicate mutates
(stable-sort (lambda (a b) (assoc! a 'visited true) (< a b)) xs)
(insert-sorted 'list xs (lambda (a b) (append! log a) (< a b)) item)

;; GOOD — the predicate only compares
(stable-sort (lambda (a b) (< a b)) xs)
```

Two spellings of the predicate are followed: an inline `lambda`, and a plain
symbol naming a `defun` **in the same file**. That hop is one level deep — the
named function's own body is scanned, but a call it in turn makes is not
followed, so a comparator that mutates two hops away is not reported. When a
file defines the same name twice, the **last** definition is the one followed,
because `defun` overwrites and that is the body the interpreter runs.

Data is skipped whole, and in all three spellings: a reader-quoted form
(`'(assoc! a b)`), an explicit `(quote ...)` form, and a `quasiquote`
template. An `(unquote ...)` or
`(unquote-splicing ...)` subtree inside a template is evaluated where it
stands, so a mutation in one is still reported. This applies to the sort form
itself as much as to the predicate's body, and a `defun` written inside data
defines nothing, so a symbol naming one resolves to no callback at all.
The evaluated forms `lisp:quote` and `lisp:quasiquote` are handled too.
ELPS searches through nested quasiquote templates for unquotes; nesting another
quasiquote does not protect an unquoted mutation from evaluation or this check.

Limits worth knowing. The package-qualified spelling of every operator this
check matches on is recognised — `lisp:stable-sort`, `lisp:insert-sorted`,
`lisp:lambda`, `lisp:defun` and `lisp:assoc!` and the other mutating builtins
read exactly as their bare names, since `lisp` is the only package exporting
them and the interpreter resolves both spellings to one function (the
diagnostic always names the canonical spelling). A same-file **rebinding** is
not tracked, however: the check keeps no scope of its own, so after
`(defun quote (x) x)` — or a shadowing definition of `quasiquote` or of a
mutating builtin — it goes on reading the shadowed builtin meaning rather than
the new one. `lisp:unquote` is the one qualified spelling deliberately left as
data, because ELPS itself recognises only the bare `unquote` marker inside a
template.

### `iteration-mutation`

**Flags a callback that mutates the collection it is iterating.** (Severity:
warning)

Covers `map`, `foldl`, `foldr`, `select`, `reject`, `all?` and `any?`. A
mutating call — `assoc!`, `dissoc!`, `append!`, `append-bytes!` or
`stable-sort` — is reported when the value it writes through is the collection
argument itself, or one of the callback's own parameters, which holds an
element of that collection.

`set!` is deliberately **not** on that list, unlike in `comparator-mutation`.
It rebinds a name rather than writing through the value the name held, so
`(set! x 1)` gives the callback's own parameter a new value and leaves the
element alone, and `(set! xs ...)` rebinds the caller's variable while the
builtin goes on walking the sequence it was already handed.

```lisp
;; BAD — writes through the sequence being walked
(map 'list (lambda (x) (append! xs x)) xs)

;; BAD — writes through an element the traversal handed over
(map 'list (lambda (x) (assoc! x 'seen true)) xs)

;; GOOD — the fold's own accumulator is neither the collection nor an element
(foldl (lambda (acc x) (assoc! acc x 1)) (sorted-map) xs)

;; GOOD — an unrelated binding
(map 'list (lambda (x) (assoc! out x 1)) xs)
```

Both an inline `lambda` and a plain symbol naming a `defun` **in the same
file** are followed, one hop deep. Quoting is honoured exactly as it is for
`comparator-mutation` above.

Blind spots worth knowing: the check is syntactic and keeps no scope of its
own, so a callback parameter that an inner `let` rebinds is still treated as
the element; a collection passed as an expression rather than a symbol has no
name to match against and is invisible; and `zip` is not covered, because it
takes no callback at all.

Qualified spellings are recognised here too — `lisp:map`, `lisp:foldl`,
`lisp:lambda`, `lisp:defun`, `lisp:assoc!` and the rest read exactly as their
bare names, and the diagnostic names the canonical one. And as with
`comparator-mutation`, a same-file rebinding of `quote`, `quasiquote` or a
builtin name (for example `(defun quote (x) x)`) is not tracked, so the check
then reads the shadowed meaning.

### `handler-order`

**Flags a `handler-bind` binding written after a binding for one of its
ancestors, or for its own type.** (Severity: warning)

`handler-bind` runs the most specific matching binding: the one for the
raised condition, else the one for its nearest ancestor, with the catch-all
`condition` last (luthersystems/elps#831). Before that change it ran the
first match in source order, so the second binding below never ran. It now
runs for `boom`:

```lisp
;; WARNING — the boom handler used to be dead code; it now runs for boom
(handler-bind ((condition (lambda (&rest _) 'any))
               (boom (lambda (&rest _) 'boom)))
  (work))

;; GOOD — reads the way it runs
(handler-bind ((boom (lambda (&rest _) 'boom))
               (condition (lambda (&rest _) 'any)))
  (work))
```

The same applies to `error` before `argument-error`, `match-error` or
`record-error`, the built-in children of `error`, and to a parent named
by a `define-condition` call in the same file whose arguments are a quoted
symbol or a string. A second binding for the same type is reported as one
that never runs. The check cannot see a parent defined in another file, by
Go code (`Runtime.DefineCondition`), or by a computed `define-condition`
call.

### `record-set-outside-method`

**Flags `s:set-field!` outside a method of a record type.** (Severity: warning)

A record type's methods are the one place that changes its objects
(luthersystems/elps#895). `s:set-field!` checks the new value against the
field's validator only. The type's `:check` function runs when a method
call returns, so a change from outside a method skips it.

```lisp
;; WARNING: changes the order from outside its type
(defun pay (o) (s:set-field! o 'status "paid"))

;; GOOD: the type changes its own field
(s:defmethod order pay (o) (s:set-field! o 'status "paid"))
(o 'pay)
```

A lambda inside a method body is part of the method, so `s:set-field! o` in
it is not reported.

The runtime refuses the write too (luthersystems/elps#908): `s:set-field!`
raises `record-error` unless a method of the object's type runs on that
object. The lint finds the write before the code runs. The two tests
differ:

| Code | Lint | Runtime |
|------|------|---------|
| `s:set-field!` in a method body, or in a lambda in it that runs during the call | not reported | allowed |
| `s:set-field! o` in a `defun` that a method on `o` calls | reported | allowed: the method runs |
| `s:set-field! other` in a method on `o` | not reported | refused unless a method runs on `other` |
| `s:set-field!` in a lambda that a method returned, called after the call | not reported | refused |

### `record-type-binding`

**Flags `s:make-record` whose type does not reach a top-level `set`.**
(Severity: warning)

A durable dump saves an object's type by the name of the global that
holds the type. A type built inside a function has no such name, so a
dump of its objects fails.

```lisp
;; WARNING: no global holds the type
(defun make-order-type () (s:make-record "order" (s:has-key "id" s:string)))

;; GOOD
(set 'order (s:make-record "order" (s:has-key "id" s:string)))

;; GOOD: a closure keeps a private helper
(set 'order
  (labels ([cents (d) (* 100 d)])
    (s:make-record "order"
      (s:has-key "dollars" s:int)
      (s:method in-cents (o) (cents (o 'dollars))))))
```

### `record-field-mutation`

**Flags a method that changes a field's value in place.** (Severity:
warning)

`(o 'field)`, where `o` is the method's receiver, returns the field's value, not a copy. A mutating builtin
on it (`assoc!`, `dissoc!`, `append!`, `append-bytes!`, `stable-sort`)
changes the field, and the field's validator does not run.

```lisp
;; WARNING: the lines validator does not see the new line
(s:method add-line (o sku qty) (assoc! (o 'lines) sku qty))

;; GOOD: s:set-field! checks the new map
(s:method add-line (o sku qty) (s:set-field! o 'lines (assoc (o 'lines) sku qty)))
```

The check sees only a direct `(o 'field)` argument, where `o` is the receiver. It does not follow
a field's value into a local variable.

### `record-unknown-member`

**Flags a member name that the record type does not declare.** (Severity:
warning)

In a method,
`(o 'name)` must name a field or a method of the type, and
`(s:set-field! o 'name ...)` must name a field, where `o` is the method's
receiver. A wrong name fails only when
the method runs.

```lisp
;; WARNING: lnies is not a field
(s:method add (o sku n) (s:set-field! o 'lnies (assoc (o 'lines) sku n)))
```

The check reads the fields and methods from the type's `s:make-record`
and from each `s:defmethod` in the same file that runs at load. With a workspace
(`elps lint --workspace`, the LSP, the MCP server), it also knows each
method an `s:defmethod` in another workspace file adds. A type's methods
are often split across files, and without that data a call to such a
method reads as an unknown member. Without a workspace, add
`; nolint:record-unknown-member` to a call of a method from another file.

The workspace methods follow these rules:

* A method counts only when a load adds it. An `s:defmethod` that runs
  at call time (see `record-defmethod-not-toplevel`) is not part of the
  type a template publishes, in the workspace or in the same file.
* A method from a `*_test.lisp` or `*_testhelpers.lisp` file counts only
  in a test file. Only test runners load those files, so their methods do
  not hide an unknown member in a production file. A test can still call
  a method its test helpers add.
* The linted file's own entries are left out. They come from the file on
  disk, and the text being linted declares its methods itself, so a method
  the unsaved text renamed or removed is unknown again. The LSP replaces a
  file's methods each time the file is saved.
* A type is matched by its name without its package:
  `(s:defmethod shop:order ...)` adds to `order`. So two packages with a
  type of one name share one method set. The check then misses a finding;
  it does not report a false one.

The check skips a type whose field forms are not literal `s:has-key` or
`s:may-have-key` calls, and a type defined in another file.

### `record-check-writes`

**Flags a `:check` function that changes the object it checks.** (Severity:
warning)

`:check`
runs after the constructor and after each method call, and it must only
read the object. To refuse a change, raise an error.

```lisp
;; WARNING: :check changes a field
:check (lambda (c) (s:set-field! c 'n 0))
;; WARNING: assoc! changes the field in place
:check (lambda (c) (assoc! (c 'seen) "k" 1))
```

An `s:set-field!` in `:check` gets this warning, not `record-set-outside-method`.

The runtime refuses an `s:set-field!` on the object while its `:check` runs
(luthersystems/elps#908), also from a method the check calls, with
`record-error`. The lint finds the write before the code runs. The runtime
cannot see an in-place change such as `assoc!` on a field's value; only the
lint reports those.

### `record-method-arity`

**Flags a member call in a method with the wrong number of arguments.**
(Severity: warning)

In a method, `(o 'field)` takes no arguments, and `(o 'method args...)`
takes the arguments the method's formals accept after the receiver. A
wrong count fails only when the method runs. A typed formal, `[name type]`, counts as its name.

```lisp
(s:method add (o sku qty) ...)
;; WARNING: method add needs at least 2 argument(s), got 1
(s:method add-one (o sku) (o 'add sku))
```

The check knows the same members as `record-unknown-member`, with the same
workspace data and limits: a type defined in another file is not checked.
A method from the linted file uses that file's formals. When `s:defmethod`
calls in other files give one method different arities, the check skips
calls to that method, because it cannot know which definition runs.

### `record-defmethod-type`

**Flags an `s:defmethod` whose first argument is not a record type.**
(Severity: warning)

The first argument is evaluated and must give the type `s:make-record`
returns. The check reports a literal, a quoted name, a name the file binds
to a literal, and a name that is a function, macro or builtin.

```lisp
;; WARNING: a quoted value
(s:defmethod 'order pay (o) ...)
;; WARNING: make-order is a function, not a record type
(s:defmethod make-order pay (o) ...)
```

A name bound in another file is not checked.

### `record-plain-set`

**Flags `set!` or `set` on a method's receiver.** (Severity: warning)

The receiver is a local name. `(set! o v)` changes the binding, not the
object, and `(set 'o v)` binds a package name. `(set! o 'field v)` is not a
field write. Use `s:set-field!` to change a field.

```lisp
;; WARNING: set! does not change a record field
(s:method pay (o) (set! o 'paid true))
;; OK
(s:method pay (o) (s:set-field! o 'paid true))
```

### `record-captured-set`

**Flags `set!` in a method of a local of the closure around the record
type.** (Severity: warning)

A record type can be built inside `let`, `let*`, `flet` or `labels`, so the
closure's locals act as private helpers of the type. Each local is one
variable for all objects of the type. A method that changes it with `set!`
changes state that every object shares and that no object holds:

* A durable dump saves an object's fields and the name of its type. It
  does not save the closure's locals, so the change is lost on load.
* A failed method call puts the object's fields back. It does not undo a
  change to a closure local.

Keep per-object state in a field and change it with `s:set-field!`.

```lisp
;; WARNING: every order shares count, and a dump does not save it
(set 'order
  (let ([count 0])
    (s:make-record "order"
      (s:has-key "n" s:int)
      (s:method add (o) (set! count (+ count 1))))))
;; OK: the count is a field of each object
(set 'order
  (s:make-record "order"
    (s:has-key "n" s:int)
    (s:method add (o) (s:set-field! o 'n (+ (o 'n) 1)))))
```

The check covers inline `s:method` forms and `s:defmethod` calls, and a
`set!` in a lambda inside the method. It does not report a `set!` of the
receiver (see `record-plain-set`), of the method's formals, of a local the
method body binds, or of a global. A read of a closure local is not
reported.

### `record-defmethod-not-toplevel`

**Flags an `s:defmethod` that does not run at load.** (Severity: warning)

A template publishes a record type with its method table as it stands at
publication. An `s:defmethod` in a function body runs at call time. In
substrate, each transaction runs in a fresh VM, from a template or a cold
load, so the method reaches only the VM that runs the call:

* The method is lost after the call.
* It is not in the type's saved form.
* A later call in another VM does not see it.

The runtime refuses the same code (luthersystems/elps#908): `s:defmethod`
raises `record-error` while a function defined in Lisp runs, that is when
the call stack holds a frame of a `lambda`, `defun`, `defmacro`, `flet` or
`labels` function. The test reads only the call stack, so it gives the same
answer in a cold-loaded VM and in every template VM. The lint finds the call
before the code runs. The two tests differ:

| Code | Lint | Runtime |
|------|------|---------|
| `s:defmethod` in a function body | reported | refused |
| `s:defmethod` in the arguments of a macro call that expands it at the top level | reported | allowed |
| `s:defmethod` at the top level of a file that a function loads with `load-file` | not reported | refused |
| `s:defmethod` in a `*_test.lisp` test body | skipped | refused |

An `s:defmethod` runs at load when nothing around it defers it:

* A function body runs at call time: `defun`, `lambda`, and a `flet` or
  `labels` function. It is reported.
* The arguments of a macro call run when the macro decides, if ever.
  They are reported. The check does not expand macros, so the body of a
  macro such as an endpoint definition is reported. The value of a
  `defconst` runs at load, so it is not reported.
* The arguments of a function call run at once, before the call:
  `(set 'x (list (s:defmethod ...)))` and `(export (s:defmethod ...))`
  at the top level run at load. They are not reported.
* A head is a macro when the analysis resolves it to one: a builtin
  macro, a libschema macro, a macro the file defines, or, with a
  workspace, one another file defines. A head it cannot resolve is taken
  for a function. The check then misses a finding, and the methods of
  that call count as members, so a guess never reports a known method as
  unknown.
* The other special forms run their parts at load: `let` and `let*`
  (bindings and body), `progn`, `if`, `when`, `unless`, `cond`, and the
  body of `flet` and `labels`. They are not reported.

The message names the innermost form that defers the call, for example
`defun`, or `lambda` for a `lambda` in an endpoint body.

```lisp
;; WARNING: the method exists only during this call
(defun add-size () (s:defmethod order size (o) (length (o 'lines))))
;; OK: the method is part of the type at load
(s:defmethod order size (o) (length (o 'lines)))
;; OK: a top-level closure around the method
(let ([limit 10]) (s:defmethod order big (o) (> (o 'size) limit)))
```

A call of your own macro around an `s:defmethod` is reported even when
the macro runs its body at load, because the check cannot see into the
macro. Suppress that line with
`; nolint:record-defmethod-not-toplevel`.

The check skips `*_test.lisp` and `*_testhelpers.lisp` files. Only test
runners load them, and the runtime refuses a call-time `s:defmethod` in a
test too. A test checks the refusal with an `s:defmethod` in a lambda.

### `schema-type-union`

**Flags a type next to a constraint on the same type in `s:has-key`,
`s:may-have-key` or `s:of`.** (Severity: warning)

The allowed types are OR'ed: a value passes when it matches one of them. So
a base type next to a constraint on that type accepts every value of the
type, and the constraint never refuses one. `s:any` next to any other type
is flagged too. A real union of types, `(s:has-key "n" s:int s:string)`, is
not flagged.

```lisp
;; WARNING: every string passes, "nope" too
(s:has-key "email" s:string (s:regexp "@"))
;; OK: both must pass
(s:has-key "email" (s:make-validator "email" s:string (s:regexp "@")))
```

### `match-unreachable`

**Flags a `match` clause after a clause that matches every value.**
(Severity: warning)

`match` tries its clauses in order. A pattern of `_` or a bare name, with
no `:when` guard, matches every value, so the clauses after it never run.

```lisp
;; WARNING: (2 'two) never runs
(match v
  (x 'any)
  (2 'two))
;; OK: the catch-all is last
(match v
  (2 'two)
  (x 'any))
```

### `use-package-hides-lisp`

**Warns when a `use-package` hides `lisp` names, and at each call that then
misses the core form.** (Severity: warning)

`use-package` copies the package's exports over the caller's bindings. After
`(use-package 's)`, `not`, `when`, `list`, `error` and `sorted-map` are the
`s` exports, so `(not x)` calls the constraint `s:not` (elps#903). The check
reports the `use-package` once, naming the hidden names, and each later
unqualified call to one of them in the same package.

```lisp
;; WARNING: use-package 's hides lisp names: error, list, not, sorted-map, when
(use-package 's)
;; WARNING: not here is s:not
(if (not ok) 1 2)

;; GOOD: use the package qualified
(s:validate v (s:not s:nil))
(if (not ok) 1 2)
```

Only standard library packages are known. A name the file defines itself is
left to [`builtin-shadowing`](#builtin-shadowing). Quoted data and
quasiquote templates are not calls.

### `with-cleanup-forms`

**Flags a degenerate `with-cleanup` spec list.** (Severity: warning)

Two shapes, both of which run without complaint.

An **empty** list makes the form a no-op wrapper around its body — it still
runs and returns the same value, which is what makes it quiet:

```lisp
;; BAD — guarantees nothing
(with-cleanup () (acquire) (work))
```

A **bare symbol** in the list is the missing-paren mistake, and the more
dangerous of the two:

```lisp
;; BAD — neither `release` nor `handle` is called; the cleanup never happens
(with-cleanup (release handle) (work))

;; GOOD
(with-cleanup ((release handle)) (work))
```

The bad version behaves identically to the good one until the body signals,
which is exactly when the cleanup was supposed to matter.

`(with-cleanup)` with no arguments is left to `builtin-arity`, which already
reports it.

### `unnecessary-progn`

**Flags redundant `progn` wrappers.** (Severity: info)

Forms like `defun`, `let`, `lambda`, and `handler-bind` already accept
multiple body expressions, so wrapping them in `progn` is unnecessary.

`with-cleanup` participates too: its body is an implicit progn, so a `progn`
wrapping the body is redundant. Its cleanup **spec list** is not a body and is
never flagged.

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

The analyzer understands:
- Standard binding forms: `defun`, `defmacro`, `lambda`, `let`/`let*`,
  `flet`/`labels`, `dotimes`, `set`, `deftype`.
- `test` and `test-let`/`test-let*` from the testing library.
- Nested `defun`/`defmacro` inside any body (e.g., inside `test`, `progn`,
  `when`). Names are registered in the enclosing scope.
- **`def` prefix heuristic:** Unknown forms starting with `def` (e.g.,
  user macros like `defmethod`) are treated as definitions — the first
  formals-like list is bound as parameters and the remaining children are
  analyzed as body in that scope.
- Prefix lambda (`#^`): implicit `%`, `%1`, `%2`, `%&rest` params.
- Quasiquote templates: only `unquote`/`unquote-splicing` are analyzed.
- Package imports via `use-package` and `in-package`.

```lisp
;; ERROR — unknown-fn is not defined anywhere
(unknown-fn 1 2)

;; OK — builtins, keywords, and qualified names are recognized
(+ 1 2)
(list :key :value)
(math:floor 1.5)

;; OK — defmethod formals are recognized via def prefix heuristic
(defmethod point :move (self dx) (+ self dx))
```

**Known limitation — opaque macros:** A macro can introduce a binding that
is absent from its unexpanded source. Supply workspace or embedder macro
information when available; suppress a remaining diagnostic only after
checking the expansion.

Delaying evaluation does not make a `let` or `let*` initializer capture its
own binding. A thunk still captures the environment where its initializer
runs. Use `labels` for recursive local functions, as described by
[`let-recursion`](#let-recursion), rather than suppressing this real missing
binding as a presumed delayed-evaluation false positive.

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

**Reports bindings that shadow another binding.**
(Severity: **warning** when the shadowed name is callable, **info** otherwise)

Shadowing has two halves:

- **Local bindings** (this check). Requires semantic analysis. Flags
  parameters, `let` bindings, and local functions (via `labels`/`flet`)
  that reuse a name from an enclosing scope or the global scope.
- **Top-level bindings** ([`builtin-shadowing`](#builtin-shadowing), which
  runs without semantic analysis too). A top-level `defun`, `defmacro`,
  `set` or `set!` that shadows a core `lisp` name is a **warning**, the same
  severity as a local binding that hides a callable.

`; nolint:shadowing` suppresses either half.

Severity follows what is hidden. Shadowing a **builtin, special operator,
macro or function** is a warning: while that binding is in scope, a call to
the name resolves to the local instead. Shadowing another local — a
variable or parameter — is info.

```lisp
;; WARNING — while this binding is live, (min ...) is an int, not the builtin
(defun foo () (let ([min (compute-min)]) (bar min)))

;; INFO — local x shadows the parameter x
(defun foo (x) (let ((x 2)) (+ x 1)))

;; WARNING (builtin-shadowing) — a top-level defun hides lisp:map for the
;; rest of this package
(defun map (f l) (cons (f (car l)) ()))

;; OK — a deliberate override, suppressed
(defun map (f l) (cons (f (car l)) ())) ; nolint:shadowing

;; OK — a parameter shadowing a builtin or special-op is idiomatic in
;; formals; the builtins themselves use names like car, map and expr
(defun foo (car) (+ car 1))
```

#### Refinement is not shadowing

A binding whose initialiser references the name it shadows is **not**
reported. It narrows one value rather than introducing a second meaning,
and it is the only way to default an `&optional` argument:

```lisp
;; OK — one value, progressively narrowed
(defun handle (&optional ctx)
  (let* ([ctx (default ctx (sorted-map))])
    ...))
```

This carve-out deliberately does **not** apply when the shadowed name is
callable, because refinement is only coherent for a value:

```lisp
;; WARNING — reported despite mentioning `car`, because the body's
;; (car xs) now applies an element as a function
(defun foo (xs) (let ([car (car xs)]) (car xs)))
```

Quoted occurrences do not count as a reference — `(let ([keys 'keys]) ...)`
rebinds `keys` to a symbol, which narrows nothing, so it is still reported.

### `builtin-shadowing`

**Warns when a top-level binding shadows a core `lisp` name.**
(Severity: **warning**)

Builtins, special operators and macros are ordinary symbols, and shadowing
one in your own package is legal, but every later call to that name in the
package then means your binding. This check flags a top-level `defun`,
`defmacro`, `set` or `set!` whose name is exported by `lisp`, in any package
(a package created by `in-package` imports `lisp`). That includes the names
that became core in elps#736: `help`, `test`, `benchmark`, `test-let`,
`test-let*` and `benchmark-simple`.

```lisp
;; WARNING — test is core (lisp:test); this hides it in the package
(defun test (x) (* 2 x))

;; GOOD — rename it
(defun double (x) (* 2 x))

;; OK — deliberately shadowed; call the core form as lisp:test
(defun test (x) (* 2 x)) ; nolint:shadowing
```

It is the top-level half of [`shadowing`](#shadowing): suppress it with
`; nolint:builtin-shadowing` or `; nolint:shadowing`. Dynamic names
(`(set name ...)`) and macro-generated definitions are not checked, and a
write into `lisp` itself is `lisp-package-seal`'s error instead.

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

### `removed-builtin`

**Reports references to standard-library builtins that elps has removed.**
(Severity: **error** — the reference fails at runtime, so `elps lint` exits 1
under the default `--fail-on error`)

Structural, so it needs no workspace.  elps#757 removed `time:utc-now`,
`time:time-elapsed` and `time:sleep` because the standard library must be
deterministic; the diagnostic names the replacement.  A qualified reference is
reported anywhere outside a quasiquote template, quoted or not.  An unqualified
call is reported when the file has a top-level `(use-package 'time)` or
`(in-package 'time)` and does not define that name itself.  It is skipped when
another top-level `use-package` (other than `lisp` and `user`) could supply the
same short name, as in `(use-package 'time) (use-package 'cc) (sleep d)`, or
when the host's semantic info names a symbol of that name from another package.

```lisp
;; ERROR — 'time:utc-now' is removed ...; use a timestamp the host supplies
(time:utc-now)

;; OK — measure between two known times
(time:time-from start end)
```

Limits: it cannot see a name built at runtime, produced by a macro, inside a
quasiquote template, or imported by another file.  A host that registers its
own `time:sleep` can suppress the diagnostic with `; nolint:removed-builtin`.

### `deprecated`

**Reports uses of symbols marked deprecated by their docstring.**

Requires semantic analysis (workspace mode). A symbol is deprecated when a
paragraph of its docstring begins with `Deprecated:` (or `DEPRECATED:`) —
the same convention Go doc comments use. The rest of that paragraph is
quoted in the diagnostic, so it is the place to say what to use instead.
`defun`, `defmacro`, workspace-scanned definitions, the interpreter's own
builtins and the builtins a Go embedder registers (`elpsutil.FunctionDoc`)
are all covered.

Only *uses* are flagged, never the declaration, and a use inside the body
of a definition that is itself deprecated is not flagged — deprecated code
is allowed to call deprecated code, exactly as in Go.

A docstring is the whole run of leading string literals, joined the way
`elps doc` joins them: an empty string `""` opens a new paragraph. A string
literal cannot contain a raw line break, so that empty string — or a `\n\n`
escape inside a single string — is how the marker paragraph is written.

```lisp
;; The declaration itself is never flagged
(defun blend-paths (a b)
  "Blend two paths."
  ""
  "Deprecated: use join-paths instead."
  (join-paths a b))

;; WARNING — use of deprecated function 'blend-paths': use join-paths instead.
(blend-paths "a" "b")

;; GOOD — call the replacement
(join-paths "a" "b")

;; OK — a deprecated function may use deprecated functions
(defun old-blend (a b)
  "Deprecated: use join-paths instead."
  (blend-paths a b))
```

A marker only counts at the start of a paragraph, so prose mentioning the
word mid-paragraph does not deprecate anything. A body made up entirely of
strings is a constant function rather than a documented one, so nothing in
it deprecates anything either. A quoted symbol is data, not a reference, so
`(funcall 'blend-paths a b)` is not flagged — `#'blend-paths` is. To keep a
call that must stay, suppress it with `; nolint:deprecated`.

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

**Note:** when the analyzer set is narrowed with `--checks`, a directive is
only reported if at least one analyzer it names actually ran. A directive
naming a valid but disabled analyzer is not stale — that check simply never
had a chance to fire — and a bare `; nolint` is never reported on a narrowed
run. Unknown analyzer names are still reported, since a typo is a typo
regardless of which checks ran.

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
