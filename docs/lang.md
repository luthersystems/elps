# Language Reference

## Basics

Lisp code interpreted by elps is given as a sequences of expressions encoded as
utf-8 text.

## Expressions

An expression is either an atom or a sequence of expressions delimited by
parenthesis `(...)`.  An environment evaluates one expression at a time,
evaluating each sub-expression before evaluating the main expression.
Expressions may also be *quoted* by prefixing it with an single quote `'`.
Quoted expressions are discussed in depth along with [expression
evaluation](#expression-evaluation).

## Atoms

Atoms can be symbols, numbers, and strings.

### Symbols

Symbols (identifiers) may consist of utf-8 letters, numbers, and many symbols
(symbols not used for language syntax).  Identifiers cannot start with a
number.

Symbols are currently case sensitive although this may change.

A symbol may also be written in its package-qualified form, `pkg:name` (see
[Packages](#packages)).  Both halves are identifiers and the rule above applies
to each of them independently, so `a:1` is not a symbol: the reader rejects it
with an `invalid-symbol` condition rather than producing a symbol named `1` in
package `a`, a name no other part of the language can write.  Note that `+1`
and `.1` *are* identifiers — `+` and `.` are ordinary symbol characters — so
`a:+1` and `a:.1` are both fine.

### Keywords

A symbol written with a leading colon and no package, `:name`, is a *keyword*.

Keywords are values, not identifiers.  A keyword evaluates to itself, is never
looked up in a package, and cannot be assigned to — `(set ':k 1)` is an error.
Because a keyword names no binding, the "identifiers cannot start with a
number" rule does not apply to it: `:1` is an ordinary keyword whose name is
`1`, in the same way that `:foo` is one whose name is `foo`.

Keywords are used to label [keyword arguments](#keyword-arguments), and a
keyword only matches a formal argument spelled the same way.  A keyword whose
name is not also a legal identifier therefore cannot match any formal argument,
so `:1` is only useful as data.

### Numbers

Numbers can be either int or floating point and will be converted between the
two forms as necessary.

A float carries only 53 bits of integer precision, which matters when numbers
arrive from outside the program: `json:load-string` and its siblings decode
every JSON number as a float by default and silently round integers above
2^53.  See [JSON numbers and integer
precision](#json-numbers-and-integer-precision).

### Strings

Strings are a sequence of utf-8 text delimited by double quotes `"`.  Strings
cannot contain line breaks.  A codepoint in a string may be escaped with a
preceding backslash `\`.

## Expression Evaluation

### Nil

The empty expression `()` is a special expression called "nil" and evaluates to
itself.  The value nil is used in the language to represent a false boolean
value (while anything non-nil represents a true boolean value).

### Atomic Expressions

Symbols evaluate to the last value bound to that symbol at the deepest
[scope](#scope) at which that symbol is bound.  Numbers and
strings evaluate to themselves.

### Quoted Expressions

Quoted expressions evaluate to themselves.  Quoted numbers and strings are
equivalent to their unquoted counterparts.  But a quoted nil value is not
equivalent to nil.

### Compound Expressions (Function Calls)

An expression containing sub-expressions is typically evaluated by evaluating
all its sub-expressions from left to right (top-down).  And then evaluating the
main expression by invoking the function resulting from the first
sub-expression with arguments bound the results of remaining sub-expressions.

```lisp
(expr1 expr2 expr3)
```

In evaluating the above expression expr1 is evaluated first (and must evaluate
to a function).  Then expr2 is evaluated, followed by expr3.  A new scope is
created, binding expr1's function arguments to the values of expr2 and expr3,
and then the expr1's function in that scope.

## Functions

A function is a symbolic expression that utilizes some number of unbound
argument symbols.

```lisp
(lambda (x) (- x))
```

The above expression evaluates to an anonymous function (a lambda function)
which has one argument `x` and evaluates to the expression `(- x)` when x is
bound to a value through [expression
evaluation](#expression-evaluation).

```lisp
((lambda (x) (- x)) 3)  ; evaluates to -3
```

The built-in macro `defun` is provided to bind names to functions.

```lisp
(defun neg (x) (- x))
(neg 3)                 ; evaluates to -3
```

If the complete list of arguments for a function cannot be known ahead of time
there are functions which you can use to assist in calling other functions.

```lisp
(defun sum-list (xs)
    (apply + xs))

(defun negative-sum? (&rest xs)
    (> 0
       (funcall sum-list xs)))

(negative-sum? 1 2 -2)  ; evaluates to false
```

Let's decompose the call to `negative-sum?`. The function negative sum has its
argument bound to `'(1 2 -2)`.  When funcall calls sum-list, it passes this
list verbatim.  When sum-list passes this list to `apply`, the list is unpacked
as if the list contents had been passed to `+` as its arguments.  Other than
this distinction the two functions, `apply` and `funcall` operate the same way.

### Optional function arguments

If a function's formal argument list contains the special symbol `&optional`
the following arguments are not required to call the function.

```lisp
(defun add1 (&optional x) (+ 1 (or x 0)))
```

The above function may be called with either zero or one argument.  If the
function is called without any arguments the optional argument x is bound to
the value nil.

```lisp
(add1)    ; evaluates to 1
(add1 2)  ; evaluates to 3
```

Functions can have multiple optional arguments.  Arguments are bound to
optional arguments in the order the arguments are defined and any left over
symbols are bound to the value nil.

```lisp
(defun add (&optional x y) (+ (or x 1) (or y 2)))
(add)     ; evaluates to 3
(add 2)   ; evaluates to 4
(add 2 0) ; evaluates to 2
```

There is no limit to the number of optional arguments a function can have. But
if the number of optional arguments is too large it may be better to use
keyword arguments instead.

### Variable argument (variadic) functions

A function's formal argument list may use the special symbol `&rest` before
the final argument to denote that the final argument should be bound to a list
containing all arguments not bound by previous argument symbols.

```lisp
(defun cons-reverse (x &rest xs) (cons x (reverse 'list xs)))
```

The above function can evaluate with one or more arguments.  The symbol `x`
will be bound to the first argument and `xs` will be bound to the remaining
(possibly empty) list of arguments.

```lisp
(cons-reverse 1)      ; evaluates to '(1)
(cons-reverse 1 2 3)  ; evaluates to '(1 3 2)
```

Variadic functions are prohibited from having keyword arguments due to
confusing semantics when mixing the two styles.  When keyword arguments are
needed avoid using `&rest` and just pass the variable argument as an
additional keyword argument.

### Keyword arguments

If a function's formal argument list contains the special symbol `&key` the
following symbols are keyword arguments.  Keyword arguments are like optional
arguments, in that they are not required to invoke the function.  Furthermore
they are bound to nil values when not provided.  However, keyword arguments
are unordered and when passed must be preceded by a keyword symbol indicating
which are follows.

```lisp
(defun point2d (&key x y) (list (or x 0) (or y 0)))
```

The above function defines two keyword arguments and may be called specifying
values for both, one, or neither.

```lisp
(point2d)           ; evaluates to '(0 0)
(point2d :y 1)      ; evaluates to '(0 1)
(point2d :x 1)      ; evaluates to '(1 0)
(point2d :y 1 :x 1) ; evaluates to '(1 1)
```

Keyword arguments are useful but they can also lead to some confusing errors.
Keywords are values.  And as values keywords can be passed to functions as
normal, required arguments.

```lisp
(defun single (x) (cons x ()))
(single :foo)  ; evaluates to '(:foo)
```

This unavoidable property of keyword arguments can lead to confusing runtime
errors when accidentally omitting required arguments or mixing keyword arguments
and optional arguments.

**NOTE**: Due to the properties of keyword arguments it follows that a
function utilizing both optional and keyword arguments may only have values
bound to their keyword arguments once values have been bound to *all* optional
arguments.

### Unbound expressions

The built-in `expr` function allows for compact construction of simple
functions.

```lisp
(expr (+ % 1))      ; evaluates to (lambda (%) (+ % 1))

; or equivalently

#^(+ % 1)
```

The special symbol `%` indicates an anonymous function argument.  Functions of
multiple arguments can be defined by using the anonymous argument symbols `%1`,
`%2`, ... or the variadic anonymous argument `%&rest`.

```lisp
(expr (+ %1 %2))         ; evaluates to (lambda (%1 %2) (+ %1 %2))
(expr (reverse 'list %&rest))  ; evaluates to (lambda (&rest %&rest) (reverse 'list %&rest))
```

## Macros

A macro is a special function which receives unevaluated arguments (values are
not quoted, they just aren't evaluated). A macro function returns a quoted
expression which is subsequently evaluated in the scope of the original call.

When writing macros the `macroexpand` and `macroexpand-1` functions help debug
macro behavior.  The arguments to these functions is a quoted s-expression (a
quoted macro invocation).  The result of these functions is the quoted
expansion of the macro.

```lisp
(defmacro m (&rest xs) (quasiquote (+ (unquote-splicing xs))))

(macroexpand '(m 1 2 3)) ; evaluates to '(+ 1 2 3)
```

The macroexpand-1 function is just like macroexpand except it will not
recursively expand macros when the result of the argument macro form is itself
a macro form.

The `gensym` builtin is used to generate a new symbol, which is most often used
with macros to avoid avoid naming collisions.

## Parens () and braces []

Matching braces produce a quoted list.  As with parens, an open brace `[`
must be closed using a close brace `]`. Using parens with braces can improve
readability. Conventionally, braces are used with `let` to define the bindings.

```lisp
(let ([x 1]
      [y 2])
  (+ x y))      ; evaluates to 3
```

## Special Operators

A special operator is like a macro, in that it receives unevaluated arguments,
but the result of a special operator will not be subsequently evaluated.
Examples of special operators are `if`, `lambda`, and `quasiquote`.  There is
no facility within the language for defining special operators.

### cond
`cond` takes an arbitrary number of arguments called clauses. A clause consists
of a list of exactly two expressions. The first expression in a clause is a
condition, and there can be any number of expressions following the condition
in a cond branch which get wrapped by an implicit progn.

For example,

```lisp
(cond (condition1 result1)
	(condition2 result2)
	...
	(:else resultN))
```

The value returned by `cond` is computed as follows: if condition1 is `true?`,
then return result1; else if condition2 is `true?` then return result2; else
if ...; else return resultN.

If none of the conditions are true (and there is no :else), then `()` is
returned.

### let vs let\*

`let` and `let*` are used to create bindings for local variables within a
new scope.  `let` bindings happens left-to-right/top-to-bottom and they can
refer to previously bound symbols.  The result of the evaluation of the last
expression within the `let` is returned.

```lisp
(let ((variable1 result1)
      (variable2 result2) ; result2 cannot reference variable1
      ...
      (variable3 result3))
  expr1 ; some expression that can use bound variables
  ...
  exprN ; exprN evaluation is returned
  )
```

```lisp
(set 'x 0)
(let ([x (+ x 1)]
      [x (+ x 1)])
  x)                ; evaluates to 1
```

```lisp
(set 'x 0)
(let* ([x (+ x 1)]
       [x (+ x 1)])
  x)                ; evaluates to 2
```

### flet vs labels

`flet` and `labels` are used to create bindings for local functions within a
new scope.  `flet` bindings are not recursive and cannot refer to each other.
`labels` bindings can be recursive and can refer to each other (left-to-right,
top-to-bottom).

```lisp
(flet ((func1 (arg1 ... argn) expr1 ... exprN)
       ...
       (func2 (arg1 ... argn) expr1 ... exprN))
  expr1 ; some expression that can use bound functions
  ...
  exprN
  ) ; exprN evaluated and returned
```

```lisp
(defun count () 0)
(flet ([count () (+ (count) 1)]
       [count () (+ (count) 1)])
  (count)) ; evaluates to 1
```

```lisp
(defun count () 0)
(labels ([count0 () (+ (count) 1)]
	 [count1 () (+ (count0) 1)])
  (count1)) ;  to 2
```

### macrolet

`macrolet` is used to create bindings for local macros within a new scope, in
an analogous way as `flet` and `labels`.

### assert

`assert` takes an expression and optional string, and evalutes the expression.
If the result of the evaluation is truthy then assert returns `()`, otherwise
`assert` will output the assertion failure message to stderr and raise an
error.

### progn

`progn` causes each of its arguments to be evaluated in sequence and then
returns the value of the last one. The preceding expressions are evaluated
only for the side effects they perform. The values produced by them are
discarded.

```lisp
(progn
  expr1
  expr2
  ...
  exprN) ; exprN evaluation is returned
```

### thread-first, thread-last

`thread-first` and `thread-last` help make nested function calls more readable,
and function similar to the clojure `->` and `->>` macros.

The word "thread" in this context (meaning passing a value through a pipeline
of functions) is unrelated to the concept of concurrent threads of execution.

`thread-first` takes the first argument and passes it as an argument to the
function defined in the second argument. The result of evaluating this
expression is then passed to the function defined in the third argument, and
so on.

```lisp
(defun add1 (x) (+ x 1))
(add1 (add1 2))                ; evaluates to 4
(thread-first 2 (add1) (add1)) ; evaluates to 4
```

`thread-first` passes the threaded value as the first argument in the chain of
functions, for example:

```lisp
(defun add1 (x) (+ x 1))
(defun addXY (x y) (+ (* 2 x) y))
(thread-first 10 (add1) (addXY 2)) ; evalutes to 24
```

`thread-last` passes the threaded value as the last argument in the chain of
functions, for example:

```lisp
(defun add1 (x) (+ x 1))
(defun addXY (x y) (+ (* 2 x) y))
(thread-last 10 (add1) (addXY 2)) ; evalutes to 15
```

## Scope

All symbol expressions are lexically scoped and resolve to the deepest binding
of that symbol.  Functions naturally create a lexical scope that binds their
argument symbols.  The other way to create a lexical scope is through the use
of `let` and `let*` which take as their first argument a list of bindings
following by expressions which are executed in a nested scope containing those
bindings.

```lisp
(defun foo (x)
    (+ x 1))        ; x evaluates to the value bound in foo's scope

(let ((x 1))
    (+ x 1))        ; x evaluates to the value bound in the let's scope

(let ((x 1))
    (let ((x 2))
        (+ x 1)))   ; x evaluates to the value bound in the first let
```

If a function or `let` expression binds a symbol which was already bound in a
higher scope the symbol will be *shadowed* inside the `let` expression.

```lisp
(let ((x 1) (y 2))
    (defun add-y (x)    ; the argument x shadows the value bound by the let
        (+ x y))
    (defun add-x (y)    ; the argument y shadows the value bound by the let
        (+ x y))

(add-y 3)               ; evaluates to 5
(add-x 3)               ; evaluates to 4
```

The scope of a function is created when the function itself is created.  In the
above example the functions `add-y` and `add-x` always use values bound by
their arguments or by the let which contains the function definition

```lisp
(let ((x 10))
    (add-x 3))      ; still evaluates to 4
```

Macros must take care if they directly evaluate an argument that contains a
lambda (outside of quasiquote/unquote) because the resulting function will
inherit the scope of the macro and not the scope of the caller, which is
probably not desired.

## Data Structures

### Lists

The most primitive data structure is a list, a quoted s-expression.

```lisp
'(1 2 3 4 "hello" ok)
```

Lists can be nested (it is not necessary to quote inner lists).

```lisp
'(1 2 (3 4 ()))
```

An empty list is equivalent to nil.

```lisp
(assert (nil? '()))
```

### Arrays

Arrays are references to continuous memory ranges.  The most common kind of
array is a vector -- a one dimensional array.  Zero dimensional arrays are a
reference to a single value.

```lisp
(vector 1 2 3)
```

Generally, functions in the standard library allow the programmer to specify
whether the output should be a vector or a list.

```lisp
(defun double (x) (* 2 x)
(map 'vector double '(1 2 3))      ; evaluates to (vector 2 4 6)
(map 'list double (vector 1 2 3))  ; evaluates to '(2 4 6)
```

### Sharing, copying and mutation

Lists and arrays are *references*.  Binding one to a second name does not copy
it, and neither does taking a sub-sequence: `slice`, `cdr` and `rest` return
**views** that share their elements with the source, much as slices of a Go
array do.

The library divides cleanly into functions that mutate and functions that do
not, and the mutating ones are spelled with a trailing `!`:

| Non-mutating (returns a new value) | Mutating (changes its argument) |
| ---------------------------------- | ------------------------------- |
| `append`, `append-bytes`           | `append!`, `append-bytes!`      |
| `assoc`, `dissoc`                  | `assoc!`, `dissoc!`             |
| `concat`, `insert-index`, `reverse`, `map`, `select` | `stable-sort` |

`stable-sort` is the exception to the naming rule: it has no `!` but it sorts
in place and returns the sequence it sorted.

Two rules follow, and together they cover essentially every surprise in this
area:

1. **Appending to a view never disturbs its source.**  A view cannot grow into
   the memory behind it; `append` and `append!` allocate instead.

   ```lisp
   (set 'v (vector 10 20 30 40))
   (append! (slice 'vector v 0 2) 999)
   v  ; still (vector 10 20 30 40)
   ```

2. **Mutating a view's own elements *is* visible through its source**, because
   those are the same elements.

   ```lisp
   (set 'v (vector 5 4 3 2 1))
   (stable-sort < (slice 'vector v 0 3))
   v  ; (vector 3 4 5 2 1) -- the source was sorted too
   ```

When you need a value nothing else can write through, copy it: `copy` takes
a deep copy with fresh backing at every level, and `concat` takes a one-level
copy of a single sequence.

```lisp
(set 'snapshot (concat 'vector (slice 'vector v 0 3)))
(set 'mine (copy some-nested-structure))
```

This matters most for **quoted literals**.  A literal is part of the program
text, not a fresh value made on each evaluation, so writing through one would
edit the program itself — permanently, for every later evaluation.  The
runtime refuses the write instead: `stable-sort` on a quoted literal (or on a
view sharing its storage), and the `(slice 'vector ...)` and
`(append 'vector ...)` forms that would wrap or write a literal's backing
array, raise the **`modify-literal-error`** condition:

```lisp
(stable-sort < '(3 1 2))
; error: cannot modify a program literal; take a (copy ...) first
```

It is an ordinary condition — `handler-bind` can name it, and
`ignore-errors` swallows it:

```lisp
(handler-bind ((modify-literal-error (lambda (c &rest args) 'refused)))
    (stable-sort < '(3 1 2)))  ; evaluates to 'refused
```

The remedy is the one the message names — take ownership with `copy` (or
build the value with `(list ...)` instead of quoting it) whenever
literal-derived data is going to be mutated:

```lisp
(stable-sort < (copy '(3 1 2)))          ; '(1 2 3)
(slice 'vector (copy '(1 2 3)) 0 2)      ; (vector 1 2)
(append 'vector (copy '(1 2 3)) 4)       ; (vector 1 2 3 4)
```

The empty list is the deliberate exception: functions like `cdr`, `rest` and
`keys` return the shared empty list, so the guarded functions accept an
empty literal-derived input — there is nothing in it to modify — and hand
back fresh storage.  `(stable-sort < (rest xs))` therefore behaves the same
however short `xs` is.

### Sorted Maps

A sorted map is a mapping between keys and values which ensures that key
traversal is always done in sorted, increasing order.  Sorted maps can contain
keys that are either symbols or strings.  Looking up values by key can be done
with either a string or a symbol, regardless which type was used to insert/set
the value originally.

```lisp
(let ((m (sorted-map 'alice 0 'bob 1 'carol 2)))
    (get m "carol"))    ; evaluates to 2
```

Maps are mutable values and can be updated with the `assoc!` function to
add/update a key-value pair to the map.

```lisp
(let ((m (sorted-map 'alice 0 'bob 1)))
    (assoc! m 'carol 2)
    (get m 'carol))     ; evaluates to 2
```

Analogously, the `dissoc!` function can be used to remove a key (and its
associated value) from the map.

```lisp
(let ((m (sorted-map 'alice 0 'bob 1)))
    (dissoc! m 'alice)
    (dissoc! m 'gary)   ; no-op
    m)      ; evaluates to (sorted-map 'bob 1)
```

There are also non-mutating versions of these functions, `assoc` and `dissoc`,
which merely return new sorted-map objects without modifying their arguments.

```lisp
(let* ((m0 (sorted-map 'alice 0 'bob 1))
       (m1 (dissoc m0 'alice))      ; does not change m0
       (m2 (assoc m1 'carol 2)))    ; does not change m1
    m2)      ; evaluates to (sorted-map 'bob 1 'carol 2)
```

It is a peculiarity of elps that `assoc` on `()` will return a new sorted-map
with the corresponding key and value set.
Similarly, `get` on `()` will return `()`.

A map remembers whether a key was written as a symbol or a string and prints
it back that way, so `keys` and the printed representation preserve the
original spelling.  That spelling is presentation only: it is not part of the
key's identity.  `get`, `key?`, `assoc` and `dissoc` all treat `'alice` and
`"alice"` as the same key, and `equal?` follows the same rule.

```lisp
(equal? (sorted-map 'alice 0) (sorted-map "alice" 0))  ; evaluates to true
(keys (sorted-map 'alice 0))                           ; evaluates to '('alice)
(keys (sorted-map "alice" 0))                          ; evaluates to '("alice")
```

### User-Defined Types

Programs can define new types with the `deftype` macro and instantiate types
with the `new` function.  New types have their names bound within the current
package.  User-defined types are represented as a "tagged-value" which
associates the type symbol with user data which can be any value.

```lisp
(deftype rect (height width)
    (sorted-map :height height
                :width width))
(set 'r (new rect 100 50))
(type r)           ; evaluates to 'user:rect
(type? rect r)     ; evaluates to true
(sorted-map? r)    ; evaluates to false
(tagged-value? r)  ; evaluates to true
(user-data r)      ; evaluates to (sorted-map :height 100 :width 50)
```

The core language only provides low-level functionality for defining and
working with custom types.  For the time being it is left it up to the
application to create more powerful abstractions over typed data.

### JSON numbers and integer precision

**By default `json:load-string`, `json:load-bytes` and `json:load-message`
decode every JSON number as a float, and silently round any integer larger
than 2^53.**  This is the single sharpest edge in the standard library, so it
gets a section of its own.

A float carries 53 bits of integer precision.  Above that the nearest
representable float is not the integer in the document, and the difference is
not reported anywhere:

```lisp
elps> (json:dump-string (json:load-string "9007199254740993"))
"9007199254740992"
elps> (json:dump-string (json:load-string "9223372036854775807"))
"9223372036854776000"
```

The first has drifted by 1.  The second — an int64 maximum, the shape of a
great many machine-generated identifiers — has drifted by 193, and is no
longer even an int64.

What makes this a footgun rather than a rounding error is that **nothing
signals**.  The corrupted value still compares `=` to the integer it was
supposed to be, so a program can read a corrupted identifier, check it against
the value it expected, match, and carry on:

```lisp
elps> (= 9007199254740993 (json:load-string "9007199254740993"))
true
elps> (= (json:load-string "9007199254740993") (json:load-string "9007199254740992"))
true
```

Two *different* documents are indistinguishable once loaded.  The only thing
that gives it away is the type, and only if you go looking:

```lisp
elps> (type (json:load-string "1"))
'float
```

#### Opting in with `:exact-integers`

Every `json:load*` function takes an `:exact-integers` keyword.  With it, a
JSON number **written as an integer** — no `.`, no exponent — decodes to a
lisp int holding its exact value:

```lisp
elps> (json:dump-string (json:load-string "9007199254740993" :exact-integers true))
"9007199254740993"
elps> (json:dump-string (json:load-string "9223372036854775807" :exact-integers true))
"9223372036854775807"
elps> (= (json:load-string "9007199254740993" :exact-integers true)
         (json:load-string "9007199254740992" :exact-integers true))
false
elps> (json:dump-string
        (json:load-string "{\"id\": 9007199254740993}" :exact-integers true))
"{\"id\":9007199254740993}"
```

The rule is **syntactic**: it looks at how the number is written, never at the
value it denotes.  That is deliberate.  A rule that depends only on the bytes
of a document gives every reader of those bytes the same answer, without
depending on shared floating-point behaviour — which is the property that
matters where this package decodes replicated state.

#### It changes more than the large numbers

`:exact-integers` is not a fix that applies only to the values that were
broken.  *Every* integer-shaped number in the document becomes an int, down to
`3`:

```lisp
elps> (type (get (json:load-string "{\"count\": 3}") "count"))
'float
elps> (type (get (json:load-string "{\"count\": 3}" :exact-integers true) "count"))
'int
```

So `(type x)`, `int?`, `float?`, `to-string` and anything that requires an
integer all change with it:

```lisp
elps> (int? (json:load-string "3" :exact-integers true))
true
elps> (float? (json:load-string "3" :exact-integers true))
false
elps> (to-string (json:load-string "9007199254740993"))
"9.007199254740992e+15"
elps> (to-string (json:load-string "9007199254740993" :exact-integers true))
"9007199254740993"
```

Some of that is code that starts working.  `nth` rejects a float index, so an
index read out of a JSON document is unusable today and usable under the
option:

```lisp
elps> (nth (vector "a" "b" "c") (json:load-string "1"))
error: lisp:nth: second argument is not an integer: float
elps> (nth (vector "a" "b" "c") (json:load-string "1" :exact-integers true))
"b"
```

Some of it is code that starts failing, which is the point of the next
section.

#### Oversized literals fail loudly

Under the option an integer literal too large for a lisp int is an error —
the catchable condition `json:integer-range-error` — instead of a rounded
float.  A document that loaded before can now fail:

```lisp
elps> (json:dump-string (json:load-string "9223372036854775808"))
"9223372036854776000"
elps> (json:load-string "9223372036854775808" :exact-integers true)
error: json:integer-range-error: json:load-string: json integer does not fit in a lisp int: 9223372036854775808
```

That is deliberate: a value elps cannot hold should say so rather than become
a different value.  Handle it like any other condition:

```lisp
elps> (handler-bind ([json:integer-range-error (lambda (c &rest args) (list c args))])
        (json:load-string "{\"id\": 9223372036854775808}" :exact-integers true))
'('json:integer-range-error '("json integer does not fit in a lisp int: 9223372036854775808"))
```

Malformed input is still catchable as `json:syntax-error` under the option, so
an existing `handler-bind` does not quietly stop firing.

#### Numbers with a fraction or an exponent are unchanged

The syntactic rule means anything written with a `.` or an `e` is untouched
and still decodes as a float, exactly as it does by default:

```lisp
elps> (type (json:load-string "1.5" :exact-integers true))
'float
elps> (type (json:load-string "1.0" :exact-integers true))
'float
elps> (type (json:load-string "1e2" :exact-integers true))
'float
elps> (json:dump-string (json:load-string "-0" :exact-integers true))
"-0"
```

`-0` is excluded on purpose so that it keeps re-serializing as `-0` rather
than as `0`.

#### Two edges worth knowing

**Exponent form normalizes on a dump.**  Because the rule is syntactic and
`json:dump` renders float text in its own normal form, a number written as
`100e7` loads as a float, dumps as plain digits, and a *re-read* of that
output makes it an int:

```lisp
elps> (type (json:load-string "100e7" :exact-integers true))
'float
elps> (json:dump-string (json:load-string "100e7" :exact-integers true))
"1000000000"
elps> (type (json:load-string "1000000000" :exact-integers true))
'int
```

The value is correct at every step and stable from the second read onwards;
what changed is the document, not the value, and every node reading the same
bytes still agrees.  Machine-generated JSON does not hit this — Go,
JavaScript and Python all render `1e9` as plain digits — so it is a footgun
for hand-written JSON inside a phylum, not a data-loss risk.

**An oversized literal is accepted when it is already canonical float text.**
This is the one exception to the range error above:

```lisp
elps> (type (json:load-string "10000000000000000000" :exact-integers true))
'float
elps> (json:dump-string (json:load-string "10000000000000000000" :exact-integers true))
"10000000000000000000"
elps> (json:dump-string 1e19)
"10000000000000000000"
```

elps renders every float between 2^63 and 1e21 as plain digits, so without
this exception a program holding an ordinary float of `1e19` could dump its
state and then be unable to read it back.  Nothing is discarded, so nothing is
hidden.  A literal that is *not* canonical float text still fails loudly —
`9223372036854775808` is rejected, as shown above.

#### `:string-numbers` still wins

When both keywords are given, `:string-numbers` takes precedence, so a caller
already using it sees no change at all:

```lisp
elps> (type (json:load-string "9007199254740993" :string-numbers true :exact-integers true))
'string
elps> (json:load-string "9007199254740993" :string-numbers true :exact-integers true)
"9007199254740993"
```

#### Why it is opt-in

Turning this on by default would change `(type x)` for every integer in every
document at once.  Two nodes running different elps versions would then
disagree about what the same bytes *mean* — one seeing `'float` where the
other sees `'int` — which is exactly the kind of divergence a replicated
system cannot absorb.  A per-call-site keyword lets a program migrate one call
at a time and observe each change; `json:use-exact-integers` flips the default
for a whole process once that migration is done.

**Practical advice.**  If a document can carry a number above 2^53 — an
account number, a nanosecond timestamp, a snowflake id — either turn
`:exact-integers` on at that call site, or carry the value as a JSON *string*
and convert it explicitly:

```lisp
elps> (to-int (get (json:load-string "{\"id\": \"9007199254740993\"}") "id"))
9007199254740993
```

## Packages

Packages allow namespace isolation for components of a code base as its
complexity increases.

### The Default Package

When the ELPS interpreter starts, all code executes in the `user` package.
This is the default package for any file that does not contain an
`(in-package ...)` declaration.  Files loaded via `load-file` inherit the
caller's current package context at the point of the load call.

### Basics

Packages are created/modified using the `in-package`
function, which changes the environment's working package.  Symbols bound using
`set`, `defun`, `defmacro`, etc will be bound in the working package.

```lisp
(in-package 'my-new-package)
(export 'my-special-function)
(defun my-special-function () (debug-print "something special"))
(set 'thing "something else")
(defun my-other-function () (debug-print thing))
```

Outside of the `my-new-package` package, the symbol `my-special-function` may
be bound to other values.  Any symbol defined inside a package may be
explicitly accessed by qualifying the symbol using the package name, regardless
of whether it was exported.

```lisp
(my-new-package:my-special-function)  ; prints "something special"
(my-new-package:my-other-function)    ; prints "something else"
```

NOTE:  Qualified access (`pkg:sym`) works for all symbols in a package, not
just exported ones.  Exports only control what `use-package` imports into the
caller's namespace — they do not restrict visibility.

NOTE:  Both halves of a qualified symbol must be [identifiers](#symbols).
Qualified access is another way to spell a name, not a way to introduce one
that could not be written unqualified, so `a:1` is rejected by the reader.

Scheme-like symbol bindings and assignment are also possible using the `define`
and `set!` operators.

```lisp
(define counter 0)  ; bind symbol 'counter to 0 initially
(define (count)
    (define old counter)
    (set! counter (+ counter 1))  ; increment the counter
    old)
(count)  ; evaluates to 0
(count)  ; evaluates to 1
```

NOTE:  While scheme-style function definitions are allowed using `define` the
argument declaration syntax is the same for all function definitions.

### Importing symbols

Symbols exported within a package may be imported to another package with the
`use-package` function.

```lisp
(in-package 'my-other-package)
(use-package 'my-new-package)
(my-special-function)           ; prints "something special"
```

In the above example, `my-special-function` becomes bound in
`my-other-package`.  But the symbol `my-other-function` remains unbound because
it was never exported.  If you really wanted to bind `my-other-function` it
would be possible by using a qualified symbol.

```lisp
(set 'my-other-function my-new-package:my-other-function)
```

**NOTE:** All packages use the "lisp" package, which defines all of the
language built-in functions and macros.  It is not currently possible to change
this behavior for packages defined by lisp code.  Embedded lisp instances are
able change this behavior globally -- something outside the scope of this
document.

### Standard library

A default lisp instance will have a standard set of packages available outside
of the language base "lisp" package.  There are packages for working with time,
json, stream encodings, math, etc.  These packages generally have simple, short
names.

```lisp
(set 'now (time:utc-now))
(debug-print (time:format-rfc3339 now))
```

### User packages

For packages outside of the standard library it in recommended that names use a
URL format for organizational clarity and to avoid package name collisions.

```lisp
(in-package 'example.com/faster-json)
(use-package 'example.com/faster-json/utils)
```

## Documentation

ELPS has built-in support for attaching documentation to functions, macros,
variables, and packages. The `elps doc` command queries this documentation
at the command line.

### Function and macro docstrings

Place one or more string literals at the beginning of a `defun` or `defmacro`
body, before any executable expressions. Consecutive strings are joined with
spaces. An empty string `""` inserts a paragraph break.

```lisp
(defun factorial (n)
  "Computes the factorial of a non-negative integer n."
  (if (<= n 1) 1 (* n (factorial (- n 1)))))

(defmacro when (test &rest body)
  "Evaluates body forms when test is truthy."
  ""
  "Like if but with no else branch and an implicit progn."
  (list 'if test (cons 'progn body) ()))
```

A body consisting entirely of strings (no executable expression after them)
is treated as a constant function returning a string, not as a docstring.

### Deprecating functions

A docstring paragraph beginning with `Deprecated:` marks a function or macro
as deprecated, the same convention Go doc comments use. The rest of that
paragraph tells callers what to use instead. `DEPRECATED:` is accepted too.

```lisp
(defun blend-paths (a b)
  "Combines two paths into one."
  ""
  "Deprecated: use join-paths instead."
  (join-paths a b))
```

A string literal cannot contain a line break, so the empty string above is what
opens the second paragraph — the same paragraph break docstrings use everywhere
else. Writing the whole docstring as one string with a `\n\n` escape works
identically:

```lisp
(defun blend-paths (a b)
  "Combines two paths into one.\n\nDeprecated: use join-paths instead."
  (join-paths a b))
```

Both forms read the same to `elps doc`, the linter and the language server: a
definition's documentation is the run of leading strings, joined.

The `deprecated` lint check reports every use of a deprecated symbol and quotes
the notice. It requires semantic analysis, so run the linter over a workspace:

```
$ elps lint --workspace . paths.lisp
warning: use of deprecated function 'blend-paths': use join-paths instead. (deprecated)
```

The declaration itself is never flagged, and neither is a use inside the body
of a definition that is itself deprecated — deprecated code may call deprecated
code. Suppress an individual use with a trailing comment:

```lisp
(blend-paths a b) ; nolint:deprecated
```

Editors connected to the language server strike deprecated uses through, show
a **Deprecated.** banner with the notice on hover, and mark the symbol
deprecated in completion lists.

### Variable and constant documentation

The `set` special operator accepts optional trailing strings after the value.
These are stored as the symbol's documentation in the current package.

```lisp
(set 'max-retries 3 "Maximum number of retry attempts.")
```

The `defconst` macro combines `set` and `export` in one step:

```lisp
(defconst pi-approx 3.14159
  "Approximate value of pi."
  "Good enough for most uses.")
```

### Package documentation

Pass trailing strings to `in-package` after the package name:

```lisp
(in-package 'my-utils
  "Utility functions for string and list manipulation.")
```

### Documenting Go builtins

Go-implemented builtins provide documentation through their definition.
Use `libutil.FunctionDoc` (for library packages) or the `langBuiltin`
struct (for core builtins) and pass a docstring as the last argument:

```go
// Library package function
libutil.FunctionDoc("my-fn", lisp.Formals("x", "y"), myFnImpl,
    `Computes something useful from x and y.`)

// Core builtin registration
RegisterDefaultBuiltin("my-builtin",
    lisp.Formals("arg"), myBuiltinImpl)
```

All builtins, macros, and exported symbols are required to have
documentation. The `elps doc -m` command checks for missing docstrings
and is typically run in CI.

### Viewing documentation

```
elps doc map              # Look up a single symbol
elps doc -p math          # List all exports in a package
elps doc --list-packages  # List all loaded packages
elps doc --guide          # Print this language reference
elps doc -m               # Check for missing documentation
```

From the REPL or within lisp code, use the `help` package:

```lisp
(help 'map)               ; Show docs for a symbol
(help-package 'math)      ; Show all exports in a package
(help-packages)           ; List all loaded packages
```

### Errors

Sometimes an improper invocation of a function will cause an error at runtime.
Programmers can also trigger errors from lisp code by using the `error`
built-in.

```lisp
(error 'my-type-of-error "Things are messed up right now")
```

The above code will unwind the function call stack, prematurely terminating any
functions executing or awaiting execution.  If there is no code to handle the
error it will eventually be returned to the application embedding the lisp
interpreter.  However lisp code has a few built-in ways to detect and deal with
errors before the entire pending evaluation is terminated.

When a function call is understood to trigger non-fatal error conditions of a
certain kind it may use the `handler-bind` built-in to intercept and correct
that type of error.  For an example, consider the above error in a broader
context.

```lisp
(defun double (x)
    (if (number? x)
        (* x 2)
        (error 'double-not-number "value to double is not a number")))

(handler-bind ((double-not-number (lambda (&rest e) e)))
    (double "abc"))
; handler-bind evaluates to '('double-not-number "value to double is not a number")
```

The handler-bind function works quite a bit like the concepts of raising
exceptions and handling/catching exceptions in other languages.  When the
expression inside handler-bind calls double, it raises an error condition.  The
error inside double terminates the function call as it unwinds the stack until
it hits the handler-bind.  The list of condition handlers in handler-bind
specifies a function to call when a 'double-not-number error is found.  That
handler function receives the arguments passed to the `error` built-in and
returns them in this scenario, producing the result `'('double-not-number
"value to double is not a number")` which is returned by handler-bind.

If a particular piece of lisp code should handle every kind of error with the
same handler function, the handler-bind function allows callers to specify a
handler for a special symbol `condition` which will match any error symbol.
From an object oriented it is a reasonable analogy to think of all error types
inheriting from the `condition` type.

```lisp
(handler-bind ( (double-not-number (lambda (&rest e) 0))
                (condition (lambda (&rest e) "ERROR DETECTED")))
    (double x))
```

In the above code double-not-number is handled by replacing the `(double x)`
function call with the value 0, while any other error (like integer overflow)
will be replaced with the string "ERROR DETECTED".

### Rethrowing Errors

Sometimes a handler needs to perform a side effect (such as logging or
recording metrics) but still allow the error to propagate.  Using `(apply
error c args)` to re-raise the error will lose the original stack trace.  The
`rethrow` function re-raises the current error being handled, preserving the
original stack trace and condition data.

```lisp
(handler-bind ((condition (lambda (c &rest args)
                            (debug-print "error detected:" c)
                            (rethrow))))
    (double "abc"))
; The error propagates with its original stack trace intact.
```

The `rethrow` function can only be called from within a handler-bind handler.
It takes no arguments.  Calling `rethrow` outside a handler signals an error.

Rethrown errors can be caught by outer handler-bind forms, allowing layered
error handling:

```lisp
(handler-bind ((condition (lambda (c &rest args) (list 'recovered c))))
    (handler-bind ((condition (lambda (c &rest args)
                                (debug-print "inner handler logging")
                                (rethrow))))
        (error 'my-error "data")))
; Evaluates to '('recovered 'my-error)
```

### Guaranteed Cleanup (`unwind-protect`)

`handler-bind`, `rethrow` and `ignore-errors` all *catch*.  None of them can
promise that a form runs on the way out.  `unwind-protect` is that promise:

```lisp
(unwind-protect protected-form cleanup-form ...)
```

It evaluates the protected form, then **always** evaluates the cleanup forms
— whether the protected form returned normally or signalled.  It returns the
protected form's value; cleanup values are discarded.

If you know `try`/`finally` from another language, this is `finally` with no
`catch` clause.  If you know Go, it is `defer`.

```lisp
(set 'in-step false)
(unwind-protect
  (progn (set! 'in-step true)
         (run-the-body))          ; may signal
  (set! 'in-step false))          ; runs regardless
```

Without it the flag leaks whenever the body signals and something upstream
recovers, and the *next* caller sees state left behind by a call that already
failed.

Note that `unwind-protect` takes exactly **one** protected form.  Wrap several
in a `progn`, as above.  The cleanup forms are an implicit `progn`, so they
need no wrapper.

#### It does not catch

The error is still live once the cleanup has run:

```lisp
(handler-bind ((condition (lambda (c &rest args) (list 'caught c))))
    (unwind-protect (error 'my-error "data")
                    (debug-print "cleanup ran")))
; prints "cleanup ran", then evaluates to '('caught 'my-error)
```

This is the difference from the `handler-bind` + `rethrow` workaround, which
needs the cleanup written twice — once in the handler and once on the success
path — and still misses `internal-panic`, which the catch-all `condition`
specifier does not match.

#### When a cleanup form itself signals

| protected form | cleanup form | what propagates |
| --- | --- | --- |
| returns normally | returns normally | the protected form's value |
| returns normally | signals | the **cleanup's** error |
| signals | returns normally | the **protected form's** error |
| signals | signals | the **cleanup's** error; the original is abandoned |
| signals `internal-panic` | either | the **`internal-panic`**, always |

A signalling cleanup form abandons the cleanup forms after it, the way an
error abandons the rest of a `progn`.

The first four rows are Common Lisp's behaviour, which Go's `defer` shares — a
panic raised inside a deferred function replaces the one already in flight.
The last row is the deliberate exception, and the next section explains it.

There is one final form of error handling, though its use is highly
discouraged.  If one finds themselves handling all errors and inserting a nil
value with an expression that looks like the following:

```lisp
(handler-bind ((condition (lambda (&rest e) ())))
    (call-function x y z))
```

The function `ignore-errors` will perform the same task.

```lisp
(ignore-errors (call-function x y z))  ; evaluates to () if any error occurs.
```

It is worth saying again, and louder, that **the use of ignore-errors is
greatly discouraged in general**.  If you must attempt to handle errors in lisp
code try to use handler-bind.

### Host Panics (`internal-panic`)

If Go code called during evaluation — a builtin or special operator supplied
by the application embedding the interpreter — panics, the interpreter
recovers the panic instead of letting it kill the host process, and turns it
into an error with the condition `internal-panic`.

That condition is deliberately **not** treated as an ordinary error.  A panic
means the host's Go code hit a bug (a nil dereference, an out-of-range index,
a failed invariant) and left its own data structures in an unknown state.
Letting a catch-all handler swallow it would make a genuine defect look
exactly like `(error 'my-condition "...")` and let the program keep running
on top of it.  So:

* `ignore-errors` does **not** suppress `internal-panic`; it propagates.
* the catch-all `condition` handler specifier does **not** match
  `internal-panic`.
* an error raised by an `unwind-protect` cleanup form does **not** mask an
  `internal-panic` from the protected form, even though it would replace any
  ordinary error there.  The cleanup still runs; the panic still wins.

The carve-out keys off a Go stack snapshot the interpreter attaches when it
recovers the panic — not off the condition name — so `(error 'internal-panic
"...")` written in lisp is an ordinary, containable condition.  Only a
genuine recovered panic escapes.  Embedders testing for one should use
`lisp.IsInternalPanic(v)` rather than comparing the condition name.

A handler that genuinely wants to intercept host panics must name the
condition explicitly:

```lisp
(handler-bind ((internal-panic (lambda (c &rest args)
                                 (debug-print "host code panicked:" args))))
    (risky-builtin))
```

The resulting error also carries the Go stack captured at the panic site, so
an embedder can identify the offending Go function.

## Execution Limits

ELPS bounds evaluation with five independent mechanisms: **context
cancellation**, **step limits**, **stack height limits**, an **evaluation
nesting limit** and a **tail-iteration limit**.  Context cancellation and step
limits are optional and impose negligible overhead when not configured; the
physical stack limit, the evaluation nesting limit and the tail-iteration
limit are on by default.

None of them bound *total* memory: `Runtime.MaxAlloc` caps the output size of
a single builtin call, not the sum across calls, so a loop that allocates many
smaller values is bounded only by whatever stops the loop.  A host that must
bound total memory has to do it outside the interpreter.

### Context Cancellation

Pass a Go `context.Context` to any of the `*Context` methods on `LEnv`:

```go
ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
defer cancel()

result := env.EvalContext(ctx, expr)
```

If the context is cancelled or its deadline expires during evaluation, a
`context-cancelled` condition is raised.  This can be caught in Lisp with
`handler-bind`:

```lisp
(handler-bind
    ((context-cancelled (lambda (err) (debug-print "timed out"))))
    (long-running-computation))
```

The context is normally observed *between* evaluation steps, so a builtin
that blocks for a long time inside a single step can outlive the deadline.
`time:sleep` is the exception that is checked explicitly: it waits on the
context as well as on its timer, so it wakes on cancellation and never sleeps
past the deadline, raising `context-cancelled` instead of returning nil when
it is cut short.  With no context configured, `time:sleep` sleeps for the
full duration it was given, however long that is.

### Step Limits

A step limit caps the number of evaluation steps in a **single top-level
evaluation**.  Each entry to `Eval`, each tail-recursion iteration, and each
macro re-expansion counts as one step.

```go
env := lisp.NewEnv(nil)
lisp.InitializeUserEnv(env, lisp.WithMaxSteps(1000000))
```

The counter is reset each time an exported entry point (`Eval`,
`EvalContext`, `EvalSExpr`, `FunCall`, `FunCallContext`, `SpecialOpCall`,
`MacroCall`, or any `Load*`) is entered from outside an evaluation.  Nested evaluation — a
builtin calling back into `Eval`, a tail-call loop, the forms evaluated by a
single `Load` — shares the enclosing budget and does not refill it.  Without
that reset, `WithMaxSteps(n)` would be a *lifetime* quota: once a long-lived
runtime had executed `n` steps in total, every later evaluation would fail
however small it was.

When the limit is reached, a `step-limit-exceeded` condition is raised.
Use `Runtime.Steps()` to read the current evaluation's usage,
`Runtime.TotalSteps()` for the lifetime total, and `Runtime.ResetSteps()`
to reset the current counter explicitly.

A step limit is the only mechanism here that bounds a loop which neither
recurses nor tail-calls — no stack limit can see such a loop.

It is not a time bound: a single step may run an arbitrary amount of work
inside a builtin.  **Context cancellation with a deadline is the only limit
here that measures elapsed time**, and it is what you want if the real
requirement is "give up after N seconds".

### Stack Height, Nesting and Tail-Call Limits

ELPS distinguishes four things that a naive "stack limit" conflates.

**Physical stack height** is the number of frames actually on the call
stack.  This is the memory guard: unbounded *non-tail* recursion exhausts the
Go goroutine stack, which aborts the whole process with a stack overflow that
no `handler-bind` can catch.  It is bounded by default
(`DefaultMaxPhysicalStackHeight`, 25000) at roughly an order of magnitude
below the measured crash threshold, and exceeding it produces an ordinary,
catchable ELPS error.  Override with
`lisp.WithMaximumPhysicalStackHeight(n)`; 0 disables the check, which is not
recommended.

It bounds *frames*, not evaluation depth — see below.

**Evaluation nesting** is how deeply the evaluator recurses into itself, which
is the true measure of Go stack consumed.  It is not the same as stack height
and is not implied by it: a call's arguments are evaluated *before* the call's
frame is pushed, so

```lisp
(identity (identity (identity ... 1)))
```

recurses through the whole evaluator while the physical stack height stays at
**zero**.  That is the exact shape the physical limit exists to stop and the
one shape it cannot see.  Nesting does not have to be written out by hand
either — a recursive macro generates it at expansion time from an integer, so
neither the parser's depth limit nor the source size bounds it:

```lisp
(defmacro nest (n)
  (if (<= n 0) 1 (quasiquote (identity (nest (unquote (- n 1)))))))
(nest 800000)   ; without the nesting limit: fatal error: stack overflow
```

Nesting is bounded by default (`DefaultMaxEvalNesting`, 100000), several times
below the measured crash threshold and well above the nesting an ordinary
recursion reaches before it hits the 25000-frame physical limit.  Exceeding it
raises a catchable `eval-nesting-exceeded` condition:

```lisp
(handler-bind ((eval-nesting-exceeded (lambda (c &rest args) 'too-deep)))
    (nest 800000))
```

Override with `lisp.WithMaxEvalNesting(n)`; a negative value disables the
check, which re-exposes the host process to an unrecoverable stack overflow.

**Tail-call iterations** count the turns of a tail-recursive loop.  Tail
calls run in constant stack space, so no stack-height limit can bound a
runaway loop; this is the limit that does.  It is bounded by default
(`DefaultMaxTailIterations`, 1,000,000) purely as a backstop against a loop
that never terminates.  Override with `lisp.WithMaxTailIterations(n)`; 0
disables the check.

It is **not** a time bound.  A million turns of a trivial body costs a few
seconds, but turns say nothing about the work done per turn — a body that
conses onto a list or calls any O(n) builtin can run for minutes inside the
same turn budget:

```lisp
(defun grow (n acc) (if (= n 0) (length acc) (grow (- n 1) (cons n acc))))
(grow 60000 ())   ; 60,000 turns — well under the backstop — but ~17s
```

A step limit does not help here either, since an O(n) builtin call is one
step.  To bound elapsed time, use a context deadline (below).

**Logical (virtual) stack height** is the physical height plus every frame
elided by tail-call optimization.  It is a useful stack-trace diagnostic but
a poor limit, because its unit is *elided frames*, not loop turns: one turn
of a tail loop adds the length of the elided terminal chain, which is 2 for a
trivial body and more when the body nests terminal forms more deeply.  The
same numeric bound therefore permits a different number of iterations
depending on the shape of the loop.  It is **disabled by default**
(`DefaultMaxLogicalStackHeight`, 0).  Callers who specifically want it can
opt in with `lisp.WithMaximumLogicalStackHeight(n)`.

**Sleep length** is the one limit whose unit is wall clock rather than work.
Every limit above counts something the interpreter *does* — steps, frames,
turns, bytes, nesting — and a sleeping goroutine does none of them, so
`time:sleep` was bounded by none of them at once and
`"9223372036854775807ns"` blocked for roughly 292 years.

A single sleep is capped at one hour (`lisp.DefaultMaxSleep`).  Over that
raises `sleep-limit-exceeded` **immediately**, without sleeping:

```lisp
(time:sleep (time:parse-duration "2h"))
; sleep-limit-exceeded: sleep of 2h0m0s exceeds the maximum 1h0m0s

(time:sleep (time:parse-duration "2h") :max (time:parse-duration "3h"))
; sleeps, because the caller said so explicitly
```

`:max` makes an unusually long sleep visible at the call site instead of
being an accident of arithmetic.  A host that does not trust the program can
set a ceiling `:max` cannot exceed with `lisp.WithMaxSleep(d)` — program
source may relax the default, only the host may relax the ceiling.

A sleep that would outlast the context deadline is also refused immediately,
with `context-cancelled`, rather than blocking until the deadline first: it
could not have completed, so waiting would only burn the budget the caller
has left to react in.  A sleep already under way is still interrupted if the
context is cancelled.

Note this bounds one call, not their sum — N sleeps just under the cap still
block for N times the cap.  A context deadline is what bounds total elapsed
time.

Because tail calls are optimized, a correctly written tail-recursive loop
runs in constant stack space for an unbounded number of iterations:

```lisp
(defun spin (n) (if (= n 0) 'done (spin (- n 1))))
(spin 500000)   ; constant stack space; evaluates to 'done
```

### Available Context Methods

| Method | Purpose |
|--------|---------|
| `EvalContext` | Evaluate an expression |
| `LoadContext` | Load from an `io.Reader` |
| `LoadFileContext` | Load a source file |
| `LoadStringContext` | Load from a string |
| `LoadLocationContext` | Load with explicit name/location |
| `FunCallContext` | Invoke a function |

Each method threads the context through the internal evaluation chain.
The older non-context methods (`Eval`, `Load`, etc.) continue to work
but are deprecated.  Builtins can access the current context via
`env.Context()`.
