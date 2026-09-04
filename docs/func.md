# Function Reference

## `debug-print`

Prints the supplied value(s).

```Lisp
elps> (set 'test (sorted-map "K" "V"))
(sorted-map "K" "V")
elps> (debug-print "Hello" test)
"Hello" (sorted-map "K" "V")
()
```

## `json:dump-string`

JSON encodes a value, returning the JSON as a string.

```Lisp
elps> (json:dump-string (sorted-map "K" "V"))
"{\"K\":\"V\"}"
```

## `json:dump-bytes`

JSON encodes a value, returning the JSON as bytes.

```Lisp
elps> (json:dump-bytes (sorted-map "K" "V"))
#<bytes 123 34 75 34 58 34 86 34 125>
elps> (to-string (json:dump-bytes (sorted-map "K" "V")))
"{\"K\":\"V\"}"
```

## `json:load-string`

Parses a JSON string into ELPS values.  JSON objects become sorted-maps, JSON
arrays become arrays, and strings, numbers, booleans and `null` map naturally.

```Lisp
elps> (json:load-string "{\"K\":\"V\"}")
(sorted-map "K" "V")
elps> (json:load-string "[1, 2.5, true, null]")
(vector 1 2.5 true ())
```

**WARNING**: by default *every* JSON number decodes as a float, and a float
carries only 53 bits of integer precision, so an integer larger than 2^53 is
silently rounded on the way in.

```Lisp
elps> (json:dump-string (json:load-string "9007199254740993"))
"9007199254740992"
elps> (json:dump-string (json:load-string "9223372036854775807"))
"9223372036854776000"
```

Nothing reports this.  The rounded value still compares `=` to the integer it
was meant to be, so a program can read a corrupted identifier, check it against
the value it expected, match, and carry on.  Pass `:exact-integers true` to
decode integer-shaped numbers as ints instead:

```Lisp
elps> (json:dump-string (json:load-string "9007199254740993" :exact-integers true))
"9007199254740993"
elps> (json:dump-string (json:load-string "9223372036854775807" :exact-integers true))
"9223372036854775807"
```

The option changes more than the large numbers, and a document that used to
load can now raise `json:integer-range-error`.  Read [JSON numbers and integer
precision](lang.md#json-numbers-and-integer-precision) before turning it on.

The `:string-numbers` keyword decodes every JSON number as a string holding its
literal text.  It takes precedence over `:exact-integers` when both are given.

## `json:load-bytes`

Like `json:load-string`, but takes bytes rather than a string.  It accepts the
same `:exact-integers` and `:string-numbers` keywords, and carries the same
number-precision [warning](#json-load-string).

```Lisp
elps> (json:load-bytes (to-bytes "{\"K\":\"V\"}"))
(sorted-map "K" "V")
elps> (json:load-bytes (to-bytes "9007199254740993"))
9.007199254740992e+15
elps> (json:load-bytes (to-bytes "9007199254740993") :exact-integers true)
9007199254740993
```

## `json:load-message`

Parses a native JSON message object (`json.RawMessage`, as produced by
`json:dump-message`) into ELPS values.  It accepts the same keywords as
`json:load-string` and carries the same number-precision
[warning](#json-load-string).

```Lisp
elps> (json:load-message (json:dump-message 9007199254740993))
9.007199254740992e+15
elps> (json:load-message (json:dump-message 9007199254740993) :exact-integers true)
9007199254740993
```

## `json:use-exact-integers`

Sets the default `:exact-integers` mode for the JSON serializer, so every later
load that passes no explicit keyword uses it.  Returns nil.

```Lisp
elps> (progn (json:use-exact-integers true) (type (json:load-string "3")))
'int
```

This is a process-wide switch and it changes the type of every number in every
document the process loads.  Prefer the per-call `:exact-integers` keyword
unless you have read [JSON numbers and integer
precision](lang.md#json-numbers-and-integer-precision) and want the whole
process migrated at once.

## `string:lowercase`

Convert letters in a string to lowercase

```lisp
elps> (string:lowercase "ABC123")
"abc123"
```

## `string:uppercase`

Convert letters in a string to uppercase

```lisp
elps> (string:uppercase "abc123")
"ABC123"
```

## `string:split`

Split a string on a substring

```lisp
elps> (string:split "hello world" " ")
'("hello" "world")
elps> (string:split "hello" " ")
'("hello")
```

## `string:join`

Join a list of strings with a separator

```lisp
elps> (string:join (list "hello" "world") " ")
"hello world"
```

## `string:repeat`

Repeat a string a given number of times

```lisp
elps> (string:repeat "1234" 2)
"12341234"
```

## `string:trim-space`

Remove leading and trailing whitespace from a string

```lisp
elps> (string:trim-space "\thello world \n")
"hello world"
```

## `string:trim`

Trim leading and trailing characters in a cutset from a string

```lisp
elps> (string:trim "${foo}" "${}")
"foo"
```

## `string:trim-left`

Trim leading characters in a cutset from a string

```lisp
elps> (string:trim-left "${foo}" "${}")
"foo}"
```

## `string:trim-left`

Trim trailing characters in a cutset from a string

```lisp
elps> (string:trim-right "${foo}" "${}")
"${foo"
```

## `to-string`

Converts primitive values to their string representation.

```Lisp
elps> (to-string 1.0)
"1"
elps> (to-string 1.01)
"1.01"
elps> (to-string true)
"true"
```

## `to-bytes`

Converts a string value to bytes.

```Lisp
elps> (to-bytes "ABC123")
#<bytes 65 66 67 49 50 51>
```

## `to-int`

Converts string digits ([0-9]+) and floats to integers. For floats the
fractional part is discarded. Allows integer to pass through.

```Lisp
elps> (to-int "42")
42
elps> (to-int 42)
42
elps> (to-int 42.9)
42
elps> (to-int "4.2")
// ERROR
```

## `to-float`

Converts strings and integers to floats. Allows floats to pass through.

```Lisp
elps> (to-float "42.2")
42.2
elps> (to-float 42)
42
elps> (to-float -1.23456e+1)
-12.3456
```

## `car`

Returns the first element in a list.

```Lisp
elps> (car '("one" "two" "three"))
"one"
```

## `cdr`

Returns the list after the first item.  Like `slice`, the result is a view
that shares its elements with the source — see
[Slices are views, not copies](#slices-are-views-not-copies).

```Lisp
elps> (cdr '("head" "body" "tail"))
'("body" "tail")
```

## `rest`

Returns the sequence (list, vector) after the first item.  Like `slice`, the
result is a view that shares its elements with the source — see
[Slices are views, not copies](#slices-are-views-not-copies).

```Lisp
elps> (rest (vector "one" "two" "three" "four"))
'("two" "three" "four")
```

## `first` / `second`

```Lisp
elps> (first '("one" "two" "three"))
"one"
elps> (second '("one" "two" "three"))
"two"
```

## `nth`

Gets the nth element of a sequence.

```Lisp
elps> (nth '(1 2 3 4) 0)
1
elps> (nth '(1 2 3 4) 3)
4
elps> (nth '(1 2 3 4) 4)
()
```

## `dotimes`

Iterate a specified number of times.  The first argument is a list that
declares an indexing symbol, the number of iterations, and an optional result
expression.  The remaining arguments for the iteration body and will be
evaulated the specified number of times.

```lisp
elps> (set 'v (vector))
(vector)
elps> (dotimes (n 5) (append! v (* 2 n)))
()
elps> (dotimes (n (length v)) (debug-print n (nth v n)))
0 0
1 2
2 4
3 6
4 8
()
elps> (dotimes (n 5 (length v)) (append! v (* 2 n)))
10
```

## `map`

Applies a function to a sequence of values.

```Lisp
elps> (defun cube (v) (* v v v))
()
elps> (map 'vector cube '(1 2 3 4 5 6))
(vector 1 8 27 64 125 216)
elps> (map 'vector (lambda (v) (* v v)) '(1 2 3 4 5 6))
(vector 1 4 9 16 25 36)
```

## `foldl`

Reduces a sequence using an applicator function and with an accumulator,
evaluating from the left.

```Lisp
elps> (defun add (x y) (+ x y))
()
elps> (foldl add 0 '(1 2 3))
6
elps> (foldl add 10 '(1 1 1))
13
elps> (foldl (lambda (acc v) (assoc! acc (to-string v) v)) (sorted-map) '(1 2 3))
(sorted-map "1" 1 "2" 2 "3" 3)
elps> (foldl - 0 '(1 2 3))
-6 ; (((0 - 1) - 2) - 3)
```

## `foldr`

Reduces a sequence using an applicator function and with an accumulator,
evaluating from the right.

```Lisp
elps> (foldr - 0 '(1 2 3))
2 ; (1 - (2 - (3 - 0)))
```

## `compose`

Returns a function that is the combination of two functions, with the first
function using the product of the second function as its input.

```Lisp
elps> (set 'fn (compose (lambda (a) (+ a 1)) (lambda (b) (+ b 2))))
(lambda (b) (lisp:funcall (lambda (a) (+ a 1)) (lisp:apply (lambda (b) (+ b 2)) b ())))
elps> (fn 1)
4
```

## `unpack`

Unpack is a special case of apply and is more cumbersome. Instead of using
unpack it is better to use apply and funcall.

```Lisp
elps> (unpack (lambda (x y) (+ x y)) '(2 7))
9
elps> (unpack (lambda (x y) (+ x y)) '(2 7 6))
stdin:1: _fun16: invalid number of arguments: 3
```

## `flip`

Returns a function with the parameter order reversed, the input function must
have two parameters.

```Lisp
elps> (set 'fn (flip (lambda (x y) (concat 'string x y))))
(lambda (x y) ((lambda (x y) (concat 'string x y)) y x))
elps> (fn "hello" "world")
"worldhello"
```

## `assoc`

Associates a new key and value to a map, returning a copy without mutating
the source map.

```Lisp
elps> (set 'test (sorted-map))
(sorted-map)
elps> (assoc test "1" 1)
(sorted-map "1" 1)
elps> test
(sorted-map)
```

## `assoc!`

Associates a new key and value to a map, mutating the source map in-place.

```Lisp
elps> (set 'test (sorted-map))
(sorted-map)
elps> (assoc! test "1" 1)
(sorted-map "1" 1)
elps> test
(sorted-map "1" 1)
```

## `dissoc`

Dissociate a value from a map via a key, returning a copy without mutating
the source map.

```Lisp
elps> (set 'test (sorted-map "A" 1 "B" 2))
(sorted-map "A" 1 "B" 2)
elps> (dissoc test "A")
(sorted-map "B" 2)
elps> test
(sorted-map "A" 1 "B" 2)
```

## `dissoc!`

Dissociate a value from a map via a key, mutating the source map in-place.

```Lisp
elps> (set 'test (sorted-map "A" 1 "B" 2))
(sorted-map "A" 1 "B" 2)
elps> (dissoc! test "A")
(sorted-map "B" 2)
elps> test
(sorted-map "B" 2)
```

## `get`

Gets a map value by key.

```Lisp
elps> (set 'test (sorted-map "A" 1 "B" 2))
(sorted-map "A" 1 "B" 2)
elps> (get test "A")
1
```

## `keys`

Returns the key values of a map.

```Lisp
elps> (set 'test (sorted-map "A" 1 "B" 2))
(sorted-map "A" 1 "B" 2)
elps> (keys test)
'("A" "B")
```

## `key?`

Checks if the a key exists in a map.

```Lisp
elps> (set 'test (sorted-map "A" 1 "B" 2))
(sorted-map "A" 1 "B" 2)
elps> (key? test "X")
false
elps> (key? test "B")
true
```

## `concat`

Concatenates values.

```Lisp
elps> (concat 'string "A" "B" "C")
"ABC"
elps> (concat 'list '("A" "B" "C") '(1 2 3))
'("A" "B" "C" 1 2 3)
```

## `copy`

Returns a deep copy of a value that shares no storage with it. Lists, vectors,
sorted-maps and bytes are rebuilt with fresh backing, recursively, so mutating
the copy at any depth is never observable through the original. Function
values are shared by reference rather than copied, and so is a native value
unless its payload implements `NativeCloner`, in which case the copy holds a
clone; strings and numbers are immutable values. Sharing between values inside
the input is preserved in the copy, including cycles — and so is sharing of a
sorted-map, bytes or cloneable-native payload between two values (what
`(quasiquote (unquote a))` produces): the copy rebuilds such a payload once
and both copied values point at the one rebuilt payload. Sharing of a list's
or vector's backing array (what `cdr`, `rest` and `slice` produce) is NOT
preserved; those come back with separate storage.

`copy` is how lisp code takes ownership of data whose provenance it does not
control. Its result is always mutable, even when the input is (or is derived
from) a quoted program literal, which is otherwise read-only shared storage
(see [sealed-ast.md](sealed-ast.md)). It replaces the one-level `(concat 'list
x)` idiom, hand-rolled recursive copiers, and `json:dump-string`/`json:load`
round-trips used as deep copies.

```Lisp
elps> (set 'lit '(3 1 2))
'(3 1 2)
elps> (stable-sort < (copy lit))    ; sorts a private copy in place
'(1 2 3)
elps> lit                           ; the literal is untouched
'(3 1 2)
elps> (set 'orig (sorted-map "k" (vector 1 2)))
(sorted-map "k" (vector 1 2))
elps> (append! (get (copy orig) "k") 99)
(vector 1 2 99)
elps> orig                          ; deep: the nested vector was copied too
(sorted-map "k" (vector 1 2))
```

### Two things `copy` does not do

**It does not copy functions, so it does not copy an object's methods.** A
lambda captures *bindings*, not values: a lambda carried into the copy keeps
reading and writing the containers its defining scope holds. Copying a
map-of-lambdas "object" therefore produces a copy whose own methods operate on
the original.

```Lisp
(defun make-obj ()
  (let ([state (vector 0)])
    (sorted-map "bump"  (lambda () (append! state 1))
                "state" state)))

(set 'm (make-obj))
(set 'c (copy m))
((get c "bump"))        ; the COPY's method...
(get c "state")         ; => (vector 0)    ...did not touch the copy
(get m "state")         ; => (vector 0 1)  ...it mutated the original
```

To get an independent object, call its constructor again (`(make-obj)`); there
is no primitive that copies a captured environment.

**It does not preserve backing-array sharing.** `cdr`, `rest` and `(slice
'list …)` return a value that shares the original's backing array, so an
in-place mutation through one is visible through the other. That aliasing does
not survive a copy — the two values land on separate backing arrays.

```Lisp
(set 'l (list 9 3 1 2))
(stable-sort < (cdr l))   ; sorts in place, through the shared backing
l                         ; => '(9 1 2 3)   the head sees it

(set 'l2 (list 9 3 1 2))
(set 'c (copy (list l2 (cdr l2))))
(stable-sort < (nth c 1)) ; => '(1 2 3)     in the copy they are independent
(nth c 0)                 ; => '(9 3 1 2)   the head does NOT see it
```

The copy has strictly fewer accidental aliases than the original, which is the
safe direction, but code must not rely on such an alias surviving `copy`.

### No `sealed?`

There is deliberately no `sealed?` predicate to pair with `copy`. Whether a
value came from program text is not the same question as whether you own it —
an unsealed value can still be aliased by another binding, a container, or a
closure. Code that intends to mutate data it did not construct takes a `copy`
unconditionally.

## `insert-index`

Inserts a value into a sequence at a specific index.

```Lisp
elps> (set 'test '(1 2 3))
'(1 2 3)
elps> (insert-index 'list test 0 999)
'(999 1 2 3)
elps> (insert-index 'list test 42 123)
stdin:1: lisp:insert-index: index out of bounds
```

## `stable-sort`

Performs a stable sort on a list using a predicate. The last argument can
optionally be a function that takes the key and returns the comparison value.
Mutates the list in-place.

Two consequences of "in-place" are worth stating outright, because neither is
visible at the call site:

- Sorting a **slice, `cdr` or `rest` view** sorts that region of the source
  too, since a view shares its elements — see
  [Slices are views, not copies](#slices-are-views-not-copies).
- Sorting a **quoted literal** rewrites the program's own text, and it stays
  rewritten for the life of the process:

  ```Lisp
  elps> (defun probe () (let ([lit '(3 1 2)]) (stable-sort < lit) lit))
  ()
  elps> (probe)
  '(1 2 3)
  elps> (probe)  ; the literal in the function body is now sorted
  '(1 2 3)
  ```

Sort a copy — `(stable-sort < (concat 'list lit))` — whenever the argument is a
literal or a view you do not own.

```
elps> (set 'test '(1 2 3))
'(1 2 3)
elps> (stable-sort > test)
'(3 2 1)
elps> (set 'test '("C" "B" "A"))
'("C" "B" "A")
elps> (set 'lookup (sorted-map "A" 9 "B" 7 "C" 8))
(sorted-map "A" 9 "B" 7 "C" 8)
elps> (stable-sort > test (lambda (key) (get lookup key)))
'("A" "C" "B")
```

## `insert-sorted`

Inserts a value in its sort position.

```Lisp
elps> (set 'test '(1 2 4))
'(1 2 4)
elps> (insert-sorted 'list test < 3)
'(1 2 3 4)
```

## `search-sorted`

Search uses binary search to find and return the smallest index i in [0, n) at
which f(i) is true, assuming that on the range [0, n), f(i) == true implies
f(i+1) == true.

```Lisp
elps> (set 'test '(1 2 4))
'(1 2 4)
elps> (search-sorted (length test) (lambda (i) (= 4 (nth test i))))
2
```

## `select`

Selects values matching the predicate.

```Lisp
elps> (select 'list int? '("A" 1 "B" 2 "C" 3))
'(1 2 3)
```

## `reject`

Rejects values matching the predicate.

```Lisp
elps> (reject 'list int? '("A" 1 "B" 2 "C" 3))
'("A" "B" "C")
```

## `zip`

Zips one or more lists, composing a list of values from each input list. Tuples
length is restricted to the smallest input list length.

```Lisp
elps> (zip 'list '(1 2 3))
'('(1) '(2) '(3))
elps> (zip 'list '(1 2 3) '("A" "B" "C") '(4 5 6))
'('(1 "A" 4) '(2 "B" 5) '(3 "C" 6))
elps> (zip 'list '(1 2 3) '("A" "B" "C") '(4 5))
'('(1 "A" 4) '(2 "B" 5))
```

## `make-sequence`

Generates a sequence, with an optional step value.

```Lisp
elps> (make-sequence 0 10)
'(0 1 2 3 4 5 6 7 8 9)
elps> (make-sequence 0 10 2)
'(0 2 4 6 8)
elps> (make-sequence 0 10 4)
'(0 4 8)
```

## `format-string`

Creates a string using format placeholders and values. Use `{}` for
sequential substitution or `{0}`, `{1}`, etc. for positional. Use `{{`
and `}}` for literal braces. Sequential and positional styles cannot be
mixed.

```Lisp
elps> (format-string "Hello {}, {} you?" "World" "how are")
"Hello World, how are you?"

elps> (format-string "{1} {0}!" "world" "Hello")
"Hello world!"

elps> (format-string "{0} said {{hello}} to {0}" "Alice")
"Alice said {hello} to Alice"
```

## `reverse`

Reverses a sequence.

```Lisp
elps> (reverse 'list '(1 2 3))
'(3 2 1)
elps> (reverse 'list (reverse 'list '(1 2 3)))
'(1 2 3)
```

## `slice`

Returns the sub-slice of a sequence.  The slice will be converted to the type
specifier.  The function works with `list`, `vector`, `bytes` and `string`
values.

```Lisp
elps> (set 'test (make-sequence 0 10))
'(0 1 2 3 4 5 6 7 8 9)
elps> (slice 'list test 3 6)
'(3 4 5)
elps> (slice 'vector test 3 6)
(vector 3 4 5)
elps> (slice 'string "hello" 1 4)
"ell"
elps> (slice 'bytes "hello" 1 4)
#<bytes 101 108 108>
elps> (slice 'vector "hello" 1 4)
(vector 101 108 108)
```

### Slices are views, not copies

For `list`, `vector` and `bytes` sources the result **shares its elements with
the source**, the way a Go slice does.  Reading is always safe, and appending
to a slice is safe — a slice cannot grow into its source, so `append` and
`append!` on the slice leave the source alone:

```Lisp
elps> (set 'v (vector 10 20 30 40))
(vector 10 20 30 40)
elps> (set 'view (slice 'vector v 0 2))
(vector 10 20)
elps> (append! view 999)
(vector 10 20 999)
elps> v  ; the source is untouched
(vector 10 20 30 40)
```

What sharing still means is that an operation which mutates the slice's own
elements *in place* is visible through the source.  `stable-sort` is the one to
watch, because it sorts in place and returns the sequence it sorted:

```Lisp
elps> (set 'v (vector 5 4 3 2 1))
(vector 5 4 3 2 1)
elps> (stable-sort < (slice 'vector v 0 3))
(vector 3 4 5)
elps> v  ; the first three elements of v were sorted too
(vector 3 4 5 2 1)
```

Take a copy with `concat` when you need a snapshot that nothing can write
through — `concat` always allocates:

```Lisp
elps> (set 'v (vector 5 4 3 2 1))
(vector 5 4 3 2 1)
elps> (set 'copy (concat 'vector (slice 'vector v 0 3)))
(vector 5 4 3)
elps> (stable-sort < copy)
(vector 3 4 5)
elps> v  ; unchanged
(vector 5 4 3 2 1)
```

This matters most when the source is a quoted literal, because a literal is
part of the program text rather than a fresh value — sorting one in place
changes it for the rest of the process.  `(concat 'list ...)` is the idiom for
taking a literal you intend to mutate.

`(slice 'string ...)` is exempt: strings are immutable, so a string slice can
never be written through.

## `list`

Returns a list compose of the supplied parameters.

```Lisp
elps> (list "A" 123 456 "B" '(0 1 2))
'("A" 123 456 "B" '(0 1 2))
```

## `vector`

Creates a vector (array) value.

```Lisp
elps> (vector)
(vector)
elps> (vector 1 "2" 'three)
(vector 1 "2" 'three)
```

## `append`

Appends to the sequence, returning a new sequence and leaving the source
untouched.  The result never shares storage with the source, so appending to
the same source twice gives two independent results.

```Lisp
elps> (set 'test (vector 1 2))
(vector 1 2)
elps> (append 'vector test 3)
(vector 1 2 3)
elps> test
(vector 1 2)
elps> (set 'a (append 'vector test 3))
(vector 1 2 3)
elps> (set 'b (append 'vector test 4))
(vector 1 2 4)
elps> a  ; unaffected by the append that produced b
(vector 1 2 3)
```

Because the source is never written to, `append` must copy, which makes it
O(n) in the length of the source.  Building a sequence with repeated
`(set 'v (append 'vector v x))` is therefore quadratic — use `append!` to
accumulate.

> Before ELPS fixed issue #373, `append` could grow into spare capacity left
> in the source and overwrite a result it had already returned — in the
> example above, `a` would have become `(vector 1 2 4)`.  Code written against
> that behaviour will now see correct values and one extra copy per append.

## `append!`

Appends to the vector, mutating the source vector in-place.  This is the
accumulator: it grows in amortised constant time and does not copy.  Use it in
loops, and use `append` when the source must not change.

```Lisp
elps> (set 'test (vector 1 2))
(vector 1 2)
elps> (append! test 3)
(vector 1 2 3)
elps> test
(vector 1 2 3)
```

## `append-bytes`

Appends to the byte vector, returning new bytes and leaving the source
untouched.  As with `append`, the result never shares storage with the source.

```
elps> (set 'test (to-bytes "hello world"))
#<bytes 104 101 108 108 111 32 119 111 114 108 100>
elps> (append-bytes test "!")
#<bytes 104 101 108 108 111 32 119 111 114 108 100 33>
elps> (to-string test)
"hello world"
```

## `append-bytes!`

Appends to the byte vector, mutating the source vector in-place.

```
elps> (set 'test (to-bytes "hello world"))
#<bytes 104 101 108 108 111 32 119 111 114 108 100>
elps> (append-bytes! test "!")
#<bytes 104 101 108 108 111 32 119 111 114 108 100 33>
elps> (to-string test)
"hello world!" ; Note exclamation mark
```

## `aref`

Gets the array element at the given index (counted from zero).

```Lisp
elps> (aref (vector "A" "B" "C") 0)
"A"
elps> (aref (vector "A" "B" "C") 0)
"A"
elps> (aref (vector "A" "B" "C") 2)
"C"
elps> (aref (vector "A" "B" "C") 3)
; Out of bounds error
```

## `all?`

Test all items in a sequence match against a function.

```Lisp
elps> (all? (lambda (x) (> x 0)) '(1 2 3 4 5))
true
elps> (all? (lambda (x) (> x 0)) '(1 2 3 4 -5))
false
```

## `any?`

Test if any item in a sequence matches against a function.

```Lisp
elps> (any? string? '(1 2 3 4 5))
false
elps> (any? string? '(1 2 "3" 4 -5))
true
```

## `true?`

Checks if a value is truthy.

```Lisp
elps> (true? ())
false
elps> (true? "")
true
elps> (true? false)
false
elps> (true? true)
true
```

## `deftype`

Defines a new type in the current package -- binding its name to a typedef.
Along with the type name `deftype` takes a list of constructor arguments
followed by constructor expressions which can reference constructor arguments
and are executed by the type constructor as if wrapped in a `progn`.

```lisp
elps> (deftype myobject (x) x)
'user:myobject
elps> myobject
#{lisp:typedef '('user:myobject (lambda (x) x))}
```

## `new`

Instantiate a type that was previously defined with `deftype`.  If given a
typedef its constructor is called and the resulting user data is placed in a
tagged-value.  Otherwise, if given a symbol, `new` looks for a package-level
symbol bound to a typedef.

```lisp
elps> (deftype myobject (x) x)
user:myobject
elps> (new myobject "hello")
#{user:myobject "hello"}
elps> (new 'myobject "hello")
#{user:myobject "hello"}
elps> (in-package 'other)
()
elps> (new user:myobject "hello")
#{user:myobject "hello"}
elps> (new 'user:myobject "hello")
#{user:myobject "hello"}
```

## `tagged-value?`

Returns a boolean value true iff the given value is a tagged-value (a
user-defined type)

```lisp
elps> (tagged-value? "hello")
false
elps> (deftype myobject (x) x)
'user:myobject
elps> (tagged-value? myobject)
true
elps> (tagged-value? (new myobject "hello"))
true
```

## `user-data`

Returns the user data associated with the tagged-value.  It is an error if the
argument is not a tagged-value.

```lisp
elps> (deftype myobject (x) x)
'user:myobject
elps> (user-data (new myobject "hello"))
"hello"
elps> (user-data "hello")
stdin:1: lisp:user-data: argument is not a tagged value: string
Stack Trace [1 frames -- entrypoint last]:
  height 0: stdin:1: lisp:tagged-value
```

## `type`

Returns the type of its argument as a symbol.  If the argument is a
tagged-value then the user-defined type name is returned.

```lisp
elps> (type 1)
int
elps> (type '())
list
elps> (type (vector 1 2 3))
array
elps> (type (sorted-map "a" 1))
sorted-map
elps> (type "abc")
string
elps> (type (to-bytes "abc"))
bytes
elps> (deftype emptyobject ())
'user:emptyobject
elps> (type emptyobject)
'lisp:typedef
elps> (type (new emptyobject))
'user:emptyobject
```

## `type?`

Returns a boolean value true iff the given type specifier matches the given
value.  A type specifier must either by a symbol or a typedef.  Symbols must be
qualified with package names in order to match user-defined types and avoid
confusion between primitive and user-defined data types.

```lisp
elps> (type? 'list '())
true
elps> (type? 'sorted-map '())
false
elps> (type? 'string "abc")
true
elps> (deftype myobject (x) x)
'user:myobject
elps> (type? 'user:myobject (new myobject "hello"))
true
elps> (type? myobject (new myobject "hello"))
true
elps> (type? 'myobject (new myobject "hello")) ; user types must be referenced with qualified names
false
```

# Type Checking

```Lisp
elps> (nil? ())
true
elps> (nil? false)
false
elps> (list? '())
true
elps> (list? '(1 2 3))
true
elps> (list? "hello")
false
elps> (sorted-map? (sorted-map "hello" "world"))
true
elps> (sorted-map? "hello world")
false
elps> (array? (vector 1 2))
true
elps> (array? (list 1 2))
false
elps> (vector? (vector 1 2))
true
elps> (vector? (list 1 2))
false
elps> (bool? true)
true
elps> (bool? ())
false
elps> (number? 1.0)
true
elps> (number? 100)
true
elps> (number? "100")
false
elps> (int? 1)
true
elps> (int? -1)
true
elps> (int? -1.0)
false
elps> (float? 1)
false
elps> (float? 1.0)
true
elps> (symbol? 'hi)
true
elps> (symbol? "hi")
false
elps> (string? "")
true
elps> (string? "hi")
true
elps> (string? 'hi)
false
elps> (bytes? "hello")
false
elps> (bytes? (to-bytes "hello"))
true
```

## empty?

Determine if a sequence or string is empty.

```Lisp
elps> (empty? "")
true
elps> (nil? "")
false
elps> (empty? (vector))
true
elps> (empty? (list))
true
elps> (nil? (vector))
false
elps> (nil? (list))
true
```
