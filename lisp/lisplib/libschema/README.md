# `libschema` - Type validation for ELPS

### What is it?
`libschema` provides basic type validation for ELPS, allowing formal structs and enums to be emulated
amongst other things. It is strongly inspired by [Clojure's schema library](https://github.com/plumatic/schema) 
and the Javascript library [yup](https://github.com/jquense/yup).

The library is exported by default under the package name `s` and all functions and types should be prefixed as such.

### How do I use it?

Validators are created by calling `s:make-validator`, which returns the
validator without binding it anywhere; bind it to a name yourself with the
core `set`. Validations are then performed by calling `s:validate` on a
value.

> `libschema` used to also offer `s:deftype`, which bound its validator as a
> global under the caller's own name for you. It was removed
> ([#736](https://github.com/luthersystems/elps/issues/736)): a prefixed
> library builtin (`s:...`) must never write into the caller's package, only
> the caller's own `set`/`set!`/`defun` may do that. `s:make-validator` plus
> `set` is a two-line equivalent, e.g. `(s:deftype "title" s:string (s:in
> "Mr" "Ms" "Dr"))` becomes `(set 'title (s:make-validator "title" s:string
> (s:in "Mr" "Ms" "Dr")))`, as used throughout this document.

#### Validating

We can validate that a value meets the required type by calling `s:validate` on it with the required value:
```lisp
(set 'x "hello")
(set 'mystring (s:make-validator "mystring" s:string))
(assert-nil (s:validate mystring x))
```

If the value does not have the required type, an error of type `"wrong-type"` will be returned. If a constraint (see below)
fails, an error of type `"failed-constraint"` will be the result.

A validator does not need a permanent binding at all -- `let` gives it a
scoped, temporary one:

```lisp
(set 'x "hello")
(let ([v (s:make-validator "mystring" s:string)])
    (assert-nil (s:validate v x)))
```

#### Defining types

To define a type, call `s:make-validator` with a name for your type, a base
type name (see below), and then, optionally, any constraints you wish to
enforce; bind the result with `set` so you can refer to it later.

At the simplest level this can be referencing an inbuilt type, for example

```lisp
(set 'mytype (s:make-validator "mytype" s:string))
```

This type will require that the supplied value is a string. Not very useful in itself as this is the same as validating against 
`s:string`. But let's say we want our string to have a length of at least eight characters. We can do
```lisp
(set 'mytype (s:make-validator "mytype" s:string (s:lengt 8)))
```
Or, more usefully, if we want to define an enum, we can specify a list of permitted values like this
```lisp
(set 'title (s:make-validator "title" s:string (s:in "Mr" "Mrs" "Miss" "Ms" "Mx" "Dr" "Prof")))
```

`s:make-validator` also works directly with tagged-values (user-defined types
created with the core language `deftype` macro): pass it the typedef itself
in place of a name, and it builds a validator that checks the tagged value's
user-data.

```lisp
(deftype abc (s) (to-string s))
(set 'abc-validator (s:make-validator abc s:string (s:in "a" "b" "c")))
```

When `s:make-validator` is passed the typedef `abc` it automatically creates a
tagged-value validator which validates the type's string contents.

If the structure of a tagged-value is known but its exact type is not then the
`s:tagged-value` type can be used when calling `s:make-validator` with a string
type name instead of a typedef.

```lisp
(set 'abc-like (s:make-validator "abc-like" s:tagged-value s:string (s:in "a" "b" "c")))
(deftype mystring (s) (to-string s))
(s:validate abc-like (new mystring "b"))
```

The `s:make-validator` function works with any data type, not just
tagged-values.  It can be used to create scoped validators with a limited
lifetime.

```lisp
(let ([v (s:make-validator "sequence-elemeent" s:sorted-map)])
    (map '() #^(s:validate v %) sequence))
```

#### Complex type schemas

So far only simple type constraints have been discussed.  Where this really
comes into its own is when we start defining more complex types. We can specify
the keys, and their types that a sorted map should have:
```lisp
(set 'mymap (s:make-validator "mymap" s:sorted-map 
    (s:has-key "first-name" s:string) 
    (s:has-key "surname" s:string) 
    (s:may-have-key "middle-name" s:string)
))
```
We now have a map type that must have a string in the `first-name` and `surname` keys and, if the `middle-name` key is
set, it must also contain a string. If we wish to constrain the keys that can be set to this list, we can wrap the key 
definitions in a call to `s:no-more-keys` like this:
```lisp
(set 'mymap (s:make-validator "mymap" s:sorted-map 
    (s:no-other-keys 
        (s:has-key "first-name" s:string) 
        (s:has-key "surname" s:string) 
        (s:may-have-key "middle-name" s:string)
    )
))
```
Now, if we tried to validate a map with the key `random-wrong-data` set, we would receive an error.

We can also use our title enum from before so that if a title is set, it must be from the options we specified:
```lisp
(set 'mymap (s:make-validator "mymap" s:sorted-map 
    (s:no-other-keys 
        (s:has-key "first-name" s:string) 
        (s:has-key "surname" s:string) 
        (s:may-have-key "middle-name" s:string)
        (s:may-have-key "title" title)
    )
))
```

We can also perform conditional validation. Let's say we wanted to check if someone is over 18 if they are marked as an
adult (a silly example I know, but trying to keep it simple here). We can use the `s:when` predicate to return an error 
if someone under 18 is marked as an adult like this:
```lisp
(set 'age-type (s:make-validator "age-type" s:int (s:positive)))
(set 'mymap (s:make-validator "mymap" s:sorted-map 
    (s:no-other-keys 
        (s:has-key "first-name" s:string) 
        (s:has-key "surname" s:string)
        (s:has-key "age" age-type)
        (s:has-key "is-adult" s:bool) 
        (s:may-have-key "middle-name" s:string)
        (s:may-have-key "title" title)
    )
    (s:when "age" (s:lt 18) "is-adult" (s:is-false))
))
```
You'll find a lot more examples in the [`libschema_test.lisp`](./libschema_test.lisp) file in this directory and a reference of all the available 
types and constraints below.

### Types
The following inbuilt types are available within the library:

|Name|Usage|
|---|---|
|`s:int`|integer|
|`s:float`|floating point|
|`s:number`|any number|
|`s:string`|string|
|`s:bytes`|binary array (ie golang `[]byte`)|
|`s:any`|any ELPS value|
|`s:array`|array|
|`s:list`|list (`()` is the empty list)|
|`s:sequence`|list or array, as a Go slice accepts either|
|`s:nil`|`()`, for a field that may be nil|
|`s:bool`|boolean (the symbols `true` and `false`; the strings `"true"` and `"false"` are rejected)|
|`s:tagged-value`|tagged-value|
|`s:error`|ELPS error|
|`s:fun`|A function|
|`s:sorted-map`|sorted map|

### Constraints

* `(s:in value[ value2 valuen...])` 
Requires the value to be one of those specified as arguments to the function.


* `(s:regexp pattern)`
Requires the value to match the supplied pattern. Any regular expression that can be parsed by go is acceptable - see https://github.com/google/re2/wiki/Syntax for syntax.
  

* `(s:len length)` 
  Requires the value to have the specified length.
  

* `(s:lengt length)`
  Requires the value to have more than the specified length.


* `(s:lengte length)`
  Requires the value to have equal to or more than the specified length.


* `(s:lenlt length)`
  Requires the value to have less than the specified length.


* `(s:lenlte length)`
  Requires the value to have equal to or less than the specified length.
  

* `(s:gt required)`
  Requires the value to be greater than `required`.


* `(s:lt required)`
  Requires the value to be less than `required`.


* `(s:gte required)`
  Requires the value to be greater than or equal to `required`.


* `(s:lte required)`
  Requires the value to be less than or equal to `required`.
  

* `(s:positive)`
  Requires the value to be greater than zero.

  NaN satisfies none of the numeric constraints (`s:gt`, `s:gte`, `s:lt`, `s:lte`, `s:positive`,
  `s:negative`), and a NaN bound such as `(s:gt (/ 0.0 0.0))` is an error when the constraint is
  built.
  

* `(s:negative)`
  Requires the value to be less than zero.
  

* `(s:of type)`
  Requires the members of an array or a list to be of type `type`.


* `(s:map-of key-type value-type)`
  Requires a sorted map whose every key is of type `key-type` and every value of type `value-type`.


* `(s:describe validator)`
  Returns the definition of a validator as a sorted map (`op`, and as it applies `name`, `type`,
  `key`, `values` and `children`), so a tool can read a schema back. A validator built in Go with
  `NewValidator` describes as `{"op": "opaque"}`.
  

* `(s:has-key name[ type [type2 typeN]])`
  Requires a map to have the key `name` set, optionally requiring the value therein to be of type `type` (or `type2` ... `typeN`).
  

* `(s:may-have-key name[ type [type2 typeN]])`
  If a map has the key `name` set, optionally require the value therein to be of type `type` (or `type2` ... `typeN`). 
  You may wish to use this without a type set when using `no-more-keys`.
  `name` is a string and is looked up as a string, exactly as `s:has-key` looks its key up. Before
  [#325](https://github.com/luthersystems/elps/issues/325) it was looked up as a symbol, which a
  map decoded by `json:load-string` rejects outright — so on JSON-derived maps the constraint
  silently behaved as though the key were always absent. A string key matches symbol-keyed entries
  of a literal `sorted-map` too, so nothing is lost.
  

* `(s:no-other-keys field-constraint[ field-constraint2 field-constraintN])`
  Require that a map has no keys other than those set in the contained field constraints.
  

* `(s:when field-name condition other-field other-condition[ other-condition2 other-conditionN]`
  Applied to a sorted map, when the field `field-name` passes condition `condition`, apply `other-condition` and any 
  subsequent conditions to field `other-field`.
  

* `(s:is-true)`
  Require the value to be the symbol `true` (not the string `"true"`)


* `(s:is-false)`
  Require the value to be the symbol `false` (not the string `"false"`)


* `(s:is-truthy)`
  Require the value to be equivalent to `true`. Strings must be non-empty and not equal to `"false"`, arrays, maps and
  bytes must be non-empty, numbers must be positive.


* `(s:is-falsy)`
  Require the value to be equivalent to `false`. Literally `(s:not (s:is-truthy))`
  
### Record types

A record type is a constructor for objects with typed fields and methods
(luthersystems/elps#895). Bind it with `set` at the top level of a file.

```lisp
(set 'order
  (s:make-record "order"
    (s:has-key "id" s:string)
    (s:has-key "lines" (s:map-of s:string s:int))
    (s:may-have-key "note" s:any)
    (s:method add-line (o sku qty)
      (s:set! o 'lines (assoc (o 'lines) sku qty)))
    (s:method qty (o)
      (foldl (lambda (n sku) (+ n (get (o 'lines) sku))) 0 (keys (o 'lines))))
    :check (lambda (o) (when (> (o 'qty) 100) (error 'order-too-big (o 'qty))))))

(set 'o (order :id "o1" :lines (sorted-map)))  ; or (order a-sorted-map)
(o 'add-line "a" 2)
(o 'qty)                                        ; 2
(o 'id)                                         ; "o1": every field has a getter
```

| Form | Use |
|------|-----|
| `(s:make-record NAME FIELD... METHOD... [:check FN])` | Build a type. A field is `s:has-key` or `s:may-have-key`. |
| `(s:method NAME (RECV ARGS...) BODY...)` | A method. Allowed only as a form of `s:make-record`. RECV is the object. |
| `(s:defmethod TYPE NAME (RECV ARGS...) [:replace true] BODY...)` | Add a method to a type after it is built. |
| `(s:set! RECV 'FIELD VALUE)` | Change a field. Checks VALUE against the field's validator. |
| `(s:fields OBJ)` | A copy of the object's fields as a sorted map. |
| `(s:record? V)` | True when V is an object of a record type. |

Rules:

* The first formal of a method is its receiver, the object it is called
  on, as in a Go method. You choose its name. `(o 'add-line "a" 2)`
  passes `o` as the receiver and `"a" 2` as the other arguments.
* The constructor checks each field and then runs `:check`. An unknown
  field is a `bad-arguments` error.
* `s:set!` checks only the field it changes. `:check` runs when the
  outermost method call on the object returns. A call that a method or
  `:check` makes on the same object runs without a check, so a method may
  pass through a state `:check` refuses, and `:check` may call methods.
* A method call is atomic: see "Method calls are atomic" below.
* A call to an unknown operation raises `unknown-operation`.
* A type takes methods only from the package that defines it. A method
  name must not be a field name.
* A type is a validator: `(s:has-key "order" order)` accepts only objects
  of that type with valid fields.
* Helper functions are ordinary `defun`s, or locals of a closure around
  the type: `(set 'order (labels (...) (s:make-record ...)))`.
* Use `s:any` for a field that takes any value.

#### Method calls are atomic

A method call works like a database transaction. If the method raises an
error, or `:check` refuses the object after it, the object's fields go back
to their state before the call. A caller that catches the error still has
a valid object.

```lisp
(set 'order
  (s:make-record "order"
    (s:has-key "lines" (s:map-of s:string s:int))
    (s:method add-line (o sku qty)
      (s:set! o 'lines (assoc (o 'lines) sku qty))   ; write the line
      (unless (in-stock? sku qty)
        (error 'out-of-stock sku)))))                 ; then fail

(set 'o (order :lines (sorted-map)))
(ignore-errors (o 'add-line "a" 5))   ; raises out-of-stock
(o 'lines)                            ; (sorted-map): the write was undone
```

Details:

* The rollback covers the outermost call on the object. A method that
  calls another method on its receiver is one call: a failure anywhere
  undoes all of it.
* The fields are saved on the first `s:set!` of a call, so a read-only
  method costs nothing.
* Go types work the same way: a Go method or `Default` that fails, or that
  breaks `Check`, leaves the fields as they were.
* The rollback restores which value each field holds. It does **not** undo
  a change made in place to a field's value, such as `(assoc! (o 'lines)
  "a" 5)`: the saved map holds the same `lines` map, so it changes too.
  Build a new value and `s:set!` it instead. The lint check
  `record-field-mutation` reports each in-place change.
* A phylum endpoint that raises an error already rolls back its whole
  transaction. The rollback matters for code that catches an error and
  keeps using the object.

Saving objects. An object saves only its fields. Its type saves as the name
of the global that holds it, and its methods are never saved. Add
`libschema.RecordCodec` to the durable registry. A load runs no Lisp, so
call `libschema.ValidateLoaded` on the loaded values before Lisp uses them.
It checks each object against the type the loading program binds to the
saved name.

Go types. `libschema.NewRecordType` defines a record type in Go with Go
methods and Go checks. Its objects work with `s:fields` and `s:record?`.
A Go type is sealed: `s:defmethod` and `s:set!` refuse it. Add
`(*RecordType).Codec()` to the durable registry to save its objects.

Lint checks: `record-set-outside-method`, `record-type-binding`,
`record-field-mutation`, `record-unknown-member`, `record-check-writes`,
`record-method-arity`, `record-defmethod-type` and `record-plain-set`
(see `docs/lint-checks.md`).

### Gotchas

* `name` is always a string when you call `s:make-validator` (or a typedef,
  for a tagged-value); it only becomes a symbol once you bind the returned
  validator to one with `set`.
* Constraints are ordinary, evaluated arguments. Write `(s:gt 1)` and refer to a defined type by its bare symbol
  (`(s:has-key "age" age-type)`); do not quote either. Before
  [#737](https://github.com/luthersystems/elps/issues/737) libschema evaluated a quoted form such as `'(s:gt 1)`, or looked
  up a quoted symbol such as `'age-type`, a second time and accepted it. Now any constraint argument that is not a schema
  constraint (type names such as `s:string` in their usual positions aside) is refused with `bad-arguments` when the validator is built.
* Subsidiary conditions must be defined inside their own sexpr. It's `(s:not (s:in "x" "y"))` so `(s:not s:is-true)`
  isn't going to work.
* Handling validation failure smoothly is best achieved by wrapping in `handler-bind` and looking for the error values
  from the validation library. In particular you should not bind to `condition` as you will miss `bad-args` errors that 
  show errors in your type definition at run time.



