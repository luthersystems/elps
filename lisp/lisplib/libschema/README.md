# `libschema` - Type validation for ELPS

### What is it?
`libschema` provides basic type validation for ELPS, allowing formal structs and enums to be emulated
amongst other things. It is strongly inspired by [Clojure's schema library](https://github.com/plumatic/schema) 
and the Javascript library [yup](https://github.com/jquense/yup).

The library is exported by default under the package name `s` and all functions and types should be prefixed as such.

### How do I use it?

Types are defined using the `s:deftype` keyword and validations are performed by calling `s:validate` on a value.

#### Validating

We can validate that a value meets the required type by calling `s:validate` on it with the required value:
```lisp
(set 'x "hello")
(assert-nil (s:validate s:string x))
```

If the value does not have the required type, an error of type `"wrong-type"` will be returned. If a constraint (see below)
fails, an error of type `"failed-constraint"` will be the result.

#### Defining types

To define a type, specify the name for your type, followed by a base type name (see below) and then, optionally, any
constraints you wish to enforce.

At the simplest level this can be referencing an inbuilt type, for example

```lisp
(s:deftype "mytype" s:string)
```

This type will require that the supplied value is a string. Not very useful in itself as this is the same as validating against 
`s:string`. But let's say we want our string to have a length of at least eight characters. We can do
```lisp
(s:deftype "mytype" s:string (s:lengt 8))
```
Or, more usefully, if we want to define an enum, we can specify a list of permitted values like this
```lisp
(s:deftype "title" s:string (s:in ("Mr","Mrs","Miss","Ms","Mx","Dr","Prof")))
```

Where this really comes into its own is when we start defining more complex types. We can specify the keys, and their 
types that a sorted map should have:
```lisp
(s:deftype "mymap" s:sorted-map 
    (s:has-key "first-name" s:string) 
    (s:has-key "surname" s:string) 
    (s:may-have-key "middle-name" s:string)
)
```
We now have a map type that must have a string in the `first-name` and `surname` keys and, if the `middle-name` key is
set, it must also contain a string. If we wish to constrain the keys that can be set to this list, we can wrap the key 
definitions in a call to `s:no-more-keys` like this:
```lisp
(s:deftype "mymap" s:sorted-map 
    (s:no-more-keys 
        (s:has-key "first-name" s:string) 
        (s:has-key "surname" s:string) 
        (s:may-have-key "middle-name" s:string)
    )
)
```
Now, if we tried to validate a map with the key `random-wrong-data` set, we would receive an error.

We can also use our title enum from before so that if a title is set, it must be from the options we specified:
```lisp
(s:deftype "mymap" s:sorted-map 
    (s:no-more-keys 
        (s:has-key "first-name" s:string) 
        (s:has-key "surname" s:string) 
        (s:may-have-key "middle-name" s:string)
        (s:may-have-key "title" title)
    )
)
```

We can also perform conditional validation. Let's say we wanted to check if someone is over 18 if they are marked as an
adult (a silly example I know, but trying to keep it simple here). We can use the `s:when` predicate to return an error 
if someone under 18 is marked as an adult like this:
```lisp
(s:deftype "age-type" s:int (s:positive))
(s:deftype "mymap" s:sorted-map 
    (s:no-more-keys 
        (s:has-key "first-name" s:string) 
        (s:has-key "surname" s:string)
        (s:has-key "age" 'age-type)
        (s:has-key "is-adult" s:bool) 
        (s:may-have-key "middle-name" s:string)
        (s:may-have-key "title" 'title)
    )
    (s:when "age" (s:lt 18) "is-adult" s:false)
)
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
|`s:bool`|boolean|
|`s:error`|ELPS error|
|`s:fun`|A function|
|`s:sorted-map`|sorted map|

### Constraints

* `(s:in value[ value2 valuen...])` 
Requires the value to be one of those specified as arguments to the function.


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
  

* `(s:negative)`
  Requires the value to be less than zero.
  

* `(s:of type)`
  Requires the members of an array to be of type `type`.
  

* `(s:has-key name[ type [type2 typeN]])`
  Requires a map to have the key `name` set, optionally requiring the value therein to be of type `type` (or `type2` ... `typeN`).
  

* `(s:may-have-key name[ type [type2 typeN]])`
  If a map has the key `name` set, optionally require the value therein to be of type `type` (or `type2` ... `typeN`). 
  You may wish to use this without a type set when using `no-more-keys`.
  

* `(s:no-more-keys field-constraint[ field-constraint2 field-constraintN])`
  Require that a map has no keys other than those set in the contained field constraints.
  

* `(s:when field-name condition other-field other-condition[ other-condition2 other-conditionN]`
  Applied to a sorted map, when the field `field-name` passes condition `condition`, apply `other-condition` and any 
  subsequent conditions to field `other-field`.
  

* `(s:is-true)`
  Require the value to be `true`


* `(s:is-false)`
  Require the value to be `false`


* `(s:is-truthy)`
  Require the value to be equivalent to `true`. Strings must be non-empty and not equal to `"false"`, arrays must be non-
  empty etc.


* `(s:is-falsy)`
  Require the value to be equivalent to `false`. Literally `(s:not (s:is-truthy))`
  
### Gotchas

* Type names are only symbols after they're defined. They're strings when you call `deftype`.
* Subsidiary conditions must be defined inside their own sexpr. It's `(s:not (s:in "x" "y"))` so `(s:not s:is-true)`
  isn't going to work.
* Handling validation failure smoothly is best achieved by wrapping in `handler-bind` and looking for the error values
  from the validation library. In particular you should not bind to `condition` as you will miss `bad-args` errors that 
  show errors in your type definition at run time.



