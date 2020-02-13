# Function Reference

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

Converts string digits ([0-9]+) and floats to integers. For floats the mantissa
is simply disgarded. Allows integer to pass through.

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

Returns the list after the first item.

```Lisp
elps> (cdr '("head" "body" "tail"))
'("body" "tail")
```

## `rest`

Returns the sequence (list, vector) after the first item.

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