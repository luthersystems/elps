# Embedding lisp

The elps project is intended to be used as an embedded language, allowing
programs to be extended easily and dynamically.

## Usage

To initialize a new environment set its Reader and load the packages you that
want to be accessible.

```go
env := lisp.NewEnv(nil)
env.Reader = parser.NewReader()
lerr := lisp.InitializeUserEnv(env)
if !lerr.IsNil() {
   log.Panicf("initialization error: %v", lerr) 
}
lerr = lisplib.LoadLibrary(env)
if !lerr.IsNil() {
    log.Panicf("stdlib error: %v", lerr)
}
```

InitializeUserEnv loads the base language package, lisp.  The remaining
packages in the standard library are loaded through the
`lisplib.LoadLibrary(env)` function call.  If there are packages in the
standard library which should not be accessible use an alternative function or
write your own library loader using the LoadLibrary source code as a reference.

### Evaluating expressions

Lisp code can be 'loaded' (parsed and evaluated) using the `env.Load` family of
functions.

```go
ret := env.LoadString("code.lisp", lispcode)
if ret.Type == lisp.LError {
    // handle an error
}
```

Instead of repeatedly parsing code, the TextLoader function can return a
function that efficiently loads parsed expressions multiple times.

```go
fn, err := lisp.TextLoader(parser.NewReader(), "code.lisp", strings.NewReader(lispcode))
if err != nil {
    // handle parse error
}
lerr := fn(env)
if lerr.Type != LError {
    // handle execution error
}
```

## Writing Functions

Programs embedding elps can write functions in Go which can be loaded into
packages, bound under a given symbol.

## Testing Functions

Use go package bitbucket.org/luthersystems/elps/elpstest and the lisp package
`testing` to write tests for custom packages.  See the standard library's tests
for examples of how to use these packages together.

TODO -- example

## Working with lisp types

All lisp values are represented in Go as the LVal type.  The lisp type of a
value can determined by checking the LType value stored in the LVal.Type field.

In general, a function **MUST NOT** modify fields of an LVal.  There are cases
where functions are "destructive" and modify storage referenced by certain data
types.  However even these functions **MUST NOT** modify top-level top level
LVal fields in order to maintain soundness of computation.  For example, a
destructive function may be defined that modifies LVal.Cells[0] by re-assigning
it to a new value.

```go
v.Cells[0] = Int(-v.Cells[0].Int)
```

On the other hand, it would be invalid behavior to instead set the value of
`v.Cells[0].Int` to a new value.  Such a modification may cause side effects in
unexpected places.

### Primitive types

String values (those with Type equal to `LString`) and Symbols (those with Type
`LSymbol`) store their data in the LVal.Str field.  Floats and Ints store their
data in the LVal.Float and LVal.Int fields respectively.

Lists are stores as SExpr types. Though typically, when returning a list from a
function, a quoted SExpr is desired.  Quoted SExprs can be conveniently created
using the `QExpr()` function.

```go
return QExpr([]*lisp.LVal{lisp.Int(1), lisp.Int(2), lisp.Float(3.0)})
```

### Boolean values

The only false value in the elps language is nil `()`, an empty expression.  An
LVal can be checked as nil by calling its `IsNil()` method.  Instead of calling
`IsNil()` to determine the falsehood of a value the `True` function will
determine a value's truth value.

```go
ok := env.Eval(lisp.SExpr([]*lisp.LVal{"ok?"}))
if lisp.True(ok) {  // equivalent to !ok.IsNil()
    fmt.Println("ok")
}
```

### Maps

TODO

### Conversion functions

Additionally, types can be converted from an LVal into a native Go type using
the functions GoString, GoInt, GoFloat, etc.

```lisp
(set 'data "hello")
```

An application could extract the string "hello" using the following code.

```go
s, _ := GoString(env.GetGlobal(lisp.Symbol("data")))
if s != "hello" {
    panic(s)
}
```

These functions for converting types to native values are experimental in
nature and their semantics could change.

## Operating on Go types

To pass a native Go value to lisp code wrap it in a call to `lisp.Native()` so
the value can be put into an S-expression.

```go
    lisptime := lisp.Native(time.Now())
    expr := SExpr([]*lisp.LVal{"my-function", lisptime})
```

You can then write functions which operate on the value by unboxing the
`Native` field of the corresponding argument LVal.

```go
func builtinPrintTime(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
    lisptime := args.Cells[0]
    if lisptime.Type != lisp.LNative {
        return env.Errorf("argument is not a time: %v", lisptime.Type)
    }
    t, ok := lisptime.Native.(time.Time)
    if !ok {
        return env.Errorf("argument is not a time: %v", lisptime)
    }
    fmt.Println(t.Format(time.RFC3339))
    return lisp.Nil()
}
```

Lisp code can operate on primitive Go types and structs using the golang
package.

```go
type AppData struct {
    Person struct {
        Name string
    }
}
```

Given the above struct definition, when an AppData object is wrapped with
`lisp.Native()` lisp code can extract exported struct fields using functions in
the golang package.

```lisp
(defun print-name (app-data)
    (let* ( (person (golang:struct-field app-data "Person"))
            (go-name (golang:struct-field app-data "Name"))
            (name (golang:string go-name)))
        (debug-print (string:format "My name is {}" name))))
```
