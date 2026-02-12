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

Use go package github.com/luthersystems/elps/elpstest and the lisp package
`testing` to write tests for custom packages.  See the standard library's tests
for examples of how to use these packages together.

`elpstest` piggybacks on the Go `testing` standard library.
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

Use `l := lisp.SortedMap()` to construct an empty sorted map LVal. Numeric
keys are not supported. Symbol keys are coerced to string to avoid programming
errors causing symbol and string keys with equal string values from existing in
the same map.

Use `l.MapSet(k,v)` to set keys on the map, which returns the mutated map.
`v` must be an LVal.

Use `l.MapGet(k)` to return the LVal corresponding to `k`.

Use `l.MapKeys()` to return the LVal list of keys in hte map.

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

## Tooling for Embedders

ELPS ships three CLI tools (`lint`, `doc`, `fmt`). The `lint` and `doc` tools
expose Go APIs so embedders can wire in their own runtime environment, making
Go-provided bindings visible to static analysis and documentation.

### Linting

The `lint` package provides `LintConfig` and `LintFiles` for running the linter
with embedder-provided symbols. Pass the embedder's `PackageRegistry` to make
Go-registered builtins visible to semantic analysis (undefined-symbol,
builtin-arity, etc.).

```go
import (
    "github.com/luthersystems/elps/lint"
)

// env is the embedder's configured *lisp.LEnv with custom packages loaded.
l := &lint.Linter{Analyzers: lint.DefaultAnalyzers()}
diags, err := l.LintFiles(&lint.LintConfig{
    Workspace: workspaceDir,
    Registry:  env.Runtime.Registry,
}, files)
```

Without the `Registry` field, the linter only knows about stdlib symbols and
will report false positives for embedder-provided bindings.

### Documentation

The `libhelp` package provides rendering functions that accept any `*lisp.LEnv`.
Embedders that have their own configured environment can use these directly:

```go
import (
    "github.com/luthersystems/elps/lisp/lisplib/libhelp"
)

// env is the embedder's configured *lisp.LEnv with custom packages loaded.

// Look up documentation for an embedder-provided symbol.
libhelp.RenderVar(os.Stdout, env, "cc:storage-put")

// List all exports in an embedder package.
libhelp.RenderPkgExported(os.Stdout, env, "cc")

// List all packages including embedder packages.
libhelp.RenderPackageList(os.Stdout, env)

// Check for missing documentation across all packages.
missing := libhelp.CheckMissing(env)
for _, m := range missing {
    fmt.Printf("  %-10s  %s\n", m.Kind, m.Name)
}
```

For convenience, `lisplib.NewDocEnv()` creates a standard environment with the
stdlib loaded. Embedders can use this as a starting point or create their own
environment from scratch.

### Reusing the CLI Commands (Recommended)

The `cmd` package exports `LintCommand()` and `DocCommand()` factory functions
that return fully configured `*cobra.Command` values with all flags, output
modes, and diagnostic rendering built in. Pass `cmd.WithRegistry` or
`cmd.WithEnv` to inject embedder symbols so that semantic analysis and
documentation queries see Go-registered builtins.

```go
package main

import (
    "github.com/luthersystems/elps/cmd"
    "github.com/spf13/cobra"
)

func main() {
    // Assume NewRuntime() creates an *lisp.LEnv with embedder packages
    // (cc:*, app:*, etc.) already registered.
    env := NewRuntime()

    root := &cobra.Command{Use: "mytool"}
    root.AddCommand(
        // Lint: injects the registry so semantic analysis recognises
        // embedder builtins (no false-positive undefined-symbol).
        cmd.LintCommand(cmd.WithRegistry(env.Runtime.Registry)),

        // Doc: injects the full env so documentation queries cover
        // all embedder packages and their docstrings.
        cmd.DocCommand(cmd.WithEnv(env)),
    )
    root.Execute()
}
```

This gives embedders the full `elps lint` and `elps doc` experience — all
flags (`--json`, `--workspace`, `--checks`, `-p`, `-m`, `--guide`, etc.),
diagnostic rendering, and exit codes — with accurate analysis of custom
builtins.

**Option functions:**

| Option | Effect |
|--------|--------|
| `cmd.WithRegistry(reg)` | Merges Go-registered symbols into semantic analysis (lint) or the doc environment. |
| `cmd.WithEnv(env)` | Uses the given `*lisp.LEnv` directly. For lint, `env.Runtime.Registry` is extracted. For doc, the env is used for queries. |

When both options are provided, `WithEnv` takes precedence for registry
resolution (the env's registry is used).

### Low-Level APIs

For more control, the underlying packages can be used directly.

#### Linting

```go
import "github.com/luthersystems/elps/lint"

l := &lint.Linter{Analyzers: lint.DefaultAnalyzers()}
diags, err := l.LintFiles(&lint.LintConfig{
    Workspace: workspaceDir,
    Registry:  env.Runtime.Registry,
}, files)
```

#### Documentation

```go
import "github.com/luthersystems/elps/lisp/lisplib/libhelp"

libhelp.RenderVar(os.Stdout, env, "cc:storage-put")
libhelp.RenderPkgExported(os.Stdout, env, "cc")
libhelp.RenderPackageList(os.Stdout, env)
missing := libhelp.CheckMissing(env)
```
