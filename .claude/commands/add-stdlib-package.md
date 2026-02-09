# Add a New Standard Library Package

Create a new stdlib package for the ELPS interpreter following the established pattern.

## Arguments

Describe the package to add (e.g., "file I/O package", "crypto hashing package").

## Steps

### 1. Plan the Package

Define:
- Package name (lowercase, e.g., `"file"`, `"crypto"`)
- Functions to export with signatures
- Any global constants
- Package-level documentation

### 2. Create Package Directory

Create `lisp/lisplib/lib<name>/lib<name>.go`:

```go
package lib<name>

import (
    "github.com/luthersystems/elps/lisp"
    "github.com/luthersystems/elps/lisp/lisplib/libutil"
)

const DefaultPackageName = "<name>"

func LoadPackage(env *lisp.LEnv) *lisp.LVal {
    name := lisp.Symbol(DefaultPackageName)
    e := env.DefinePackage(name)
    if !e.IsNil() { return e }
    e = env.InPackage(name)
    if !e.IsNil() { return e }

    env.SetPackageDoc(`Short description of the package.

Detailed description of what the package provides.`)

    for _, fn := range builtins {
        env.AddBuiltins(true, fn)
    }
    return lisp.Nil()
}

var builtins = []*libutil.Builtin{
    libutil.FunctionDoc("my-func",
        lisp.Formals("arg1", "arg2"),
        builtinMyFunc,
        `Description of what my-func does.`),
}

func builtinMyFunc(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
    arg1 := args.Cells[0]
    _ = arg1
    return lisp.Nil()
}
```

### Key Rules

- Always use `libutil.FunctionDoc()` -- never `libutil.Function()`. CI enforces docstrings.
- Error propagation: `env.Errorf(...)` returns `*LVal`, never Go `error`
- Export all public symbols: `env.Runtime.Package.Exports("func1", "func2")`

### 3. Register in `LoadLibrary()`

Edit `lisp/lisplib/lisplib.go`:

1. Add import: `"github.com/luthersystems/elps/lisp/lisplib/lib<name>"`
2. Add to loader list:
```go
e = lib<name>.LoadPackage(env)
if !e.IsNil() { return e }
```

### 4. Write Tests

Create `lisp/lisplib/lib<name>/lib<name>_test.go`:

```go
package lib<name>_test

import (
    "testing"
    "github.com/luthersystems/elps/elpstest"
)

func TestLib<Name>(t *testing.T) {
    runner := &elpstest.Runner{}
    runner.RunTestFile(t, "testdata/<name>_test.lisp")
}
```

Create `lisp/lisplib/lib<name>/testdata/<name>_test.lisp`:

```lisp
(use-package 'testing)
(use-package '<name>)

(test "my-func basic usage"
  (assert-equal (my-func "arg1" "arg2") expected-result))

(test "my-func error handling"
  (assert-error (my-func)))
```

### 5. Verify

```bash
go test -v ./lisp/lisplib/lib<name>/...   # package tests
make test                                  # full suite
./elps doc -m                              # docstring check
./elps doc <name>                          # docs render correctly
```

### 6. Update Language Reference

Add the package to `docs/lang.md` if it's user-facing.
