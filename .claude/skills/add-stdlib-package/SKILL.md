# /add-stdlib-package — New Standard Library Package Skill

Creates a new stdlib package for the ELPS interpreter. Follows the established pattern from existing packages like `libmath`.

## Trigger

Use when asked to add a new standard library package (e.g., file I/O, crypto, HTTP client, etc.).

## Workflow

### 1. Plan the Package

Before writing code, define:
- Package name (lowercase, e.g., `"file"`, `"crypto"`)
- Functions to export with their signatures
- Any global constants to expose
- Package-level documentation

### 2. Create the Package Directory

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
	if !e.IsNil() {
		return e
	}

	e = env.InPackage(name)
	if !e.IsNil() {
		return e
	}

	env.SetPackageDoc(`One-line description of the package.

Detailed description of what the package provides.`)

	// Optional: global constants
	// env.PutGlobal(lisp.Symbol("my-const"), lisp.Int(42))
	// env.SetSymbolDoc("my-const", "Description of the constant.")
	// env.Runtime.Package.Exports("my-const")

	for _, fn := range builtins {
		env.AddBuiltins(true, fn)
	}

	return lisp.Nil()
}

var builtins = []*libutil.Builtin{
	libutil.FunctionDoc("my-func",
		lisp.Formals("arg1", "arg2"),
		builtinMyFunc,
		`Description of what my-func does.

Returns the result of ...`),
}

func builtinMyFunc(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	arg1 := args.Cells[0]
	arg2 := args.Cells[1]
	// Implementation
	_ = arg1
	_ = arg2
	return lisp.Nil()
}
```

### Key Rules

- **Always use `libutil.FunctionDoc()`** — never `libutil.Function()`. All functions must have docstrings. CI enforces this via `elps doc -m`.
- **Use `lisp.Formals()`** for argument declarations. Available modifiers:
  - Required: `lisp.Formals("arg1", "arg2")`
  - Optional: `lisp.Formals("required", lisp.FmtString("&optional"), "opt-arg")`
  - Variadic: `lisp.Formals("required", lisp.FmtString("&rest"), "rest-args")`
  - Keyword: `lisp.Formals("required", lisp.FmtString("&key"), "key-arg")`
- **Error propagation**: Return `env.Errorf("message: %v", detail)` for errors — never use Go's `error` interface
- **Export symbols**: Call `env.Runtime.Package.Exports("func1", "func2", ...)` for all public symbols

### 3. Register in `LoadLibrary()`

Edit `lisp/lisplib/lisplib.go`:

1. Add import: `"github.com/luthersystems/elps/lisp/lisplib/lib<name>"`
2. Add to the package loader list in `LoadLibrary()`:

```go
e = lib<name>.LoadPackage(env)
if !e.IsNil() {
    return e
}
```

### 4. Write Tests

Create a lisp test file at `lisp/lisplib/lib<name>/lib<name>_test.go`:

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

Create the lisp test at `lisp/lisplib/lib<name>/testdata/<name>_test.lisp`:

```lisp
(use-package 'testing)
(use-package '<name>)

(test "my-func basic usage"
  (assert-equal (my-func "arg1" "arg2") expected-result))

(test "my-func edge case"
  (assert-nil (my-func "" "")))

(test "my-func error handling"
  (assert-error (my-func)))  ; missing args
```

### 5. Verify

```bash
go test ./lisp/lisplib/lib<name>/...    # Package tests pass
make test                                # Full suite passes
./elps doc -m                            # No missing docstrings
./elps doc <name>                        # Package docs render correctly
```

## Checklist

- [ ] Package directory created at `lisp/lisplib/lib<name>/`
- [ ] `LoadPackage()` follows DefinePackage → InPackage → SetPackageDoc → AddBuiltins pattern
- [ ] All functions use `libutil.FunctionDoc()` (not `Function()`)
- [ ] All public symbols exported via `Exports()`
- [ ] Package registered in `lisplib.go` `LoadLibrary()`
- [ ] Tests written with `elpstest.Runner` + `.lisp` test file
- [ ] `elps doc -m` passes (no missing docstrings)
- [ ] `make test` passes
- [ ] `docs/lang.md` updated if package is user-facing
