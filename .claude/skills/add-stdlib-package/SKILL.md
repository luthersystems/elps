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
	"github.com/luthersystems/elps/lisp/lisplib/internal/libutil"
)

// DefaultPackageName is the package name used by LoadPackage.
const DefaultPackageName = "<name>"

// LoadPackage adds the <name> package to env.
func LoadPackage(env *lisp.LEnv) *lisp.LVal {
	prevPkg := env.Runtime.Package.Name
	defer env.InPackage(lisp.Symbol(prevPkg))
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

//elpsvet:allow package builtin table; formals are sealed by libutil at construction and shared via registrationFormals (lisp.LEnv.AddBuiltins)
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
- **Use `lisp.Formals()`** for argument declarations. Modifiers are plain symbols:
  - Required: `lisp.Formals("arg1", "arg2")`
  - Optional: `lisp.Formals("required", lisp.OptArgSymbol, "opt-arg")` (`&optional`)
  - Variadic: `lisp.Formals("required", lisp.VarArgSymbol, "rest-args")` (`&rest`)
  - Keyword: `lisp.Formals("required", lisp.KeyArgSymbol, "key-arg")` (`&key`)
- **Error propagation**: Return `env.Errorf("message: %v", detail)` for errors — never use Go's `error` interface
- **Exports**: `env.AddBuiltins(true, fn)` exports each function. Globals need an explicit `env.Runtime.Package.Exports("name", ...)`
- **The `//elpsvet:allow` on the `builtins` table is required**: `make elpsvet` flags any package-level var that keeps `*lisp.LVal` reachable. Keep the justification; see `/elpsvet`
- **Native Go values** returned to lisp must be publishable by templates — a scalar or a struct value embedding `internal/templatepolicy.Marker` (see `libtime`, `libregexp`). `make elpsvet` reports anything else; see `/elpsvet`
- **Allocation limits**: size-dependent results must check `env.Runtime.CheckAlloc` / `MaxAllocBytes()` before allocating (see `libbase64`)

### 3. Register in the stdlib loader

`lisplib.LoadLibrary()` and `LoadRuntimeLibrary()` both delegate to
`stdlib.Load` in `internal/stdlib/stdlib.go`. Edit that file:

1. Add import: `"github.com/luthersystems/elps/lisp/lisplib/lib<name>"`
2. Add to the loader sequence in `Load()`, before the final `InPackage` (only
   inside the `if testing` block if the package holds mutable per-VM state,
   like `libtesting`):

```go
e = lib<name>.LoadPackage(env)
if !e.IsNil() {
    return e
}
```

### 4. Write Tests

Create `lisp/lisplib/lib<name>/lib<name>_test.go` (the `_test` package suffix avoids an import cycle):

```go
package lib<name>_test

import (
	"testing"

	"github.com/luthersystems/elps/elpstest"
)

func TestPackage(t *testing.T) {
	r := &elpstest.Runner{}
	defer r.Close()
	r.RunTestFile(t, "lib<name>_test.lisp")
}
```

Create the lisp test beside it at `lisp/lisplib/lib<name>/lib<name>_test.lisp`:

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
- [ ] Package registered in `internal/stdlib/stdlib.go` `Load()`
- [ ] `builtins` table carries its `//elpsvet:allow` justification; `make elpsvet` clean
- [ ] Package listed in the `lisp/lisplib/` bullet of `AGENTS.md`
- [ ] Tests written with `elpstest.Runner` + `.lisp` test file
- [ ] `elps doc -m` passes (no missing docstrings)
- [ ] `make test` passes
- [ ] `docs/lang.md` updated if package is user-facing
