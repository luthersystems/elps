# Add a New Builtin Function

Add a new builtin function to the ELPS interpreter. This is a specialized workflow for the most common type of change in this repo.

## Arguments

Describe the builtin to add (e.g., "string-reverse that reverses a string", "list-flatten that flattens nested lists").

## Steps

### 1. Read Existing Patterns

Read `lisp/builtins.go` to understand the naming conventions and argument handling patterns used by existing builtins.

### 2. Implement the Builtin

Add to `lisp/builtins.go`:

```go
func builtinMyFunc(env *LEnv, args *LVal) *LVal {
    // args.Cells contains the arguments after arity checking
    arg1 := args.Cells[0]
    // Implementation here
    return result
}
```

### 3. Register with Docstring

Add to `DefaultBuiltins()` in `lisp/builtins.go`:

```go
{
    "my-func",
    Formals("arg1", FmtString("&optional"), "arg2"),
    builtinMyFunc,
    `Short description of what my-func does.

Returns the result of the operation.`,
},
```

Available format types for `Formals()`:
- Required: `"arg1"` (untyped), `FmtString("arg1")` (typed)
- Optional: `FmtString("&optional"), "opt-arg"`
- Variadic: `FmtString("&rest"), "rest-args"`
- Keyword: `FmtString("&key"), "key-arg"`

### 4. Write Tests

Add test cases in `lisp/lisp_test.go` using the `TestSuite`/`TestSequence` pattern:

```go
{
    Expr:   `(my-func "hello")`,
    Result: `"olleh"`,
},
{
    Expr:   `(my-func "")`,
    Result: `""`,
},
{
    Expr:  `(my-func)`,
    Error: true, // arity check fails
},
```

### 5. Update Language Reference

Add documentation to `docs/lang.md` under the appropriate section. This file is embedded in the binary and served via `elps doc --guide`.

### 6. Verify

```bash
go build -o elps .
go test -v -run TestMyFunc ./lisp/...   # targeted test
make test                                # full suite
./elps doc -m                            # docstring check
./elps doc my-func                       # verify docs render
```

## Key Rules

- Docstrings are **mandatory** -- CI will fail without them
- Error handling uses `env.Errorf(...)` returning `*LVal`, NOT Go `error`
- Always check return values: `if v.Type == LError { return v }`
- Use `Formals()` for argument declaration -- never parse args manually
