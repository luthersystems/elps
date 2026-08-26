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

### Parse once, load many: sealed Programs

An embedder that caches parse results (for example, keyed by a hash of the
source) should cache `lisp.Program` values rather than raw `[]*lisp.LVal`
slices.  A `Program` is an opaque handle produced where the parse happens —
`env.ParseProgram`, `lisp.ReadProgram`, or `lisp.ReadLocationProgram` — and
consumed by `env.LoadProgram` / `env.LoadProgramContext`:

```go
p, err := env.ParseProgram("code.lisp", "code.lisp", strings.NewReader(lispcode))
if err != nil {
    // handle parse error
}
ret := env.LoadProgram(p)
if ret.Type == lisp.LError {
    // handle execution error
}
```

Because `Program` exposes no accessor for its expressions, a cache built on
it cannot hand raw AST nodes to callers — the aliasing bugs that come from
sharing `*lisp.LVal` pointers between caches and environments are ruled out
at compile time, at zero runtime cost.  Deep-copy machinery for code that
genuinely needs the AST (tooling, serialization, transfer between runtimes)
exists in-kernel (`detach`, returning hermetic deep copies) but is
unexported until a real embedder consumer materializes.

The guarantee runs in both directions.  Outward, `Program` seals the
parse/cache boundary so AST nodes cannot *escape* to the embedder.  Inward,
the constructors establish the hermetic seal (`docs/sealed-ast.md`) on the
expressions they admit: reader output that is not already sealed throughout
— a format-preserving parser, a caller-written `Reader` — is privately
copied and sealed, and output the seal cannot protect (reference types,
function values) is rejected with an error (elps#394).  A cached `Program`
is therefore always safe to load from many environments — see the `Program`
godoc for details.

### Caching `load-file`: `Runtime.LoadCache`

`Program` covers the parse/load path an embedder drives *directly*.  It does
not cover `load-file`, which is how a lisp program loads its own sources: that
path runs through `Runtime.Reader`, and before elps#368 the only place to put
a cache in front of it was a custom `lisp.Reader` — which means taking custody
of `[]*lisp.LVal` and handing the same nodes to every environment.

`Runtime.LoadCache` is the elps-owned hook for that.  The embedder supplies
policy; elps keeps the data:

```go
type LoadCache interface {
        Load(key string) (*lisp.CachedSource, bool)
        Store(key string, src *lisp.CachedSource)
}
```

A `*lisp.CachedSource` is opaque in the same way a `Program` is: only elps
mints one, and no exported member yields a `*lisp.LVal`.  A minimal cache is a
map behind a mutex; a real one is usually bounded by size or age.  Install it
before loading anything:

```go
env.Runtime.LoadCache = myCache // any type implementing lisp.LoadCache
```

Every `Load*` entry point then consults it — `LoadFile`, `Load`, `LoadString`,
`LoadLocation` and their `Context` variants, which is to say the `load-file`
builtin as well.  On a miss elps reads the stream, derives the key, parses,
**seals** the result through the same admission `Program`'s constructors use,
stores it, and hands the sealed tree to the load.  On a hit elps hands that
same sealed tree to the next environment **by reference** — no copy, no walk.

What makes the alias legal is that elps owns the AST type: the cached tree is
sealed throughout, lisp-level writes through it raise `modify-literal-error`,
the evaluator's own metadata writes skip sealed nodes (so an attached debugger
needs no private copy), and checked builds re-verify the tree's fingerprint
after every load.  See `docs/sealed-ast.md` §2.9.

Notes for implementers:

- **The key is elps's.**  It is derived from the source bytes *and* the
  stream's name and location.  Keying on content alone — which an embedder
  cache typically does — makes two files with identical text share an entry,
  and the served tree carries the first file's parse locations, so errors
  raised from the second name the wrong file.
- **`Load` must be honest.**  An entry returned under a key it was not stored
  under is treated as a miss, not trusted.
- **`Store` may refuse.**  elps never assumes a stored entry is later
  loadable, so eviction needs no coordination.
- **Concurrency.**  A cache shared by `Runtime`s on more than one goroutine
  must have concurrency-safe `Load`/`Store`.  The entries themselves are
  immutable, so nothing else needs locking.
- **The key binds the producer, not just the input.**  Besides the bytes,
  name and location, the key folds in the identity of the `Reader` that parses
  them (its Go type, or `ReaderIdentity()` if the reader implements it) and
  which method — `Read` vs `ReadLocation` — is in use.  Without this, two
  Runtimes with different `Reader`s sharing one cache served each other's
  parses, a swapped `Runtime.Reader` re-served the stale parse, and `Load`
  (`Read`) and `LoadLocation` (`ReadLocation`) collided on the same
  `(name, "", src)` tuple.  Reader identity defaults to the Go type, so many
  Runtimes each holding their own `parser.NewReader()` of the same type still
  share entries; a reader that varies its parse behind one Go type distinguishes
  itself by implementing `lisp.ReaderIdentity`.
- **`Load`/`Store` must not re-enter the load path.**  A cache that warms
  itself by loading is defended against — the re-entrant load is treated as a
  miss and parses without the cache — but relying on that gives up caching for
  the warmed load, so do the warming outside the hook.
- **A `Reader` must not retain and later mutate the nodes it returned.**  On
  the zero-copy fast path (a reader whose output is already sealed throughout)
  admission stores the reader's own nodes, so a reader that keeps a reference
  and writes through it (in Go — the seal stops lisp-level writes, not
  `v.Cells[0] = x`) corrupts the shared cached tree.  This is the same residual
  the seal design carries for all embedder Go code; checked builds
  (`-tags elpscheck`) re-verify each cached tree's fingerprint after every load
  and catch it, production builds do not.
- **The `[]*lisp.LVal` a `Reader` returns is not retained**, so reusing one
  output slice per call is safe.  Admission clones the slice header (and
  clamps its capacity) before an entry keeps it.  Without that clone a reader
  that refilled its buffer on the next parse silently rewrote the *previous*
  file's cache entry — every root in it still sealed and still matching its
  own fingerprint, so only an entry-level check can see it.
- **A `Reader` that returns `""` from `ReaderIdentity()` disables the cache**
  for its own loads: they parse every time.  An empty token states nothing,
  and two readers returning it would be declared interchangeable producers and
  would serve each other's parses.
- **A nil `LoadCache` changes nothing.**  With no cache installed the load
  path is exactly what it was before the hook existed — the reader receives
  the caller's own `io.Reader`, unbuffered, and admission allocates nothing.
  (`BenchmarkReadProgramAdmit` and `BenchmarkTextLoaderAdmit` hold that to
  account; the claim is about the `Load*` family *and* about the
  `lisp.Program` constructors, which share the same admission walk.  What
  those constructors newly *reject* is a separate matter — see below.)
- **Not every parse is cacheable, and un-cacheable is not a load failure.**
  A `Reader` that returns a reference type, a `nil` node, a node the seal
  cannot cover, a literal carrying a `Native` payload the seal cannot vouch
  for, or simply more nodes than the cache admission's budget (counted both
  as distinct nodes and as unfolded size), produces a parse that is
  handed to that one load and never stored.  The load itself behaves exactly
  as it would with no cache installed — a cache is an optimization and must
  never turn a working program into a broken one.
- **Node sharing is admitted.**  A `Reader` that interns symbols, constants or
  whole subexpressions returns a DAG, which is an ordinary memory
  optimization; it is cached and aliased normally.  What the cache measures is
  not whether anything is shared but the **unfolded** size — the number of
  nodes an evaluation walks, counting a shared subtree once per path — which
  admission computes exactly, in time linear in the *distinct* nodes.  A
  heavily interned very large source is simply over budget (above), so it
  loads uncached.
- **Two shapes are a hard load error instead**, because they are unsafe to
  *evaluate* rather than merely unshareable: reader output containing a cycle,
  and sharing whose unfolded size is astronomical (4.3e9 node evaluations —
  reachable only by sharing that multiplies, and never by a program that
  finishes).  Both are refused with "reader output is not a finite tree".
- **The node budget is the cache's alone.**  `lisp.ReadProgram`,
  `lisp.ParseProgram` and `lisp.TextLoader` impose no limit on how many nodes
  a `Reader` may return, and never did; only cache admission does, because
  only a cache entry is aliased into unboundedly many environments.

#### What `ReadProgram` / `ParseProgram` / `TextLoader` newly reject

The bullet above says a nil `LoadCache` changes nothing, and for the
`Load*` family that is exact.  The `lisp.Program` constructors are the other
half of the same admission, and they are **not** unchanged: they run the same
walk with no cache installed, so a few `Reader` outputs that used to be
accepted now return an error.  Every one of them was a latent crash or a
silently-shared mutable node; none can be produced by a parser in this
repository.  Migrating an embedder `Reader` means checking this list:

| Reader output | before | `ReadProgram` / `ParseProgram` | `TextLoader` |
|---|---|---|---|
| a `nil` node (root or cell) | panic (nil dereference) | error: *reader output contains a nil expression* | same |
| a cycle | unbounded recursion, Go stack overflow | error: *not a finite tree* | same |
| nesting past 100,000 | Go stack overflow | error: *not a finite tree* | same |
| a `Native` payload on a sealable node (`LInt`, `LString`, `LSymbol`, `LSExpr`, …) | accepted | error: *cannot admit … carrying a native payload* | **accepted** |
| a reference type (bytes, map, array, native) | error | error (unchanged) | error (unchanged) |
| node sharing (interning), any size | accepted | accepted | accepted |
| one very large expression | accepted | accepted | accepted |

The `Native` row is the only one where a previously *working* program
changes, and it is confined to the two constructors that hand every
environment the **same** tree.  `TextLoader` gives each load `expr.Copy()`, so
nothing is newly shared there and the payload is tolerated: `Native` is the
only exported per-node slot an embedder's `Reader` has (`source`, `meta` and
`macroExpansion` are unexported), so a `Reader` that annotates nodes has
nowhere else to go.  A `Program` cannot make the same allowance — the seal is
the only thing standing between environments, and the seal cannot vouch for
what is on the other end of an `interface{}`.

On the cache path none of these is a load failure except the cycle and the
unbounded-sharing case: an un-admissible parse is handed to that one load and
never stored.
- **The guest can mint entries.**  `load-string` and `load-bytes` are builtins,
  so semi-trusted phylum source populates the cache too — retention bounds must
  account for guest-driven loads, not only host call sites.

### Migration hazard: installing a cache can change lisp semantics

Installing a `LoadCache` in front of a **non-sealing** `Reader` can change the
behaviour of previously-working lisp code, so treat it as a migration step, not
a transparent optimization:

- Admission's copy path runs `SealAST`, so a guarded in-place mutation —
  `(stable-sort < <literal>)`, `(append 'vector <literal>)`,
  `(slice 'vector <literal>)` — that succeeded against a reader that did not
  seal begins raising `modify-literal-error` once the cache is installed.  The
  standard parser already seals, so its callers see no change; a
  format-preserving parser or a hand-written `Reader` are the ones affected.
- The zero-copy hit is **conditional**: a wrapping `Reader` that synthesizes
  even one node forces the whole file down the copy-and-seal path.
- With a cache installed the stream is drained with `io.ReadAll` before parsing,
  so a streaming `Reader` that delivers a full program and then a non-EOF error
  succeeds cache-less but fails with a cache.

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

### Sharing packages and registries

Several of the APIs below take a `*lisp.PackageRegistry` from a booted
environment (`cmd.WithRegistry`, `mcpserver.WithRegistry`,
`lsp.WithRegistry`). The doc paths merge that registry's packages into the
environment they build, and those merges — like any embedder that installs a
hand-built package — go through `PackageRegistry.AddPackage`, which is an
**admission point** rather than a store (elps#524):

- What gets registered is a private **snapshot** of the package. Binding into
  your own `*Package` after `AddPackage` does not change what the runtime
  serves — bind through the environment, or finish the package before
  registering it.
- A binding that is a **code-like tree** (a list, symbol, string or number
  built at runtime rather than produced by the parser) is copied privately
  and sealed, so the registry cannot rewrite what you still hold and you
  cannot rewrite what it evaluates.
- **Functions, natives, sorted-maps, arrays and byte strings are admitted by
  reference** — no seal covers those classes. `AddPackage` transfers custody
  of them: stop mutating them once they are registered, and remember that a
  lisp closure carries its captured environment, so sharing one between
  runtimes is still sharing mutable state.
- The snapshot reads the package's maps on the calling goroutine, so no other
  goroutine may be writing that package while `AddPackage` runs.

[docs/sealed-ast.md §2.8](sealed-ast.md) states the rule per value class and
the reasoning behind it.

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

### Deprecating a builtin

An embedder retires a Go builtin the way Go retires an identifier: a docstring
paragraph beginning `Deprecated:` (or `DEPRECATED:`) marks the function, and
the rest of that paragraph says what to call instead. Register the builtin with
`elpsutil.FunctionDoc` so the docstring reaches the runtime — any definition
type with a `Docstring() string` method carries it the same way.

```go
import (
    "github.com/luthersystems/elps/elpsutil"
    "github.com/luthersystems/elps/lisp"
)

env.AddBuiltins(true,
    elpsutil.FunctionDoc("blend-paths", lisp.Formals("a", "b"), blendPaths,
        "Combines two paths into one.\n\nDeprecated: use join-paths instead."),
    elpsutil.FunctionDoc("join-paths", lisp.Formals("a", "b"), joinPaths,
        "Combines two paths into one."),
)
```

Lint the embedded lisp sources with that environment's registry — the same
`LintConfig` as above — and the `deprecated` check reports every call site,
quoting the notice:

```go
diags, err := l.LintFiles(&lint.LintConfig{
    Workspace: workspaceDir,
    Registry:  env.Runtime.Registry,
}, files)
// paths.lisp:1:2: use of deprecated function 'substrate:blend-paths':
// use join-paths instead.
```

The check needs the registry: without it the builtin has no docstring to read
and its call sites go unreported. Passing the same registry to the language
server (`lsp.WithRegistry`) or the MCP server (`mcpserver.WithRegistry`) gives
authors the same treatment in the editor — struck-through call sites, a
**Deprecated.** banner on hover, and a deprecated tag in completion lists.

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

### MCP Server Environments

The `mcpserver` package exposes ELPS language tooling over MCP. Its `doc`,
`eval`, and `test` tools each need an environment, which the embedder supplies
with `mcpserver.WithRequestEnvFactory`:

```go
srv := mcpserver.New(
    mcpserver.WithRegistry(env.Runtime.Registry),
    mcpserver.WithWorkspaceRoot(root),
    mcpserver.WithRequestEnvFactory(func(ctx context.Context) (*lisp.LEnv, func(), error) {
        env, closeEnv, err := NewRuntime(ctx) // embedder runtime, may own a DB, files, goroutines
        if err != nil {
            return nil, nil, err // the factory cleans up after its own failure
        }
        return env, closeEnv, nil
    }),
)
```

The server calls `release` exactly once, as soon as it is finished with the
environment — before the tool handler returns, and per environment in batch
`eval`, so peak usage stays at one environment rather than one per expression.
Do not tie the environment's lifetime to `ctx` alone: the request context is
cancelled only after the response is written, and it outlives every individual
environment a batch request builds. The context is there for the request's
deadline and for correlating an environment with its request.

An environment that owns nothing beyond memory can return a `nil` release; the
server treats it as a no-op.

`WithEnvFactory(func() (*lisp.LEnv, error))` is the older form of the same
option and is deprecated: it has no way to signal that an environment is
finished with, so environments backed by OS resources or background goroutines
accumulate for the life of the process.

Two related options control which environment a tool sees:

| Option | Effect |
|--------|--------|
| `mcpserver.WithDocEnv(env)` | One shared, reusable environment for the read-only `doc` tool. Documentation lookup is a symbol query, so it needs no per-request isolation. Never released by the server. |
| `mcpserver.WithEnv(env)` | Backs `doc` *and* the diagnostics path (workspace macro loading and expansion). Use `WithDocEnv` when only the `doc` tool should be redirected. |

For the `doc` tool the precedence is `WithDocEnv`, then `WithEnv`, then the
request env factory, then a default stdlib documentation environment.

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
