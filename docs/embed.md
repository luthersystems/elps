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
the constructors establish the hermetic seal (`docs/internals/sealed-ast.md`) on the
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
after every load.  See `docs/internals/sealed-ast.md` §2.9.

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
- **`ReaderIdentity`/`Load`/`Store` must not re-enter the load path.**  A cache that warms
  itself by loading is defended against — the re-entrant load is treated as a
  miss and parses without the cache — but relying on that gives up caching for
  the warmed load, so do the warming outside the hook.
- **Cache-hook panics fall back to uncached work.** A failing identity hook
  skips lookup and storage for that load. A failing lookup is a miss; a
  failing store leaves the parsed program usable. Diagnostic writes to
  `Stderr` are best effort and a panicking writer is not retried. Reader,
  input-stream and library panics instead return a marked `internal-panic`
  error with the Go stack. Checked-build ownership and seal violations
  remain hard failures throughout these paths.
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

Use `l.MapSetString(k,v)` (string key) or `l.MapSetLVal(k,v)` (LVal key) to
set keys on the map, which returns the mutated map. `v` must be an LVal.

Use `l.MapGetString(k)` or `l.MapGetLVal(k)` to return the LVal corresponding
to `k`.

`MapSet` and `MapGet`, which take the key as `interface{}` and reject any other
key type only at run time, are deprecated in favour of the typed forms.

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
    lisptime := libtime.Time(time.Now())
    expr := lisp.SExpr([]*lisp.LVal{lisp.Symbol("my-function"), lisptime})
```

For the standard time library, use `libtime.Time` and `libtime.Get` rather
than asserting the native payload to `time.Time`. The payload is private;
both boundaries isolate timezone objects while preserving their calendar
rules. `Get` returns an independent Go time that the caller can freely use.
Other host-native types can use `lisp.NativeValue[T]` to check the value header
and unbox their own payloads.

```go
func builtinPrintTime(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
    lisptime := args.Cells[0]
    if lisptime.Type != lisp.LNative {
        return env.Errorf("argument is not a time: %v", lisptime.Type)
    }
    t, ok := libtime.Get(lisptime)
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

## Errors and Host Panics

The language reference (`elps doc --guide`, "Errors" and "Host Panics")
describes how Lisp code raises, handles and rethrows conditions, and why
`internal-panic` escapes catch-all handlers. This section covers the Go side.

### Go errors

For Go embedders, `GoError` still returns an `*ErrorVal`; `errors.Unwrap`,
`errors.Is` and `errors.As` can recover the original Go error. `rethrow`
preserves that error and its original stack. Host errors implementing
`NativeCloner` retain their usual copy behavior.

Passing an `*ErrorVal` back into `Error` or `ErrorCondition` returns that
same value, condition and stack intact, so a handler can hand back exactly
what it was given. Passing a Go error that merely *wraps* an `*ErrorVal`
(`fmt.Errorf` with `%w`) is a request to reclassify: the result carries the
condition you asked for and the wrapper's text as its message, and
`errors.As` still reaches the inner value. The one exception is a wrapped
`internal-panic`, which keeps its identity so the marker of a host fault
survives a host wrapper.

### Host panics

If Go code called during evaluation — a builtin or special operator supplied
by the host — panics, the interpreter recovers the panic and returns an error
with the condition `internal-panic`.
This also applies to direct Go calls through `FunCall`, `FunCallContext`,
`EvalSExpr`, `MacroCall`, `SpecialOpCall` and `New`, and to source reader,
input stream and library callbacks used by the `Load*` methods. Debugger and
profiler callback panics are recovered at these evaluation/call boundaries.

The carve-out that stops `ignore-errors` and the catch-all `condition` handler
from swallowing the panic keys off a Go stack snapshot the interpreter attaches
when it recovers the panic, not off the condition name. Embedders testing for
one should use `lisp.IsInternalPanic(v)` rather than comparing the condition
name.

The resulting error also carries the Go stack captured at the panic site, so
an embedder can identify the offending Go function.

Recovery does not invoke debugger error hooks: the debugger may itself have
failed while holding a lock. Ordinary errors still notify the debugger.
Panic diagnostics preserve primitive values and Go runtime fault messages;
other payloads are described by type without calling application
`String`, `Error` or `Format` methods, which could re-enter the failed object.

Optional cache hooks have a different fallback: a panic in `ReaderIdentity`,
`LoadCache.Load` or `LoadCache.Store` disables that operation and the source
is parsed or evaluated without it. Diagnostics to `Stderr` are best effort;
a panicking diagnostic writer is not retried. Nested loads from these hooks
bypass identity and cache hooks on the same runtime.

In `elpscheck` builds, detected ownership, sealed-program and singleton
corruption deliberately remain hard Go panics so recovery cannot hide a
failed invariant. These developer checks are distinct from language errors.

## Execution Limits

The language reference (`elps doc --guide`, "Execution Limits") describes each
limit as a Lisp program experiences it: what it bounds, its default, and the
condition it raises. This section covers how a Go host configures and
observes them.

### Configuring limits

Limits are set with `lisp.Config` options passed to `lisp.InitializeUserEnv`:

```go
env := lisp.NewEnv(nil)
lisp.InitializeUserEnv(env, lisp.WithMaxSteps(1000000))
```

| Option | Default | Notes |
| --- | --- | --- |
| `WithMaxSteps(n)` | unlimited | Steps per top-level evaluation (see below). |
| `WithMaxAlloc(n)` | 10,485,760 (`DefaultMaxAlloc`) | Per-container size cap in bytes or elements; non-positive selects the default. Stored in `Runtime.MaxAlloc`. |
| `WithMaxMacroExpansionDepth(n)` | 1,000 (`DefaultMaxMacroExpansionDepth`) | Successive macro expansions; nonpositive selects the default. |
| `WithMaximumPhysicalStackHeight(n)` | 25,000 (`DefaultMaxPhysicalStackHeight`) | 0 disables the check. |
| `WithMaxEvalNesting(n)` | 100,000 (`DefaultMaxEvalNesting`) | A negative value disables the check. |
| `WithMaxTailIterations(n)` | 1,000,000 (`DefaultMaxTailIterations`) | 0 disables the check. |
| `WithMaximumLogicalStackHeight(n)` | 0, disabled (`DefaultMaxLogicalStackHeight`) | Opt-in logical (virtual) stack limit. |
| `WithMaxValueDepth(n)` | 1,000,000 (`lisp.MaxValueDepth`) | Must be at least 1024. |
| `WithMaxSleep(d)` | one hour (`DefaultMaxSleep`) | Ceiling that `time:sleep`'s `:max` cannot exceed. |

`WithMaxMacroExpansionDepth(n)` selects the limit; a nonpositive value uses
the default of 1,000.

The physical stack limit can be overridden with
`lisp.WithMaximumPhysicalStackHeight(n)`; 0 disables the check, which is not
recommended.

Override the evaluation nesting limit with `lisp.WithMaxEvalNesting(n)`; a
negative value disables the check, which re-exposes the host process to an
unrecoverable stack overflow.

Override the tail-iteration limit with `lisp.WithMaxTailIterations(n)`; 0
disables the check.

The logical stack height limit is disabled by default. Callers who
specifically want it can opt in with `lisp.WithMaximumLogicalStackHeight(n)`.

`lisp.WithMaxValueDepth(n)` sets the runtime limit to any value **at least 1024**.
Both lowering and raising the default are supported because these traversals
are iterative. Invalid options return an error. Copying (including condition
data), equality, JSON dumping, quasiquote, macro stamping
and template admission honor the runtime setting. Templates retain it in their
VMs. Depth counts traversed value edges, including internal array storage and
captured environments where visited, rather than printed delimiters alone.
APIs without a runtime, including `lisp.GoValue`, use the default limit.
`GoValue` returns an `*lisp.ErrorVal` implementing Go's `error` interface on
excessive depth; `GoSlice` and `GoMap` return `(nil, false)`. The deprecated
JSON serializer conversion methods use the same convention.

Sealing and source-location assignment use explicit stacks throughout; these
metadata-only APIs cannot return an error and finish the graph.

### Context cancellation

Pass a Go `context.Context` to any of the `*Context` methods on `LEnv`:

```go
ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
defer cancel()

result := env.EvalContext(ctx, expr)
```

A direct Go `FunCallContext` rejects an already cancelled context before
invoking a native body.

Native callbacks temporarily expose the active context through their
environment so nested evaluations inherit it. The previous context is
restored after the callback and any terminal expression, including ordinary
errors and recovered host panics. Finishing a request must not install its
cancelled context on an environment that previously had none.

Embedded REPL callers can pass `repl.WithContext(ctx)` to bind evaluation
and input waits to a context, without installing process signal handlers.
Cancellation closes a pending REPL input wait and stops the session.

### Available Context Methods

| Method | Purpose |
|--------|---------|
| `EvalContext` | Evaluate an expression |
| `LoadContext` | Load from an `io.Reader` |
| `LoadFileContext` | Load a source file |
| `LoadStringContext` | Load from a string |
| `LoadLocationContext` | Load with explicit name/location |
| `FunCallContext` | Invoke a function |

Each method threads the context through the internal evaluation chain.
The older non-context methods (`Eval`, `Load`, etc.) continue to work
but are deprecated.  Builtins can access the current context via
`env.Context()`.

### Step accounting

The counter is reset each time an exported entry point (`Eval`,
`EvalContext`, `EvalSExpr`, `FunCall`, `FunCallContext`, `SpecialOpCall`,
`MacroCall`, or any `Load*`) is entered from outside an evaluation.  Nested evaluation — a
builtin calling back into `Eval`, a tail-call loop, the forms evaluated by a
single `Load` — shares the enclosing budget and does not refill it.  Without
that reset, `WithMaxSteps(n)` would be a *lifetime* quota: once a long-lived
runtime had executed `n` steps in total, every later evaluation would fail
however small it was.

Use `Runtime.Steps()` to read the current evaluation's usage,
`Runtime.TotalSteps()` for the lifetime total, and `Runtime.ResetSteps()`
to reset the current counter explicitly.

Steps are counted only while a step budget or an evaluation context is
configured; with neither, both counters stay at zero. Both saturate at
`math.MaxInt64` instead of wrapping.

### Charging steps from a Go builtin

The evaluator charges a native builtin's call like any other (the call form
and its arguments), but nothing for the work inside it. When a native
builtin replaces a Lisp loop — a fold over a map, say — work that cost
hundreds of steps now costs a handful, which silently loosens a
`WithMaxSteps` budget. `LEnv.ChargeSteps(n int64) *LVal` lets the builtin
charge its work to the same budget. How many steps a unit of work costs is
the host's choice: one per element is the simplest bound on work; to keep
an existing budget's behaviour close to the Lisp it replaces, measure that
Lisp's `Runtime.Steps()` per element and charge that instead.

```go
func builtinSumValues(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	sum := 0
	for _, x := range args.Cells[0].Cells {
		// One step per element: a host-chosen cost for the work done.
		if lerr := env.ChargeSteps(1); lerr.Type == lisp.LError {
			return lerr
		}
		sum += x.Int
	}
	return lisp.Int(sum)
}
```

`ChargeSteps` returns `Nil()` when evaluation may continue and an `LError`
otherwise; return that error unchanged. It applies the evaluator's own
accounting `n` times at once:

- With neither a step budget nor a context configured it counts nothing and
  returns `Nil()`, exactly as the evaluator counts nothing.
- Otherwise `Steps()` and `TotalSteps()` grow by `n`, saturating at
  `math.MaxInt64`. The whole `n` is recorded even when it overruns the budget.
- Past the budget it returns the evaluator's `step-limit-exceeded` condition
  (`lisp.CondStepLimitExceeded`) with the same message, so `handler-bind` and
  Go callers cannot tell a native overrun from an evaluator overrun. Every
  later charge or step in the same top-level evaluation fails the same way.
- Otherwise, if the evaluation's context is done, it returns
  `context-cancelled`, so a long native loop that charges per element also
  stops at a deadline.
- `n == 0` is a no-op that checks nothing. A negative `n` returns an ordinary
  error and changes no counter; charges cannot be refunded.
- A count that saturates exceeds every budget, including
  `WithMaxSteps(math.MaxInt64)`.

The charge lands on the current top-level evaluation and follows the reset
rules above. It is per-`Runtime` state: a VM created from a `Template`
charges only its own budget, and, like evaluation itself, `ChargeSteps` must
only be called from the goroutine evaluating on that runtime.

### Shared step budgets

`WithMaxSteps` refills at every top-level evaluation. A host that runs several
top-level evaluations for one unit of work (a transaction, a JSON-RPC batch)
bounds their total with a shared budget instead:

```go
env.Runtime.SetStepBudget(1_000_000) // n <= 0 clears it (unlimited, the default)
env.LoadString("a", srcA)            // both draw from the same budget
env.LoadString("b", srcB)
budget, used := env.Runtime.StepBudget()
env.Runtime.ResetStepBudget()        // zero the usage, keep the budget

vm, err := tmpl.NewVM(lisp.VMWithStepBudget(1_000_000))
```

It counts the same steps as `WithMaxSteps`, including every `ChargeSteps`
charge, so a program's usage is deterministic and identical on a cold
environment and a template VM. Exhaustion raises `CondStepBudgetExceeded`
(`step-budget-exceeded`), and every later step or charge fails the same way
until the budget is reset or changed. If one step exceeds both limits,
`step-limit-exceeded` wins. `NewTemplate` never publishes a source runtime's
budget; each VM gets its own.

### Rendering from Go

Go's `LVal.String`
uses the default cap without an evaluation context; `LEnv.Render` uses the
environment's cap and context. Errors retain the output cap captured when they
were created, but no context: an error outlives the request that produced it,
so cancellation comes from the context the caller hands a reader such as
`ErrorMessageContext` or `WriteTraceContext`. A context that is already dead
bounds nothing and is ignored, so a diagnostic logged after its request ended
still renders in full under the byte cap.

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

[docs/internals/sealed-ast.md §2.8](internals/sealed-ast.md) states the rule per value class and
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

### Documenting Go builtins

Go-implemented builtins provide documentation through their definition.
Use `libutil.FunctionDoc` (for library packages) or the `langBuiltin`
struct (for core builtins) and pass a docstring as the last argument:

```go
// Library package function
libutil.FunctionDoc("my-fn", lisp.Formals("x", "y"), myFnImpl,
    `Computes something useful from x and y.`)

// Core builtin (an entry in langBuiltins, lisp/builtins.go)
{"my-builtin", Formals("arg"), builtinMyBuiltin,
    `Returns something useful from arg.`},
```

`RegisterDefaultBuiltin` takes no docstring, so a builtin registered through
it is undocumented; prefer the forms above.

`libutil` is internal to the standard library; code outside this module uses
`elpsutil.FunctionDoc`, which takes the same arguments (see "Deprecating a
builtin" below).

All builtins, macros, and exported symbols are required to have
documentation. The `elps doc -m` command checks for missing docstrings
and is typically run in CI.

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
