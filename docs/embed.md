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

### Go functions run in their own package

**Breaking change (elps#736).** A Go builtin or Go macro registered in any
package other than `lisp` now runs with **its own package current**, like a
Lisp function defined in that package. Before, it ran in whatever package its
caller had current. The caller's package is restored when the builtin returns,
fails, panics, or hands back a terminal expression, so a library builtin
cannot change the caller's package or resolve a global name in it. Core `lisp`
builtins, macros and all special operators are unchanged: they are the
language and act in the caller's package. There are no exceptions.

Only the **package** switches. A builtin still receives the caller's `env`,
so a lexical lookup still sees the caller's lexical scope: `env.Get` of a
`let`-bound name the caller passed finds the caller's binding, while a
global name resolves in the library's package — a mix no Lisp function can
produce. Likewise an expression a builtin hands back with `env.Terminal` is
evaluated in the caller, after the switch is undone, exactly like a macro
expansion. Both are reported by `elpsownpkg` (below); avoid them in favour of
values.

The rule is the one ELPS applies to every name: **a name resolves in the
package of the code doing the lookup** (see "Where a name resolves" in
`docs/lang.md`). ELPS symbols are plain names resolved at run time, unlike
Common Lisp's interned, package-carrying symbols or Clojure's compile-time
namespace resolution, so where the lookup happens decides what a name means.

What this means for a builtin you write:

- **Any global name it resolves resolves in its own package.** `env.Get`,
  `env.GetFun`, `env.GetFunGlobal`, `env.PutGlobal`, `env.Eval*`,
  `env.Load*` and `env.Lambda` all act in the builtin's package, not the
  caller's (lexical names excepted, as above). A package created with `DefinePackage` or `elpsutil.PackageLoader`
  does not import `lisp`, so an unqualified `set` or `+` evaluated there is
  unbound: qualify it (`lisp:set`).
- **Take values, not names.** Accept a function value rather than a quoted
  symbol; the caller evaluates it in its own package. `env.FunCall` on a
  caller's function is fine — that function runs in its own package.
- **To bind or evaluate in the caller, write a Go macro.** The macro runs in
  its own package, but its expansion is evaluated where the caller wrote it,
  so an expansion built from qualified core forms (`lisp:set`, `lisp:lambda`)
  acts in the caller's package. Use `elpsutil.MustTemplate` and
  `elpsutil.FunctionDoc` as shown in [Writing a Go macro](#writing-a-go-macro).
- **Name a fixed package explicitly.** A builtin that must work in some
  package (a loader that starts in `user`, say) switches to it by name with
  `env.InPackage` and restores it itself.
- **Calling another builtin's Go code directly** (`LBuiltinDef.Eval`, or a
  builtin value's `Builtin()`) bypasses the switch; it runs in whatever
  package is current at that point.
- **Special operators are syntax.** They receive the caller's unevaluated
  forms and lexical environment and never switch. Define them only in `lisp`;
  a library should use a function or a macro. elps itself defines none
  outside `lisp` (`TestNoLibrarySpecialOps`).

What moved as a result: `help` is core (`lisp:help`; `help:help` is gone, with
no alias; the `help` package keeps `help-package`, `help-package-symbols` and
`help-packages`, now ordinary functions, so their argument is evaluated: the
documented `(help-package 'math)` is unchanged, but an unquoted
`(help-package math)` now evaluates `math`), and `test`, `benchmark`, `test-let`, `test-let*` and
`benchmark-simple` are core forms that raise `no test suite` where the
`testing` package is not loaded. Package `testing` re-exports them, so
`(use-package 'testing)` and `testing:test` keep working.
`libtesting.TestSuite.Ops` is deprecated (it returns lisp's definitions), and
`Ops`/`Macros` now return `[]lisp.LBuiltinDef`.

**Enforcement.** The `elpsownpkg` analyzer (package
`github.com/luthersystems/elps/elpsvet/ownpkg`, importable into your own
`go/analysis` multichecker) reports package-sensitive operations inside a
library builtin: `Eval*`, `Load*`, `Lambda`, `Terminal`, `InPackage`, reads
of `Runtime.Package`, and symbol lookups that are not literal qualified names.
Suppress an intended one with `//elpsvet:allow-ownpkg <justification>`.

### Per-VM settings

A builtin keeps Lisp-controlled modes and per-VM metadata in runtime
settings. It must not keep them in Go closure or receiver state. A template
shares each approved builtin's Go function with every VM it mints, so state
hidden there leaks between VMs (luthersystems/elps#678). `Serializer.setMode`
(`lisp/lisplib/libjson/json.go:720`) is the in-repository example: it stores
the JSON modes on the calling VM's runtime.

| Method | Purpose |
| --- | --- |
| `Runtime.Setting(name) (bool, bool)` | Reads a boolean setting and whether it is set. |
| `Runtime.SetSetting(name, value bool)` | Sets a boolean setting. |
| `Runtime.SettingValue(name) (any, bool)` | Reads a value setting and whether it is set. |
| `Runtime.SetSettingValue(name, v any) error` | Sets a value setting. Returns an error for a type that is not allowed. |
| `Runtime.DeleteSettingValue(name)` | Removes a value setting. Removing an unset name does nothing. |

Boolean and value settings are separate namespaces, so one name can hold
both. Settings are Go API only. Lisp code reaches a setting only through a
builtin that reads or writes it.

All settings follow the same template rules:

- `NewTemplate` publishes the source runtime's settings.
- Every VM that `Template.NewVM` mints starts from the published settings.
- A write in one VM is seen by no other VM, by the template, or by the source.
- A write to the source after `NewTemplate` returns does not change the template.
- A cold runtime has no settings.

VMs share stored values without copying them, so a value setting must be
immutable. `SetSettingValue` accepts the payloads that template publication
shares without a host policy, plus a copied byte slice:

| Go type of `v` | Treatment |
| --- | --- |
| A scalar kind: `bool`, `string`, an integer, float or complex kind, named types included | Stored as given. `SettingValue` returns the same dynamic type. |
| Exactly `[]byte` | Copied on set and on every read. Nil and empty slices read back with length zero. |
| A struct value that embeds `templatepolicy.Marker` | Stored as given. Embedders cannot mark their own structs. They can store a marked value that an ELPS API returns, such as the payload of `libtime.Time`. |
| Anything else: nil, any pointer, unmarked structs, maps, slices other than `[]byte`, arrays, channels, functions, `uintptr`, `unsafe.Pointer` | Rejected. The setting is unchanged. |

A payload that only `TemplateWithNativePolicy` admits cannot be a value
setting. Store an immutable scalar form of it instead, such as an id or
encoded bytes.

A VM reads the template's value map until its first value write copies it,
so `NewVM` does no per-VM work for value settings.

Record a string while the program loads, and read it in a VM:

```go
if err := source.Runtime.SetSettingValue("program:id", "example-v1"); err != nil {
    return err
}
tmpl, err := lisp.NewTemplate(source, lisp.TemplateWithBuiltinPolicy(approveAuditedBuiltin))
if err != nil {
    return err
}
vm, err := tmpl.NewVM()
if err != nil {
    return err
}
v, ok := vm.Runtime.SettingValue("program:id")
id, _ := v.(string) // "example-v1"; ok is false in a cold runtime
```

### Writing a Go macro

A Go macro receives unevaluated forms and returns an expansion for the caller
to evaluate. Build an `elpsutil.Template` once at package initialization, then
substitute the argument forms on each call. For example, this `unless` macro
runs its body only when the condition is falsey:

```go
var unlessForm = elpsutil.MustTemplate(
    `(lisp:if (unquote condition) () (lisp:progn (unquote-splicing body)))`,
    "condition", "body")

func macroUnless(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
    return unlessForm.Expand(args.Cells[0], lisp.SExpr(args.Cells[1:]))
}
```

Register it in your package with `elpsutil.FunctionDoc`, which carries the
docstring through registration; registering the definition as a macro is what
makes it one. The definition belongs in `AddMacros` or your package's
`Macros() []lisp.LBuiltinDef` method:

```go
// With your package current (for example, inside its PackageInit):
env.AddMacros(true, elpsutil.FunctionDoc("unless",
    lisp.Formals("condition", lisp.VarArgSymbol, "body"), macroUnless,
    "Evaluates body only when condition is falsey."))
```

`MustTemplate` reads its source with the elps reader, once, so a template is
one ordinary elps form: symbols, lists, `()`, strings, numbers and `'form` are
copied as written. Placeholders are the forms elps quasiquote uses, naming a
declared parameter: `(unquote name)` inserts that argument and
`(unquote-splicing name)` splices an **unquoted** list's cells (or nothing for
nil) into the enclosing list; `lisp.SExpr(args.Cells[1:])` above wraps the
unevaluated body for that purpose. A placeholder is recognised anywhere,
including under a quote, so `'(unquote name)` quotes the argument;
`unquote-splicing` must sit directly inside a list. Because every unquote
form is a placeholder, a template cannot contain a literal `unquote` form or
a nested `quasiquote`: in `(quasiquote (a (unquote x)))` the inner unquote is
substituted. Build such a form in Go and pass it as an argument. Parameter names declare
argument order. `MustTemplate` panics if the source is not exactly one form or
fails to parse, and on undeclared, duplicate or unused parameters or a
malformed placeholder; `Expand` panics on an argument count mismatch or an
invalid splice argument. libtesting's assert macros are built this way.

Each expansion allocates fresh, unlocated syntax, shares the inserted
argument forms, and uses the immutable nil singleton for `()`; it never
parses or evaluates. This follows the Go macro contract: the evaluator
locates new syntax at the macro call site, in place. Reuse the template, never
a previously returned expansion or a binding looked up by the macro. Qualify
generated core names (`lisp:if`, `lisp:progn`), and use `lisp.NewGenSyms(args)`
for temporary bindings. For example, `pair-once` evaluates a form once and
returns its value twice:

```go
var pairOnceForm = elpsutil.MustTemplate(
    `(lisp:let (((unquote value) (unquote form))) (lisp:list (unquote value) (unquote value)))`,
    "value", "form")

func macroPairOnce(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
    syms := lisp.NewGenSyms(args)
    return pairOnceForm.Expand(syms.Symbol("value"), args.Cells[0])
}
```

Register it with `elpsutil.FunctionDoc`, formals `lisp.Formals("form")`, and a
docstring. Create one generator per expansion and reuse each returned symbol
where that binding is referenced. `Symbol(hint)` returns a fresh symbol on
every call; hints must not contain `:`. Names such as `value@1@1` use a
namespace the source reader cannot produce and depend only on argument forms
and the order of `Symbol` calls. Generation is lazy, charges no steps, and
allocates only the symbol and its name when the generator stays local.
Keep its temporary bindings within the expansion: independent expansions
may reuse names. The [design and limits](internals/gensym.md) explain why
this prevents capture and why ordinary source nesting can reuse a level.
The Lisp `gensym` builtin and `env.GenSym()` retain their history-dependent
`genNNNNNNNN` names. A macro ported to `NewGenSyms` no longer advances that
counter, so the numbers later `gensym` calls print change: `get-default`
used to take two, and a `(gensym)` after it now prints a lower number than
before.

Use `env.ErrorfAt(form, format, values...)` for argument validation so an
error points at the offending form. For example, this macro binds a name to
a pair of values:

```go
var definePairForm = elpsutil.MustTemplate(
    `(lisp:set '(unquote name) (lisp:list (unquote left) (unquote right)))`,
    "name", "left", "right")

func macroDefinePair(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
    name := args.Cells[0]
    if name.Type != lisp.LSymbol || name.IsQuoted() {
        return env.ErrorfAt(name, "first argument is not a symbol: %v", name)
    }
    return definePairForm.Expand(name, args.Cells[1], args.Cells[2])
}
```

Register it with formals `lisp.Formals("name", "left", "right")`. The
template quotes the name itself, so the check also rejects a quoted name:
`(define-pair 'q 1 2)` gets the macro's own error, located at `'q`. In this
call the error reports line 2, column 3, where `42` starts:

```lisp
(define-pair
  42 1 2)
```

`ErrorfAt` copies the form's location; nil forms and forms without a valid
source position fall back to the evaluator's current location, as `Errorf`
does. The captured call stack
still describes the macro call, and debugger error notifications see the
error with its chosen location.

### Toolkit for replacing Lisp with Go builtins

A Go builtin that replaces a Lisp definition must behave exactly as the Lisp
did: same results, same error conditions and messages, same writes and, for a
host that meters steps, the same step count. Error text and steps are program
output: a host that runs the same program on several machines (substrate's
endorsing peers, for one) needs every machine to produce the same bytes. The
helpers below make that the easy path (luthersystems/elps#745). None of them
charges a step, so adopting one changes no step count.

One return convention runs through them. A helper that only reports success
or failure (`BindBuiltins`, `ExtendPackage`, `ArgReader.Err`, the step and
context helpers) returns `lisp.Nil()` on success and the `LError` otherwise,
like `ChargeSteps` and package loaders, so `lerr.Type == lisp.LError` is
always the check and a loader can return the result directly. A helper that
computes something (`CallBuiltin`, `CallGlobal`) returns the value or the
`LError`.

**Calling a language builtin: `lisp.BuiltinFunc` and `LEnv.CallBuiltin`.**
Reuse a builtin for its exact errors and guards (sealed maps, `MaxAlloc`,
typed keys) rather than copying its checks. Resolve the handle once, at
package initialization, where a misspelled name panics at start-up:

```go
var builtinAssocMut = lisp.BuiltinFunc("assoc!")

func builtinRemember(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	return env.CallBuiltin(builtinAssocMut, args.Cells[0], lisp.String("seen"), lisp.True())
}
```

`CallBuiltin` binds the arguments against the builtin's formals as a Lisp call
does (arity and keyword errors included; the caller's slice is never
written), checks the context like the evaluator's call boundary, evaluates a
terminal expression the builtin returns (`funcall`, `apply`), and pushes no
frame, so an error is attributed to your builtin.

**Calling a Lisp function: `LEnv.CallGlobal`.** For a callee that exists
only in Lisp, `env.CallGlobal("utils:set-exception-business", args...)`
resolves the (qualified) name at call time, as a Lisp call site would, and
calls it. The callee costs what its body evaluates; a Lisp call form would
also charge the form and each argument expression.

**Registering: `elpsutil.ExtendPackage` and `LEnv.BindBuiltins`.**
`ExtendPackage(env, name)` enters a package with `in-package` semantics: it
creates the package if needed, and a package it creates uses the language
package. `BindBuiltins(lisp.BindOpts{Export: true, Shadow: true}, defs...)`
then binds the builtins. `Shadow` lets a native rebind a name the package
imports, as the `defun` it replaces did (a native `statedb:get` shadowing
`lisp:get`); `AddBuiltins` refuses such a name by panicking. `BindBuiltins`
checks every definition before binding any and returns an error rather than
panicking.

```go
func LoadPackage(env *lisp.LEnv) *lisp.LVal {
	if lerr := elpsutil.ExtendPackage(env, "statedb"); lerr.Type == lisp.LError {
		return lerr
	}
	return env.BindBuiltins(lisp.BindOpts{Export: true, Shadow: true}, builtins...)
}
```

`elpsutil.PackageLoader` packages do not use the language package; keep them
as they are unless `in-package` semantics are what the package had.

**Decoding arguments: `lisp.ReadArgs`.** An `ArgReader` decodes the
argument list into Go values with the usual `"<what> is not a string: <type>"`
messages. You pass the subject exactly as your hand-written `Errorf` spelled
it, or a whole format to `Typed`, so the text stays byte-identical. The first
failure wins, so check `Err` once at the end, reading arguments in the order
the checks ran:

```go
a := lisp.ReadArgs(env, args)
key := a.String(0, "first argument")
m := a.Typed(1, lisp.LSortMap, "second argument is not a map: %s")
limit := a.OptInt(2, "limit", 100) // &optional or &key; nil means the default
if lerr := a.Err(); lerr.Type == lisp.LError {
	return lerr
}
```

It does not allocate unless a check fails. The reads are `Value`, `String`,
`Typed`, `Opt`, `OptString` and `OptInt`. For a check that is not a type
test, `a.Check(ok, format, args...)` records a failure (and reports whether
the reader is still clean) and keeps the first failure, so a decoder of your
own is ordinary Go:

```go
func dateArg(a *lisp.ArgReader, i int) cctime.Date {
	s := a.Typed(i, lisp.LString, "argument is not a date: %v").Str
	d, err := cctime.Parse(s)
	a.Check(err == nil, "invalid date: %q", s)
	return d
}
```

**Typed builtins: `lisp.Func1` and `lisp.Func2`.** For a builtin of one or two
arguments, list one decoder per formal, in order, and write the body against
Go types. The decoders are the `ArgReader` reads above, so the check order and
the messages are fixed at compile time and match the hand-written checks.
Generics do this with no reflection and no allocation:

```go
var builtinRepeat = lisp.Func2(
	lisp.StringArg("first argument"),                              // "first argument is not a string: <type>"
	lisp.TypedArg(lisp.LInt, "second argument is not an int: %v"), // any other wording, verbatim
	func(env *lisp.LEnv, s string, n *lisp.LVal) *lisp.LVal { /* ... */ })
```

Decoders: `ValueArg`, `TypedArg` and `StringArg`. A function like `dateArg`
above is an `ArgDecoder` too. Beyond two arguments, or with `&rest`, use
`ArgReader` directly: Go has no variadic type parameters, which is where
generics stop. `string:split` and `string:repeat`
are written this way.

**Maps.** `env.MapRange(m, func(k lisp.MapKey, val *lisp.LVal) bool)` walks a
sorted-map in its documented order (int keys by value, then string and symbol
keys by spelling). Keys arrive by value, and for the interpreter's own map
backings it allocates nothing in the steady state, unlike `MapKeys` and
`MapEntries`, which build lists.
Where `(keys m)` would fail (m not a map, or larger than `MaxAlloc`),
`MapRange` returns that error with the same message and calls `fn` for
nothing. It charges no step and makes no context check.

**Steps and cancellation.** See "Charging steps from a Go builtin" below:
`env.Step()`, `lisp.ChargeStartedKiB`, `lisp.ChargeRecord` and
`env.CheckContext()`.

**Keyword arguments that cost no steps: `lisp.FreeKeywords`.** Every argument
of a call form is evaluated, and evaluating the keyword literal `:k` (which
only returns `:k`) is one step. So `(f x :a 1 :b 2)` costs two steps more than
a positional `(f x 1 2)`, which has pushed embedders into adding positional
twins of keyword builtins. Register a new builtin as
`lisp.FreeKeywords(def)` and the evaluator passes a keyword literal written
at one of its key-name positions as itself, unevaluated and uncharged. The
builtin receives the same value it would have received, so only the step
count changes:

```go
env.AddBuiltins(true, lisp.FreeKeywords(elpsutil.FunctionDoc("open",
	lisp.Formals("name", lisp.KeyArgSymbol, "order", "cache"), builtinOpen, "...")))
// (open "t" :order 32 :cache 8) costs what (open "t" 32 8) would.
```

The rules are exact, so step counts stay deterministic:

- Only bare, unquoted keyword literals (`:k`) in a direct call form, at
  argument index r, r+2, ..., where r is the number of required formals, are
  skipped. Values, keywords in required positions, a variable holding a
  keyword, `':k`, and every argument of a call through `funcall` or `apply`
  are evaluated and charged as always.
- The flag lives on the function value, so every binding and copy of it, and
  every template VM (lazy, eager, prewarmed), counts identically.
- The formals must be required names, then `&key` and at least one key: no
  `&optional` or `&rest`. `AddBuiltins` panics and `BindBuiltins` returns an
  error otherwise.
- Nothing else changes. A builtin without `FreeKeywords`, any Lisp function,
  and every existing program count exactly the steps they counted before
  (`TestKeywordStepsGolden` pins them against the counts measured before the
  feature existed).
- `FreeKeywords(def)` returns a wrapper, not `def`: a type assertion to your
  definition's concrete type fails on it, and reflection that walks
  definitions sees one more level (the wrapped definition is an embedded
  field). `Name`, `Formals`, `Eval` and `Docstring` forward to `def`.
- Wrapping an **existing** builtin with `FreeKeywords` changes the step count
  of every program that calls it with keywords. For a host that meters steps,
  that is a coordinated upgrade like any other step change. A new builtin can
  adopt it freely.

### Inspecting local variables

`env.Locals()` returns the local variables visible from an environment,
sorted by name, as `[]lisp.Binding{Name, Value}`. A Go builtin's `env` has
the caller's lexical scopes as parents, so inside a builtin it reports the
variables in scope at the call site: function parameters, `let` bindings and
`flet`/`labels` local functions (the function is the value) of every
enclosing scope, the innermost binding winning when a name
is shadowed. Package globals are not included (read them with `env.Get`).
The debugger's variables pane uses the same walk.

```go
// (trace-locals) prints the caller's local variables to the runtime's Stderr.
func builtinTraceLocals(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	for _, b := range env.Locals() {
		fmt.Fprintf(env.Runtime.Stderr, "%s = %v\n", b.Name, b.Value)
	}
	return lisp.Nil()
}
```

```lisp
(defun area (width height)
  (let ((result (* width height)))
    (trace-locals)   ; prints height = 3, result = 6, width = 2
    result))
(area 2 3)
```

The values are the live bound values, not copies: never mutate them, and
`Copy` any value kept after the builtin returns.

## Testing Functions

Use go package github.com/luthersystems/elps/elpstest and the lisp package
`testing` to write tests for custom packages.  See the standard library's tests
for examples of how to use these packages together.  The test definition
forms (`test`, `test-let`, `benchmark`, ...) are core `lisp` forms; they
register into the suite the `testing` package installs, which `elpstest`
loads for you.

`elpstest` piggybacks on the Go `testing` standard library: each lisp `test`
form in a file becomes a Go subtest.

```go
func TestMyPackage(t *testing.T) {
	r := &elpstest.Runner{
		// Load your package into each fresh test env (optional).
		LoaderFn: mypackage.LoadPackage,
	}
	defer r.Close()
	r.RunTestFile(t, "mypackage_test.lisp")
}
```

```lisp
; mypackage_test.lisp
(use-package 'testing)

(test "adds numbers"
  (assert= 3 (+ 1 2)))
```

### Parity tests for a Lisp-to-Go migration

`elpstest.ParityCheck` runs a native and the verbatim Lisp definition it
replaces, each in a fresh environment, over fixed and seeded generated
argument lists. It reports every difference in the result, the error
condition and message, the step count, and the writes (an `Observe`
expression evaluated after the call):

```go
elpstest.ParityCheck{
	Runner:   &elpstest.Runner{LoaderFn: loadWithNatives}, // registers cc:incr-key
	Legacy:   `(defun incr-key (m k n) (assoc! m k (+ (let ([x (get m k)]) (if (nil? x) 0 x)) n)))`,
	LegacyFn: "incr-key",
	NativeFn: "cc:incr-key",
	Setup:    `(set 'm (sorted-map "a" 1))`,
	Observe:  `m`,
	Cases:    [][]string{{"m", `"a"`, "1"}, {"m", `"b"`, "2.5"}, {"3", `"a"`, "1"}},
	Gen:      genIncrArgs, N: 200, Seed: 1,
}.Run(t)
```

Arguments are Lisp source evaluated on each side, so each side gets its own
fresh values. With `StepBudget` set, both sides run under that budget and must
fail (or succeed) alike; step counts are not compared then, because a native
that charges in bulk overshoots the budget where the Lisp stopped one step
past it. `Diff` returns the differences without failing the test.

`Extra` takes `elpstest.ParityCase` values for what source text cannot say:
`GoArgs` builds Go-valued arguments per side (a map with a custom backing,
say), `Setup` runs after the check's own, and `StepDelta` is the step
difference a native is meant to have (negative when it charges less). For
`Cases` and `Gen`, `StepDelta func(args []string) int64` does the same, so a
native with an intended difference keeps its step check instead of setting
`IgnoreSteps`.

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

A quoted symbol (`'sym`) is an `LSymbol` whose quoted flag is set: build one
with `lisp.Quote(lisp.Symbol("sym"))` and test for one with
`v.Type == lisp.LSymbol && v.IsQuoted()`.

**API change (#732).** The `lisp.LQSymbol` type tag and the `lisp.QSymbol`
constructor are removed. Nothing in the interpreter produced an `LQSymbol`;
replace `lisp.QSymbol(s)` with `lisp.Quote(lisp.Symbol(s))` (it renders the
same, `'s`) and drop `lisp.LQSymbol` from `switch` statements. Every `LType`
declared after it is one lower numerically, so do not persist `LType` numbers.

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

Use `l := lisp.SortedMap()` to construct an empty sorted map LVal. Keys may be
ints, strings or symbols; floats are not supported. Symbol keys are coerced to
string to avoid programming errors causing symbol and string keys with equal
string values from existing in the same map. An int key is distinct from the
string spelling the same digits (`lisp.Int(1)` and `lisp.String("1")` are two
keys), and int keys sort before string and symbol keys, in numeric order.
`GoMap` converts an int key to a Go `int`.

**Compatibility note for embedders (int keys, #733).** Three behaviours Go
code can observe changed when int keys were added:

- `MapSetLVal(lisp.Int(n), v)` on a stock map used to return an
  `unhashable type` error; it now stores the entry.
- `Keys()`, `Entries()`, `MapKeys()` and `MapEntries()` of a stock map can now
  return `LInt` keys (first, in numeric order). Code that assumed every key is
  an `LString` or `LSymbol` and read `key.Str` must handle `LInt`.
- `json:dump` of a map whose keys include an int -- a stock map or your own
  `Map` implementation -- used to fail with an invalid key type error; it now
  writes the int as its decimal string (`{"7":...}`), and fails only when that
  spelling is also one of the map's string keys. `json:load` still produces
  string keys only, so such a map does not round-trip to int keys.

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

### Typed JSON

`libjson.DumpTyped` and `libjson.LoadTyped` implement `:typed true` on the
`json:dump-bytes`, `json:dump-string`, `json:dump-message` and matching load
family. Typed JSON preserves elps types with Transit-verbose tag spellings on
the elps canonical byte form: shortest round-trip number text, UTF-8 byte key
order and the plain encoder's escape set, with float types preserved. A host
can store values and read them back with the same types in another process or
release. `libjson.Tag`, `libjson.Untag` and `libjson.Canonize` define the
format: `DumpTyped(v)` writes the bytes of `Dump(Tag(v), false)`.

```go
v := lisp.QExpr([]*lisp.LVal{lisp.Int(1), lisp.String("a"), lisp.Symbol(":k")})
b, err := libjson.DumpTyped(v) // ["~#list",[1,"a","~:k"]]
if err != nil {
    return err // a function, native, error or cyclic value cannot be stored
}

back, err := libjson.LoadTyped(b)
if err != nil {
    return err // bytes DumpTyped could not have produced
}
```

`libjson.Tag(v)` returns a plain JSON value. `libjson.Untag(v)` restores its types.
`DumpTyped(v)` writes exactly `Dump(Tag(v), false)` without building the transformed value.
`LoadOpts{Strict: true, ExactIntegers: true}` accepts canonical plain bytes for composition with `Untag`.
Whole floats use `~d` plus plain float text, including `~d1`, `~d-0`, and `~d1e+21`.

For hashing and cache/state keys, use canonical output and let errors fail the
operation. An explicit string-number mode fixes the numeric representation:

```go
key, err := libjson.DumpWith(v, libjson.DumpOpts{Canonize: true, StringNumbers: false})
if err != nil {
    return err
}
sum := sha256.Sum256(key)
```

Canonical bytes are frozen and equal plain dump with the same explicit number
mode whenever canonize succeeds. See [Hashing guidance](typed-json.md#hashing-guidance).

The encoding is finer than `equal?`: `1` and `1.0` encode differently, so do
a string and a symbol of one spelling. `DumpTyped` returns an error, never a
lossy encoding, for any value it cannot represent. `LoadTyped` is safe on
untrusted input: it never panics, accepts only canonical bytes, and stops at
the limits set by `WithTypedMaxDepth`, `WithTypedMaxBytes` and
`WithTypedMaxValues` (defaults `DefaultTypedMaxDepth`, `DefaultTypedMaxBytes`,
`DefaultTypedMaxValues`); a limit error wraps `ErrTypedLimit`.
`WithTypedCharge` meters an encode as its output grows. Every value
`LoadTyped` returns is freshly allocated, so the caller owns it.

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

### Source names in error stacks

The `"file"` key of each frame `error-stack` returns, like the file in a
rendered stack trace, is the `name` your `SourceLibrary.LoadSource` returned
for that file -- not a host path. `RelativeFileSystemLibrary` and the other
built-in libraries return the bare file name (`filepath.Base`).

A custom `SourceLibrary` should return names that do not depend on the host:
no absolute paths, home directories, temporary directories or machine names.
Lisp code can read these names through `error-stack` and fold them into
results, so where several peers must compute identical results from the same
program (a replicated ledger, for example), a name that differs between
machines makes their results differ.

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
| `WithMaxSleep(d)` | one hour (`DefaultMaxSleep`) | Ceiling that the `:max` of a host sleep built on `libtime.BuiltinSleep` cannot exceed. |

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

`GoValue`, `GoSlice`, `GoMap` and their `WithRuntime`/`Of` forms may return
results that share Go containers where the lisp value shared them: a list or
map reached along several paths can appear as one `[]any` or `map[any]any` in
each place. This keeps a value with nested sharing, such as one built by
`(set! x (list x x))` repeated many times, linear to convert rather than
exponential. The deprecated JSON serializer's `GoValue`, `GoSlice` and `GoMap`
follow the same rule, sharing `[]any` and `map[string]any` values the same
way. Treat converted results as read-only, or deep-copy them before mutating.

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

The standard library charges this way for work proportional to value size.
`json` (`load-string`, `load-bytes`, `load-message`, `dump-string`,
`dump-bytes`, `dump-message`, `message-bytes`), `base64` (`encode`,
`decode`), `string` (`split`, `join`, `repeat`, `lowercase`, `uppercase`,
`trim`, `trim-left`, `trim-right`, `trim-space`, `contains?`) and `regexp`
(`regexp-compile`, `regexp-match?`, and a string pattern compiled on demand)
charge one step per complete KiB of the input they scan, or of the output
they build (`join`, `repeat`, and the JSON encoders, which charge after
encoding). Values under 1 KiB add nothing, so step counts change only for
programs that handle larger values. The charge depends only on the values'
sizes, so it is deterministic.

Limits and choices worth knowing:

- **JSON dumps are charged after encoding.** The encoded size is not known
  until the walk finishes, so a budget cannot stop a huge dump part way. The
  value's size is still bounded by the steps that built it and by
  `WithMaxAlloc`, and the charge makes the dump fail the evaluation right
  after. The charge is deterministic because the encoded bytes are a function
  of the value alone: sorted-map entries are emitted in key order, native Go
  values go through `encoding/json` (which sorts Go map keys), and floats are
  formatted by `strconv.AppendFloat` with shortest round-trip precision. The
  one exception is a sorted-map backed by an embedder's own `Map`
  implementation, which is emitted in the order that implementation returns.
- **`regexp-match?` charges the input text only.** Go's RE2 engine matches in
  time linear in the input for a compiled program. The pattern is charged
  when it is compiled from a string, by `regexp-compile` or by a
  string-pattern argument compiled on demand, and a precompiled regexp is not
  charged again.
- **Rounding.** The stdlib charges complete KiB, floor(n/1024), because the
  call's own evaluation step covers the first KiB. An embedder's builtins may
  round differently. For example, luthersystems/substrate's storage builtins
  charge every started KiB, ceil(n/1024).

Helpers for the common charges return `lisp.Nil()` to continue, or the
`LError` to return unchanged, as `ChargeSteps` does; check a result with
`lerr.Type == lisp.LError`. For n steps at once, call `env.ChargeSteps(n)`.
They charge exactly what their names say, so replacing a hand-written
`ChargeSteps` call with one changes no count:

| Helper | Charge | Use |
|--------|--------|-----|
| `env.Step()` | 1 | once per element of a native loop that replaces a Lisp loop |
| `lisp.ChargeStartedKiB(env, n)` | ceil(n/1024) | every started KiB (substrate's storage convention) |
| `lisp.ChargeRecord(env, n)` | max(1, ceil(n/1024)) | per record handed to a reducer, empty records included |
| `env.CheckContext()` | 0 | the evaluator's `context-cancelled` check, between charges |

There is no public floor(n/1024) helper: the stdlib's is internal. Which
convention a builtin uses is observable in its step counts, so keep the one
the code you replace used.

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

#### Macro-generated definitions

A macro that expands into definitions (`defun`, `defmacro`, `set`,
`deftype`) is invisible to name-based analysis: the analyzer sees the call,
not the names it creates. Give `analysis.Config` a `MacroExpander` and the
analyzer expands macro calls and analyzes the expanded code. The usual
expander is `&analysis.EnvMacroExpander{Env: env}`, which expands only macros
already defined in `env`: load the workspace's macros into it first with
`expander.LoadWorkspaceMacros(forms)`. `LintFiles` does both itself when
given `LintConfig.Env`.

- Top-level macro calls are expanded before the deep walk, so a generated name
  resolves even where it is used before the call, like an ordinary `defun`.
- Each generated package-level definition records the call that produced it in
  `Symbol.GeneratedBy` (`*analysis.MacroOrigin`: the macro's name, the package
  it was expanded in and the call site). Nested macros record the outermost
  call, the one written in the source.
- `Result.GeneratedDefinitions()` returns those definitions as
  `[]analysis.ExternalSymbol` with `GeneratedBy` set. They are plain data, so
  an embedder can index them and pass them as `Config.ExtraGlobals` when
  analyzing other files, much as `go/analysis` passes facts between packages.

```lisp
; counters.lisp
(defmacro defcounter (name getter bumper)
  (quasiquote
    (progn
      (set (quote (unquote name)) 0)
      (defun (unquote getter) () (unquote name))
      (defun (unquote bumper) () (set! (unquote name) (+ 1 (unquote name)))))))

(defcounter hits hits-value bump-hits)
```

```go
// forms is the parsed source of counters.lisp. Loading it defines
// defcounter in env, so the expander can expand the call below it.
expander := &analysis.EnvMacroExpander{Env: env}
expander.LoadWorkspaceMacros(forms) // returns one error per form that failed
lib := analysis.AnalyzeFile(src, "counters.lisp", &analysis.Config{
    MacroExpander: expander,
})
facts := lib.GeneratedDefinitions() // hits, hits-value, bump-hits; GeneratedBy.Macro == "defcounter"
other := analysis.AnalyzeFile(otherSrc, "report.lisp", &analysis.Config{ExtraGlobals: facts})
```

With an expander, a call whose head starts with `def` is analyzed through its
expansion instead of the name-based guess; `Config.DefForms` entries still
take priority. When expansion fails (the macro is not loaded in the env, or
it signals), the analyzer falls back to that name-based guess, and a name it
guesses has `GeneratedBy` nil. Each call site is expanded at most once per
analysis.

### Walking and expanding code

Tools that analyze ELPS source need to know which parts of a form are code.
`lisp.CodeWalker` walks a form as code: it knows the binding shape of every
special form, never descends into quoted data, tracks lexical bindings, and
optionally expands macros as it goes. It reports what it sees to a visitor as
`lisp.WalkNode` events: `WalkForm` (a compound form, after expansion),
`WalkRef` (a symbol evaluated as a reference; `Bound` says whether a binding
inside the walked form introduces it), `WalkSet` (`set!` targets), `WalkBind`
and `WalkDefine` (names introduced), `WalkEnter` / `WalkLeave` (scopes;
`Function` marks function bodies), `WalkLiteral` and `WalkData`.

```go
import (
    "github.com/luthersystems/elps/astutil"
    "github.com/luthersystems/elps/lisp"
)

// Collect the global functions a form calls, ignoring local functions.
calls := map[string]bool{}
astutil.ExpandAll(form, nil, "user", func(n *lisp.WalkNode) bool {
    if n.Event == lisp.WalkRef && n.Head && !n.Bound {
        calls[n.Node.Str] = true
    }
    return true
})
```

Two entry points share one walker:

- `astutil.ExpandAll(form, expander, pkg, visit)` expands every macro call an
  `astutil.MacroExpander` can expand (an `*analysis.EnvMacroExpander` is one),
  then visits the expanded code (with a nil expander it only walks). Calls
  to local macros are not entered.
- `(*lisp.LEnv).MacroExpandAll(form)` resolves heads in a live environment and
  expands `macrolet` macros too. It is what `macroexpand-all` calls.

None of them writes to the input. A list on the path to an expansion is
rebuilt as a fresh, unsealed list carrying the original's source location;
every untouched subtree is returned as the same node, so positions in the
result still point into the original file. A special operator an embedder
registers has no known shape, and the walker treats a form headed by one as
opaque: its arguments are neither walked nor expanded.

### Asking where a node is

Two queries in `astutil` are built on the same walker:

- `astutil.ClassifyNodes(form)` gives each node a `Role`. `RoleData` means
  quoted data, including anything inside it. `RoleSyntax` means structure a
  form reads but does not evaluate. The reader turns `[x (f)]` in a `let` and
  `'(x (f))` into the same kind of value; `Role` tells them apart.
- `astutil.FindCalls(form, names...)` returns each call to one of `names` in
  code position, skipping local functions that shadow those names. Each
  result lists the special forms and function bodies on the call's path, as
  `Enclosure`s. A macro that must reject a call inside a `lambda`, handler or
  `quasiquote` can inspect them.

Scope questions (what a name refers to, what a closure captures) go to the
`analysis` package. Its resolver walks code with `lisp.CodeWalker` too, so
the repository has one walker that knows special-form syntax and one scope
resolver.

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
builtin" below). For Go macros, use `elpsutil.FunctionDoc` and register through
`AddMacros` or `PackageMacros` (see [Writing a Go macro](#writing-a-go-macro)).

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
