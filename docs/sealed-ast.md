# Sealed ASTs: write protection for shared program literals

This document explains the `IsSealed` mechanism end-to-end: what it defends
against, how it works, what each verification layer catches (and does not),
and what an embedder must do to stay inside the contract. Every claim below
is grounded in code or a test in this repository; file references are given
inline.

Audience: reviewers of the sealing work, and authors of Go code that embeds
elps or extends it with builtins.

The invariant this document establishes also pays a construction dividend:
`LEnv.Fork` clones a loaded environment by *sharing* every sealed value and
copying only the mutable remainder. See [docs/fork.md](fork.md) for that
API and its embedder contract.

---

## 1. Threat model: cross-environment corruption of shared parses

A host that caches parser output evaluates the same `[]*lisp.LVal`
expression tree in many environments — substrate's parse cache is the
motivating consumer, handing one warm parse to every transaction in the
process. Safety used to rest on an *implicit* structural invariant: elps
literal syntax can only produce lists, symbols, strings and numbers, and
(almost) every mutating builtin type-errors on those types, so (almost) no
lisp-reachable mutation can touch an AST node.

The parenthetical "almost" was the bug class. When any code path writes a
parsed node in place, it silently rewrites the program for **every**
environment sharing the parse — past, present, and future evaluations in the
process (luthersystems/substrate#378 was exactly this class). These are the
shipped instances:

| Vector | Mechanism | Where fixed / pinned |
|---|---|---|
| elpspath `!` ops (substrate#378) | `?set!`/`?del!`/`?nil!` obtained a list's live cell backing via `toCells` and shifted it in place before panicking on the (absent) LArray dims cell; a quoted-literal input meant the shift landed in the shared AST | `errMutateList` in `lisp/lisplib/libelpspath/path.go` rejects list inputs on every mutating entry point; `TestMutateListRejected` (`path_mutate_list_test.go`) parses once, evaluates in two envs, and fingerprints the AST before/after |
| `stable-sort` on a quoted literal (elps#369, mechanism 1) | `(stable-sort < '(3 1 2))` sorted the literal's `Cells` in place — the "documented in-place effect" on a program literal *was* the corruption | Sealed-write guard in `builtinSortStable` (`lisp/builtins.go`); kill tests in `lisp/cow_seal_test.go` and `lisp/sealed_write_error_test.go` |
| append through slice capacity (elps#369, mechanism 2) | `(append 'vector (slice 'list '(1 2 3) 0 1) x)` wrote `x` into the spare capacity of the literal's backing array, overwriting its second element — Go-style `append` semantics applied to shared storage | Sealed-write guards in `builtinAppend` and `builtinSlice` (`lisp/builtins.go`) |
| macro-stamp metadata write (elps#370) | `stampMacroExpansion` re-stamped a shared node's `source` location after expansion — a cross-environment metadata write and a data race under concurrent envs | Sealed-subtree skip in `lisp/macro.go`; caught by the `-race` watchdog class of checks (§3.4) |
| `macroexpand` `&rest` laundering (commit 8d18071) | `builtinMacroExpand`/`builtinMacroExpand1` sliced `form.Cells[1:]` of a possibly-sealed input and handed it to the macro, so a `&rest` parameter became an **unsealed** header over sealed backing; `stable-sort` or `append!`-through-`slice 'vector` in the macro body then rewrote the literal. The normal call path was safe (`evalSExprCells` copies arguments); this was the one path reaching a macro without copying | `macroExpandArgs` (`lisp/builtins.go`) copies into fresh backing; `TestMacroexpandRestDoesNotLaunderSealedBacking` (`lisp/cow_seal_test.go`) |
| format-mode `Meta` write (commit 8d18071) | Format-preserving `Parse()` attached a same-line trailing comment to `expr.Meta` *after* `ParseExpression` sealed the node — a write to already-sealed storage and a stale fingerprint in checked builds | Format-preserving parses are no longer sealed (they are tooling-only trees, never evaluated or shared); `parser/rdparser/seal_format_test.go` |

Two properties of this class make it worth a dedicated mechanism:

- **The write looks innocent at the site.** `cells[i] = x` on a local slice
  variable, `append(cells, v)`, a metadata stamp — nothing in the expression
  says "shared program storage". #369 and #370 shipped past a manual audit.
- **The blast radius is process-wide and delayed.** The corrupted literal is
  re-evaluated by unrelated environments arbitrarily later, so the failure
  appears far from the cause.

## 2. The mechanism

### 2.1 The sealed bit

`LVal` carries an unexported `sealed bool` (`lisp/lisp.go`) that marks a node
of a parsed program. The field occupies an existing padding byte: `LVal` is
112 bytes on 64-bit platforms with or without it, pinned by
`TestLValSizeUnchanged` (`lisp/cow_seal_test.go`).

The parser sets it: `rdparser.ParseExpression` calls `(*LVal).SealAST()` on
each completed top-level expression at nesting depth 1 — after every nested
parse call has finished its construction-time fixups, so the parser itself
never writes a sealed node's fields (`parser/rdparser/parser.go`,
`lisp/seal.go`). Every parse path an evaluator uses — `Reader.Read`,
`LoadString`, `ReadProgram`/`ParseProgram`, the REPL — funnels through it.
Format-preserving parses (`rdparser.NewFormatting`) are deliberately
excluded: they are consumed only by tooling (formatter, minifier, lint,
mcpserver, analysis) and `Parse()` finishes constructing their `Meta` after
the seal point (see the 8d18071 entry above).

`SealAST` walks `Cells` and marks only parser-producible node types
(`LSExpr`, `LQuote`, `LSymbol`, `LQSymbol`, `LString`, `LInt`, `LFloat`);
runtime-only types (functions, arrays, maps, bytes, natives) stop the walk —
freezing storage the evaluator legitimately mutates would be wrong. The
Nil/true/false singletons are **born sealed** (elps#376): the flag is set in
their composite literals at package init (`lisp/singleton.go`), so no
post-construction write exists to race with anything and `SealAST`'s
already-sealed check stops the walk at one. `IsSealed()` therefore reports
the do-not-mutate contract on `Nil()`/`Bool()` results too, the guarded
mutation sites below treat a singleton operand as an empty sealed input —
accepted under the empty carve-out of §2.4, with fresh storage handed back
(`TestSingletonCoWContainerOps`, `lisp/singleton_seal_cow_test.go`) — and
`SetSource` on a singleton is a no-op. Atoms *are* sealed: unlike the
eager-copy design, where copying atoms measured a +56% geomean regression,
marking one costs a bit at parse time, and it makes `IsSealed` meaningful on
every node a literal can produce.

The flag is **monotone**: set once by `SealAST` before the tree can be
shared, never cleared on the same storage, so concurrent readers of a shared
parse are race-free.

### 2.2 Why quote/macro/lambda hand out references

The alternative design (the `exp-ast-leakpoints` experiment) copied the
subtree at every point where evaluation hands an AST node to the value
domain — quote evaluation, quasiquote literal leaves, macro argument
binding, lambda body embedding. That buys pointer disjointness outright but
costs a deep copy per quoted literal per evaluation, measured at **+1.5% to
+15.5% per warm transaction** on a production workload (`lisp/seal.go`).

The sealed design keeps those paths byte-identical to the historical
evaluator: quote, quasiquote, macro binding and lambda embedding hand out
the sealed nodes directly. Zero copies, zero allocations, zero
per-evaluation cost — the warm-transaction cost of the whole scheme measured
statistically zero (geomean −0.11%, allocations bit-identical; PR #374).
The cost moves to the *mutation* sites, which are rare.

### 2.3 Seal propagation

The constraint follows the **storage**, not the header:

- Kernel sites that create a *new header over sealed backing* propagate the
  flag explicitly: `builtinCDR`, `builtinRest`, and the two-index
  intermediate in `builtinSlice` set `r.sealed = v.sealed`
  (`lisp/builtins.go`).
- Header copies that share backing (`Quote`, `Splice`, `shallowUnquote` —
  the `*cp = *v` struct-copy idiom) inherit the flag through the struct
  copy.
- `Copy()` and the in-kernel `detach()` **clear** the flag on the fresh
  storage they create (`lisp/lisp.go`, `lisp/detach.go`). A `Copy` owns fresh top-level
  backing, so the constraint on the original does not apply to it; elements
  shared with the sealed tree remain individually sealed, which is exactly
  the seal contract — restructure the copy freely, never the shared nodes
  inside it.

### 2.4 The sealed-write error at kernel mutation sites

The mutating builtins that can legally receive a sealed value check the flag
and refuse a non-empty sealed input with the catchable `modify-literal-error`
condition — message `cannot modify a program literal; take a (copy ...)
first` (`lisp/builtins.go`, `CondModifyLiteral` in `lisp/conditions.go`):

- `stable-sort`: refuses to sort a sealed list — the documented in-place
  effect on a program literal was never useful; observing it *was* the
  corruption.
- `append 'vector`: refuses a sealed sequence — appending within spare
  capacity would write the shared program's storage, and even a no-value
  append would wrap the literal's backing in a mutable vector.
- `slice 'vector`: refuses to wrap sealed backing in a mutable vector;
  `slice 'list` returns a sealed intermediate (backing still shared).

The **empty carve-out** is deliberate: ordinary builtins (`cdr`, `rest`,
`keys`, empty `make-sequence`) return the shared *sealed* empty list, so
erroring on it would make `(stable-sort < (rest xs))` fail only when `xs`
happens to be short — a data-dependent error in correct runtime code. An
empty sealed input has no storage to write or alias, so the guarded sites
accept it and hand back fresh, unsealed storage.

These sites originally resolved a sealed input by *silently copying*
(copy-on-write), preserving compatibility while the policy question of
elps#378 was decided on evidence. Two checked-mode censuses answered it:
every firing across this repository, its benchmarks and examples, the
production phylum corpus, and the downstream consumer's full suites came
from test machinery built to exercise the copy paths — zero from real code.
The silent copy also bifurcated one expression's semantics by its input's
provenance and could mask code that believed it owned the value, so all
three sites were flipped together to the hard error and the census
machinery was retired. The lisp-level remedy is `(copy x)` (§4.4), which
returns a fully unsealed deep copy, so the mutating builtin takes its
ordinary in-place path; the error message names it.

`assoc!`/`dissoc!`/`append!`/`append-bytes!` need no guard: they type-error
on lists, and maps/vectors/bytes cannot be produced by the parser, so a
sealed value cannot legally reach their mutation path.

The evaluator's own metadata writes respect the flag too:
`stampMacroExpansion` skips sealed subtrees (`lisp/macro.go` — a sealed
node's descendants are all sealed, so the whole subtree is skipped), and
`SetSource` is a no-op on sealed values (`lisp/lisp.go`), so a parsed node
keeps its parse-time location forever.

libelpspath (`lisp/lisplib/libelpspath`) sits in front of the kernel guards
with its own policy: the in-place `!` path ops **refuse** list inputs
outright (`errMutateList`) rather than copy — arrays and sorted-maps keep
their documented in-place semantics (`TestMutateArrayMapInPlace`), and the
non-mutating ops build fresh nodes via `lisp.SExpr`/`lisp.Array`, which are
unsealed by construction.

### 2.5 The embedder boundary

The parse/cache boundary exposes no raw AST: `lisp.Program`
(`lisp/program.go`) wraps parse output opaquely, the per-file load cache hook
does the same for `load-file` (§2.9), and the package registry
seals its LVal-bearing surface (§2.8 for what its admission promises per
value class). Deep-copy machinery for owned expressions
exists in-kernel (`detach()`, `lisp/detach.go` — whose walker also backs the
lisp-level `copy` builtin in a within-env mode, elps#378) but is unexported:
it will be re-exported
when a real embedder consumer (debugger workflows, cross-runtime transfer)
materializes. An embedder that hand-builds expression trees and
shares them across environments may call `SealAST()` itself for the same
protection.

### 2.6 Sealed builtin formals

The repository's own definition tables are the first in-tree consumer of
that hand-seal contract. Builtin, macro and special-op formals are built
once at Go program initialization in package-level tables (`langBuiltins`
et al. in `lisp/`, the `libutil.Function`/`FunctionDoc` tables in
`lisp/lisplib/`) and consulted by every Runtime in the process — the
mutable-aliasing producer pattern behind issue #363. The tables' formals
are sealed at construction (`sealDefaultFormals` in `lisp/builtins.go`, the
`libutil` constructors, `libgolang`'s package init), and registration
(`registrationFormals` in `lisp/env.go`) aliases the sealed list into each
environment — the same treatment lisp-defined functions get, whose formals
are sealed parser output aliased into every closure. An unsealed formals
list from a third-party `LBuiltinDef` still gets a defensive deep copy at
registration. Sealing instead of eagerly copying keeps environment
construction free of per-builtin formals copies (the eager copy measured
~90KiB and >1000 allocations per `LoadLibrary` environment);
`TestNoCrossEnvironmentLValSharing` (`lisp/shared_formals_test.go`)
asserts the resulting sharing is sealed-only.

### 2.7 The exported-field surface (issues #362, #382)

The seal is a runtime layer; the field-privatization layer closes the same
write channels at compile time.  Every historical metadata corruption went
through an exported `LVal` field — the #333/#334 singleton race wrote
`Quoted`, #370's stamp wrote `MacroExpansion` and source metadata onto
shared parser nodes, and the post-seal leak fixes were `Meta`-adjacent
writes — so those fields are unexported (#362 for `source`, #382 for
`quoted`, `spliced`, `meta`, `macroExpansion`):

- **Reads are mediated**: `Source()` returns a location copy, `IsQuoted()`
  reads the quote flag, `MacroExpansion()` returns a `MacroExpansionMeta`
  snapshot, and function identity stays on nil-safe `FID()`/`Package()`/
  `Builtin()`.  Formatting metadata has no exported reader at all — it is
  typed by `internal/fmtmeta` and reachable only inside this module through
  `internal/fmtraw`.
- **Writes are construction-time or in-kernel only**: `Quote`/`Splice`/the
  parser set the flags, `stampMacroExpansion` is the only expansion-metadata
  writer, and the format-preserving parser is the only `meta` writer, on
  trees it owns.
- **In-repo tooling crosses on internal hook bridges** (`internal/astraw`
  precedent): `internal/fmtraw` (formatting metadata), `internal/funraw`
  (captured closure environments — the deepest aliasing channel `Env()`
  used to hand embedders), `internal/macroexp` (test-only metadata
  fabrication for debugger tests).
- **The remaining exported fields are the deliberate data-read surface**:
  `Native`, `Str`, `Cells`, `Type`, `Int`, `Float`, `FunType` (accessor
  migration priced at ~3,000 downstream sites and rejected; writes there
  are covered by the seal, elpsvet, and checked mode).  `MapData`'s backing
  is fixed at construction (`NewMapData`).  `TestLValFieldSeal`
  (`lisp/lval_fields_seal_test.go`) is the regression guard: re-exporting a
  metadata field, or adding a new exported field without a review
  conversation, fails the suite.
- **`LEnv` got the same treatment one layer up.**  `LVal` was never the
  only mutable channel an embedder holds: every builtin is handed an
  `*LEnv`, and while its scope map was exported, `env.Scope[sym] = v`
  rebound a symbol in a live environment — or, through a closure's captured
  environment, in every function value that closed over it — without
  passing `Put`, the seal, or elpsvet; `env.Loc = loc` aliased a caller's
  mutable location into every error and frame stamped afterwards (the #362
  class).  `scope`, `funName`, `parent` and `loc` are unexported; reads go
  through `Bindings()` (an `iter.Seq2` over the immediate scope),
  `NumBindings()`, `Parent()` and `Source()` (a location *copy*).
  `Runtime` and `ID` stay exported — neither is a container an embedder can
  corrupt in place.  `TestLEnvFieldSeal` guards it.

The decisions are census-driven, not aesthetic: each field was counted with
a type-checked selector census over this repo (outside package `lisp`) and
over a production-scale downstream embedder.

| Field | Downstream prod | Downstream test | In-repo (outside `lisp`) | Verdict |
|---|---|---|---|---|
| `LVal.Spliced` | 0 | 0 | 0 | unexported, no accessor |
| `LVal.Meta` | 0 | 0 | format tooling | unexported, `internal/fmtraw` bridge |
| `LVal.MacroExpansion` | 0 | 0 | debugger | unexported, snapshot accessor |
| `LVal.Quoted` | 1 read | 6 | wide | unexported, `IsQuoted()` |
| `LVal.Native`/`Str`/`Cells`/`Type`/`Int`/`Float`/`FunType` | ~3,000 | — | wide | **stay exported** |
| `LEnv.Scope`/`FunName`/`Parent`/`Loc` | 0 | 0 | 4 / 0 / 5 / 0 reads | unexported, mediated reads |
| `LEnv.Runtime`/`ID` | 16 / 3 reads | 17 / 0 | 110 reads / 0 | **stay exported** |
| `Runtime.*` (`Stderr`, `Reader`, `Library`, `Debugger`, `LoadCache`, `Profiler`, `Registry`, `Package`, `Stack`) | 8 writes, 7 reads | 15 | 20 writes, 89 reads | **stay exported** |
| `CallStack.Frames`/`GoStack`, `CallFrame.*` | 0 | 1 read | 30 reads, 0 writes | **stay exported** |

`Runtime` is the deliberate non-break, and that reasoning deserves to be as
explicit as the breaks.  Its exported fields are embedder *configuration* —
`Stderr`, `Reader`, `Library`, `Debugger`, `LoadCache` and `Profiler` are set
by the host
before or between evaluations, the documented way to attach a logger, a
debugger, a source library or a parse cache — and its live fields (`Registry`, `Package`,
`Stack`) are per-interpreter state, not shared parse-tree bytes: a bad write
corrupts *that* interpreter, in its own goroutine, where the damage is
observable.  None of the incidents this design answers (#333/#334, #369,
#370) travelled through a `Runtime` field.  Splitting it into a config
struct plus sealed live state would break 8 downstream production writes
and 15 downstream test sites to close a channel with no incident history
and no cross-value blast radius: cost without the argument the other breaks
have.  It stays open knowingly, not by omission.

`CallStack` is the same call, cheaper to make: no downstream production site
touches it, but the 30 in-repo reads (diagnostics, the DAP server, the REPL)
would all need a copying accessor, and the worst an exported `Frames` slice
buys an attacker is a wrong stack trace in their own interpreter — no shared
bytes, no other goroutine.  Deferred, with the numbers on the table rather
than an implicit "nobody asked".

### 2.8 Package registry admission (issue #524)

`PackageRegistry.AddPackage` is the second exported surface with the shape
§2.5's `Program` constructors had: a caller hands the kernel a container of
`*LVal`s it still holds pointers to, and a `Runtime` starts serving them. A
package built by hand in Go, or lifted out of another `Runtime`'s registry
by the doc/LSP/MCP merges (`cmd/doc.go`, `mcpserver`'s per-request doc env —
the path a booted downstream registry reaches the tools through), used to be
stored as-is.

`newProgram`'s rule does not transfer, because a package's contents are not
parse output. A symbol table legitimately holds Go builtins, lisp closures,
natives, sorted-maps, arrays and runtime data — values the seal *deliberately*
declines to mark, because the evaluator mutates them. "Seal everything" would
freeze storage the evaluator writes, and "reject the unsealable" would reject
every real package. So the admission is stated per value class
(`lisp/package_admit.go`); `AddPackage` registers a private **snapshot** of
the package whose bindings are:

| Value class | Admission |
|---|---|
| singletons; values **sealed throughout** | shared by reference — the sanctioned share |
| **sealable throughout but unsealed**: runtime-built lists, symbols, strings, numbers ("code-like trees") | private `Copy()` + `SealAST()` |
| everything else: functions, natives, sorted-maps, arrays, bytes, errors, tagged values, and trees holding one | shared by reference; **custody transferred** |

The middle row is the hazard. An unsealed code-like tree is fresh mutable
storage the caller still aliases: `stable-sort` in the registry's `Runtime`
rewrites it under the caller and under every other registry the same package
was added to, and a write through the caller's retained pointer rewrites what
the `Runtime` evaluates — substrate#378's corruption class with a package for
a vehicle. `Copy()` severs the alias, `SealAST()` freezes the registry's copy
and (in checked builds) enrols it in the census, which is why the
fingerprint verifier covers admitted bindings at all.

The sealed fast path is load-bearing rather than an optimization note: values
produced by evaluating literals are *already* sealed, so a package built by
loading lisp source is admitted with its bindings shared, exactly as a
`Program` of sealed parser output is.

The last row is where this admission is knowingly weaker than
`newProgram`'s, and the honesty is the point: a closure's captured `*LEnv`
cannot be copied, and reference types are reference types on purpose. For
those, `AddPackage` promises custody transfer, not isolation — the caller
must stop writing them, and evaluating one shared closure under two
`Runtime`s stays a bug that checked mode reports rather than one the
admission prevents.

Two consequences for callers: the registry does not hold the caller's
`*Package`, so binding into it after `AddPackage` no longer reaches the
`Runtime` (bind through the environment instead); and the snapshot reads the
package's maps on the calling goroutine, so it needs the same "no concurrent
writer" discipline every other read of a `*Package` does (#397).

**Sibling mutators.** The audit behind #524 covers every exported member of
`PackageRegistry`/`Package` that stores a caller-supplied `*LVal`.
`Package.Put`/`Update` keep storing what they are given: they are the write
path every `set` reaches through `LEnv.PutGlobal`, where the value belongs to
the environment already evaluating it and `LEnv.Put`/`PutGlobal` have taken
the ownership sighting — an admission walk there would tax the interpreter's
hot path to guard a transfer that is not happening. `Export`/`Exports` store
names only (`Exports` already copies its slice), and `DefinePackage` builds a
fresh package. The read side is the residual: `PackageRegistry.Package` hands
out the registry's live `*Package` — which is how the merges enumerate a
booted registry — and a caller can `Put` through it. Same residual as §2.7's
exported fields: the boundary stops accidental sharing, not a caller that
goes looking for interpreter state.

### 2.9 The sealed load-file cache hook (issue #368)

`Program` (§2.5) closes the parse/cache boundary an embedder drives
*directly*. It leaves one seam open, and it is the seam a phylum actually
loads through: `load-file` reaches the parser via `Runtime.Reader`, so an
embedder that wants a per-file parse cache has exactly one place to put it —
a custom `Reader` — and that place hands it raw `[]*lisp.LVal`.
substrate's `cacheReader` is that shape, and it is not a misuse of the API;
it is the only shape the API offered.

`Runtime.LoadCache` (`lisp/loadcache.go`) is the elps-owned hook that closes
it. The embedder supplies *policy* — key lookup, retention, eviction — and
never touches data:

```go
type LoadCache interface {
        Load(key string) (*CachedSource, bool)
        Store(key string, src *CachedSource)
}
```

`*CachedSource` is opaque exactly as `Program` is: only elps mints one, and
no exported member yields a `*LVal` (`TestCachedSourceIsOpaque` is the
reflection guard, `TestProgramSeal`'s sibling). Every `Load*` entry point
funnels through `(*LEnv).readCached`: on a miss it reads the stream, derives
a key, parses, admits the parse through **`newProgram`** — the same single
admission point §2.5 describes — stores the entry, and hands the sealed tree
to the load; on a hit it hands that same sealed tree to the next environment
**by reference**.

**What the aliasing rests on** is entirely existing rails; no new admission
class was added:

- the tree is sealed throughout by construction, because `newCachedSource`
  routes every parse through `newProgram` (reference types rejected,
  already-sealed output admitted as-is, anything else privately copied and
  sealed, the unsealable rejected);
- a sealed node may be reached by more than one `Runtime` **by design** —
  §3.3's ownership allowlist entry 2, whose first named topology is
  substrate's warm parse cache, which is this one;
- lisp-level writes through the shared tree raise `modify-literal-error` at
  the guarded kernel sites (§2.4);
- the evaluator's own metadata writes skip sealed nodes, so a **debugger**
  needs no private copy of a cached tree (below);
- checked builds re-verify the tree after every load through `(*LEnv).load`
  (§3.3), so a corrupted entry is reported at the load that corrupted it.
  What that per-root verification covers is exactly one claim — *the sealed
  bytes never change after admission* — so `-tags elpscheck` proves no load
  rewrites a cached node (and, with the flag-vs-type fix above, that a
  laundered non-sealable node still trips the cross-runtime gate);
- checked builds also re-verify each entry **as it is served**
  (`verifyCachedSourceOnHit`), against the fingerprint taken at admission.
  That is a different claim from the one above, and per-root verification is
  structurally incapable of making it: a `Reader` that refills one output
  slice per call substitutes another file's roots wholesale, and those roots
  are legitimately sealed and match their own seal-time records perfectly. An
  entry-level fingerprint sees the substitution; a per-root one cannot. It
  costs one fingerprint walk per hit, in checked builds only — which gives up
  the hook's zero-work hit under `-tags elpscheck`, the right trade for the
  mode whose job is to make the seal's claims checkable. What remains
  unproven by the checked build is a wrong-program serve that goes through the
  *key* — that class is closed by the key's composition and the admission
  conjunction instead.

A parse the admission refuses is **not cached**: it is handed to that one
load and forgotten, so the load behaves exactly as it would with no cache.
The alternative — store it and copy on every load — would put an unsealed
tree in a process-wide cache, which is the topology the hook exists to make
impossible.

**Keying deviates from substrate's deliberately.** `cacheReader` keys on a
hash of the source content alone, which is safe for its own topology (a
file's name and location are stable across loads) but not in general: two
files with identical text would share an entry and the served tree carries
the *first* file's parse locations, so every error raised from the second
names the wrong file. elps digests the content **and** the stream's name and
location — and the identity of the *reader* that parses them and which reader
*method* (`Read` vs `ReadLocation`) is in use — each length-prefixed. The key
binds the entry's producer, not merely its input: without the reader and
method components, two Runtimes with different `Reader`s served each other's
parses, a swapped `Runtime.Reader` re-served the stale parse, and `Load` and
`LoadLocation` collided on the same `(name, "", src)` tuple. Reader identity
defaults to the reader's Go type — stable across instances (so per-Runtime
readers of the same type still share entries, the warm-cache topology) and
distinct across types — with `lisp.ReaderIdentity` as an optional hook for a
reader that varies its parse behind one type.

**Admission trusts the node type, not the seal flag.** The fast-path check
(`firstUnsealed`) and the checked-mode ownership exemption both require a node
to be sealed **and** of a sealable type (`sealableNodeType`), not merely to
carry the flag. The flag is one byte an untrusted `Reader` can set on any node;
trusting it alone let a mutable/closure node with the flag set launder past
admission and be aliased across Runtimes. With the conjunction, such a node is
routed to the copy path (where `SealAST` declines to mark it) and rejected, and
under `-tags elpscheck` it is no longer exempt from the cross-runtime gate.

**The admission walk is bounded, and the bound is stratified.** Putting
`newProgram` on the `load-file` path meant the admission walk's recursion now
ran over arbitrary `Reader` output. It carries an on-path cycle guard, a
validated-node memo and a depth cap (mirroring `sealfp.go`), so a cyclic or
very deep tree is refused in O(distinct nodes) rather than overflowing the Go
stack. Those two rules apply on **every** path, because the alternative to
refusing is not a worse answer but an unrecoverable crash.

Two further rules are the **cache's alone**, and keeping them there is the
point. `lisp.ReadProgram`, `lisp.ParseProgram` and `lisp.TextLoader` hand
their output to one load; a cache entry is aliased into unboundedly many
environments, so it can afford neither:

- **no repeated composite node.** A shared subtree is unfolded by the copy
  path and re-evaluated once per path — exponential in the sharing depth (a
  depth-40 DAG is ~10^12 paths). Refused with `errReaderTreeUnbounded`, which
  fails the load, because handing it to an uncached eval would not terminate
  either. A repeated **leaf** is explicitly fine: symbol interning is the
  common case and a leaf has no children to re-descend.
- **a node budget.** Overflow yields `errReaderTreeTooLarge`, which
  `readCached` treats as "do not cache this one": the load proceeds uncached.
  A node count is not a safety property, so refusing to *share* a large
  program must not stop it *running*. Applying this bound to the public
  constructors — which is where it first landed — rejected programs they had
  always accepted, and that is a regression the cache has no licence to cause.

`firstUnsealed` carries the same memo for the same reason: node sharing is
legal off the cache path, so without it a shared subtree would be re-descended
once per path. The memo records only nodes already found *sealed*, so it can
never mask an unsealed one. `(*LVal).Copy` still has no memo, so a DAG it
copies is unfolded — pre-existing behaviour of the copy path, and one more
reason the cache forbids composite sharing.

**A node the seal cannot vouch for is not admitted.** `SealAST` freezes an
`LVal`'s own fields; it cannot freeze whatever an embedder hung off `Native`,
the fingerprint oracle skips `Native` by design, `(*LVal).Copy` shallow-copies
it, and the admission conjunction keys off the node's *type*, which is
sealable for an `LInt`. A mutable Go box riding on a sealed integer literal
would therefore cross Runtime boundaries aliased, unfingerprinted and
unreported. The walk refuses a non-nil `Native` on a type the seal marks: on
the cache path that means an uncached load, for `Program` an error — which is
what its contract already promises for output the seal cannot protect. No
parser produces such a node (the standard parser sets `Native` on none), so
the rule costs nothing real.

**Reader custody covers the slice, not only the nodes.** The zero-copy fast
path shares the reader's *nodes* deliberately; it must not retain the reader's
`[]*LVal` *header*, and before this it did. A reader that refills one output
buffer per call — an ordinary optimization, and not a violation of "do not
mutate the nodes you returned" — rewrote the previous file's entry in place.
Admission clones the header and clamps its capacity (the `clampCap`
discipline, §2.6), which is `len(exprs)` pointer copies on a path that has
just parsed a file.

**An empty `ReaderIdentity()` disables the cache** for that reader's loads
rather than being folded into the key. The empty token states nothing, so two
readers returning it would be declared interchangeable producers and would
serve each other's parses — the exact failure the reader component of the key
exists to prevent, reached by implementing the optional interface badly.
Falling back to the Go type would be no better, since a reader that
multiplexes parse behaviour behind one type is precisely why it implements
`ReaderIdentity` at all.

**Behaviour a cache can change** (see `docs/embed.md` for the migration note):
installing a cache in front of a non-sealing reader can make guarded mutation
sites raise `modify-literal-error` on previously-working code (admission runs
`SealAST`); a re-entrant `Load`/`Store` is defended by an in-flight guard that
treats re-entry as a miss; and `io.ReadAll` changes the outcome for a streaming
reader that errors after a complete program.

**The debugger question, answered.** Attaching a debugger disables TCO
globally and makes `macroCall` build a `macroExpansionContext`, which
`stampMacroExpansion` turns into per-node metadata. Those are writes, and a
macro receives its arguments unevaluated, so a caller's parse nodes — here,
*cached* nodes — are spliced straight into the expansion. The write does not
land, for two independent reasons: `stampGuarded` returns at the first sealed
node (skipping the whole subtree, §2.4), and rdparser gives every node it
emits a real location so the stamp's `source == nil || Pos < 0` condition is
false in the first place. Only the first of those covers a cached tree that
came from a *caller-written* `Reader`, which may emit location-less nodes —
so that is the case
`TestLoadCacheDebuggerDoesNotStampSharedNodes` drives, and removing the
sealed skip makes it fail. Debug mode therefore pays no copy on this path.

**What it buys.** The hazard half of this story was already closed: since the
parser seals and the mutation sites refuse sealed input, an embedder cache
that aliases is no longer the corruption vector it was. What remained was a
*cost* — an embedder holds `[]*LVal`, not a promise, so the only way it can
reach safety on its own terms is to deep-copy every hit. Measured in this
repository over a 78 KiB source loaded into a fresh environment
(`lisp/loadcache_bench_test.go`, `-count=8`):

| arm | sec/op | B/op | allocs/op |
|---|---|---|---|
| no cache | 16.81 ms | 9169 KiB | 235,036 |
| embedder-shaped cache, copy per hit | 6.07 ms | 5559 KiB | 64,308 |
| `LoadCache`, aliased hit | 2.00 ms | 1644 KiB | 18,700 |

Alias against copy: **−67.0% sec/op, −70.4% B/op, −70.9% allocs/op**
(p=0.000, n=8). A deployment that preheats a pool of environments multiplies
that by pool size on every refill.

## 3. Verification layers: what each prevents, and its blind spots

The seal design reduces to one checkable sentence: **the bytes of a sealed
program node never change after parsing completes.** #369 and #370 shipped
past a manual audit, so that sentence is now checked by four mechanically
different layers, all looking through one lens — the canonical fingerprint
walker in `lisp/sealfp.go` — so a node field can never be covered by one
tool and silently missed by another.

### 3.1 elpsvet (static; `cmd/elpsvet`)

Three `go/analysis` rules, run as `go run ./cmd/elpsvet -test=false ./...`:

- **elpsownership** (`main.go`): no package-level var may keep a
  `*lisp.LVal` reachable — the process-wide-shared-table producer pattern
  behind #363. Suppression: `//elpsvet:allow` with a justification.
- **elpsfreshness** (`freshness.go` + `alias.go`): no function may write a
  `lisp.LVal` field on a value it did not construct (#333/#334's pattern),
  including writes laundered through local slice aliases of LVal backing —
  index assignment, `append` on a tainted base, `copy` into a tainted
  destination, and the `sort.*`/`slices.*` mutators, tracked transitively
  through assignments, var declarations, slice expressions and slice-type
  conversions (#369's laundering gap, #371). Suppression: `//elps:mutates`
  with a justification.
- **elpsescape** (`escape.go`, #375): the mirror image of freshness — no
  function may store a runtime-owned `*token.Location` (`env.Loc`,
  `v.source`, parser/scanner token state, a location-returning method on a
  non-fresh receiver) uncopied into a field of an escaping value: an LVal
  field or composite literal, a `SetSource` call, a field of a returned
  value, a returned location-capturing composite literal, or package-level
  state. The pre-fix `ErrorCondition`/`ErrorConditionf` (ac0a326) and
  `ErrorAssociate` (d922290) bugs stored `env.Loc` into freshly built
  errors — fresh write targets, so freshness was structurally blind; this
  rule retro-catches all three shapes (fixtures in
  `cmd/elpsvet/testdata`). The cleanser is any function PROVEN to allocate
  the location it returns: `copyLocation`, an explicit deref copy, a
  `&token.Location{...}` literal, or anything built only out of those.
  That proof is a `go/analysis` **fact** (`locfact.go`), computed per
  location-returning function from its own body and exported along the
  import graph, so the sanctioned copying accessors — `lisp.LEnv.Source`
  (returns `copyLocation(env.loc)`) and `token.Scanner.LocStart` (mints a
  literal per token) — are clean at every call site in every package with
  no annotation, while a by-reference accessor of identical signature
  (`rdparser.Parser.Location`) is still flagged. A callee with no fact —
  un-analysed package, interface method, a body with one leaking return —
  keeps the conservative treatment, so the fact can only ever retire a
  proven false positive. Suppression: `//elps:aliases` with a
  justification.

*Blind spots (documented in the analyzers' own headers):* intraprocedural
within a function body — the escape rule's location-freshness fact is the
one summary that crosses a call, and it carries a single bit, nothing about
arguments or aliasing. `[]*LVal` function parameters are not taint sources in the callee
(and neither are `*token.Location` parameters for the escape rule);
storing a tainted slice into a field of a pre-existing struct escapes
tracking; a tainted location passed as a call argument escapes the escape
rule's tracking; a value-typed LVal variable is treated as a fresh root
even though its `Cells` still alias shared backing. And it is
**elps-repo-only**: it sees nothing an embedder compiles outside this
module. Every suppression is an audited claim, not a proof.

### 3.2 Fuzz corruption oracle (dynamic, offline; `lisp/eval_fuzz_test.go`)

Every eval fuzz target parses the input once, fingerprints the sealed parse,
evaluates it, and re-fingerprints: any drift fails the run. This searches
program space for mutation paths nobody thought to write a kill test for —
it is how one would have found the `macroexpand` `&rest` leak mechanically.
The stdlib application fuzzer (`lisp/lisplib` fuzz tests) applies the same
oracle to sealed arguments across builtin calls.

*Blind spots:* coverage is probabilistic (only reached paths are checked);
same-value and equal-content-metadata writes leave the fingerprint unchanged
(see §3.4); it runs offline, not in CI's ordinary test job.

### 3.3 Checked-mode inspector (dynamic, tagged builds; `lisp/seal_check_elpscheck.go`)

Under `-tags elpscheck`, `SealAST` records each sealed root's fingerprint,
and every `LEnv.load` re-verifies its own top-level expressions after
evaluation (error or not), panicking at the load that caused a corruption —
not at end of suite. `VerifySealedASTs` re-checks every recorded root and is
called by the lisp package's `TestMain` and by `elpstest.Runner` at the end
of each test file; embedders can call it at their own teardown points.
Untagged builds compile all of it out to empty inlined hooks
(`seal_check_default.go`) — release binaries carry zero bookkeeping;
tagged overhead is about +3% suite wall time (CI-only).

The Nil/true/false singletons are held as **permanent roots** (elps#376):
fingerprinted at package init, before any user code runs, and re-verified
on every top-level load and at every teardown verification. Unlike the
bounded roots table they are never part of a verify-and-drop cycle, so a
singleton corruption is caught at the load that caused it for the life of
the process — not only at the value-drift checkpoints `checkSingleton`
covers. Red-proof: `TestPermanentSingletonRoots_*`
(`lisp/singleton_seal_elpscheck_test.go`) mutates a singleton and proves
both hooks report it, naming the root.

*Blind spots:* fingerprint-value comparison is structurally blind to a write
that stores what a field already holds, and to a metadata write that swaps a
pointer for an equal-content one; swapping a sealed node's unsealed child
for a *different* unsealed child hashes as the same "hole" marker
(`lisp/sealfp.go` documents both). Only runs in tagged builds.

### 3.4 `-race` seal watchdog (dynamic, test binary; `lisp/seal_watchdog_test.go`)

A goroutine keeps unsynchronized reads of registered sealed nodes live for
the lifetime of the lisp package's test binary, so under `-race` **any**
unsynchronized write to a watched node — same-value and metadata writes
included, exactly the class the fingerprint cannot see; #370's write is this
class — is reported with a stack naming the offender. Registration is
explicit (`registerSealWatch`, used by the deterministic red-proof tests)
plus, in elpscheck builds, a bounded rotating sample of the inspector's
recorded roots.

*Blind spots (from its own header):* teeth only under `-race`; only the lisp
package's test binary; sampled coverage is probabilistic; a write made while
holding the watchdog mutex is silent (that is what `pauseSealWatchdog` is
for, restricted to tests that are *about* deliberate sealed-node mutation).

### 3.5 The honest residual

What no layer stops: **embedder Go code assigning to exported `LVal` fields
on a sealed node.** `v.Cells[0] = x` compiles no matter what — the flag is
advisory to Go code. This residual is the same one the eager-copy design
carries (eager copying ensures values *handed out* are not shared; it does
nothing about Go code that reaches the AST itself). The mitigations are the
boundary (no raw AST escapes the parse/cache surface, §2.5), elpsvet over
in-repo code, and the `IsSealed()` contract for everything else:

> Go code holding a value for which `IsSealed()` reports true must not
> modify it in place. Either return an error ("cannot modify a program
> literal") or work on a `Copy()`, whose fresh storage reports false.

Second residual: same-value writes in untagged production builds. In a
release binary nothing observes them; they are benign in effect on the tree
bytes but are still data races on shared storage. They are caught in
development by the watchdog (§3.4) — that split is deliberate, and it
applies to the singletons the same way: the permanent inspector roots
(§3.3) catch any value-changing singleton write at the next load, while a
same-value singleton write remains `-race`-only by design, kept
deterministic by the singleton write watchdog
(`lisp/singleton_watchdog_test.go`; mprotect approaches were evaluated and
rejected in #334).

### 3.6 The retired copy-on-write census

Until the elps#378 flip, every then-copy-on-write site carried a tagged
(`-tags elpscheck`) event counter with lisp-level attribution
(`lisp/cow_check_elpscheck.go`, deleted with the flip), compiled away
entirely in untagged builds. It existed to answer one policy question with
data — does any real program reach these sites? — and two censuses (recorded
on elps#378) answered *no*: all 1,625 elps-side events were the seal/CoW
test machinery asserting the behavior, and the downstream consumer's full
suites fired zero. With the sites now raising a loud, catchable condition,
an event census has nothing left to measure; the anti-vacuity duty it
performed for the benchmark smoke run moved into the benchmark program
itself, which counts its own caught `modify-literal-error` conditions and
asserts the total (`lisp/seal_error_bench_test.go`). The verification layer
that *matters more* after the flip — the fingerprint proof that sealed trees
stay unmutated — is §3.2/§3.3 and is unchanged.

## 4. Footguns

### 4.1 Go-style append/slice capacity sharing (elps#373)

`slice` shares backing including retained capacity, exactly like Go, and
`append` writes into shared capacity, exactly like Go. The maintainer
decision on #373 is to **keep Go semantics** and document them rather than
add copy-on-append: detecting shared backing cheaply is impossible without
reference counting, and copy-on-every-append turns amortized O(1) vector
building into O(n²). The one *dangerous* instance of the class — program
literals shared across environments — is mechanically closed by the seal
(seal propagates through `slice`; `append 'vector`/`slice 'vector` refuse a
non-empty sealed input, §2.4). What remains is ordinary within-environment value aliasing,
i.e. language semantics consistent with the language's deliberate mirroring
of Go slices.

- **The copy idiom is `concat`**: `(concat 'list xs)`, `(concat 'vector v)`,
  `(concat 'bytes b)` allocate fresh exactly-sized backing (safe on the
  empty case since #334). The Go-side deep copy for transfer cases is the
  in-kernel `detach()` (unexported until a consumer appears).
- **Under evaluation** (#373 work item 3, open): capacity-clamped slice
  results — Go's three-index `s[a:b:b]` idiom built in — so `append` on a
  slice result can never write into the source's retained tail. Not
  implemented; pending benchmarks.

### 4.2 The sealed-node embedder contract

A sealed node can reach embedder code through completely ordinary values: a
`&rest` argument bound to a quoted literal, a `cdr` of a program list, an
element pulled out of one. Treat `IsSealed()` as "shared with the program
text": refuse or `Copy()`, never write.

### 4.3 What NOT to do in builtins

- Do **not** take `v.Cells` (or `seqCells(v)`, or `v.Bytes()`) and index-write,
  `append`, `copy`-into, or sort it without either proving the value fresh
  or checking `IsSealed()` and copying first. This is exactly the laundering
  pattern the elpsvet alias rule flags.
- Do **not** wrap a possibly-sealed list's backing in a mutable container
  (the `slice 'vector` bug shape) — a vector is a mutable window onto
  whatever backs it.
- Do **not** hand a sealed backing array to lisp code under an unsealed
  header (the `macroexpand` bug shape). If you build a new header over
  cells you did not allocate, either propagate the seal
  (`r.sealed = v.sealed` — kernel-internal) or copy the cells.
- Do **not** stamp metadata (`SetSource`, formatting metadata, macro-
  expansion metadata) onto nodes you did not construct; `SetSource` no-ops
  on sealed values, and since issue #382 the metadata fields are unexported
  — a direct write no longer compiles outside the kernel, so the remaining
  in-repo write paths (`internal/fmtraw`, the stamp) carry the whole
  ownership burden.
- Do **not** suppress an elpsvet finding without a justification that a
  reviewer can audit; `//elps:mutates` is a claim of deliberate, owned
  mutation, not an off switch.

### 4.4 Lisp-side: `(copy x)`, unconditionally

Lisp code has one ownership primitive, `copy` (elps#378): a deep copy with
fresh backing for every container, the seal cleared, and function/native
leaves shared by reference (a within-env copy cannot smuggle anything, and
lisp cannot mutate a function's internals). It replaces the one-level
`(concat 'list x)` idiom and the json round-trip.

There is deliberately **no `sealed?` predicate**. `(if (sealed? x) (copy x) x)`
looks like the careful version and is the footgun: the seal bit reports
program-text provenance, not exclusive ownership, and an unsealed value can
still be aliased by another binding, a container, or a closure. Code that
intends to mutate data it did not construct copies unconditionally.
`IsSealed()` stays a Go-side tool, where refuse-or-copy is a real choice.

## 5. Embedder checklist

1. **Parse through a sealing path.** `Reader.Read`, `LoadString`,
   `ParseProgram`/`ReadProgram` all seal. Prefer `lisp.Program` at your
   cache boundary, and `Runtime.LoadCache` (§2.9) for the `load-file` path
   rather than a caching `Reader`: its constructors *establish* the seal at admission
   (elps#394) — reader output that is not already sealed throughout is
   privately copied and sealed, and output the seal cannot protect
   (reference types, function values) is rejected with an error. (Owned
   expressions need the in-kernel `detach()` machinery, which is unexported
   today; it will be re-exported when a real embedder consumer
   materializes.) If you build trees by hand and share them across
   environments, call `SealAST()` on each root yourself.
2. **Hand `AddPackage` a finished package.** The registry stores a snapshot
   whose code-like bindings are privately sealed copies (§2.8, elps#524), so
   writes through your `*Package` after registration do not reach the
   `Runtime` — bind through the environment instead. Values the seal cannot
   cover (functions, natives, maps, arrays) are admitted by reference: after
   `AddPackage` they belong to the registry, so stop mutating them.
3. **Never install a format-preserving reader into an evaluating runtime.**
   `parser.NewReader(parser.WithFormatPreserving())` produces *unsealed*
   trees for tooling (formatter/minifier/lint); it satisfies `lisp.Reader`,
   so nothing but this contract stops you on the `Load*` paths. The
   `Program` constructors are the exception: they detect the unsealed
   parse and admit a private sealed copy instead (elps#394).
4. **In every builtin or Go helper that mutates a value in place:** check
   `v.IsSealed()` first; refuse with an error or operate on `v.Copy()`.
   Remember slices of `Cells` share backing — copy the cells, not just the
   header, before restructuring.
5. **Use `copy` (lisp) / `Copy()` (Go) as the copy idioms** when you
   need storage that no one else can write (§4.1). `copy` is deep and
   unconditional -- prefer it to the one-level `(concat 'list x)`. (The
   hermetic cross-runtime deep copy lives in-kernel as `detach()`,
   unexported until a consumer appears; `copy` is its within-env mode.)
6. **Run the verification stack you can afford:** `go run ./cmd/elpsvet
   -test=false ./...` over code living in this module; `go test -tags
   elpscheck` (inspector) and `-race` (watchdog) in CI; call
   `lisp.VerifySealedASTs` at teardown in long-lived checked-build hosts.
7. **When elpsvet flags you and the mutation is deliberate**, annotate the
   line with `//elps:mutates <justification>` and make the justification
   say *why the storage is owned* — every annotation in this repo is an
   auditable claim (50 `//elps:mutates` and 19 `//elpsvet:allow` live
   suppressions at the time of writing, each individually justified).

---

*History: the zero-cost hardening layer, the seal layer (then copy-on-write),
and the verification tooling are stacked in PR luthersystems/elps#374; the
copy-on-write-to-error flip is elps#378, decided on the censuses recorded
there; the eager-copy alternative and its measured costs are summarized in
`lisp/seal.go`.*
