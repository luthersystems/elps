# Sealed ASTs: copy-on-write protection for shared program literals

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
| `stable-sort` on a quoted literal (elps#369, mechanism 1) | `(stable-sort < '(3 1 2))` sorted the literal's `Cells` in place — the "documented in-place effect" on a program literal *was* the corruption | CoW guard in `builtinSortStable` (`lisp/builtins.go`); kill tests in `lisp/cow_seal_test.go` |
| append through slice capacity (elps#369, mechanism 2) | `(append 'vector (slice 'list '(1 2 3) 0 1) x)` wrote `x` into the spare capacity of the literal's backing array, overwriting its second element — Go-style `append` semantics applied to shared storage | CoW guards in `builtinAppend` and `builtinSlice` (`lisp/builtins.go`) |
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
Nil/true/false singletons are skipped: they are already immutable by decree
and writing even a flag to one would race. Atoms *are* sealed: unlike the
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
  the copy-on-write contract — restructure the copy freely, never the
  shared nodes inside it.

### 2.4 Copy-on-write at kernel mutation sites

The mutating builtins that can legally receive a sealed value check the flag
and copy first (`lisp/builtins.go`):

- `stable-sort`: sorts a fresh header with a fresh backing array and returns
  it. Element pointers are shared — sorting only permutes them.
- `append 'vector`: copies a sealed sequence's cells before appending within
  spare capacity; the returned vector is unsealed, so *chained* appends
  extend it in place as before.
- `slice 'vector`: copies instead of wrapping sealed backing in a mutable
  vector; `slice 'list` returns a sealed intermediate (backing still
  shared).

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
(`lisp/program.go`) wraps parse output opaquely, and the package registry
seals its LVal-bearing surface. Deep-copy machinery for owned expressions
exists in-kernel (`detach()`, `lisp/detach.go` — it backs the planned
lisp-level copy builtin, elps#378) but is unexported: it will be re-exported
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

## 3. Verification layers: what each prevents, and its blind spots

The seal design reduces to one checkable sentence: **the bytes of a sealed
program node never change after parsing completes.** #369 and #370 shipped
past a manual audit, so that sentence is now checked by four mechanically
different layers, all looking through one lens — the canonical fingerprint
walker in `lisp/sealfp.go` — so a node field can never be covered by one
tool and silently missed by another.

### 3.1 elpsvet (static; `cmd/elpsvet`)

Two `go/analysis` rules, run as `go run ./cmd/elpsvet -test=false ./...`:

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

*Blind spots (documented in the analyzers' own headers):* intraprocedural
only — `[]*LVal` function parameters are not taint sources in the callee;
storing a tainted slice into a field of a pre-existing struct escapes
tracking; a value-typed LVal variable is treated as a fresh root even though
its `Cells` still alias shared backing. And it is **elps-repo-only**: it
sees nothing an embedder compiles outside this module. Every suppression is
an audited claim, not a proof.

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
development by the watchdog (§3.4) — that split is deliberate.

## 4. Footguns

### 4.1 Go-style append/slice capacity sharing (elps#373)

`slice` shares backing including retained capacity, exactly like Go, and
`append` writes into shared capacity, exactly like Go. The maintainer
decision on #373 is to **keep Go semantics** and document them rather than
add copy-on-append: detecting shared backing cheaply is impossible without
reference counting, and copy-on-every-append turns amortized O(1) vector
building into O(n²). The one *dangerous* instance of the class — program
literals shared across environments — is mechanically closed by the seal
(seal propagates through `slice`; `append`/`slice 'vector` copy on sealed
input, §2.4). What remains is ordinary within-environment value aliasing,
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
- Do **not** stamp metadata (`SetSource`, `Meta`, `MacroExpansion`) onto
  nodes you did not construct; `SetSource` no-ops on sealed values, but the
  fields are exported and a direct write compiles.
- Do **not** suppress an elpsvet finding without a justification that a
  reviewer can audit; `//elps:mutates` is a claim of deliberate, owned
  mutation, not an off switch.

## 5. Embedder checklist

1. **Parse through a sealing path.** `Reader.Read`, `LoadString`,
   `ParseProgram`/`ReadProgram` all seal. Prefer `lisp.Program` at your
   cache boundary. (Owned expressions need the in-kernel `detach()`
   machinery, which is unexported today; it will be re-exported when a real
   embedder consumer materializes.) If you build trees by hand and share
   them across environments, call `SealAST()` on each root yourself.
2. **Never install a format-preserving reader into an evaluating runtime.**
   `parser.NewReader(parser.WithFormatPreserving())` produces *unsealed*
   trees for tooling (formatter/minifier/lint); it satisfies `lisp.Reader`,
   so nothing but this contract stops you.
3. **In every builtin or Go helper that mutates a value in place:** check
   `v.IsSealed()` first; refuse with an error or operate on `v.Copy()`.
   Remember slices of `Cells` share backing — copy the cells, not just the
   header, before restructuring.
4. **Use `concat` (lisp) / `Copy()` (Go) as the copy idioms** when you
   need storage that no one else can write (§4.1). (The hermetic
   cross-runtime deep copy lives in-kernel as `detach()`, unexported until
   a consumer appears.)
5. **Run the verification stack you can afford:** `go run ./cmd/elpsvet
   -test=false ./...` over code living in this module; `go test -tags
   elpscheck` (inspector) and `-race` (watchdog) in CI; call
   `lisp.VerifySealedASTs` at teardown in long-lived checked-build hosts.
6. **When elpsvet flags you and the mutation is deliberate**, annotate the
   line with `//elps:mutates <justification>` and make the justification
   say *why the storage is owned* — every annotation in this repo is an
   auditable claim (50 `//elps:mutates` and 19 `//elpsvet:allow` live
   suppressions at the time of writing, each individually justified).

---

*History: the zero-cost hardening layer, the seal/CoW layer, and the
verification tooling are stacked in PR luthersystems/elps#374; the eager-copy
alternative and its measured costs are summarized in `lisp/seal.go`.*
