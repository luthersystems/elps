# Borrowed backing storage

An `LVal` minted over another value's live Go backing storage is a **second
header onto storage a first header already governs**. Whatever constraints
attach to that STORAGE must travel with it; a header that does not inherit
them is a laundering step, and the next write through it corrupts the
original.

Today there is exactly one such constraint: `sealed`, the parse-cache
no-write flag ([sealed-ast.md](sealed-ast.md), `lisp/seal.go`). A sealed
node belongs to a parse that every environment evaluating that program may
share, and the kernel's copy-on-write sites — `stable-sort`,
`append 'vector`, `slice 'vector` — all key off the flag on the value they
are handed. Strip the flag off a window onto a literal and all three guards
switch off at once.

That is luthersystems/elps#392: `rangePath.Get`, the READ-ONLY elpspath
range query, returned `lisp.SExpr(cells[from:to])` over a program literal's
live cells, and

```lisp
(append 'vector (elpspath:? (limits) '(range 0 2)) 999)
```

rewrote the literal, permanently, for every environment sharing the parse.

## Two mechanisms, only one of which this covers

The bug class has two distinct failure modes and they need different fixes.

| Mode | Shape | Closed by |
| --- | --- | --- |
| Write PAST the view's length, into capacity the source still owns | `(slice 'bytes src 0 2)` retains `src`'s spare capacity; a later `append-bytes` writes bytes `src` owns (#373) | a three-index (capacity-clamped) slice at the producer |
| Write WITHIN the view's own length | a stable-sort through a laundered window permutes the literal's own cells (#392) | transferring the constraint |

The material in this repo covers the **second** mode. A capacity clamp does
nothing for it, which is why capacity is not the invariant. Conversely,
transferring a constraint does nothing for the first mode, and there is no
constraint to transfer when the source is an ordinary runtime value —
`(to-bytes "ABCDEF")` carries none. The first mode is open.

## The propagation, and why it is a mechanism rather than a rule

The propagation itself is one line. The kernel already had it in three
places (`builtinCdr`, `builtinRest`, `builtinSlice`), it was missing in a
fourth (`opCond`, found by the detector below), and it had been forgotten
twice outside the kernel — once in the #369 audit and once in libelpspath.
A rule that must be remembered at every new producer has a defect rate that
scales with the number of producers, and the producers keep arriving.

Two things carry it now.

**Inside package `lisp`**, `lisp/borrow.go` makes the source a PARAMETER:

```go
mintBorrowedCells(src, cells)   // list header over src's cells
mintBorrowedSExpr(src, cells)   // ditto, unquoted (a form, not a value)
```

All of them route through `borrowFrom`, the single point of truth for what
"inheriting provenance" means: a future storage-scoped flag ("owned by
runtime R", "immutable", "tainted") is added there once and every borrow in
the kernel inherits it. These helpers are deliberately **unexported**. Go
cannot make `lisp.SExpr(src.Cells[i:j])` a compile error short of
unexporting `LVal.Cells`, so an exported constructor set would be public API
for a property the language cannot enforce anyway.

**Outside package `lisp`**, where `sealed` is unreachable, the route is the
exported `(*LVal).InheritSeal(src)`, which libelpspath's `alias` helper uses:

```go
out := lisp.SExpr(cells)
out.InheritSeal(in)
```

`InheritSeal` cannot invent a constraint (nothing happens unless `src` is
sealed) and refuses node types the flag would be a lie on — arrays in
particular, because `append!` and `assoc!` write vector backing without
consulting it. Code that must wrap constrained backing in a vector has to
COPY, as `builtinSlice` does for `(slice 'vector <sealed>)`.

## The read-half detector

Checked builds (`-tags elpscheck`) add a provenance detector,
`lisp/borrow_check_elpscheck.go`. It exists because the sealed-AST verifier
is a **corruption-time** oracle: it re-fingerprints sealed parses, so it
only ever fires for a test that performs the write. #392's own package had
tests that performed the read half and none that performed the write half,
so the oracle had nothing to see and the bug shipped.

* `sealAST` registers the extent of every sealed node's cell backing.
* `SExpr` and `QExpr` note a header minted inside a registered extent. No
  mutation is required to observe it.

### Discharge is a post-condition, not a route

The check is **deferred**, and that is the load-bearing design decision.
Every legitimate borrow in this codebase is written construct-then-inherit,
and outside package `lisp` it has to be — `sealed` is unexported and
`InheritSeal` is necessarily a second statement. A mint-time detector
reports all of those sites, and the only way to quiet it is a per-call-site
whitelist: the hand-maintained discipline the mechanism exists to replace.

So a noted header is held PENDING and swept later. One that has acquired
the constraint by then is discharged and dropped; one that has not is a
fault. In-kernel `r.sealed = v.sealed` and out-of-kernel
`r.InheritSeal(src)` are the same write to the same field and discharge
identically. `cdr`, `rest`, `slice 'list`, `slice 'vector` and
`append 'vector` all pass with no suppression of any kind, and no file in
`lisp/`, `lisp/lisplib/` or `parser/` carries one.

(The in-kernel helpers set the constraint before the detector inspects the
header, so an in-kernel borrow never enters the pending table at all. That
is a cost optimisation, not a correctness requirement.)

### Reporting

Faults are folded into `VerifySealedASTs`' error rather than exported
separately. That function is the existing process-teardown hook — the `lisp`
package's `TestMain`, `elpstest.Runner`, `elpstest.RunBenchmark`,
libelpspath's `TestMain` and any embedder's checked CI already call it — so
every one of them gains the read-half oracle without a line of change, and
the detector adds **no public API**. Both halves are reported when both
fire: a run that corrupts a sealed tree usually laundered a header first,
and the launder is the line a human has to edit.

### Production cost

None. `lisp/borrow_check_default.go` compiles the detector out entirely.
An untagged binary links no detector symbol, no detector string, and package
`lisp` does not import `unsafe` at all without the tag.

### The uintptr caveat

Extents are compared as `uintptr` ranges, which the Go spec does not
sanction. Three things bound the consequence:

1. The registry holds a STRONG reference to every array whose extent it
   records. That is stronger than a `runtime.KeepAlive` at the recording
   site: the storage stays reachable for as long as the extent is
   registered, and registration is monotone, so an address cannot be freed
   and handed to an unrelated allocation underneath a later comparison.
2. The failure the spec permits is RELOCATION, which today's collector does
   not perform for heap objects. A future moving collector could make an
   innocent mint fall inside a stale range, or a guilty one fall outside it.
3. Both directions are bounded by the mechanism being diagnostic and
   tag-gated. The first costs a spurious report in a checked build; the
   second costs a missed report in a checked build. Neither can change what
   a production binary does, because a production binary contains none of
   this code, and neither can change what a checked binary COMPUTES: the
   detector only ever appends to a diagnostic slice.

The residual risk is "a future Go release makes the checked build noisy or
blind", not "a future Go release breaks elps". The mitigation for that day
is to key extents on the `*LVal` that owns the storage instead of on an
address; the only reason that is not the mechanism today is that a borrowed
Go subslice carries no pointer back to its owner's header.

## Coverage by payload type

| Payload | Borrowable? | Constraint to inherit? | Covered |
| --- | --- | --- | --- |
| `LSExpr` cells (list) | yes | `sealed` | detector + kernel helpers |
| `LArray` cells (vector) | yes | none — `SealAST` refuses to mark arrays, and `InheritSeal` refuses too | detector reports a vector minted over sealed cells; the fix is to copy |
| `LBytes` | yes | none — `SealAST` cannot mark one, the parser cannot emit one | **not covered**: nothing to inherit; #373 is the capacity shape, closed only by a three-index slice |
| `LString` | yes | n/a — Go strings are immutable, so no write path exists through a view | not a hazard |
| `LSortMap` | yes, via `SortedMapFromData` | none today | **not covered** |
| `LNative` | yes | unknowable — the kernel cannot know a Go value's aliasing rules | **not covered** |

## What this is not

It is **detection, not prevention**. A checked build reports a laundering
step; an unchecked build does not stop one. The `internal/aliasprobe` census
(a measurement harness, not a deliverable) makes that concrete: of its 12
class probes, 5 are open at the chain root and the detector closes none of
them and flags none of them, because all 5 launder storage that carries no
constraint in the first place. What the detector changes is the REGRESSION
surface: revert `rangePath.Get` to its pre-#392 form, or delete the
propagation from `builtinCdr`, and the package's existing suite fails at
teardown naming the exact Go line — with no test that performs a write, and
no new test at all.
