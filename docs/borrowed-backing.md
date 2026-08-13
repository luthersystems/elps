# Borrowed backing: minting an LVal over storage another value governs

This document covers a bug class that sits one level below
[sealed ASTs](sealed-ast.md). Sealing answers *"may this storage be
written?"*. This answers *"does the header doing the writing know?"*

Audience: anyone adding a builtin, an embedder library, or a path walker
that returns a *view* of a value it was given.

---

## 1. The class

An `LVal` is a header. Its payload — `Cells []*LVal`, the `*[]byte` behind
`LBytes`, the `*MapData` behind `LSortMap` — is Go backing storage that the
header does not own exclusively. Minting a *second* header over the *same*
storage is routine and cheap: `cdr`, `rest`, `slice` and every path walker
do it, and that sharing is why a list walk is O(n) rather than O(n²).

The class is what happens when the second header does not inherit the
first's constraints:

```go
cells = cells[from:to]     // live backing of a possibly-sealed value
newVal = lisp.SExpr(cells) // fresh header; sealed == false
```

The constraint is a property of the **storage**, not of the header, so a
header that drops it is a laundering step: it converts constrained storage
into an unconstrained value, and the next write through that value corrupts
the original. Two shipped instances:

| Issue | Producer | Failure mode |
|---|---|---|
| elps#392 | `rangePath.Get` in `lisp/lisplib/libelpspath/path.go` — the **read-only** query path | `(append 'vector (elpspath:? (limits) '(range 0 2)) 999)` permanently rewrote the `'(10 20 30 40)` literal for every environment sharing the parse |
| elps#373 | `(slice 'bytes src 0 2)` | `(append-bytes win …)` wrote into `src`'s retained capacity: `#<bytes 65 66 122 122 69 70>` |

### Two distinct failure modes

* **Past the view's length**, into retained spare capacity. This is #373,
  and it is the mode a three-index slice (`b[i:j:j]`) closes.
* **Within the view's own length.** `stable-sort` through a laundered view
  permutes cells that *are* the source's cells. A three-index slice changes
  nothing here.

Capacity is therefore **not** the invariant. Provenance is. The clamp is
worth doing — it is free — but it is hygiene, not the fix.

---

## 2. Why a constructor rather than a rule

The propagation itself is one line, and the kernel already had it in three
places (`builtinCdr`, `builtinRest`, `builtinSlice`):

```go
r.sealed = v.sealed
```

Hand-maintained propagation has now failed twice in this repository:
elps#369 mechanism 2, and elps#392. A rule that must be *remembered* at
every new producer has a defect rate proportional to the number of
producers, and the producers keep arriving.

So `lisp/borrow.go` makes the source a **parameter**. There is no way to
spell a borrowed view without naming what it was borrowed from:

```go
v.SliceCells(i, j)              // the receiver IS the provenance
v.SliceBytes(i, j)
lisp.BorrowCells(src, cells)    // when the caller already has the subslice
lisp.BorrowVector(src, cells)
lisp.BorrowBytes(src, b)
lisp.BorrowSortedMap(src, data)
```

Every one of them routes through a single function, `borrowFrom`, which is
the one place that says what "inheriting provenance" means. A future
storage-scoped flag is added there once and every borrowed view in the tree
inherits it. That is the property the hand-maintained discipline never had.

`BorrowVector` and `BorrowSortedMap` are the two that hand out a *mutable*
window, so they copy when the source is sealed rather than transfer — the
same copy-on-write bargain `(slice 'vector <sealed>)` already strikes.

---

## 3. What the constructor cannot do, and what backs it up

Go has no way to make `lisp.SExpr(src.Cells[i:j])` a compile error short of
unexporting `LVal.Cells`, which would break every embedder. Two backstops
cover the residual, and neither is a substitute for the constructor:

**Mint-time detection** (`lisp/borrow_check_elpscheck.go`, `-tags
elpscheck`). Every parse registers the extent of its sealed backing arrays;
`SExpr`, `QExpr` and `Bytes` check whether the storage they are handed falls
inside a registered extent while the header being minted carries no
constraint. That is the class, detected at the moment it is committed —
**with no mutation required**.

The distinction matters. The pre-existing sealed-AST verifier
(`lisp.VerifySealedASTs`) is a *corruption-time* oracle: it fires only for a
test that performs the write. libelpspath's suite exercised the read half of
#392 and never the write half, and the verifier was not wired into that
package's `TestMain` at all. With the mint-time detector installed and
`rangePath.Get` reverted to its pre-#392 form, the package's **existing**
test suite fails — every test still passes, and `TestMain` reports:

```
borrowcheck: cells — an LVal (type=list) was minted over CONSTRAINED backing
storage without inheriting the constraint.
  minted at: lisp/lisplib/libelpspath/path.go:187 (…libelpspath.toList)
```

**Generative composition** (`lisp/lisplib/libelpspath/aliasgen_test.go`).
Detection at corruption time needs a program that composes a *query* (which
mints a view) with a *mutator* (which writes through it). No test did. The
generator supplies exactly that composition; on the unfixed tree the sealed
verifier fires after 9 generated programs (~1 ms).

---

## 4. Coverage by payload type

| Payload | Class reachable? | Covered by the borrow API? |
|---|---|---|
| `Cells` (`LSExpr`, `LArray` backing) | Yes — `lisp.SExpr`/`QExpr`/`Vector` over a subslice | Yes: `SliceCells`, `BorrowCells`, `BorrowVector`; mint-time detector |
| `LBytes` (`*[]byte`) | Yes — `#373` | Yes: `SliceBytes`, `BorrowBytes` (capacity-clamped). No in-length byte mutator exists today; if one is added, the transferred flag is already in place for it to check |
| `LSortMap` (`*MapData`) | Yes, but **Go-API only**: `SortedMapFromData(other.Map())`. No lisp-reachable route — `assoc`/`dissoc` copy the `MapData`, and only the documented `!` variants write in place | Yes: `BorrowSortedMap` |
| `LArray` dims | No — `Array()` copies its `dims` argument | n/a (verified by `TestGeneralisationLArrayDims`) |
| `LString` | No — `Str` is a Go string; the language provides no write path through a view | n/a |
| `LError` call stack | No — every construction copies (`Runtime.Stack.Copy()` allocates fresh `Frames`) | n/a |
| `LNative` | Yes — `lisp.Native(other.Native)` aliases an arbitrary Go value | **No.** The kernel cannot know a Go type's aliasing rules. Residual risk; `detach` already refuses `LNative` for the same reason |

---

## 5. Cost

The borrow constructors allocate exactly what the raw ones did: one header.
The three-index clamp is a compile-time constant in the slice expression.
`borrowFrom` is one branch and one byte store. The mint-time detector is
compiled out entirely in untagged builds
(`lisp/borrow_check_default.go` — four empty inlined functions).

Measured on the query path (`libelpspath.Range(…).Get`, the call substrate
makes per transaction) and on the byte and list builtins: no statistically
significant change at n=12 interleaved rounds. The alternative designs that
*do* cost — copying on slice makes `cdr` O(n) and a recursive list walk
O(n²) — are quantified in the design packet.
