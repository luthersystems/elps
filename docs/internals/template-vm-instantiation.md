# Cheap Template VMs: Frozen Packages, Slot Writes and Lazy Instantiation

This document describes how `Template.NewVM` became cheap. It covers the
problem, the design, the rules that keep it correct, the costs, and the
measured gain. It covers PR #685.

## Summary

An embedder such as substrate publishes one `Template` per program and makes
a new VM for each request. Before this change, `NewVM` copied every package
table and rebuilt every value in the template, even when the request used
only a few of them. With a large program, that copy was most of the cost of
a request.

This change adds three layers. Each layer is its own commit and can be
reverted by itself:

| Layer | What is shared | What each VM owns | Default |
|---|---|---|---|
| 1. Frozen packages | Symbol-to-slot index, function names, docs, exports | Value slots | Opt-in (`TemplateWithFrozenPackages`) |
| 2. Slot writes | (same as 1) | A rebinding of an existing name writes only that VM's slot | On with 1 |
| 3. Lazy instantiation | The template's value plan | Values built on first use | On (`TemplateWithEagerInstantiation` turns it off) |

Program behaviour does not change. A program cannot tell whether its VM is
frozen, lazy, or built the old way.

## Problem

A published template holds packages. Each package holds a symbol table
(name to value), function names, docstrings and an export list. It also
holds the value graph that those tables reach: functions, lists, sorted maps
and so on.

`NewVM` did two expensive things for each request:

1. It copied every package table, because a request can write to a package
   (`set`, `defun`, `export`).
2. It rebuilt every value in the graph, because values are mutable and must
   not be shared between VMs.

Measured on a large real program (13,343 values), `NewVM` took 2.8 ms and
29,500 allocations. A request that calls three endpoints touches about 2% of
those values.

## Design

### Layer 1: frozen packages (copy on write)

At publication, the template builds each frozen package's tables once:
the symbol-to-slot index, the function names, the docs and the exports. All
VMs share these tables. They live in `internal/packagetable`, whose types
give only reads, iteration and copies, so no code can change them by mistake.

Each VM owns only its value slots (`baseValues`), one slot per name.

All writes pass one gate, `Package.ensureWritable`. The first write that
needs a table change (a new name, a doc, an export, a `use-package`) calls
`Package.thaw`. `thaw` gives that VM a private copy of that one package. The
other VMs and the template do not see the change.

A thaw is correct but costs a copy, so a host may want to see it happen.
`TemplateWithThawHook(func(pkg string))` installs a callback that `thaw`
calls with the package name before copying. It runs at most once per
package per VM, on the VM's goroutine, and must be safe to call from every
VM at once. The callback is stored on the shared package base at
publication and never written again. When no hook is set the thaw path pays
one nil check, and a slot write (Layer 2) never reaches it. Substrate
exports it as the `substrate_elps_package_thaw_total{package}` counter; a
healthy program thaws nothing per transaction.

### Layer 2: slot writes

Most writes at request time rebind a name the package already has: `set`,
`set!` or `defun` of an existing global. These writes do not need a table
change, so they do not thaw. `Package.putSlot` writes only that VM's slot.
When the new value is a function, its name goes into the VM's
`slotFunNames` overlay. `GetFunName` reads the overlay before the shared
table, and `thaw` merges the overlay into the private copy.

With slot writes, freezing every package, including the program's own, is
cheap. Substrate freezes all packages.

### Layer 3: lazy instantiation

`NewVM` now builds only the package shells and the root environment. A
package binding, a sorted-map entry and everything they reach are built the
first time the VM reads them. Each value is built once per VM, and the
result is remembered by its plan index. So identity and sharing are the same
as in the source graph: two bindings that point to one list still point to
one list.

- An unbuilt base slot is `nil` in `baseValues`.
- An unbuilt thawed binding or sorted-map entry holds the `lazyPending`
  marker.
- The builder uses an explicit work queue, not recursion, so a deep graph
  cannot overflow the Go stack.

**Prewarm.** `VMWithPrewarm` builds, when the VM is made, every value that an
earlier VM of the same template used. The template keeps this "hot set" in
an atomic per-template set. A host that makes VMs ahead of time, off the
request path, uses prewarm so the request pays nothing.

**Walk-everything operations** (copy, republish, a full JSON dump, `help`
over a whole package) build every value first. They stay correct but lose
the gain.

## Rules that keep it correct

| Rule | How it is enforced |
|---|---|
| Shared tables are never written | `internal/packagetable` exposes no mutators. The `elpsfrozenpackage` elpsvet rule confines every write to `Package` tables to an audited allowlist of guarded methods. |
| Lazy tables are read only through the accessors that fill them | The `elpslazyread` elpsvet rule reports any direct read of `Package.symbols`, `Package.baseValues` or `sortedmap.m` outside the audited functions, including through conversions and generic type parameters. |
| The shared base is not changed after publication | Checked builds (`-tags elpscheck`) fingerprint the base at publication and verify it in `Template.NewVM`. Production builds compile this out. |
| One VM is used by one goroutine at a time | Reads now write (they fill lazy slots). Checked builds panic on an overlapping fill (`lazyGuard`). This is documented on `NewVM`. |
| No VM keeps another VM's memory alive | Weak-pointer tests for escaped scalars, lists, maps, closures and cross-package values. One retention bug (shared slot backing array) was found in review and fixed. |

## Tests

- Every kind of frozen write succeeds, matches a cold environment, and is
  not visible to other VMs or to the template. Slot writes never thaw.
- Lazy and eager VMs give the same results for eval, copy, detach, JSON,
  help, the debugger, republish and iteration.
- Determinism: five VMs with different histories, prewarmed, match cold lazy
  VMs, which match eager VMs, on results, error traces and printed output.
  The programs use gensym, sorted-map order, self-referential maps and
  `handler-bind`.
- `go test ./...`, `-tags elpscheck`, `-race`, both `make elpsvet` passes,
  and `FuzzEval` and `FuzzTemplateCancellationParity` for five minutes each.
- Substrate, pinned to this branch: all tests pass, and a real customer
  program installs and answers the same with every package frozen.

## Results

| Measurement | Before | After |
|---|---|---|
| `NewVM`, large real program | 2.8 ms, 29.5k allocs | 126 µs |
| Whole transaction, large real program, 3 endpoints | ~2.2 ms, 37k allocs | ~0.85 ms (−61%), ~4.6k allocs |
| TemplatePlan fork, 250 functions | 520 µs, 4,868 allocs | 21 µs, ~57 allocs |
| Substrate `CCHandle` (lazy + prewarm) | 751 µs | 670 µs (−11%), −38% B/op |

The gain grows with program size. A small program sees a small gain,
because there was little to copy.

## Costs and tradeoffs

| Cost | Size | Why it is accepted |
|---|---|---|
| Code complexity: two new invariants (frozen tables, lazy reads) and about 4,000 lines including tests | High | Each invariant has a static check (elpsvet) and a runtime check (elpscheck), so a new violation fails in review or in tests, not in production. |
| One VM must not be used from two goroutines at once | Behaviour contract | Hosts already made one VM per request. Checked builds detect a violation; production builds race silently. |
| Publication: +106 allocations per template, once | Small | Paid once per template, not per request. Waived in `scripts/benchstat-waivers.txt` (#687). |
| Symbol lookup: about +1 ns | Small | Lookup now goes through the slot index. |
| A map or package with unbuilt entries keeps its VM's lazy state alive | Memory | Measured in `TestTemplateForkEscapedLeafDoesNotRetainVM`. The VM is freed when nothing refers to it. |
| Walk-everything operations lose the gain | Performance | They stay correct. They are not on the request path. |

**Visible behaviour changes** (both intended):

- `(set 'user:x v "doc")` now stores the doc on `x` in package `user`.
- The hidden `json:%string-numbers-mode%` and `%exact-integers-mode%`
  bindings are removed. The JSON modes are per-VM runtime state.

## Alternatives considered

| Alternative | Why not |
|---|---|
| Share values between VMs and make them immutable | Changes the language: ELPS values are mutable, and existing programs mutate them. |
| Error on writes to a frozen package | Breaks existing programs that write to packages at request time. Copy on write keeps them working. |
| Pool and reset VMs | A reset must undo every possible write, which is harder to prove correct than never sharing mutable state. |
| Eager copy, but faster | Still proportional to program size. Lazy build is proportional to what the request uses. |

## Turning it off

| Switch | Effect |
|---|---|
| `TemplateWithEagerInstantiation()` | Restores the eager build (layers 1 and 2 stay). |
| Do not pass `TemplateWithFrozenPackages` | Restores private package tables per VM. |
| Substrate: `SUBSTRATE_DISABLE_FROZEN_PACKAGES=true` | Kill switch for frozen packages in substrate. |
