# Per-callsite macro-expansion caching

Status: **experimental POC, default off** (issue #381). This document
records the design, the safety argument for each admission tier, and the
measurements that justify where the whitelist boundary was drawn.

Audience: reviewers of the cache, and embedders deciding whether to enable
it.

Implementation: `lisp/macrocache.go` (cache and admission),
`lisp/macrocache_purity.go` (the user-macro prover),
`lisp/macrocache_stats.go` (instrumentation).

---

## 1. What it does and why it is possible

A macro callsite is expanded every time it is evaluated. In a request-path
embedding — one warm environment serving many transactions against a stable
program — the same callsites are re-expanded on every transaction, producing
a structurally identical tree each time.

Sealed AST nodes (see [sealed-ast.md](../sealed-ast.md)) have stable pointer
identity and immutable content. That is exactly what a callsite cache key
needs: seal the expansion the first time, memoize it against the callsite
node, and reuse the tree on later evaluations instead of re-running the
macro.

The cache is off by default. Hosts opt in with `SetMacroCacheMode` or
`ELPS_MACRO_CACHE=runtime|shared`; `ELPS_MACRO_CACHE_CAP` bounds the shared
table.

## 2. The safety condition

Reusing an expansion *tree* is observationally identical to re-expansion
only when the macro is a **pure structural rewrite of its callsite's
argument nodes**. Three properties must hold:

1. **No expansion-time effects.** The expansion must not depend on or mutate
   anything outside the argument nodes — no free symbol reads, no I/O, no
   state.
2. **No expansion-time allocation with identity.** Nothing allocated during
   expansion may be captured in the tree in a way that makes the first
   expansion's instance semantically privileged. This is what excludes
   closure-embedding macros (§3.1).
3. **Fixed gensym names must be inert.** A cached tree freezes the gensyms
   minted by its first expansion (§4).

Anything that cannot be shown to satisfy all three simply bypasses the
cache. Bypass is always safe; the design is failure-closed.

## 3. Admission: three tiers

### 3.1 Native macros — hand-audited whitelist

A small whitelist of kernel and lisplib macros audited against §2:
`get-default`, `trace`, `curry-function`, and the `testing:` assertion and
`test-let` family. Embedders may extend it with `RegisterPureNativeMacro`
under the same audit obligation.

**Deliberately excluded: `defun`, `defmacro`, `deftype`.** Their expansions
embed a closure allocated at expansion time (`env.Lambda`). Caching one
would hand every later evaluation a closure bound to the *first* expander's
environment — a lexical-scope corruption, not merely a stale result. This is
a semantic exclusion, not a heuristic.

`defconst` is pure but top-level-only, so it has no request-path value and
is left out.

### 3.2 User macros — a conservative syntactic prover

`defmacro` macros are admitted only when `proveUserMacroPure` can verify,
from the macro function's own sealed body, that the expansion is a pure
template instantiation:

- the body is a quasiquote template, optionally wrapped in a small allowlist
  of structural operators (`if`, `let`, `let*`, `progn`);
- every unquote is a bare formal parameter or a gensym-bound local.

Rejected outright: free symbol reads, computed unquotes, nested quasiquote,
any side-effecting form, and a gensym escaping under `quote` (§4). Verdicts
are memoized process-wide keyed by the macro's sealed, parse-shared formals
node.

This tier is what captures the `when`/`unless`/`default` utility layer that
dominates real request paths (§7).

### 3.3 Everything else bypasses

Unsealed (runtime-constructed) callsites, debugger-attached runtimes — where
`MacroExpansionInfo` IDs must stay unique per expansion — and any macro that
fails §3.1 and §3.2.

## 4. Gensym reuse

A cached tree fixes the gensym names its first expansion minted. That is
safe for admitted macros because their gensyms only ever appear in *binding*
positions inside the expansion (`let`/`let*`/`lambda`/`labels`). Evaluating
the tree creates a fresh binding for the fixed name on every call, exactly
like a hand-written local. Shadowing an identical name in a lexically nested
cached expansion is inert, because no admitted expansion references a gensym
it did not itself bind.

The one shape where gensym identity is observable is a gensym used as
*data* — under `quote` — where the caller can compare symbols across
expansions. The prover rejects it, and the native whitelist was audited for
the same property.

Tests: `TestMacroCacheCrossRuntimeGensymCollision` forces the worst case (two
runtimes minting the same first gensym name into lexically nested cached
callsites) and shows the shadowing is harmless;
`TestMacroCacheGensymLeakNotCached` pins that the rejected shape still yields
distinct symbols per call under every cache mode.

## 5. Invalidation

Exact identity comparison; there is no epoch counter to bump.

Every entry records the identity of the macro that produced it: package+FID
for native macros, the sealed formals-node pointer for user macros. Every
lookup validates the entry against the macro the callsite resolves to *right
now*. Redefinition — whether by `defmacro` or by shadowing a kernel macro —
necessarily creates a fresh function with a fresh formals node, so a stale
entry can never be served; it is simply overwritten by the next expansion.

Test: `TestMacroCacheInvalidationOnRedefinition`, covering user redefinition
and kernel-macro shadowing, in both cache modes.

## 6. Sharing scope

Two scopes, selected by `MacroCacheMode`:

- **`MacroCacheRuntime`** — a plain map on the `Runtime`, unsynchronized
  (single-threaded by the `Runtime` contract). Memory is scoped to the
  environment and dies with it; a fresh runtime starts cold.
- **`MacroCacheShared`** — one process-wide table (`sync.Map`, or an LRU
  under `SetMacroCacheCap`). Entries are **sealed before publication**,
  mirroring the shared-formals precedent: immutable content, copy-on-write
  protected.

A cross-runtime *hit* requires cross-runtime-shared sealed callsites, which
is precisely the embedder parse-cache aliasing the sealing work exists to
make safe. Under `-tags elpscheck` the ownership checker forbids
cross-runtime AST sharing outright, so a cross-runtime hit is unreachable in
checked builds by construction — a shared callsite would panic at eval entry
before macro dispatch is ever reached. Within one runtime the checker is
satisfied trivially.

## 7. Where the reuse actually is

Instrumented full-suite run against a production-scale phylum: **786,432**
macro dispatches across **704,538** distinct callsites and **28** distinct
macros. "Repeat opportunity" is dispatches beyond the first at a given
callsite — the hits an ideal cache could serve.

| macro (role) | dispatches | callsites | repeat opportunity | share |
|---|---:|---:|---:|---:|
| `when`-class template macro | 45,742 | 2,961 | 42,781 | 52.2% |
| `default`-class template macro | 17,788 | 962 | 16,826 | 20.6% |
| domain validation wrapper | 13,805 | 318 | 13,487 | 16.5% |
| `unless`-class template macro | 8,494 | 476 | 8,018 | 9.8% |
| kernel `get-default` (native tier) | 424 | 37 | 387 | 0.5% |
| logging wrapper (native tier) | 304 | 78 | 226 | 0.3% |
| *22 other macros* | 65,245 | 64,126 | 129 | 0.2% |
| **kernel `defun` (excluded, §3.1)** | **633,630** | **633,590** | **40** | **0.05%** |

Two conclusions, and they are the empirical justification for the boundary:

- **Reuse is extremely concentrated.** Four user template macros carry
  **99.05%** of all repeat opportunity. Admitting exactly the provable
  template tier is where all the value is.
- **The excluded kernel macros cost nothing and save a great deal.** `defun`
  is **80.6% of all dispatches** but yields **40** repeats across 633,590
  callsites, because each parse mints fresh callsites. Admitting it would
  buy 0.05% more reuse for roughly **463 MB** of entries. The exclusion was
  argued from semantics in §3.1; it turns out to be the dominant *memory*
  decision as well.

Against that ceiling the implementation serves **81,584 of 81,894**
repeat-dispatches — **99.62% of all achievable reuse**, across every macro
including the ones it deliberately refuses.

### 7.1 Why the user-macro prover has to exist

Issue #381 offered "conservative whitelisting of kernel macros only" as one
way to resolve the impurity hazard. The measurement rules that option out:
the admissible native macros — `get-default`, the `testing:` family, an
embedder logging wrapper — account for **695 repeat-dispatches, 0.85%** of
the available reuse. The remaining **99.15%** lives in user `defmacro`
template macros defined by the embedder, which a kernel-only whitelist
cannot reach by construction.

So the prover in §3.2 is not a refinement of the whitelist approach; it is
the only tier that captures anything. That is the main design conclusion of
this POC.

## 8. Cost

Warm-transaction benchmark against the same phylum (one warm environment, a
four-endpoint request mix), n=10 interleaved, benchstat: sec/op geomean
**−4.30%** shared / **−4.82%** per-runtime, with the heaviest write endpoint
at **−5.80%** / **−9.04%**. Allocations are the deterministic column: B/op
geomean **−5.44%**, allocs/op **−6.37%**, p=0.000, bit-identical between the
two modes because they save the same expansion work.

`internal/synthphylum` generates a synthetic corpus from the same shape
statistics (counts, depths, densities — no corpus content), so the hit-rate
result is reproducible without a private corpus: 98.0% on a warm request
mix.

Note that the elps micro-benchmark suite cannot observe this cache at all:
it is macro-light, and `elpstest.RunBenchmark` evaluates a per-iteration
unsealed copy, so every dispatch takes the bypass path. That is a
bench-fidelity gap worth closing separately.

## 9. Memory, and the one real hazard

In the topology the cache is built for — a warm environment with stable
program identity — the working set is small and flat:

| config | entries | cache bytes | hit rate |
|---|---:|---:|---:|
| shared, unbounded | 154 | 110 KB | 95.9% |
| shared, LRU 1024 / 256 | 154 | 110 KB | 95.9% (0 evictions) |
| shared, LRU 64 | 64 | 46 KB | 51.6% (thrash) |
| per-runtime | 154 | 110 KB × envs | 95.9% |

Hundreds of entries at ~730 B/expansion. Memory is a non-issue here.

**The hazard is parse-churn topologies**, and it is worse than the
entry-size estimator suggests: *the sealed callsite keys pin entire dead
parse trees*. Running the same phylum's unit-test suite (a fresh parse per
test file) under uncapped shared mode reached **15.6 GB RSS and was
OOM-killed** — the cache's own node-size estimate said 192 MB, while what it
actually retained was every AST those key nodes came from. The same suite
with `ELPS_MACRO_CACHE_CAP=8192` serves the same reuse (81,584) in 27 MB and
passes.

Consequences:

- **Per-runtime mode is the safe default recommendation.** It is
  structurally immune — the cache dies with the runtime — and it is at least
  as fast as shared mode.
- **The shared table must never run uncapped** outside tests.
- A weak-pointer keyed table (Go 1.24 `weak`) would remove the failure mode
  structurally, and is the natural next iteration if shared mode is wanted
  as more than an experiment.

## 10. A load-path fix that fell out of this

The first instrumented corpus run recorded 108 hits in 655k dispatches. The
prover was not at fault. `lisp.TextLoader` evaluated a per-environment
`Copy()` of its parse, and `Copy` clears the sealed flag — so every macro
*defined by loader-loaded code* bound an unsealed formals node in every
environment and was disqualified, while its callsites sat sealed and ready.
The embedder's whole utility-macro layer lives exactly there.

Every other load path (Reader, `LoadString`, `Program`, REPL) evaluates
sealed trees. The fix seals the loader's private per-environment copy, which
is sound for the same reasons the other paths are: the content is
parser-shaped, the copy is environment-private, and nothing mutates loader
output.

Independent of caching, this closes a gap in the sealed-AST model where
loader-loaded code was invisibly second-class.
