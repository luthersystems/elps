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
any side-effecting form, and a gensym escaping under `quote` (§4).

The prover's unquote recognition must stay a *superset* of the evaluator's.
`getUnquoteType` matches the operator name on an `LSymbol` head and ignores
that symbol's quote flag, so `('unquote (f))` really does evaluate `(f)` at
every expansion; a prover that skipped quoted heads scanned that form as
inert template content and admitted an arbitrarily impure macro (fixed, and
red-proven by `TestMacroCacheQuotedUnquoteHeadNotCached`). Any future change
to `getUnquoteType` has to be mirrored in `pureMacroTemplateQ`.

This tier is what captures the `when`/`unless`/`default` utility layer that
dominates real request paths (§7).

### 3.2.1 The syntactic verdict is only half the admission test

Matching an operator by name proves nothing on its own. `if`, `let`, `let*`,
`progn`, `quasiquote` and `gensym` are ordinary bindings in ELPS, and
rebinding one of them changes what a macro body does without changing a
character of its syntax. Every such shadow was demonstrated to produce a
**wrong answer** rather than a misclassification: the shapes below return
`'(1 2 3)` with the cache off and `'(1 1 1)` with it on.

| shadow | mechanism |
|---|---|
| `gensym` → impure function | `(gensym)` was the one expression the grammar admitted *without proving it*, short-circuited on the name; the binding value was never examined |
| `if`, `progn`, `let`, `let*`, `quasiquote` | the operator switch matched the head symbol by name, admitting an impure body |
| any of the above via `set`, after the macro was proven | a verdict cached against an environment cannot see a later rebinding |
| any of the above inherited through `use-package` | the shadow need not live in the macro's own package |

So admission is split in two:

- **Syntactic verdict** (`macroPurity.pure`) — a function of the macro's
  sealed, immutable formals and body nodes and *nothing else*. This is what
  is memoized process-wide, keyed by the formals node.
- **Name-resolution obligations** (`macroPurity.defRefs` / `.callRefs`) —
  the operator spellings the proof interpreted, each of which must resolve
  to the kernel binding. These are environment dependent, so they are
  **re-checked on every dispatch** and never memoized. Re-checking is also
  what makes a later `(set 'if ...)` take effect.

Body operators resolve in the macro's **defining** environment, where the
body evaluates. The template's binder syntax resolves at the **callsite**,
where the expansion evaluates — see the boundary note below. A spelling that
does not resolve to the kernel binding simply makes the macro uncacheable;
refusing to cache is always sound.

Resolution has to mirror the evaluator exactly, including a detail that
produced a further defeat found by attacking the fix: `funCall` switches the
runtime into the *function's own* package for the duration of a call, so a
macro defined in package A reads A's bindings whoever called it. A shadow
installed in A is therefore invisible from the caller's package, and a check
that looked names up through the caller's current package admitted it. The
lookup now walks the lexical chain and falls through to the macro's own
package. Package-qualified spellings (`lisp:if`) name their package
explicitly and are resolved as written
(`TestMacroCacheShadowedInDefiningPackageNotCached`,
`TestMacroCacheQualifiedSpellingResolvedAsWritten`).

Two names the prover interprets carry *no* obligation, deliberately:

- **`quote`** (and the parser's own quote flags / `LQuote` wrappers).
  Reading a form as a quote only ever *raises* the quote depth, and quote
  depth is used in exactly one place: rejecting a gensym that escapes as
  data. Mis-reading a shadowed `quote` as the kernel one can only reject a
  macro that might have been admissible — never admit one.
- **`unquote` / `unquote-splicing`.** These are not bindings at all;
  quasiquote consumes them as syntax, matching the name on an `LSymbol`
  head. Requiring the enclosing `quasiquote` to *be* the kernel quasiquote —
  which the defining-environment obligations do — is what licenses that
  reading. Asserted rather than argued: rebinding `unquote` to an impure
  function leaves quasiquote's behaviour and the counter untouched
  (`TestMacroCacheUnquoteIsNotABinding`).
- **`true` / `false`,** the only *free* symbol reads the body grammar
  admits. They are kernel constants — `set`, `defun`, `defmacro`, `let`,
  `let*`, `labels` and `flet` all refuse to rebind them — so there is no
  binding for an obligation to check. Every one of those routes is asserted
  to refuse (`TestMacroCacheTrueFalseAreKernelConstants`); should one ever
  stop refusing, this read needs an obligation exactly like `if`'s.

The memo key was itself a defeat, twice. Keyed on the formals node while the
checks were environment dependent, a verdict computed in an unshadowed
environment licensed caching in a shadowed one (one sealed parse, two
runtimes); keying on what the verdict actually depends on — pure syntax —
removes that leak rather than documenting it. The key then still assumed
"one formals node, one macro", which a macro-generating macro breaks: a
template that splices ONE argument node into two `defmacro` forms

```lisp
(defmacro two (fs)
  (quasiquote (progn (defmacro p (unquote fs) 7)
                     (defmacro q (unquote fs) (bump)))))
(two (a))
```

gives `p` and `q` the same sealed formals node with different bodies, and
the verdict proven from `p`'s body admitted `q` — whose body is not pure by
any reading. Cache off: `(7 1) (7 2) (7 3)`; cached, before the fix:
`(7 1) (7 1) (7 1)`. The verdict now records the body nodes it was proven
from and is reused only for that body; a second body under the same key is
refused rather than re-proven, which also keeps the *identity* honest —
only one body per formals node is ever cacheable, so an entry cannot be
validated against a macro that merely shares its key
(`TestMacroCacheSharedFormalsTwoBodiesNotCached`, which asserts the shared
node as a premise so it cannot pass vacuously).

The legitimate version of the same shape is admitted unchanged: one source
form evaluated twice in defining environments that resolve `if` differently
shares a formals node AND a body, so the syntactic verdict is genuinely
shared, and it is the per-dispatch obligations that separate the two
instances (`TestMacroCacheSameFormalsDifferentDefiningEnv`).

Cost of the narrowing: **zero measured reuse**. The committed synthetic
corpus reproduces bit-identically before and after (706 entries, 5,000 hits,
98.0% hit rate), and no shape in the admitted tier acquires an obligation
that a normal program fails: a `when`-class macro records one
(`quasiquote`), a `default`-class macro four (`quasiquote`, `let*`, `gensym`
in the defining environment; `let*` at the callsite).

**Remaining boundary.** The obligations cover the operators the prover
*interprets*. They do not cover the **content** of a template, which is
output code evaluated at the callsite: `(quasiquote (if ...))` is inert to
the prover and reused verbatim, so whatever `if` means at the callsite it
means identically with and without the cache. The one place template content
*is* interpreted is the binder-syntax discharge — the prover reads
`[,g expr]` inside a template `let`/`let*`/`labels`/`flet`/`lambda` as
binder syntax rather than data, which is what admits a gensym in a binding
position. That is a claim about code that runs at the callsite, so those
spellings become callsite obligations whenever the macro mints a gensym.
A macro defined in a clean package and called from one that rebinds `let*`
to something that treats its binding list as data was a live defeat before
that check existed (`TestMacroCacheShadowedBinderAtCallsiteNotCached`).

One reading is known to be **wrong** and is nonetheless harmless, which is
worth stating rather than leaving to be rediscovered. Defining-environment
obligations resolve in `funData.env`, while the body evaluates one frame
*below* that, in a call environment carrying the macro's own **formals**. A
macro whose formal is named `if` (or `let*`, or `gensym`) is therefore
admitted on the strength of a binding the body will never consult. It cannot
change an answer, because a macro formal is bound to an *unevaluated
argument node* and an argument node is never a function: the body's
`(if ...)` can only fail to call it, deterministically and identically with
the cache on or off. Pinned behaviourally by
`TestMacroCacheFormalShadowingAnOperatorAgrees`; should macro formals ever
carry callable values, the obligations must move to the call environment.

Beyond that: the prover reasons about the macro's own body, not about what
the *arguments* at a callsite evaluate to (they are spliced by reference,
identically either way) and not about native macros, which remain a
hand-audited whitelist rather than a proof — the whitelist now at least
distinguishes two implementations that share a registration name (§5), but
whether one implementation deserves to be on the list is still an assertion
by whoever put it there.

Tests: `lisp/macrocache_shadow_test.go` — fourteen defeat shapes, each asserted
behaviourally (the cached and uncached evaluations of the same program must
agree; nothing inspects the classification), plus pins on which obligations
each admitted shape records and on the FID formats the admission path builds
by hand. `lisp/macrocache_boundary_test.go` attacks the narrowed prover
rather than re-testing the fixes: it pins the three readings above that carry
no obligation, pins the gensym obligation as an *identity* check (a pure but
non-kernel `gensym` must still be refused), and carries the memo-leak defeat
into the shared **table** — two runtimes, one shared parse, the second
shadowing the operator, where the entry's identity matches exactly and only
the per-dispatch resolution can refuse it.

Red-proofs, each executed by deleting the guard and re-running the named
test:

| guard removed | test that goes red | wrong answer it produces |
|---|---|---|
| `needBuiltin` gensym obligation | `…ShadowedGensymNotCached` | `'(1 1 1)` vs `'(1 2 3)` |
| `needSpecialOp` on `if` | `…ShadowedStructuralOpsNotCached` | `'(1 1 1)` vs `'(1 2 3)` |
| defining-package fall-through | `…ShadowedInDefiningPackageNotCached` | `'(1 1 1)` vs `'(1 2 3)` |
| quoted-head unquote recognition | `…QuotedUnquoteHeadNotCached` | `'('(1 1) '(1 2) 1)` vs `'('(5 1) '(6 2) 6)` |
| same, for `'unquote-splicing` | `…QuotedUnquoteSplicingHeadNotCached` | `'('(1 1) '(1 2) '(1 3) 1)` vs `'('(1 1) '(2 2) '(3 3) 3)` |
| `sameMacroBody` memo-key check | `…SharedFormalsTwoBodiesNotCached` | `'(7 1)×3` vs `(7 1) (7 2) (7 3)` |
| per-dispatch `defRefs` re-check | `…PurityMemoIsEnvironmentIndependent` | `'(1 1 1)` vs `'(1 2 3)` |
| `callRefs` binder promotion | `…ShadowedBinderAtCallsiteNotCached` | one gensym vs two |
| per-runtime cap enforcement | `TestMacroCacheRuntimeCap` | cap=8 exceeded at 9 entries |
| `elpscheck` control's unsealed arm | `…CrossRuntimeCheckerStillFires` | checker did not fire |

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

Every entry records the identity of the macro that produced it:
package+FID+registration id for native macros, the sealed formals-node
pointer for user macros. Every lookup validates the entry against the macro
the callsite resolves to *right now*. Redefinition — whether by `defmacro`
or by shadowing a kernel macro — necessarily creates a fresh function with a
fresh formals node, so a stale entry can never be served; it is simply
overwritten by the next expansion.

The registration id is there because a NAME is not an identity. A native
macro's FID is derived from its registration name, so two environments that
register different implementations under one qualified name were
indistinguishable to the process-shared table and the second was served the
first's expansion — a wrong answer, not a misclassification.
`funData.impl` separates them: one id per process-global definition (every
kernel macro, and everything `RegisterDefaultMacro` adds — so cross-runtime
hits survive for that tier), a fresh id per registration for anything bound
environment-locally through `AddMacros`, and no id at all — hence no
caching — for a macro value built directly with `lisp.Macro`.

Tests: `TestMacroCacheInvalidationOnRedefinition`, covering user
redefinition and kernel-macro shadowing in both cache modes, and
`TestMacroCacheNativeIdentityDistinguishesImplementations`, which runs two
same-named implementations in two runtimes over one shared sealed callsite
and requires every cache mode to agree with cache-off.

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
make safe — and which the `-tags elpscheck` ownership checker **permits**,
because sealed nodes are exempt from it by design (that exemption is the
whole point of sealing). The cross-runtime cache tests therefore RUN under
the tag rather than skipping; an earlier revision skipped them and offered
the skip as proof that a cross-runtime hit could not trip the checker, which
was an assumption stated as a result. Their passing is backed by an executed
positive control: the same two-runtime program shared *unsealed* must panic
with an ownership violation
(`TestMacroCacheCrossRuntimeCheckerStillFires`). Within one runtime the
checker is satisfied trivially.

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
  buy 0.05% more reuse for 633,590 entries — **0.46 GB** at the warm
  topology's 730 B/expansion, or **2.1 GB** at the 3,281 B/entry this very
  run measured (26.9 MB across 8,192 capped entries) — and that is before
  counting the dead parse trees each key would pin (§9). The exclusion was
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

- **Per-runtime mode is the safe default recommendation.** Its footprint is
  scoped to one runtime and released with it, and it is at least as fast as
  shared mode. That scoping is a lifetime bound, not immunity — see below.
- **The shared table must never run uncapped** outside tests. Note that
  entries published while the table was unbounded carry no LRU bookkeeping;
  `SetMacroCacheCap` adopts them (oldest-first) the next time a bounded
  store sees the drift, so raising a cap onto an already-filled table still
  bounds it. Without that adoption the eviction loop could only reach the
  entry it had just pushed, so every new store evicted itself while the
  untracked backlog stayed pinned forever — neither bound nor cache.
- **Per-runtime mode's bound is the runtime's LIFETIME**, and that is the
  precise claim — not "structurally immune". A long-lived runtime that keeps
  evaluating *fresh parses* (a REPL, a host that hot-reloads programs) pins
  dead parse trees exactly like uncapped shared mode. It is safe in the
  warm-pool topology because program identity there is stable, not because
  the map was bounded. `SetMacroCacheCap` / `ELPS_MACRO_CACHE_CAP` now bound
  it too, for hosts whose runtimes outlive their programs. The per-runtime
  bound is a wholesale **drop**, not an LRU: the table is an unsynchronized
  hot-path map, and in the only topology where it can grow without limit
  every old key is a dead parse, so recency carries no information worth
  paying for. Correctness is unaffected — a dropped callsite re-expands
  (`TestMacroCacheRuntimeCap`, red-proven).
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
