# Environment forking: cloning loaded templates by sharing sealed structure

`LEnv.Fork` (issue #380) clones a fully loaded, quiescent environment — the
*template* — into an independent environment on a fresh `Runtime`, sharing
every sealed value with the template and hermetically copying only the
mutable fraction.

This is a dividend of the sealed-AST design and lives directly on its
invariant: [docs/sealed-ast.md](sealed-ast.md) establishes that a sealed
node's bytes never change after parsing completes, and machine-verifies the
claim (fingerprint oracle, checked-mode inspector, `-race` watchdog). A
value that can never change is safe to hand to any number of runtimes, so a
fork can *share* it instead of rebuilding it — and at production scale
sealed program structure is the overwhelming majority of a loaded
environment. This document is separate from sealed-ast.md because its
audience is different: sealed-ast.md explains a protection mechanism to
kernel maintainers; this page specifies a construction API and its embedder
contract.

## What it is for

Fork removes the *reload* cost, not the *resident memory* cost, of
environment construction:

- **Pool refill.** An embedder serving requests from a pool of warm
  environments pays a full load (parse cache hit + evaluate every top-level
  form) per new environment. Forking one prepared template replaces that:
  measured on a production-scale phylum (~72k lines of lisp), fork cost
  **15.2 ms / 88.8k allocs / 9.5 MiB churn** against **71.4 ms / 631.5k
  allocs / 43 MiB churn** for a full load — 4.7× wall, 7.1× fewer
  allocations. The win grows with program size because the sealed fraction
  grows (59.3% of reachable values at stdlib scale, 82.5% at
  production-phylum scale).
- **Fork-served test runners.** A lisp test suite that pays a full load per
  test case can instead load a template once and serve every case a fresh
  fork (`elpstest.Runner.NewEnvFn`); the production-scale POC ran a full
  phylum suite 9.5× faster end-to-end (20.1 s vs 191.4 s), with identical
  case results.
- **NOT a memory feature.** Resident memory per fork was measured ~24%
  below a fully loaded environment (6.09 vs 7.96 MiB) — sealed-AST sharing
  through the process-wide parse cache already collapsed per-environment
  residency before Fork existed. Do not adopt Fork to save memory; adopt it
  to save CPU, GC pressure, and refill latency.

Fork is O(mutable + closure fraction) — milliseconds at production scale.
Run it at pool-refill time, in the background; never on the request path.

## Measured numbers

Fork beats a full load at every scale measured, and the margin widens with
program size — a full load re-evaluates every top-level form, while a fork
walks only the mutable and closure fraction, which shrinks proportionally
as programs grow (program AST is sealed parser output).

**Stdlib scale** — `BenchmarkEnvConstruction` in `lisp/lisplib/fork_test.go`,
the in-repo pair CI can track. Template: `InitializeUserEnv` + `LoadLibrary`
+ a small program with closures, macros, `labels` mutual recursion and
mutable globals. `benchstat -col /mode`, n=20:

```
          │     fork     │               fullload                │
          │    sec/op    │    sec/op     vs base                 │
EnvConst    366.8µ ± 17%   844.6µ ± 17%  +130.28% (p=0.000 n=20)
          │     B/op     │     B/op      vs base                 │
EnvConst    284.4Ki ± 0%   525.9Ki ± 0%   +84.94% (p=0.000 n=20)
          │  allocs/op   │  allocs/op    vs base                 │
EnvConst    1.257k ± 0%    5.354k ± 0%   +325.93% (p=0.000 n=20)
```

2.3× wall, 4.3× fewer allocations. The in-package implementation also beats
the exported-API prototype the feasibility pass measured (471 µs / 1,367
allocs) by cloning symbol maps directly instead of replaying `Package.Put`.

**Embedder-runtime scale** — an embedder's own lisp runtime library
(~11k lines) plus a loaded program, measured out-of-tree against this
implementation: fork **8.7 ms / 36.8k allocs / 4.3 MiB churn** vs full load
**50.9 ms / 440.9k allocs / 106 MiB churn** — 5.9× wall, 12.0× fewer
allocations (n=20, p=0.000). The full-load arm includes the embedder's
per-environment fixture setup, so read the ratio as indicative rather than
as a pure interpreter measurement.

**Production scale** — the POC numbers quoted above (15.2 ms vs 71.4 ms,
4.7× wall / 7.1× allocs on a ~72k-line phylum), measured with the same
algorithm against a proprietary corpus. Those remain the production-scale
evidence; the corpus is not reproducible in-repo, which is why the two
smaller scales are reported alongside.

## The contract

### Quiescence (asserted, no bypass)

The template must be *quiescent*: fully loaded, no evaluation in flight.
`Fork` errors if the call stack is non-empty, an evaluation entry is
active, or condition handlers are pending. There is deliberately no option
to skip the check — a mid-evaluation environment contains torn state, and a
fork of it would too. Fork never mutates the template, so concurrent forks
of the same quiescent template are safe; forking concurrently *with
evaluation on the template* is not (the check asserts, it does not
synchronize).

### Distinct runtimes

Each fork is a separate `Runtime`, in exactly the sense the `Runtime` doc
comment requires for concurrent evaluation: one runtime (and `LEnv` tree)
per goroutine. Template and forks may evaluate concurrently with each
other. Under `-tags elpscheck` the ownership checker enforces the model;
sealed values are its sanctioned cross-runtime class (they are immutable —
the same reasoning that exempts the nil/true/false singletons).

### What is shared, what is copied

| Value class | Policy |
|---|---|
| Sealed values (program AST, formals, quoted literals) | shared |
| Singletons (`()`, `true`, `false`) | shared |
| Functions (`LFun`) | header copied; captured environment remapped onto fork copies; builtin Go code travels by reference |
| Native payloads | shared by reference, unless `NativeCloner` / `ForkWithNativeReplacer` (below) |
| Mutable data (vectors, sorted maps, bytes, error stacks, tagged values) | hermetically copied, aliasing and cycles preserved |
| Source locations, format metadata (`Meta`) | shared (read-only after parse) |
| Macro-expansion debug metadata | dropped (debugger-only; aliases template state) |
| `Reader`, `SourceLibrary` | shared (process-wide cache / read-only) |
| `LoadCache` | shared (entries are immutable and sealed; see below) |
| `Stderr` | shared unless `ForkWithStderr` |
| Limit configuration (`MaxAlloc`, stack bounds, step budget, ...) | copied |
| `Profiler`, `Debugger` | do not travel (fork starts with none) |
| Call stack, condition stack, step accounting | fresh |
| Env-ID and gensym counters | **continued** past the template's |

`LoadCache` is shared for the same reason `Reader` is, and because
"preheat a template, fork per environment" is the topology the load cache
exists to serve: a fork that started with no cache would silently reparse
every file the template had already parsed. Sharing is safe because a
`CachedSource` is immutable and sealed throughout — `lisp/loadcache.go`
states the entry contract, and an implementation must already be safe for
concurrent use when several `Runtime`s hold it. The per-runtime re-entrancy
guard behind it is *not* carried: it describes a load in progress, and a
template must be quiescent to fork at all.

The counter continuation is load-bearing: lambda FIDs are minted as
`"_fun<envID>"`, so a fork whose counter restarted would eventually mint
FIDs colliding with the ones it inherited (corrupting function-name tables
and tail-call FID matching), and a restarted gensym counter would re-mint
load-time gensym names at runtime.

### Stateful natives: the one policy decision an embedder owns

The kernel cannot copy an `LVal.Native` payload — it is an opaque
`interface{}`. The default is to share payloads by reference, which is
correct for the immutable handles that dominate real templates (compiled
regexps, timestamps). Payloads that carry per-environment *state* are the
embedder's to handle, with three tools, in order of preference:

1. **Keep state out of the template.** Build the template *stopping before*
   the hooks that open stateful handles, and run those hooks on each fork —
   exactly where a per-fresh-environment design already runs them. This is
   the pattern for accumulators whose ops/macros are Go closures over the
   instance (e.g. the lisp testing suite: `elpstest`'s fork-served runner
   test builds the template without `libtesting` and loads it per fork,
   because a closure captured at template-load time can only ever see the
   template's instance).
2. **`NativeCloner`.** A payload type that implements
   `CloneNative() interface{}` is duplicated at fork time; the clone must
   be independent of the original and must not retain references into the
   template's runtime. This is the fork half of the native contract
   protocols sketched in issue #383.
3. **`ForkWithNativeReplacer`.** A per-fork substitution hook consulted
   before `NativeCloner`, for payload types the embedder cannot modify and
   for instance-specific rebinding (a per-fork storage handle).

A shared stateful native is the one way to leak state between template and
forks that no isolation test in this repository can see from the outside —
audit your template's native census when adopting Fork.

### Context

The template's `context.Context` never travels into a fork. Bind a
request-scoped context at checkout time with `ForkWithContext`, or use the
`*Context` evaluation methods per call.

The bound context is also the sanctioned channel for per-fork *values*:
builtins registered at template-load time are Go closures shared by every
fork, so the per-fork half of their state (a storage handle, a transaction
context) cannot live in the closure. Carry it as a `context.WithValue`
entry on the context bound to the fork and read it inside the builtin via
`env.Context().Value(key)`, falling back to the closure's load-time state
when the key is absent. The value follows the same scoping as
cancellation: it is visible through intervening lisp call frames, a
per-call `EvalContext` context overrides it for that evaluation only, and
neither the template nor any other fork can observe it.
`lisp/fork_context_test.go` pins this contract.

## Embedder patterns

Pool refill:

```go
template := buildTemplate()          // load once, at startup or upgrade
...
fork, err := template.Fork(lisp.ForkWithContext(reqCtx))
if err != nil { ... }
pool.Put(fork)                       // background refill, off the request path
```

Fork-served test runner (see `elpstest/fork_runner_test.go` for the
complete reference, including the stateful testing-suite rebind):

```go
r := &elpstest.Runner{
    NewEnvFn: func(t testing.TB) (*lisp.LEnv, error) {
        fork, err := template.Fork(lisp.ForkWithStderr(elpstest.NewLogger(t)))
        if err != nil {
            return nil, err
        }
        // per-fork stateful hooks here (testing package, storage handles...)
        return fork, nil
    },
}
```

## Verification

- `lisp/fork_test.go` audits the entire forked graph pairwise against the
  template: sealed values pointer-shared (with an anti-vacuity floor),
  mutable values pointer-distinct with identical content, template
  aliasing and cycles reproduced in the fork.
- `lisp/lisplib/fork_test.go` proves bidirectional isolation over real
  parsed programs two ways: observable mutations (neither side sees the
  other's writes) and a full-state fingerprint (structure-only hash of
  everything reachable; fork-side activity leaves the template's hash
  bit-identical, and a fresh fork reproduces it exactly).
- `lisp/fork_ownership_elpscheck_test.go` pins the checker model: sealed
  cross-runtime sharing sanctioned, mutable cross-runtime leaks still
  panic.
- Correctness at production scale (POC, issue #380): transaction results
  byte-identical between forked and fresh-loaded environments; a full
  phylum unit-test suite fork-served with identical results.
