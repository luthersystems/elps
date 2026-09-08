# VM template design experiments

The production design and migration contract are in [Immutable VM templates](../fork.md).
This directory retains public, synthetic experiment artifacts; it is not a
second supported implementation or an alternative embedding API.

## Designs compared

| Design | Result |
| --- | --- |
| Recursive clone with preindexed slice storage | Establishes admission/alias correctness, but rebuilds identity lookups per VM |
| Exact pre-sizing of clone lookups | Small useful optimization, but still discovers the graph for each VM |
| Indexed plan with whole-VM allocation blocks | Faster allocation, but a scalar survivor retains unrelated VM state |
| Indexed plan with individual allocations | Selected: fixed reconstruction work and ordinary object lifetimes |

The initial PoC retained a privately cloned environment and indexed it again.
The production design compiles directly into owned immutable descriptors and
deletes the recursive VM clone engine. Its retained metadata is not a live VM.

## Production-plan measurements

Apple M3, darwin/arm64, Go 1.26.5, six 500ms samples per arm at revision
`c0372d1`. These are public synthetic fixtures, using the same runtime library
set and cached program in the cold/reference arm. Values below are medians,
not latency guarantees. Subbenchmarks run sequentially; compare the raw spread
before drawing conclusions about small timing differences.

| Fixture | Publish once | Fork | Cached cold | Steady-state speedup |
| --- | ---: | ---: | ---: | ---: |
| Runtime stdlib and aliased list | 255 µs | 37 µs | 79 µs | 2.12× |
| Plus 250 functions, stock/JSON maps, bytes and lists | 1.52 ms | 167 µs | 984 µs | 5.91× |

Forks allocate 140,968 bytes / 1,109 allocations and 521,577 bytes / 4,863
allocations respectively. Publication allocates approximately 0.55 MB and
3.17 MB. Initial source loading is **additional** to publication, so a template
is an amortized optimization, not the fastest path for every one-shot VM.

Reproduce the [raw measurements](production-benchmark.txt):

```sh
go test ./lisp/lisplib -run '^$' \
  -bench '^BenchmarkTemplateConstruction$/functions=(0|250)$/(NewTemplate|TemplateFork|ColdLoadCachedProgram)$' \
  -benchmem -benchtime=500ms -count=6
```

Correctness controls check independent cold literals, alias mutation and
function/map behavior before timing. Publication remains outside the fork
timer. The separate direct-API `BenchmarkTemplatePlan` also checks the final
constructed VM after stopping the timer.

The whole-VM-block experiment has a synthetic retention counterexample:
keeping one independent scalar retained about 4 MB instead of about 114 bytes;
keeping a 17-byte slice retained about 1 MB instead of 48 bytes. The production
implementation uses individual object/backing allocations. A subprocess
retention regression keeps unrelated test allocations out of that measurement.

The [Substrate sample experiment](substrate/README.md) and raw sample benchmark
files record the earlier PoC revision. Their timings and extra clone step are
historical; do not treat them as production-plan benchmark results. The sample
uses public repository code and actual Substrate handlers with its in-memory
MockStub, not a deployed Fabric peer.

## What was removed after the experiment

The initial custom-map factory protocol is gone: JSON maps moved into the
interpreter, preserving their string-only key policy without opaque callbacks.
Mutable native cloning/replacement is gone from VM construction. New tests
reject these old admission cases; their surviving alias, map-operation and
per-VM setup behavior remains covered.

The original regression fixtures continue to exercise source independence,
sibling/later-instance isolation, explicit schema captures, cycles, overlapping
cell/byte views and rejected partial seals. Function-definition error locations
also have an independent cold-load cancellation sweep.

Tracking: [#622](https://github.com/luthersystems/elps/issues/622).
