# Historical Substrate integration experiment

This directory retains benchmark output from an early local PoC, not runnable
source fixtures. Its recorded ELPS revision `b0bb64e` was a transient local
commit, not a supported public revision. Substrate's baseline was `2a76d4344`.
The obsolete, uncompiled `.go.txt` copies and their copy-into-another-repository
instructions were removed in issue #631.

Current executable integration tests and benchmarks belong to
[Substrate's preheater package](https://github.com/luthersystems/substrate/tree/refactor/template-preheater/internal/substrate/shiro/preheat)
in [Substrate #483](https://github.com/luthersystems/substrate/pull/483).
Use that PR's revision-labelled CI for current performance evidence and
[the current template contract](../../fork.md) for API guidance.

## What the historical experiment measured

The experiment exercised the public `e2e/sample-network/phylum_a` through the
shirocore loader, load probe, router, storage and event API. It compared business
responses, requested writes/events and ledger contents with independent expected
values. It used an in-memory MockStub, not a deployed Fabric peer; event requests
were observed, not downstream delivery.

Construction measurements compared template instantiation, legacy VM cloning
and cached cold loading. Publication was outside the per-VM timer. Two Apple M3
runs each used six interleaved 500ms rounds. The
[earlier run](sample-benchmark-prepacking.txt) followed the cell-view lookup
optimization; the [later run](sample-benchmark.txt) followed schema constructor
packing. The workload does not invoke schema forms, so that constructor change
was not exercised. Both datasets remain; the later run was substantially noisier.

| Historical construction | Earlier median | Later median (range) | Later bytes | Allocations |
| --- | --- | --- | --- | --- |
| Validated template instance | 576 microseconds | 612 microseconds (584–712) | 1.106 MB | 6,285 |
| Legacy VM clone | 529 microseconds | 575 microseconds (538–662) | 1.079 MB | 6,283 |
| Cached cold load | 3.213 ms | 3.293 ms (3.252–3.725) | 5.104 MB | 49,555 |
| One-time template publication | 6.983 ms | 7.197 ms (7.072–7.274) | 7.668 MB | 32,114 |

The later PoC median was 5.4 times faster than cached cold loading
(benchstat p=0.002, n=6). Its median was 6.31% slower than legacy cloning, but
that difference was not statistically resolved (p=0.310). The earlier run found
8.99% overhead (p=0.002); the later run does not establish that overhead
disappeared. Both runs measured 2.54% more allocated bytes and two additional
allocations per instance.

These historical observations show a correctness/performance tradeoff, not
optimal performance or current production results. Publication amortizes across
instances and is not transaction execution time. A deployment must validate its
initialization contract and real business effects independently.
