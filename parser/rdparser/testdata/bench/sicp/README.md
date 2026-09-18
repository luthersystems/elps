# Frozen parser benchmark inputs

These six files are byte-for-byte copies of `_examples/sicp/*.lisp` from
ELPS commit
[`6d267f827c727072afd2b7d7cca4786e5755b150`](https://github.com/luthersystems/elps/tree/6d267f827c727072afd2b7d7cca4786e5755b150/_examples/sicp),
the base revision of PR #658. They preserve the inputs previously measured
by `BenchmarkParser` and `BenchmarkParserFaultTolerant`, including the same
six benchmark names.

The runnable examples now include a corrected recursive closure and extra
assertions. Parsing that larger example under the same benchmark name
reported allocation growth caused by different source, not a same-input
parser comparison (issue #657). This snapshot separates benchmark workload
from example maintenance; the live example and its regression assertions
remain under `_examples/sicp`.

These files are **parser input, not executable examples**. In particular,
the historical `approx.lisp` contains the old recursive `let` pattern.
Do not repair, reformat, or run this snapshot as a runtime correctness test.

`TestParserBenchmarkFixtures` checks the directory, exact file count,
byte lengths, SHA-256 values, and this provenance table. Any intentional
corpus update must update that test and this provenance together, explaining
how benchmark comparisons will continue to use identical inputs. Do not
change benchmark thresholds to compensate for a workload change.

| File | Bytes | SHA-256 |
|---|---:|---|
| approx.lisp | 1745 | b63cffdf28b5a30e64c12167e7aa4986bdfcf79b8cdd61468dcce055333fd3dd |
| complex.lisp | 5318 | 52d3971e7cabbb8fad9e800b7a1a78b461f62cc794a06a02949f9ab7217df44e |
| diff.lisp | 1774 | 760a20200cca9df3ab4ed5845612bdd4ad4cda53f9a1b51957efb6ffa6166d92 |
| scheme-math.lisp | 12076 | b449bb077e92a3ae1aa41c825c099309411070ac51ed96d7c4cb46c960ffe9c4 |
| sicp.lisp | 7235 | efb243d625b331e3d3af428f6070a88c00d1f5a03aab513afc1f071572c896c9 |
| stream.lisp | 7116 | a6ab578d7f1d97dcaf41900797f1b268b1e5cfb7c1d8f743c6e97cb4a308e4d2 |
