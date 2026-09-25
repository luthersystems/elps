# Value-walker behavioral guard

Issues [#598](https://github.com/luthersystems/elps/issues/598) and
[#600](https://github.com/luthersystems/elps/issues/600) originally described a
guard tied to the removed `Fork` API. The surviving requirement is a shared
behavioral guard for every current rebuilding walker, not restoring that API.

`lisp/walker_oracle_test.go` drives actual entry points on one generated graph:

| Entry point | Registered implementation | Contract checked |
| --- | --- | --- |
| Lisp `copy` builtin | `detacher` | Private containers and cloneable natives; preserve payload aliases |
| Private `detach` | `detacher` | Same data ownership; refuse opaque annotations |
| `(*LVal).Copy` | `copier` | Private containers and cloneable natives; preserve payload aliases |
| `stampMacroExpansion` with a non-nil debug context | `macroStamper` | Private stamps, untouched input, shared value payloads, cyclic syntax remains cyclic |
| `NewTemplate` followed by two `NewVM` calls | `templateInventory`, `templateCompiler` | Private mutable state; preserve aliases and overlapping cell slots; share admitted immutable values |

The registry test requires every registered walker to have a behavioral adapter
and every cross-walker payload kind to appear in the graph census. Existing
memo-field, payload-switch and struct-copy source scans remain in place. These
are complementary: behavioral tests observe mistakes in real copying, while
source scans require an explicit decision about newly added fields and walkers.

## What the oracle observes

- Independently traversed source/result paths, with cycle and size bounds.
- Distinct equal-content maps, bytes and native objects, plus multiple headers
  over one payload. Pairwise identity must agree in both directions: neither
  de-aliasing nor coalescing equal but distinct objects is allowed.
- Source, result and sibling writes to map entries, bytes, cloneable natives,
  list slots and array backing-list slots. The target must actually change,
  while both other arms remain unchanged.
- Scalar values, quote/splice flags, source locations and format metadata.
  Snapshots encode token/location contents, not just their addresses.
- Debug metadata on changed descendants, including the cyclic rerun, not only
  the root. The input is observed before construction, catching metadata-only
  writes even when location stamping is otherwise correct.
- Native annotations under non-`LNative` headers, without losing the annotated
  container's descendant payloads. Separate fixtures check the different
  refusal/sharing contracts rather than labeling every native a leak.
- Positive immutable-code/native sharing, distinct-header cell views, and
  construction failures returning no usable partial container.

The stamper is not a deep-copy operation. Its acyclic syntax headers need not
retain identity, and its value payloads intentionally remain shared. Likewise,
copy/detach do **not** preserve overlapping cell-slot aliases; templates do.
These differences are explicit policies, not a known-broken-walker allowlist.

Template source/format metadata is frozen program state, shared by contract.
Writing an internal field of a frozen location is not a valid VM operation.
The guard instead checks public `SetSource` header replacement isolation;
copy/detach also get direct mutation probes for their independently owned
locations/comment tokens. No claim is made that templates deep-copy metadata.

## Negative controls and reproduction

The permanent controls run clean real walks before injecting a defect and
require a witness from the relevant channel. They cover lost payload aliases,
coalescing equal payloads, retained bytes, sibling sharing, wrong byte contents,
lost flags/locations/comments, retained debugger state, input-only metadata
writes, a no-op stamper and missing descendant stamps.

The following production mutations were also applied individually during
development. For each mutation, we ran the common behavioral test alone
(`TestWalkerBehaviorOracle`, command below), and it detected the defect. This
demonstrates coverage by the common oracle, not that it is the first or only
detector; existing targeted regressions also cover some of these defects. All
mutations were restored afterwards. To reproduce, change the named lookup's `ok` condition to
`ok && false`, except for the two explicitly stated assignment mutations:

| Function / mutation | Common oracle witness |
| --- | --- |
| `detacher.detachMapData`: bypass `d.maps[md]` lookup | `copy` and `detach`: map payload alias |
| `detacher.byteSlice`: bypass `d.bytes[b]` lookup | `copy` and `detach`: byte payload alias |
| `detacher.cloneNative`: bypass `d.natives[payload]` lookup | `copy` and `detach`: native payload alias |
| `copier.mapData`: bypass `c.maps[md]` lookup | `LVal.Copy`: map payload alias |
| `copier.byteSlice`: bypass `c.bytes[b]` lookup | `LVal.Copy`: byte payload alias |
| `copier.cloneNative`: bypass `c.natives[payload]` lookup | `LVal.Copy`: native payload alias |
| `templateCompiler.mapData`: bypass `c.maps[source]` lookup | `Template.NewVM`: map payload alias |
| `macroStamper.syntax`: store `s.copies[v] = v` instead of `cp` | Cyclic seed: extra output paths instead of a closed back-edge |
| `macroStamper.stampedCopy`: assign debug metadata to `v`, not `cp` | Source changed during construction |

```sh
go test ./lisp -run '^TestWalkerBehaviorOracle$' -count=1
go test -race -tags=elpscheck ./lisp -run '^(TestWalkerBehaviorOracle|FuzzWalkerBehaviorOracle)' -count=1
go test ./lisp -run '^$' -fuzz '^FuzzWalkerBehaviorOracle$' -fuzztime=30s
```

`FuzzWalkerBehaviorOracle` runs one adapter and at most eight graph-building
instructions per input, varying payload groups, header aliases, list/array
nesting, flags and cyclic syntax. It never evaluates unbounded Lisp code and
has no inconclusive-success path. Use the command above to measure throughput
at the current revision; counts from different revisions or machines are not
directly comparable. This is a bounded structural oracle, not a replacement
for the existing cold-load/template transaction parity fuzzers.

Not every memo has an independently observable identity effect. In particular,
the template native descriptor memo deduplicates already-approved immutable
objects that remain shared anyway. Its structural registry guard matters; a
promise that removing *every* memo must break alias semantics would be false.
