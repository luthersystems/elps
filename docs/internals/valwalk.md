# Unified value walker

A recursive walker over `*lisp.LVal` value graphs gets its children from one
place. `lisp.ShapeOf(LType)` is the single exhaustive classification of every
`LType`. `internal/valwalk` is the traversal engine built on it. A walker
supplies a visitor and does not switch on `LVal.Type` to find children.

This document is the plan for the refactor and the record of its review.

## Problem

On `origin/main` at `ed71723`, counted with a type-checked scan of non-test
Go (`go/types`, so `t := v.Type; switch t` counts):

| Measure | Count |
|---------|-------|
| Switches over `lisp.LType` | 91 |
| Of those, with a top-level `default:` arm | 84 |
| Recursive runtime value walkers | 13 families (table below) |
| Recursive syntax walkers over `*LVal` source | about 25 |
| Shared value walker | 0 |

`.golangci.yml` sets `exhaustive.default-signifies-exhaustive: true`. A
switch with a `default:` arm passes the linter whatever types it names. A new
`LType` reaches 84 switches with no compile or lint error. In a value walker
the `default:` arm usually treats the new type as an opaque leaf, so a new
container type is serialised, copied or compared as if it had no children.

## Design

### Package layout

`internal/valwalk` imports `lisp`. Package `lisp` cannot import
`internal/valwalk`. So the parts that walkers in `lisp` also need live in
`lisp`, and the engine lives outside it:

| Part | Package | Used by |
|------|---------|---------|
| `Shape`, `ShapeOf(LType)` | `lisp` (`lisp/shape.go`) | the engine, the hot paths in `lisp`, classifiers |
| Traversal engine `Walk` | `internal/valwalk` | walkers outside `lisp` (libjson, libelpspath) |

`ShapeOf` is exported because libjson and libelpspath are outside `lisp`. It
is an addition, so the API break gate passes.

### `lisp.ShapeOf`

```go
// Shape is how a value of an LType holds other values.
type Shape uint8

const (
    ShapeLeaf    Shape = iota + 1 // LInt LFloat LString LSymbol LBytes
    ShapeList                     // LSExpr LQuote: Cells
    ShapeArray                    // LArray: Cells[0] dims, Cells[1] data
    ShapeMap                      // LSortMap: entries
    ShapeTagged                   // LTaggedVal: Cells[0] payload
    ShapeError                    // LError: condition in Str, Cells
    ShapeFun                      // LFun: opaque to value walks
    ShapeNative                   // LNative: opaque to value walks
    ShapeMark                     // LMarkTerminal LMarkTailRec LMarkMacExpand
    ShapeInvalid                  // LInvalid and out-of-range tags
)

func ShapeOf(t LType) Shape
```

`ShapeOf` is one `switch t` that names every `LType` constant and has no
`default:` arm. An out-of-range tag (`LTypeMax` or more) returns
`ShapeInvalid` after the switch. A `Shape` is a classification only. It does
not say which children a walker visits. Each walker decides that, because
the walkers differ (see "Review", finding 4).

### `internal/valwalk.Walk`

The engine owns traversal: the explicit frame stack, the child order, the
snapshot of children, and the error path. It does not own limit policy. The
walkers order their depth, count, byte and cycle checks differently, and the
order is observable in error text and callback calls (finding 6). So the
engine exposes the state, and each visitor runs its checks in its own order.

```go
type Visitor[R any] interface {
    // Visit is called once per value, before any check. It returns
    // Descend with the children to walk, or Done with v's result.
    Visit(w *Walker[R], v *lisp.LVal) (Step, R, error)
    // Child is called before each child is walked (separators, charges).
    Child(w *Walker[R], parent *lisp.LVal, i int) error
    // Leave builds v's result from its children's results.
    Leave(w *Walker[R], v *lisp.LVal, children []R) (R, error)
}

type Step struct {
    Done     bool
    Children []*lisp.LVal // snapshot, taken when v is entered
    Edge     EdgeFunc     // path segment for child i; nil means transparent
}

func Walk[R any, V Visitor[R]](root *lisp.LVal, vis V) (R, error)

// Walker state, read by the visitor in the order it needs.
func (w *Walker[R]) Depth() int               // container frames above v
func (w *Walker[R]) OnPath(v *lisp.LVal) bool // v is an active ancestor
func (w *Walker[R]) Ancestors() []*lisp.LVal  // for boundary-time diagnosis
func (w *Walker[R]) Path() string             // rendered on demand
```

Rules:

1. **Borrowed children.** `Visit` returns the children to walk in
   `Step.Children`, and optionally a second slice in `Step.Following` (an
   array's data after its dimensions). The engine reads these slices as the
   walk goes. It does not copy them. A visitor that must not see later
   mutation passes a slice it built itself: the map visitors build their
   pair snapshot from `Map.Entries` once, as `lisp/embed.go:224` does. List
   and array visitors pass the value's own `Cells`. The engine loads each
   child before it calls `Child`, so a charge callback in `Child` cannot
   change which child is visited.
2. **Iterative.** The frame stack is a slice. A deep value does not grow the
   Go stack.
3. **Results.** `children` in `Leave` is valid only during the call. A
   visitor that keeps the slice copies it. The engine clears the slots after
   `Leave`.
4. **Paths.** A frame stores its parent, child index and `EdgeFunc`. `Path`
   renders segments only when called. A visitor that puts a path in an error
   calls `Path()` at that point, so the error holds a string and does not
   borrow frames.
5. **No limit defaults.** The engine has no `Limits` struct. A zero limit
   stays a real limit, because each walker keeps its own config
   (`WithTypedMaxDepth(0)` is tested at
   `lisp/lisplib/libjson/typed_test.go:502`).
6. **Performance is measured, not assumed.** Go 1.26 calls a method of a
   type-parameter constraint through the instantiation dictionary. A
   struct visitor does not make the calls direct. Commit 2 adds a prototype
   benchmark, assembly (`-gcflags=-S`) and escape output (`-m`). The engine
   is accepted only if it is within CI's gate (15% timing, 5% allocations)
   of the hand-rolled tag walker on `BenchmarkWorkload/json` and the libjson
   benchmarks.

## Migration list

A walker moves only if the engine reproduces its output, error text, error
path, error precedence and callback calls on golden fixtures.

| # | Walker | Location | Cycle behaviour kept | Status |
|---|--------|----------|----------------------|--------|
| 1 | `json:tag` | `lisp/lisplib/libjson/tag.go` | reports a cycle only when the container repeats on the path at the depth boundary | migrated |
| 2 | `json:untag` | `lisp/lisplib/libjson/untag.go:65` | none; limits end a cycle | kept: +18% to +24% time |
| 3 | `json:canonize` | `lisp/lisplib/libjson/canonize.go` | rejects an active ancestor before dispatch, after the count check | migrated |
| 4 | Typed encoder (traversal only) | `lisp/lisplib/libjson/typed.go:199` | scans the full path for any repeat at the depth boundary | kept: +23% time |
| 5 | `libelpspath` validator | `lisp/lisplib/libelpspath/libelpspath.go:287`, `lisp/cycle.go:192` | tracking after 64 levels, width-weighted memo | kept: +5.6% allocs/op |
| 6 | `libelpspath` copy | `lisp/lisplib/libelpspath/path.go:169` | memo with height, operation-scoped budget | kept: +24% B/op, +33% time |

"Kept" means the migration matched every golden but went over CI's gate
(15% timing, 5% allocations) at `GOMAXPROCS=1`, n=10. The code stays
hand-rolled, and its `elpsvalwalker` allowlist row records the measurement.
The cost is the engine itself: each value goes through three
dictionary-dispatched calls and a `Step` value, which a walker doing little
work per value cannot absorb. Selected-path accessors (`dotPath.Get`,
`path.go:1225`) and mutating iterators were out of scope.

### Dropped from migration

| Walker | Reason |
|--------|--------|
| `convertContainer` (`lisp/embed.go:140`) and the Serializer copy (`lisp/lisplib/libjson/json.go:1236`) | Different work weights, memo grain (64), root-failure mapping and array policy. They stay as two adapters. Both gain an exhaustive `ShapeOf` dispatch. |
| Template admission and plan (`lisp/template.go:657`, `lisp/template_plan.go:178`) | Admission registers identity before descent and walks environments and capacity tails. The plan compiler is indexed, not recursive. |
| `elpsutil` template compile (`elpsutil/template.go:111`) | Parsed syntax. It compiles the same pointer twice with a different quote flag, so identity is not a key. |
| Fork checker (`elpstest/forkcheck.go`, `forkcheck_storage.go`) | It is the oracle for the storage walkers. Sharing their traversal could make it miss the same edge. |
| DAP expand and format (`lisp/x/debugger/dapserver/translate.go:160`) | One level per request, no recursion. |
| Ownership key, seal predicate | Constant-time classifiers. They get exhaustive switches instead. |
| `render_bounded` (`lisp/render_bounded.go`) | Multipass rendering with cycle probes and markers. No cycle mode describes it. |

## Exhaustive dispatch without migration

These keep their own code. Each one drops its `default:` arm and names every
`LType`, or dispatches through `ShapeOf`:

| Code | Location | Kind |
|------|----------|------|
| `Copy` | `lisp/copier.go:495` | recursive, hot |
| `equalShallow`, `equalIter` | `lisp/lisp.go:1671` | recursive, hot |
| Plain JSON encoder | `lisp/lisplib/libjson/encode.go:508` | recursive, hot |
| Map key classifiers | `lisp/maps.go:328`, `:352`, `:367`, `encode.go:643` | classifier |
| `convertValue`, Serializer `convert` | `lisp/embed.go:98`, `json.go:1236` | recursive |
| `sealableNodeType` | `lisp/seal.go:136` | classifier |
| `ownershipKey` | `lisp/ownership_check_elpscheck.go:481` | classifier |

`loader.go:722` and `detach.go:245` already name every `LType`. They join the
analyzer scope so they stay that way.

## Guards

1. **Constant coverage.** `TestShapeOfCoversEveryLType` (`lisp`) asserts
   `ShapeOf` returns a shape other than `ShapeInvalid` for every constant from
   `LInt` to `LTypeMax-1`, and `ShapeInvalid` for `LInvalid` and `LTypeMax`.
2. **Edge coverage.** `TestValwalkReachesEveryChild` (`internal/valwalk`)
   builds, for each container shape, a value whose every child is a unique
   sentinel: list cells, array dims and data, map keys and values, tagged
   payloads, error cells and mark cells. Each migrated visitor must reach
   each sentinel it claims, with the expected path. A new container type
   classified as `ShapeLeaf` fails this test, because its sentinels are not
   reached.
3. **`elpsltypeswitch` analyzer** (elpsvet). Type-checked. It reports, in
   its scope list, any `switch` whose tag has type `lisp.LType` (including
   `switch t` on a local and a method result) that has a `default:` arm or
   misses a constant. Scope: `lisp/shape.go`, `internal/valwalk`, and every
   file in the table above. The global `exhaustive` setting stays as it is.
   Flipping `default-signifies-exhaustive` would flag 84 switches, most of
   them builtin type errors whose default is correct. That change is a
   separate decision.
4. **`elpsvalwalker` analyzer** (elpsvet). Type-checked. It reports a
   function that both dispatches on `lisp.LType` (a switch, an `if` on
   `.Type`, or a call to `ShapeOf`) and walks children (a call that reaches
   itself through the package's static call graph, including methods on the
   same receiver and closures, or a loop that pushes `*lisp.LVal` children
   onto a slice). It runs outside `internal/valwalk`. An allowlist in the
   analyzer names every existing walker with a reason (syntax walker,
   hot path, oracle, dropped above). Limits, documented in its doc comment
   and covered by fixtures: calls across packages, interface calls, and
   method values are not followed. A clean run means no new walker of the
   recognised forms. It does not prove no walker exists.

## Code walkers

| Site | Finding | Plan |
|------|---------|------|
| `analysis/codewalk.go:204` | Already on `codewalk.Walker`. The switch labels scope kinds. | No traversal change. Scope kind comes from neutral metadata on the internal source event, so the operator-name switch goes. |
| `lint/analyzers.go:2806`, `:2814` (`walkLambdaListCalls`, `:2917`) | Needs every formals list, including malformed and empty ones, and skips quote and quasiquote. | Add an internal formals-occurrence event to the source walker, emitted before grammar rejection. Keep it internal (`codewalk_internal_test.go:24`). |
| `minifier/minifier.go:1010` (`firstGlobalFallback`) | Treats all of defmacro, macrolet and quasiquote as template, quoted data included. | Kept hand-rolled. On `internal/codewalk.Syntax` it matched the goldens but was 121% to 382% slower. |

Each move is kept only if its existing tests and new goldens pass unchanged.

## Behaviour preservation

1. **Goldens before each move.** A commit before each migration adds
   fixtures of output, aliases, error text, error type and cause, path,
   callback trace (charges, `Map.Entries` calls), competing limits at the
   boundary and one past it, custom maps and malformed values.
2. **Differential check.** The golden generator runs on the base commit.
   The fixtures are generated once there and checked in.
3. **Plain JSON bytes.** For each serialisation option, the fixture corpus
   is encoded on base and on head, and the bytes must match.
4. **Benchmarks.** Each migration step runs base and head interleaved,
   `GOMAXPROCS=1`, `-count=10`, and adjudicates with `cmd/benchgate` at CI's
   thresholds. A step over the gate is reverted, and the walker stays
   hand-rolled with an allowlist entry that says so.

## Commit plan

One pull request. One commit per line.

1. This plan.
2. `lisp.ShapeOf`, `TestShapeOfCoversEveryLType`.
3. `internal/valwalk` engine, edge coverage test, prototype benchmark.
4. `elpsltypeswitch` and `elpsvalwalker` analyzers with fixtures and the
   allowlist of existing walkers.
5. Goldens for `json:tag`, `json:untag`, `json:canonize`, typed encoder.
6. One commit per migration step 1 to 4, each removing its allowlist row.
7. Goldens, then one commit each, for `libelpspath` validator and copy.
8. Exhaustive dispatch for the table above (no `default:`).
9. The three code walker changes.

## Review

An adversarial review of the first draft of this plan found 25 issues. Each
is resolved in the plan above.

| # | Severity | Finding | Resolution |
|---|----------|---------|------------|
| 1 | blocker | `internal/valwalk` imports `lisp`, so walkers in `lisp` cannot use it. | `ShapeOf` moves to `lisp`. The engine serves walkers outside `lisp`. |
| 2 | major | 13 runtime walker families were missing (export validation, sealing, admission, macro stamping, `sealFP`, `checkContainerDepth`, `containsCycle`, oracle digests). | Listed in the `elpsvalwalker` allowlist with reasons. |
| 3 | major | About 25 syntax walkers needed classification. | Allowlisted as syntax walkers. They are not value walks. |
| 4 | major | One fixed child shape fits no two walkers (array wrappers, capacity tails, error cells, mark cells). | `Shape` classifies only. Each visitor returns its children. |
| 5 | major | Cycle modes did not match tag, typed, untag or canonize. | No engine cycle mode. Visitors use `OnPath` and `Ancestors` in their own order. |
| 6 | major | Limit order and inter-child events are observable. | Visitor-ordered checks and a `Child` hook. |
| 7 | major | Zero limits meant "default" and broke `WithTypedMaxDepth(0)`. | No engine limits. Walkers keep their config. |
| 8 | major | `MemoAfter` fit neither conversion walker. | Conversion walkers dropped from migration. |
| 9 | major | Lazy live indexing broke the snapshot contract. | `Visit` returns a snapshot. |
| 10 | major | `libelpspath` is not `Ignore`. | Named validator and copy steps keep their memo and order. |
| 11 | major | Template admission registers identity before descent. The plan compiler is not recursive. | Dropped. |
| 12 | minor | `elpsutil` compile keys on pointer and quote flag. | Dropped. |
| 13 | major | The fork checker is an independent oracle. | Dropped. |
| 14 | major | DAP, ownership key and seal predicate are not recursive walkers. | Dropped. They get exhaustive switches. |
| 15 | major | `render_bounded` is multipass. | Dropped. |
| 16 | major | Value visitors do not devirtualise in Go 1.26. | Prototype benchmark, assembly and escape output gate the engine. |
| 17 | major | Arena results conflict with owned output. | `children` is borrowed and cleared. Visitors copy what they keep. |
| 18 | major | `PathFormat` alone cannot keep paths. | `EdgeFunc` per frame, transparent edges, path rendered into the error string at failure. |
| 19 | major | A syntactic guard has bypasses. | Type-checked analyzers with call-graph reachability, fixtures per bypass, and stated limits. |
| 20 | major | The bijection test misses dropped children and is impossible as written. | Constant coverage and edge coverage are separate tests. |
| 21 | minor | Counts were 91/84. Loader and detach already enumerate. | Counts corrected. Global flip recorded as a separate decision. |
| 22 | minor | `analysis` already uses the code walker. | Metadata change only. |
| 23 | major | Lint needs a formals-occurrence event. | Internal event before grammar rejection. |
| 24 | major | The minifier needs raw syntax traversal. | `internal/codewalk.Syntax` with sticky state. |
| 25 | major | Goldens covered only steps 1 to 4. Byte identity was not testable. | Goldens before every move, generated on base. Base-versus-head byte comparison. |

## Implementation review

A read-only adversarial review of the finished implementation found five
issues. Each is fixed in this pull request.

| # | Severity | Finding | Fix |
|---|----------|---------|-----|
| 1 | major | `Walk` reserved result slots for `Children` and `Following` at once. A tagged `1024x1024` array rejected by its value limit allocated about 8 MiB first. | `Following` storage is reserved after its checks pass. The rejection now allocates 120 B/op. A test bounds it. |
| 2 | minor | `Child` ran before the child was loaded, so a charge callback that mutated the next cell changed the `json:tag` output. | The engine loads each child before `Child`. Goldens generated on `origin/main` pin the order for tag and canonize. |
| 3 | minor | `elpsvalwalker` missed a tagless `switch { case v.Type == ...: }`. | Case conditions of tagless switches count as dispatch. Fixtures cover fields, locals and an unrelated `Type int`. |
| 4 | minor | This document did not record the final status or the borrowing contract. | Status column in "Migration list" and the "Borrowed children" rule. |
| 5 | nit | The goldens were 11 MB. | Payloads over 512 bytes are stored as length and SHA-256. The goldens are 1.6 MB, and they still pass on the code before this change. |
