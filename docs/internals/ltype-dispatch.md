# Exhaustive LType dispatch

A new `lisp.LType` must fail a check at every place that walks values or
classifies them. Three parts enforce this:

| Part | Location | Rule |
|------|----------|------|
| `lisp.ShapeOf` | `lisp/shape.go` | One switch over every `LType` with no `default:` arm |
| `elpsltypeswitch` | `cmd/elpsvet/ltypeswitch.go` | In its scope, an `LType` switch names every constant and has no `default:` arm |
| `elpsvalwalker` | `cmd/elpsvet/valwalker.go` | A new recursive value walker is reported unless an allowlist row gives its reason |

When you add an `LType`, run `make elpsvet` and fix every report. Then add
the type to `TestShapeOfCoversEveryLType` (`lisp/shape_test.go`).

## Problem

`.golangci.yml` sets `exhaustive.default-signifies-exhaustive: true`. A
switch with a `default:` arm passes the linter whatever types it names. On
`origin/main` at `ed71723`, 84 of the 91 switches over `lisp.LType` had a
`default:` arm. In a value walker, that arm treats a new container type as a
leaf, so its children are dropped with no error.

The global setting stays. Most of the 84 defaults are builtin type errors,
and a default is correct there.

## `lisp.ShapeOf`

`ShapeOf(t LType) Shape` says how a value of type `t` holds other values:

| Shape | Types |
|-------|-------|
| `ShapeLeaf` | `LInt`, `LFloat`, `LString`, `LSymbol`, `LBytes` |
| `ShapeList` | `LSExpr`, `LQuote` |
| `ShapeArray` | `LArray` (dimensions in `Cells[0]`, data in `Cells[1]`) |
| `ShapeMap` | `LSortMap` |
| `ShapeTagged` | `LTaggedVal` |
| `ShapeError` | `LError` |
| `ShapeFun` | `LFun` |
| `ShapeNative` | `LNative` |
| `ShapeMark` | `LMarkTerminal`, `LMarkTailRec`, `LMarkMacExpand` |
| `ShapeInvalid` | `LInvalid` and tags at or above `LTypeMax` |

A shape is a classification only. Each walker decides which children it
visits, because walkers differ: array dimension headers, error cells and
mark cells are children for some walkers and not for others.

## `elpsltypeswitch`

The analyzer is type-checked, so it also sees `t := v.Type; switch t` and a
method that returns an `LType`. It applies to the files and functions in
`lTypeSwitchScope`:

| File | Functions |
|------|-----------|
| `lisp/shape.go`, `loader.go`, `detach.go`, `copier.go`, `maps.go`, `embed.go`, `seal.go`, `ownership_check_elpscheck.go` | all |
| `lisp/lisp.go` | `equalShallow`, `equalIter` |
| `lisp/lisplib/libjson/encode.go`, `json.go` | all |
| `lisp/lisplib/libjson/canonize.go`, `tag.go`, `untag.go` | `value` |

These switches name every constant. A type that the old `default:` arm
handled is listed in an explicit case with the same body. An out-of-range
tag is handled after the switch, as before.

## `elpsvalwalker`

The analyzer reports a function that dispatches on `LType` (a switch, a
tagless `switch` case, an `if` on `.Type`, or a call to `ShapeOf`) and walks
children (it reaches itself through the package's static call graph, or it
appends `*lisp.LVal` values to a slice in a loop).

Each existing walker has a row in `valueWalkerFunctions` with a reason.
The 144 rows are:

| Reason | Rows |
|--------|-----:|
| syntax walker (source forms with syntax and quote rules) | 58 |
| hot path | 38 |
| specialized traversal | 17 |
| hand-rolled JSON walker | 11 |
| hand-rolled path walker | 10 |
| oracle (independent checker for another walker) | 10 |

Limits: cross-package calls, interface calls, method values, function
parameters and reflection are not followed. A clean run means no new walker
of the recognised forms. It does not prove that no walker exists. The
analyzer's doc comment lists every limit, and `cmd/elpsvet/testdata/valwalker`
covers each one.

## JSON walkers

The JSON walkers are recursive and hand-rolled. Their limits, error text,
error paths, check order and charge-callback order are pinned by goldens
generated on `origin/main`:

| Golden | Location |
|--------|----------|
| `walker-tag`, `walker-untag`, `walker-canonize`, `walker-dump-typed`, `walker-plain-bytes` | `lisp/lisplib/libjson/testdata/` |
| `walker-canonize-condition`, `walker-*-mutating-charge` | `lisp/lisplib/libjson/testdata/` |
| `walker-validator`, `walker-copy` | `lisp/lisplib/libelpspath/testdata/` |

A payload over 512 bytes is stored as its length and SHA-256, so the goldens
keep byte identity at 1.6 MB.

### Canonize paths

`json:canonize` reports a path such as `$[0]["a"]` in its errors. The walker
keeps a stack of path edges (index or key) and renders the path only when it
returns an error. It does not build a path string for each value. The walker
state comes from a `sync.Pool`.

Measured against `origin/main` (`GOMAXPROCS=1`, n=10, interleaved,
`cmd/benchgate`):

| Benchmark | Time | Allocations |
|-----------|-----:|------------:|
| `JSONWalkers/Canonize/small` | -26.3% | -22.2% |
| `JSONWalkers/Canonize/records400` | -24.1% | -26.2% |

On `origin/main`, `fmt.Sprintf` and `strconv.Quote` for paths took 13.1% of
canonize CPU samples. Lazy paths alone account for the allocation drop
(18,853 to 13,913 allocs/op on `records400`). `json:tag` and `json:untag`
build no paths on success, so the change does not apply to them.

### Alternatives considered

A shared generic walker engine (visitor callbacks over an explicit frame
stack) was measured on the same benchmarks. Each value costs three
dictionary-dispatched calls in Go 1.26, and walkers with little work per
value could not absorb that:

| Walker | Engine cost against hand-rolled |
|--------|--------------------------------|
| `json:tag` | +7% to +13% time, +4% allocs |
| `json:untag` | +18% to +24% time |
| Typed encoder | +23% time |
| elpspath validator | +5.6% allocs/op |
| elpspath copy | +24% B/op, +33% time at depth 8 |

The engine's canonize gain came from lazy paths, which the hand-rolled
walker keeps without the engine.

## Code walkers

Code walkers use `lisp.CodeWalker` and the `formKind` registry
(`lisp/codewalk.go`). The `analysis` scope categories and the `lint` formals
occurrences come from the internal source walk event, so neither package
switches on operator names. The event is internal:
`lisp/codewalk_internal_test.go` fails if it appears on the public
`CodeWalker` or `WalkNode`.

The minifier's `firstGlobalFallback` (`minifier/minifier.go`) stays a
hand-rolled syntax walker. On `internal/codewalk.Syntax` it was 121% to 382%
slower.
