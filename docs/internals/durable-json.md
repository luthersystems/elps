# Durable typed JSON

Durable typed JSON saves a value graph and restores it with the same sharing.
It is an opt-in extension of [canonical typed JSON](typed-json.md). The Go API
is `libjson.DumpDurableRoots` / `LoadDurableRoots` for a set of named values,
and `DumpDurable` / `LoadDurable` for one value, in
`lisp/lisplib/libjson/durable.go`, `durable_decode.go` and
`durable_registry.go`. There is no Lisp builtin. The first user is substrate's `defflow`
(luthersystems/substrate#683), which saves the live locals of a flow at an
await and restores them in a later transaction.

The default typed JSON does not change. `DumpTyped`, `LoadTyped` and the
`json:` builtins write and read the same bytes as before.
`TestTypedGoldenCorpus` pins them, and `TestDurableExtendsTyped` checks that a
durable document of a tree holds the typed bytes of that tree unchanged.

## What durable mode adds

| Need | Typed JSON | Durable JSON |
|---|---|---|
| Two names for one map, list, vector, array, bytes or tagged value | Written twice. Restored as two values. | Written once plus a back-reference. Restored as one value. |
| A value that contains itself | Refused | Saved and restored |
| Native values | Refused | Saved through a registered `NativeCodec` |
| A named global function | Refused | Saved as its package-qualified name |
| Format version | None (closed tag set) | A frozen version number at the top |

## Requirements and how they are met

| Requirement | How |
|---|---|
| One encoding per value graph | Fixed root order, fixed traversal order, ids in first-visit order. See [Determinism](#determinism). |
| Identity of mutable values, never pointer order | Pointers are map keys for identity only. Nothing is sorted or iterated by pointer. See [Objects and identity](#objects-and-identity). |
| Full reference checks on decode | No forward reference, no dangling id, no unreferenced definition, no reference as an object's own value. A reference into an unfinished container is supported (a cycle of containers). A native payload may not reach an unfinished object, directly or through finished ones. See [Canonical decoding](#canonical-decoding). |
| Same bytes whatever the codec registration order | The registry is looked up by type and by name only. `TestDurableRegistryFrozen` dumps through two registries built in opposite orders. |
| Bounded before materialization | Byte, value and depth limits are checked as the walk goes, capped by the runtime's allocation cap. See [Limits and charges](#limits-and-charges). |
| Deterministic charges, codecs included | Output or input KiB plus each codec's declared charge, in walk order. See [Limits and charges](#limits-and-charges). |
| Codec contract | See [Codec contract](#codec-contract). |
| Registry frozen and identical across peers | `Freeze`, `Fingerprint`. See [Registry](#registry). |
| Old native versions stay readable | Every payload carries its version. See [Codec contract](#codec-contract). |
| No map-iteration or pointer nondeterminism | Go maps in the encoder and decoder are lookups only. `Fingerprint` sorts. Map members use typed JSON's canonical order. |
| Decoder fuzzed for panics and limits | `FuzzDurableJSON`. See [Tests](#tests). |

## Roots

`DumpDurableRoots` takes the saved values as an ordered slice of
`DurableRoot{Name, Value}`. It writes them as one list of alternating names
and values, so sharing between roots is kept:

```text
["~#durable",[1,["~#list",["order",["~#obj",[0,{}]],"alias",["~#ref",0],"n",3]]]]
```

- The slice order is the write order, and it decides the ids. Pass the roots
  in a fixed order, for example sorted by name. `defflow` passes its saved
  names in the order its compiler fixes for each label.
- Names are nonempty UTF-8 strings and distinct.
- No roots is `null`.
- `LoadDurableRoots` returns the roots in the same order. It rejects a value
  that is not such a list, a duplicate or non-string name, and a root list
  that is itself a shared object.

## Encoding

A durable document is one JSON array:

```text
["~#durable",[1,VALUE]]
```

`1` is the format version. It is frozen. A decoder rejects any other number.
A change that reinterprets existing text needs a new version number.

Version 1 was tightened before its first release (luthersystems/elps#797).
Payload sharing is refused unless a codec registers `WithSharedPayload`. A
native may not be part of any cycle. Overlapping lists and arrays are
refused. A function is written under its first sorted name, and any of its
package's names for it is accepted on load. No release wrote version 1
under the earlier rules.

`VALUE` uses every typed JSON spelling unchanged, plus four extension tags:

| Tag | Form | Meaning |
|---|---|---|
| `~#obj` | `["~#obj",[ID,X]]` | Defines object `ID` as `X`. |
| `~#ref` | `["~#ref",ID]` | The object `ID`, defined earlier in the document. |
| `~#native` | `["~#native",["NAME",VERSION,PAYLOAD]]` | A native value, saved by the codec registered as `NAME`. |
| `~#fn` | `["~#fn","PKG:NAME"]` | The function bound to the global `PKG:NAME`. |

`X` is the typed spelling of a list, vector, array, sorted map, tagged value
or bytes value, or a `~#native` form. `PAYLOAD` is a durable `VALUE`, so it can
hold `~#obj`, `~#ref`, `~#fn` and other natives. A `~#ref` can point at an
object defined anywhere earlier in the document, including inside a payload.

Example. Two locals `a` and `b` hold one map, and the map holds itself:

```text
["~#durable",[1,["~#list",[["~#obj",[0,{"self":["~#ref",0]}]],["~#ref",0]]]]]
```

### Objects and identity

An object is a value whose identity Lisp code can observe through mutation or
`eq?`-like sharing. The encoder tracks these kinds:

| Kind | Identity |
|---|---|
| Sorted map | Its map storage (`*lisp.MapData`). Two headers over one map are one object. |
| Vector or array | Its data list header (`Cells[1]`). The dims header (`Cells[0]`) must be the same one too. |
| Bytes | Its buffer (`*[]byte`). |
| Nonempty list | Its cells: the address of the first cell and the length. Two headers over the same cells are one list, because `stable-sort` through one is seen through the other. |
| Tagged value | Its header. |
| Native | Its payload when the payload is a Go pointer; the type and address when it is a non-nil Go map, channel or unsafe pointer; else its header (so distinct nil maps stay distinct). |

Numbers, strings, symbols, keywords, `()` and functions are not objects. They
are written in full at each occurrence. Two symbols of one spelling are equal,
and a function is restored by name.

Two objects whose cells overlap without being the same cells are refused:
a list and its tail (`rest`, `cdr`, a `slice` of the list), a `slice` of a
vector, or a vector's data list held as a list. A write through one is seen
through the other, and the format cannot express a view. Copy one of them
before saving. The check is a yes or no over the address ranges of every
list and array data, so its answer does not depend on address order.

Restored values are fresh and mutable. A saved program literal, which raises
`modify-literal-error` on `stable-sort`, restores as an ordinary list that
sorts (`TestDurableRestoresLiteralsMutable`). A flow that relies on that
error must copy the literal itself.

### Determinism

The same value graph gives the same bytes on every machine and every peer.

1. The root is fixed: one value, or the roots in the caller's order.
2. The traversal is depth-first: a list's cells and an array's cells in
   order, a sorted map's values in typed JSON's member order (UTF-8 order of
   the member text), a tagged value's data, then a native's payload.
3. A first pass counts the references to each object, in the order of the
   second pass.
4. An object with two or more references (a cycle counts) gets an id at its
   first visit. Ids start at 0 and follow first-visit order.
5. Every later visit writes a `~#ref` to that id.
6. An object with one reference is written inline with no `~#obj`, exactly as
   typed JSON writes it.

Identity is the identity of the mutable value (see the table above).
Pointers serve only as keys of Go maps that answer "seen before?". No order
is ever taken from a pointer or from Go map iteration, so document order is a
function of the value graph alone.

### Canonical decoding

`LoadDurable` accepts only what `DumpDurable` writes. It rejects:

- every input that typed JSON rejects inside `VALUE`;
- a missing `["~#durable",[1,` header, another version, or trailing bytes;
- an `~#obj` whose id is not the next id, or whose `X` is a scalar, string,
  `null`, `~#ref`, `~#fn` or another `~#obj`;
- an `~#obj` with no `~#ref` to it;
- a `~#ref` to an id not yet defined;
- a `~#ref` to the object being defined (`["~#obj",[0,["~#ref",0]]]`);
- a native payload that reaches an object still being restored, directly or
  through finished objects (see [Self-reference](#self-reference));
- an `~#obj` or `~#ref` inside the payload of a codec that does not keep
  sharing (see [Codec contract](#codec-contract));
- an unknown native name, a version outside `1` to the registered version, or
  a codec result whose Go type is not the registered type;
- a `~#fn` name that is unbound, not a regular function, or bound to a
  function of another package;
- an array dimension that is not an integer: a JSON integer, or a `"~n"`
  string past 2^53. Dimensions are read on the scalar path, so no tag and no
  codec runs there.

Two inputs are accepted that re-encode to other bytes:

- A `~#fn` may name the function by any name its package binds it to. The
  encoder writes the first name in sorted order. Documents stay readable when
  a package adds or drops an alias.
- A native payload is canonical only when the codec is canonical: when
  `SaveNative(LoadNative(p))` gives `p` again. Each codec owns that property.
A document with an older native version loads, and a dump of the result
writes the current version, so its bytes change. `FuzzDurableJSON` checks the
canonical property with strict codecs.

## Native values

An embedder registers one `NativeCodec` per Go type, under a stable name and
a version, and then freezes the registry:

```go
type NativeCodec interface {
	SaveNative(env *lisp.LEnv, v *lisp.LVal) (*lisp.LVal, error)
	LoadNative(env *lisp.LEnv, version int, payload *lisp.LVal) (*lisp.LVal, error)
}

reg := libjson.NewDurableRegistry()
err := libjson.RegisterNative[*bptree.Handle](reg, "substrate:bptree", 1, bptreeCodec{},
	libjson.WithNativeCharge(10))
reg.Freeze()
```

- The registry matches a native by the Go type of `LVal.Native`. A type
  defined outside elps works the same way as one defined inside it.
- `SaveNative` returns a payload. The payload is any value durable JSON can
  write, for example a list of `cc:bptree-open` arguments.
- `LoadNative` receives the version the document holds. It rebuilds the
  native and can reject a version it no longer reads.
- `DumpDurable` writes the registered version. Bump the version when the
  payload shape changes. Keep reading the old versions in `LoadNative`.
- `SaveNative` runs once per native object in one dump, during the first pass.
- `LoadNative` must return a new native on each call. Two natives that share
  one pointer payload are one object, so a dump writes them as one.
- A native with no registered codec is refused. The error names its Go type.

### Codec contract

A codec must follow these rules. Peers that save or restore one value must
agree byte for byte, and a codec runs inside a transaction.

| Rule | Detail |
|---|---|
| Deterministic | The result depends only on the arguments: no clock, random source, Go map iteration order or pointer value. |
| No transaction context | No ledger read or write and no context value the arguments do not carry. A B+-tree codec saves the tree's open arguments and reopens it in `LoadNative`; it reads no tree content. |
| No result-changing cache | `LoadNative` returns a new native on every call. A pointer payload of size zero can share one address with another, so it would be taken for one object; use a type with a field. |
| Graph identity | By default a codec rebuilds its payload, so it cannot keep sharing. elps then refuses a payload that holds a shared object, in either direction. A codec whose `SaveNative` returns the exact payload values `LoadNative` received registers with `WithSharedPayload()`, and its payload may share. |
| Declared charge | `WithNativeCharge(units)` declares the fixed cost of one call. It is charged before every `SaveNative` and `LoadNative` call, through the dump's or load's charge function. Work that grows with the input past that is charged by the codec through `env.ChargeSteps`. A failed charge is returned as the error. |
| Bounded output | The payload counts against the byte, value and depth limits like any value, so a codec cannot write past them. |
| Versioned | `LoadNative` gets the document's version and reads every version from 1 to the registered one. Change the payload shape only with a new version, and keep the old reader. A version is at most 2^53, so it fits a JSON number. |
| Identity-capable type | `Register` refuses a func, slice or interface type: such a payload has no identity. It also refuses an unnamed type other than a pointer to a named one (`chan int`, `struct{...}`), so the fingerprint's qualified name always identifies the type. Value types (structs, numbers) are identified by their LVal header, so they must hold no shared mutable state; use a pointer type for one that does. |

A codec that breaks a rule can make peers disagree. elps cannot check the
rules at run time. It checks what it can: the result type, the version range,
the limits and the charges.

### Registry

- Register every codec when the environment is built, then call `Freeze`.
- `Register` on a frozen registry fails. `DumpDurable` and `LoadDurable`
  refuse a registry that is not frozen.
- Registration order does not change a byte of output.
- `Fingerprint` returns a JSON array of
  `{"name","type","shape","version","charge","shared"}` objects sorted by
  name. The type is the package-qualified name of a named type, with one
  `*` per pointer level. The shape is its complete structure: kind, channel
  direction, array length, map key and element, struct fields (name,
  package path, embedding, tag and type), function parameters, results and
  variadic flag, and interface methods (name, package path and signature).
  A repeated unnamed component is written once and then as `#n`, so the
  shape is linear in the number of distinct types and has no cutoff. A
  named component appears by its qualified name only. Two types declared
  inside different functions of one Go package can share a name, so a
  collision remains possible when two function-scope types, or the named
  components they contain, share qualified names: `C struct{ V Leaf }` with
  a local `Leaf int` in one function and `Leaf string` in another gives one
  fingerprint. Declare codec types, and the types they contain, at package
  level. Peers compare it to confirm they hold the
  same registry.
- A frozen registry is read-only and safe for concurrent use.

### Self-reference

A native cannot be part of a cycle. The encoder refuses a payload that
reaches its own native, or any object that encloses the native, directly or
through objects that are already finished. Each walk records, for every
finished object, the outermost unfinished object it reached (Tarjan's
low-link), and a reference to a finished object follows those links through
every finished object on the way (with path compression) to the outermost
object that is still unfinished. So the check sees through any chain of
finished objects. The decoder
refuses the same shapes. So `LoadNative` always receives a fully restored
payload. Cycles of containers alone are allowed.

## Functions

A function value is saved by name when its defining package binds it to a
global. Its identity is its package and its `FID`; an `FID` is unique within
its package, and builtins of one short name in two packages (`lisp:not`,
`s:not`) share an `FID`. The encoder writes `PKG:NAME`, where `PKG` is the
function's package and `NAME` is the first name, in sorted
`Package.SymbolNames` order, under which `PKG` binds a regular function with
the same package and `FID`. Binding history does not change the name.
`LoadDurable` accepts any `PKG:NAME` whose current global is a regular
function of package `PKG`.

The encoder reads each package's names once per dump through
`Package.FunNamesByFID`. It visits every binding in any order and keeps the
smallest name per FID, so it neither sorts nor allocates per binding. It
reads a binding a lazy template has not built from the template's plan, so
it builds no value. The read is charged ceil(n/4) units for a package of n
bindings (`Package.NumBindings`), before the read starts, so a step budget
or a cancelled context stops it. The work and the charge scale with the
package's size, not with the saved graph. One unit pays for four bindings:
one elps step costs about 175 ns, and reading one binding costs 22 ns on a
cold environment and up to 100 ns on a template VM.
`BenchmarkDurableFunctionName` dumps one function from a package of 6000
bindings:

| Environment | Before (sort, materialize) | After (`FunNamesByFID`) |
|---|---|---|
| Cold env | 1.6 ms, 109 KB | 0.13 ms, 3.5 KB |
| Eager template VM | 1.9 ms, 108 KB | 0.64 ms, 1.7 KB |
| Lazy template VM | 15.8 ms, 5.87 MB, 72,612 allocs | 0.51 ms, 1.7 KB, 16 allocs |

This belongs in the codec, not in a native hook. A B+-tree handle's payload
then holds its `:compare` function as an ordinary value, and every native
codec gets the same rule for free.

These are refused:

- an anonymous lambda, and a local `flet` or `labels` function: no global
  name holds them;
- a function that no global of its package binds;
- a macro or special operator.

A restored function is the current global binding. After a code upgrade it is
the new definition of that name. This is what `defflow` wants: in-flight runs
resume on the current code. A closure bound to a global restores as whatever
that global holds when the document loads; the state it captured is not
saved.

## Refused values

| Value | Error |
|---|---|
| Error (condition) value | `durable json: cannot encode an error` |
| Anonymous or local function, or one no global of its package binds | `durable json: cannot encode an anonymous function` |
| Lists or arrays whose cells overlap | `durable json: two values share storage (a list and its tail, or a slice of a vector); copy one of them before saving` |
| A shared value in the payload of a codec without `WithSharedPayload` | `durable json: native "NAME" payload shares a value, and its codec does not keep sharing` |
| Macro or special operator | `durable json: cannot encode a macro or special operator` |
| Native with no codec | `durable json: no codec registered for native type T` |
| Native that reaches itself or an enclosing object | `durable json: native "NAME" payload refers to a value that encloses the native` |
| Two arrays over one data list with different dims | `durable json: two arrays share data with different dimensions` |
| Nested quote, empty symbol, invalid UTF-8, malformed value | the typed JSON error |

## Limits and charges

Both directions use typed JSON's options and defaults. The byte and value
limits are lowered to the environment's per-operation allocation cap
(`Runtime.MaxAllocBytes`), and no option raises them past it. Every limit is
checked during the walk, before the next value is built or written, so no
intermediate grows past a limit:

| Option | Default | Counts |
|---|---|---|
| `WithTypedMaxDepth` | 1024 | Container nesting. `~#obj` adds no level. A native payload adds one. |
| `WithTypedMaxBytes` | 16 MiB | Output bytes, or input bytes. Strings, symbols, keys and names are checked at their exact escaped size before they are written. During the first pass the key bytes of every map are summed (each key as its exact encoded text, Transit prefix and `~i` digits included, plus three bytes), and that exact length is checked before the key is copied, so a document of exactly the limit is written. A map of n members is refused before its members are collected when 4n bytes would pass the limit. |
| `WithTypedMaxValues` | 2^20 | Every value position, map key and array dimension. `~#obj` counts one plus its object. `~#ref` and `~#fn` count one. The first pass counts a map's keys before it copies the map's members, and no codec runs past the limit. `DumpDurableRoots` checks the root count before it allocates. |
| `WithTypedCharge` | none | See the charge order below. |

The first pass is bounded by the same depth and value limits. Every object is
walked once, so a small graph with much sharing stays small. The decoder
reserves nothing from a count the input declares.

Charges are a function of the value graph (dump) or the input bytes (load),
so every peer charges the same units in the same order:

| Call | Charges, in order |
|---|---|
| `DumpDurable` | In first-pass order: each codec's declared charge before its `SaveNative`, and ceil(n/4) units before a package of n bindings has its function names read. Then one unit per started KiB of output as the output grows. The charges are the same on a cold environment, an eager template VM and a lazy template VM (`TestDurableFunctionNamesChargeParity`). |
| `LoadDurable` | ceil(n/1024) units for n input bytes, before decoding. Then each codec's declared charge before its `LoadNative`, in document order. |

A charge error stops the call and is returned wrapped.

A limit error wraps `ErrTypedLimit`.

## Tests

| Test | File | Pins |
|---|---|---|
| `TestDurableGoldenCorpus` | `typedgolden/durable_test.go` | The bytes of every corpus entry in `typedgolden/testdata/durable.txt`. |
| `TestDurableExtendsTyped` | `typedgolden/durable_test.go` | A tree's durable document holds its typed bytes unchanged. |
| `TestDurable*`, `TestLoadDurableRejects` | `durable_test.go` | Aliasing, cycles, natives, functions, refusals and limits that agree in both directions. |
| `TestDurableLeavesTypedUnchanged` | `durable_test.go` | `DumpTyped` and `json:dump-string :typed true` still write shared values in full and refuse cycles. |
| `TestDurableRegistryFrozen`, `TestDurableNativeCharge`, `TestDurableAllocationCap`, `TestDurableRoots` | `durable_test.go` | Freezing, registration order, charges, the allocation cap and roots. |
| Round-5 regression tests for luthersystems/elps#797 | `durable_internal_test.go`, `durable_review3_test.go` | A linear shape for a 20-level repeated subtype, and shapes that differ by function results, method signatures and an unexported method's package path. |
| Round-4 regression tests for luthersystems/elps#797 | `durable_review3_test.go` | Map keys at the exact byte limit (`{"":0}` and 3,000 random maps of every key kind), complete type shapes (function signatures, interface methods, embedded fields) and a nine-level pointer chain. |
| Round-3 regression tests for luthersystems/elps#797 | `durable_review2_test.go`, `durable_internal_test.go`, `lisp/package_funnames_test.go` | Integer key text, the member scratch bound, charge before the name read, charge parity across VM kinds, function-scope type shapes, the exact `~#fn` reserve and the thawed lazy table. |
| Regression tests for the reviews of luthersystems/elps#797 | `durable_review2_test.go`, `lisp/package_funnames_test.go` | Chained low-links, `"~n"` dimensions, summed key bytes, named types and the fingerprint, nil reference natives, the function-name index (no materialization, rebinding, charge) and the `~#fn` reserve. |
| Regression tests for the reviews of luthersystems/elps#796 | `durable_review_test.go` | Functions of one FID in two packages, alias history, overlapping storage, limits before allocation and codec calls, exact escaped sizes, the fingerprint, reference-kind natives, the version cap, indirect native cycles, payload sharing, dims, mutable literals and pinned value and depth counts. |
| `FuzzDurableJSON` | `durable_fuzz_test.go` | No panic on any input. An accepted input re-encodes to itself, through `LoadDurable` and `LoadDurableRoots`, except for `~#fn` names: the bytes are compared with those names masked, each masked name must resolve to the same function (package and FID) as the name in its place, and the names alone must reach a fixed point. Two decodes charge the same units. A decode under small limits fails with `ErrTypedLimit` or re-encodes under them. |
