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
| A closure (a lambda no global binds) | Refused | Saved with its code and the frames it captured |
| An error value | Refused | Saved with its condition and data |
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
native may not be part of any cycle. Lists and arrays that share storage
are written as views of one storage object. A function is written under its first sorted name, and any of its
package's names for it is accepted on load. No release wrote version 1
under the earlier rules.

`VALUE` uses every typed JSON spelling unchanged, plus these extension tags:

| Tag | Form | Meaning |
|---|---|---|
| `~#obj` | `["~#obj",[ID,X]]` | Defines object `ID` as `X`. |
| `~#ref` | `["~#ref",ID]` | The object `ID`, defined earlier in the document. |
| `~#native` | `["~#native",["NAME",VERSION,PAYLOAD]]` | A native value, saved by the codec registered as `NAME`. |
| `~#fn` | `["~#fn","PKG:NAME"]` | The function bound to the global `PKG:NAME`. |
| `~#lit` | `["~#lit",X]` | `X`, a `~#list` or `~#view`, is a program literal. |
| `~#error` | `["~#error",["CONDITION",[DATA...]]]` | An error value: its condition type and its data. |
| `~#closure` | `["~#closure",["PKG",ENV,CODE]]` | A lambda of package `PKG` with its captured frames and its code. See [Closures](#closures). |
| `~#env` | `["~#env",[ENV,["NAME",VALUE,...]]]` | A captured frame: its parent and its bindings. Only in a closure's `ENV` position. |
| `~#code` | `["~#code",[FORMALS,BODY...]]` | A lambda's code. Only in a closure's `CODE` position. |
| `~#quote` | `["~#quote",NODE]` | A quoted code node. Only inside `~#code`. |
| `~#view` | `["~#view",[STORAGE,OFF,LEN,CAP,[CELL...]]]` | A list header over cells `OFF` to `OFF+LEN` of `STORAGE`, with capacity `CAP`. The cells are those of `OFF` to `OFF+CAP` that no earlier view wrote. |
| `~#cells` | `["~#cells",N]` | Storage of `N` cells. It appears only as `STORAGE`: inline for one vector's data, else as `["~#obj",[ID,["~#cells",N]]]` at its first view and `["~#ref",ID]` after. |
| `~#array` with data | `["~#array",[[DIMS...],DATA]]` | An array whose data list has an identity of its own. `DATA` is a `~#obj` of a list (empty allowed), a `~#ref` to one, or a `~#view`. Any rank. |

The grammar of the view forms:

```text
VIEW    = ["~#view",[STORAGE,OFF,LEN,CAP,[CELL,...]]]   LEN <= CAP, OFF+CAP <= N, CAP > 0
STORAGE = ["~#cells",N]                                  one view, a vector's data, CAP > LEN
        | ["~#obj",[ID,["~#cells",N]]] | ["~#ref",ID]     two or more views
CELL    = VALUE                     each cell of [OFF,OFF+CAP) no earlier view wrote, in order;
                                    null where no view's LEN covers it
ARRAY   = ["~#array",[[DIM,...],DATA]]                   DATA as above, LEN = product of DIMs
```

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
| Vector or array | Its dims header and its data list header together. Two arrays with different dims headers over one data list are two arrays that share the data list. |
| Array data list | Its header, because `append!` replaces the header's cells and every array over that header sees it. A list value that is that header is the same object, even when it is empty. |
| Bytes | Its buffer (`*[]byte`). |
| Nonempty list | Its cells: the address of the first cell and the length. Two headers over the same cells are one list, because `stable-sort` through one is seen through the other. (A list header that is an array's data list is that data list.) |
| Tagged value | Its header. |
| Native | Its payload when the payload is a Go pointer; the type and address when it is a non-nil Go map, channel or unsafe pointer; else its header (so distinct nil maps stay distinct). |

Numbers, strings, symbols, keywords, `()` and functions are not objects. They
are written in full at each occurrence. Two symbols of one spelling are equal,
and a function is restored by name.

#### Views

Lists and array data lists hold their cells in Go slices, and `rest`,
`cdr` and `slice` return new headers over the same cells. `append!` grows a
vector's data in place while its capacity allows. So two values can share
storage without being one value: a write through one (`stable-sort`,
`append!`) is seen through the other. The encoder keeps that sharing:

1. A first walk (discovery) records every holder header it meets: each
   nonempty list and each array data list. It records a header before it
   skips an object it has walked, because a list header can turn out to be
   an array's data list later in the walk. It also saves every native and
   names every function, once.
2. Once every data list is known, each header gets its final identity, and
   holders are grouped by the address range their cells cover. A list
   covers its length. A vector's data covers its capacity, because
   `append!` writes there. Other array data (no vector in the graph uses
   it) covers its length, and its capacity is written as its length:
   nothing can append to it. Holders whose ranges overlap form one group.
3. A group of two or more holders is one storage object. A vector's data
   alone with spare capacity is a storage of its own, written inline.
4. Each holder in the group is a `~#view` of the storage, with its offset,
   length and capacity. A list view's capacity equals its length. A
   vector's data keeps its capacity, so `append!` after a load writes in
   place exactly when it did before the save.
5. Each view writes the cells of its range `[OFF,OFF+CAP)` that no earlier
   view wrote, in offset order, so every cell is written once, by the first
   view the walk reaches that covers it. A cell no holder's length covers is
   dead: nothing can read it before an `append!` writes it, so it is written
   as `null`. Each written cell, dead ones included, counts as one value.

Addresses decide only which holders overlap and their offsets. Both are
properties of the memory layout, not of address order, so the bytes are a
function of the value graph alone. On load the storage is allocated once
and each view becomes a list header over a slice of it.

Each storage cell is a node of the cycle check of its own. A view reaches
the cells of its range, not its whole storage. So a native's payload can
hold a view of the storage that holds the native, as long as the view does
not cover the native's cell (`TestDurableNativeInViewStorage`).

The nesting limit applies to the counting pass and the output, which walk
exactly what is written. Discovery meets a container by another path (it
walks holders' lengths before storage is grouped, so a vector's spare
capacity that another view makes live is met later and deeper), so it is
bounded only by a recursion safety bound, `discoveryMaxDepth` (65,536, or
the nesting limit when higher), and dump and load agree at every nesting
limit (`TestDurableViewsDepthAgreement`).

The work is linear in the cells and views, up to a log factor, in both
directions. Discovery walks each cell address once, however many holders
cover it, and counts it once. A view finds its unclaimed cells through skip
links (a union-find), and reaches the claimed ones through a minimum tree of
their nodes: it takes the claimed cell of smallest node that still reaches
an open node, and drops each cell that no longer does. That one is enough:
if a later cell reaches an open node further out, the earlier cell's open
node encloses the later cell, and already holds its reach. The liveness check is a
union of the views' lengths: a step per view and per dead cell. A list and
all of its tails costs O(n log n), not n²/2. Sealing the atoms of
restored literals (see Literals) seals the union of their ranges once per
storage, so sealed tails cost each cell once (`TestDurableAllTailsLinear`
bounds the claim, liveness and sealing steps from both sides).

An array's data list is written in its own `DATA` form (above) when it is a
view, or when another array or a list value shares it. Otherwise the array
is written exactly as typed JSON writes it. An array inside its own data
list restores: its size is checked once the whole document is read.

##### Capacity

A vector's capacity is part of the language, not of the Go runtime. A new
vector's capacity is its length (`lisp.Array`; `select` and `reject`, and
the `?del!` and `?set!` reworks, set it the same way). `append!` writes in
place while the values fit, and otherwise grows the capacity to
`lisp.GrowCap(c, n)`, `max(2c, n, 4)` for capacity `c` and needed length
`n`, lowered to the runtime's allocation cap but never below `n`. Go's
own `append` growth rounds to allocation size classes, which differ between
32-bit and 64-bit builds and between Go versions, so a capacity it chose
would make the document depend on the machine. A template VM keeps
each value's capacity, so a cold environment, an eager or lazy template VM
and a prewarmed VM write the same bytes (`TestDurableCapacityParity`).

`LoadDurable` rejects a view that is out of its storage's range, has
`LEN > CAP` or `CAP == 0`, has spare capacity but is no vector's data, or
equals another list view of the same storage. It rejects inline storage
that is not one vector's data with spare capacity covering it. It also
rejects storage whose views do not form one run of overlapping ranges from
its first cell to its last, storage with a dead cell that is not `null`, a
storage object that only one view uses, a `~#ref` to storage outside a view,
`~#cells` anywhere but in a view, a shared empty list that is no array's
data, and storage larger than the rest of the input or the value limit can
hold (`ErrTypedLimit`). A shared empty data list counts against the
nesting limit like any container.

#### Literals

A program literal (a quoted list the reader sealed) restores as a literal.
Its list is written as `["~#lit",X]`, where `X` is the list or view, once per
object: inside the `~#obj` of a shared literal, never on a `~#ref`. After a
load, `stable-sort`, `append 'vector` and `slice 'vector` raise
`modify-literal-error` on it exactly as before, and `rest` and `cdr` of it
are literals too. A list built at run time is written without the marker and
stays mutable. An array whose data list is a literal writes that list in
its own form, `["~#lit",...]`.

A restored literal is a protected copy: it is not the program's own cells,
and its identity with the program text is not kept. Only list headers carry
the marker. As each literal is read, the atoms in its cells (ints,
floats, strings, symbols) are sealed as the reader seals them, because
template publication admits a sealed list only when its atoms are sealed
too. A list inside the literal keeps its own marker, so one built at run
time stays mutable (`TestDurableRestoredLiteralsInTemplates`, eager, lazy
and prewarmed VMs). Sealing happens before any codec runs, so a native's
`LoadNative` sees the literals in its payload sealed
(`TestDurableLiteralsSealedBeforeLoadNative`). A literal that is a view
seals only the cells of its storage no literal sealed before (skip links),
so a list and all of its sealed tails seal each cell once. An empty list
never carries the marker. When two headers over the same cells are one
list and only one is sealed, the first the walk meets decides. `LoadDurable` rejects `~#lit`
around anything but a nonempty `~#list` or `~#view` (a scalar, a vector, a
map, a `~#ref`, an `~#obj`, another `~#lit`, or an empty view of a vector's
data). `LoadDurableRoots` takes only a plain root list, as
`DumpDurableRoots` writes it: it rejects a root list that is shared, a
literal or a view.

#### Error values

An error (a condition value) is written as
`["~#error",["CONDITION",[DATA...]]]`. `CONDITION` is its condition type,
the name `handler-bind` matches. `DATA` is its data, each a durable value:
for `(error 'c "message" x)`, the string and `x`. The message renders from
the data, so it is the same after a load, and raising the restored error
reaches the handler for its condition with the same data. An error is an
object: shared, it is written once and referenced after, and it can sit in
a cycle through its own data.

Not saved, because each would make the bytes depend on where the error was
raised or on host state:

- the call stack (`error-stack` in a handler shows the stack of the new
  raise, not the original one);
- the source location (`ErrorVal.Source` reports none);
- the Go error a host error wraps (`errors.Unwrap` and `errors.Is` no
  longer reach it; its text is the message).

An internal panic is never saved: its marker is evidence of a host fault,
and a load must not forge one. `DumpDurable` refuses it, and an error whose
condition is empty or not valid UTF-8. `LoadDurable` rejects the condition
`internal-panic` and an empty condition.

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

A function no global binds is a closure when it is a lambda: an anonymous
lambda, a local `flet` or `labels` function, or a function whose global
name now holds another definition. It is saved with its code and the
frames it captured (see [Closures](#closures)). These are refused:

- a builtin no global binds;
- a macro or special operator, also when a closure captured it (a
  `macrolet` macro).

A restored named function is the current global binding. After a code
upgrade it is the new definition of that name. This is what `defflow`
wants: in-flight runs resume on the current code. A closure bound to a
global is saved by that name too, so it restores as whatever that global
holds when the document loads, and the state it captured is not saved.

### Closures

```text
CLOSURE = ["~#closure",["PKG",ENV,CODE]]
ENV     = null | ["~#env",[ENV,["NAME",VALUE,...]]]      or its ~#obj / ~#ref
CODE    = ["~#code",[FORMALS,BODY...]]                   or its ~#obj / ~#ref
NODE    = INT | FLOAT | STRING | SYMBOL | null | ["~#list",[NODE,...]]
        | ["~#quote",NODE] | ["~#lit",NODE]
```

`PKG` is the package the lambda was defined in. Its body resolves globals
there when it is called, exactly as a function restored by `~#fn` does.

`ENV` is the innermost frame the lambda captured, and each frame names its
parent. `null` is the root environment: its names are globals, resolved
by name in `PKG` at call time.

#### Frames are saved whole

A frame is saved whole: every binding, in name order. Which captured names
a closure's code may reach cannot be decided statically. `eval` reached
through `apply`, a map or a parameter, `funcall` of a symbol, and macros
all reach names the code does not spell, so nothing is dropped, and a
restored closure sees exactly the bindings the original saw. A value in a
frame that cannot be saved refuses the closure, with the path to it:
`durable json: captured variable "held": captured variable "captured":
...`. A flow's transaction context is saved through the codec its host
registers for it, like any native.

The cost is size: a closure saves every binding of every frame it
captured, including bindings its code never reads. A closure created deep
inside a function with many locals saves all of them. Each frame is saved
once however many closures capture it.

Frames with no bindings are left out of the chain. A frame is an object,
so two closures over one frame still share it after a load, and a `set!`
through one is seen by the other. A closure is an object too, so a closure
in its own frame (a `labels` function, recursion) restores.

The dump finds each closure's frames through the chain of environments it
captured, skipping frames with no bindings. It memoizes each
environment's nearest frame for the whole dump, so a long chain of empty
environments is walked once, not once per closure and pass. Each
environment walked for the first time is a step (at most 16 per value of
the value limit), and the dump charges ceil(n/4) units for the n new
environments a closure's walk meets (`TestDurableClosureWorkBounded`).

The dump reads each frame's bindings once. Before it copies a frame's
bindings it reserves them against the value limit, summed over every frame
the dump reads, so a graph of many large frames stops at the first frame
past the limit instead of copying them all
(`TestDurableClosureFramesReservedCumulatively`). Every binding read is
written, so frames add no charge beyond the output's KiB.

#### Code

`CODE` is the formals and the body as code, not as data. Each node keeps
whether it is quoted, which decides whether the evaluator evaluates it: a
call form is `["~#list",[...]]`, `'(1 2)` is
`["~#quote",["~#list",[1,2]]]` and `''x` is `["~#quote",["~#quote","~$x"]]`.
Code holds only scalars, lists, quotes and literals. `["~#lit",NODE]`
marks a sealed program literal: a sealed list or quote outside any other
sealed node. The load seals exactly those nodes and everything in them,
so a literal a macro or a quasiquote put inside new, unsealed code keeps
refusing `stable-sort` (`TestDurableClosureCodeLiterals`). A sealed
literal holding a mutable list is refused. Formals may be a quoted list,
as `lambda` accepts.

Code is an object identified by its cells. Closures restored from one
code object share its cells, so they write one code object again; each
closure `lambda` makes holds its own copy of the form's cells, so each
writes its own (the dump costs what the graph holds). The key has constant
size, and each code object is checked once. A load validates the formals
once per code object (`LEnv.NewLambdaCode`, which records the runtime's
keyword-formal policy; `RestoreLambda` validates again in a runtime with
another one) and restores every closure over it with one copy of its
cells (`LEnv.RestoreLambda`), so 20,000 closures over a 20,000-form body
load and dump again in bounded memory
(`TestDurableClosuresShareRestoredCode`). Code and data restore
as separate objects. A sealed literal in the code and the same list held
as data are two objects after a load (both are sealed, so no write can
tell). A mutable code list that shares cells with anything else the dump
saves (a value, or other code) cannot keep that sharing, so the dump
refuses it: `durable json: a closure's code shares a mutable list with
another value; ...` (`TestDurableClosureCodeSharingRefused`).

A closure keeps the code it was saved with. A named function is the
current definition of its name. So after an upgrade, a restored closure
runs its old body, and any named function its body calls runs the new
one (`TestDurableClosureUpgrade`).

#### Loading

A load evaluates and macroexpands nothing. It builds each frame with
`lisp.NewEnv` and `Put`, chained to the loading environment's root, and
each lambda with `LEnv.RestoreLambda` over code `LEnv.NewLambdaCode`
validated as `lambda` does. A restored closure has a fresh FID and no
source location; the local name `flet` stamped on it is not saved. The
runtime API this needs is new in luthersystems/elps#797: the `LambdaCode`
type, `LEnv.NewLambdaCode` and `LEnv.RestoreLambda`. The dump reads a
closure's captured frame through `internal/funraw`, not an exported
accessor (issue #382).

Whoever can write the stored bytes can make a resumed flow run code of
their choosing: a closure's code runs when it is called. This is no more
than plain ELPS gives the same writer, who could change the program
itself, so a store of durable documents needs the same protection as the
program source.

`LoadDurable` rejects a closure of an unknown package, a frame or code
anywhere but its position, a reference to a frame or code object from
another position, a frame with no bindings or with names out of order or
repeated, a name `Put` refuses, code holding anything but scalars, lists,
quotes and literals, a literal that is not a nonempty list or a quote or
that sits inside another literal, formals that are not a list or that
`lambda` refuses, and a literal directly inside a quote unless the
literal is itself a quote (quoting a sealed node moves its seal onto the
quote, so DumpDurable writes `["~#lit",["~#quote",...]]`). No frame is
checked against the names a closure's code mentions, so the closure checks
do not depend on the loading environment's globals, and a code upgrade
cannot make them fail. Named functions still do: a `~#fn` anywhere in the
document, a captured one included, must name a regular function of its
package when the document loads (see Functions).

## Refused values

| Value | Error |
|---|---|
| Internal panic | `durable json: cannot encode an internal panic` |
| Error whose condition is empty or not UTF-8 | `durable json: cannot encode an error whose condition is empty or not UTF-8` |
| Builtin no global binds | `durable json: cannot encode an anonymous function` |
| Closure whose frame holds a refused value | `durable json: captured variable "NAME": ...` |
| Closure whose code holds a value that is not a scalar, list or quote | `durable json: a closure's code holds a T` |
| Closure whose code shares a mutable list with a value or other code | `durable json: a closure's code shares a mutable list with another value; ...` |
| Closure whose sealed literal holds a mutable list | `durable json: a closure's code holds a mutable list inside a sealed literal` |
| A shared value in the payload of a codec without `WithSharedPayload` | `durable json: native "NAME" payload shares a value, and its codec does not keep sharing` |
| Macro or special operator | `durable json: cannot encode a macro or special operator` |
| Native with no codec | `durable json: no codec registered for native type T` |
| Native that reaches itself or an enclosing object | `durable json: native "NAME" payload refers to a value that encloses the native` |
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
| `DumpDurable` | In first-pass order: each codec's declared charge before its `SaveNative`, ceil(n/4) units before a package of n bindings has its function names read, and ceil(n/4) units for the n environments a closure's frame lookup walks for the first time. Then one unit per started KiB of output as the output grows. The charges are the same on a cold environment, an eager template VM and a lazy template VM (`TestDurableFunctionNamesChargeParity`). Captured frames add no charge of their own: each binding read is written, so the output KiB pay for it. |
| `LoadDurable` | ceil(n/1024) units for n input bytes, before decoding. Then each codec's declared charge before its `LoadNative`, in document order. |

The two schedules differ in what they pay for. A dump does work the
output does not show (it reads the package name tables and calls
`SaveNative`, and walks captured environment chains), so it charges for
that work before doing it. A load's work
is bounded by its input, which it charges up front, plus the codecs'
`LoadNative` calls.

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
| Closures | `durable_closures_test.go`, `lisp/restore_lambda_test.go` | Whole frames (an unread binding is saved; a macro, an `eval` alias and `eval` through `apply` and a map work after a restore; a refused binding refuses with its path), frames reserved cumulatively against the value limit, 20,000 closures over a 20,000-form body in bounded memory both ways, restored closures sharing one copy of their code, the formal-validation policy across runtimes, literals inside macro-built and quasiquoted code, code sharing a mutable list refused, quoted and Go-built formals, a counter pair over one binding (`set!` through one is seen by the other), a recursive `labels` function, a closure over a vector and its view, every kind of formal and quoted code, an upgrade (the closure keeps its code, a named function changes), refusal paths, shared code, limits that agree, and a rejection table. `FuzzDurableJSON` seeds closure documents. |
| Error values | `durable_errors_test.go` | An error raised by `error` round-trips with its condition, message and data, and `handler-bind` matches it after the load; no stack or source; an error with no data; a shared error and an error in a cycle; a host error's text without its Go error; refusals; limits that agree; and a rejection table. `FuzzDurableJSON` seeds error documents. |
| Literals | `durable_literal_test.go` | A literal and its tail restore as literals that refuse `stable-sort`, a run-time list stays mutable, a literal shared by two roots is one object, and malformed markers are rejected. `FuzzDurableJSON` seeds literal documents. |
| Views | `durable_views_test.go`, `lisp/vector_capacity_test.go` | A list with its tail and a middle slice (either order), `cdr`, overlapping vector slices of a dead vector, `append!` in place, a vector holding its own slice (a cycle through a view), views made after a restore, a vector's capacity kept alone and with its data list, normalized capacity of non-vector data, arrays of different dims over one data list, every order of a data list, an alias and the vector, a data list held by a map, an array inside its own data, a shared empty data list, a native holding a view of its own storage, views in a native payload, limits at the exact boundary (dump and load accept the same smallest value limit, for five cells and a chain of 30 tails), the nesting limit of a shared empty data list, linear work for a list and all of its tails, capacity pinned after `append!` and equal across VM kinds, and a canonical-rejection table. `FuzzDurableJSON` also builds overlapping views from its input and checks that writes are shared the same way after a restore. |
| Round-5 regression tests for luthersystems/elps#797 | `durable_internal_test.go`, `durable_review3_test.go` | A linear shape for a 20-level repeated subtype, and shapes that differ by function results, method signatures and an unexported method's package path. |
| Round-4 regression tests for luthersystems/elps#797 | `durable_review3_test.go` | Map keys at the exact byte limit (`{"":0}` and 3,000 random maps of every key kind), complete type shapes (function signatures, interface methods, embedded fields) and a nine-level pointer chain. |
| Round-3 regression tests for luthersystems/elps#797 | `durable_review2_test.go`, `durable_internal_test.go`, `lisp/package_funnames_test.go` | Integer key text, the member scratch bound, charge before the name read, charge parity across VM kinds, function-scope type shapes, the exact `~#fn` reserve and the thawed lazy table. |
| Regression tests for the reviews of luthersystems/elps#797 | `durable_review2_test.go`, `lisp/package_funnames_test.go` | Chained low-links, `"~n"` dimensions, summed key bytes, named types and the fingerprint, nil reference natives, the function-name index (no materialization, rebinding, charge) and the `~#fn` reserve. |
| Regression tests for the reviews of luthersystems/elps#796 | `durable_review_test.go` | Functions of one FID in two packages, alias history, overlapping storage, limits before allocation and codec calls, exact escaped sizes, the fingerprint, reference-kind natives, the version cap, indirect native cycles, payload sharing, dims and pinned value and depth counts. |
| `FuzzDurableJSON` | `durable_fuzz_test.go` | No panic on any input. An accepted input re-encodes to itself, through `LoadDurable` and `LoadDurableRoots`, except for `~#fn` names: the bytes are compared with those names masked, each masked name must resolve to the same function (package and FID) as the name in its place, and the names alone must reach a fixed point. Two decodes charge the same units. A decode under small limits fails with `ErrTypedLimit` or re-encodes under them. |
