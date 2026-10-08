# /elpsvet — elps's Own Go Static Analysis

`cmd/elpsvet` is a `golang.org/x/tools/go/analysis` multichecker over elps's
*own* Go source. It enforces invariants the compiler cannot, and CI runs it on
every PR (`Seal contract (elpsvet)` in `.github/workflows/elps.yml`).

## Trigger

Use when:
- `make elpsvet` (or the CI step) reports a diagnostic;
- you add a native payload (`lisp.Native`, `lisp.NativeOf`, a `.Native`
  write) or a new payload type (it needs a durable codec or a transient
  mark, see "Durable natives");
- you add a package-level table holding `*lisp.LVal` (e.g. a stdlib
  `builtins` slice), write `LVal` fields, or store a `*token.Location`;
- you register a builtin as a method value or closure, or give one package-level state;
- you touch `Package` tables, `packageBase`, `sortedmap.m`, or the lazy template path (`lisp/template_lazy.go`);
- you change an analyzer rule, a tier, or an allowlist;
- a library builtin evaluates code, builds a lambda, reads `Runtime.Package`
  or resolves a symbol it was handed (`elpsownpkg`: take a value, qualify the
  name, or turn it into a macro over core forms).

## The twelve rules

| Analyzer | File | Flags | Suppression marker |
|----------|------|-------|--------------------|
| `elpsownership` | `main.go` | package-level var whose type reaches `*lisp.LVal` (#363) | `//elpsvet:allow <reason>` |
| `elpsfreshness` | `freshness.go`, `alias.go` | `LVal` field write on a value the function did not construct, incl. via a local slice alias (#333/#334, #369/#371) | `//elps:mutates <reason>` |
| `elpsescape` | `escape.go` | runtime-owned `*token.Location` stored uncopied into an escaping value (#375) | `//elps:aliases <reason>` |
| `elpsnativepayload` | `elpsvet/nativepayload/nativepayload.go` (importable) | native payload whose type a template could not publish safely | `//elpsvet:allow-native <≥3-word reason>` |
| `elpsbuiltinstate` | `builtinstate.go` | a builtin that writes state outliving the call: its receiver, a captured variable, or a package-level var (#680, the class behind #678) | `//elpsvet:allow-shared <≥3-word reason>` |
| `elpsfrozenpackage` | `frozenpackage.go` | a write to `Package` tables or a `packageBase` field outside the audited write paths | allowlist row in `packageWriteFunctions` (no marker) |
| `elpslazyread` | `lazyread.go` | a direct read of `Package.symbols`, `Package.baseValues` or `sortedmap.m` that bypasses the lazy-filling accessor | allowlist row in `lazyTableFunctions` (no marker) |
| `elpsownpkg` | `elpsvet/ownpkg/ownpkg.go` (importable) | inside a library builtin (LBuiltin shape, outside package `lisp`): `Eval*`, `Load*`, `Lambda`, `Terminal`, `InPackage`, a `Runtime.Package` read, or a symbol lookup that is not a literal qualified name -- a library builtin runs in its own package (#736) | `//elpsvet:allow-ownpkg <≥3-word reason>` |
| `elpsltypeswitch` | `ltypeswitch.go` | an audited `lisp.LType` switch has a default arm or misses a constant | scope list in the analyzer (no marker) |
| `elpsvalwalker` | `valwalker.go` | a function dispatches on `lisp.LType` and recurs or pushes child values in a loop | in elps: audited `pkgpath.FuncName` row in `valueWalkerFunctions`; in another module: `//elpsvet:allow-valwalker <≥3-word reason>` in the declared function's doc comment (ignored inside elps) |
| `elpsmarkerfields` | `markerfields.go` | a struct carrying `templatepolicy.Marker` whose fields reach a map, slice, pointer, func, chan, interface, `uintptr`, `unsafe.Pointer` or type parameter, through nested structs and arrays; covers types in function bodies, `type T U` and anonymous struct literals (luthersystems/elps#778) | `//elpsvet:allow-marker <≥3-word reason>` on the type doc, or on a field line or the line above (any nesting depth) |
| `elpsdurablenative` | `elpsvet/nativepayload/durable.go` (importable) | a native payload type with no visible package-level `libjson.DurableCodec[T]` value and no transient mark; a type-parameter payload; a pointer or unlisted codec value | a documented `TransientNative()` method on the type; for a type of another module, `//elpsvet:transient <reason>` at one construction |

The header comment of each file is the full design rationale — read it before
changing a rule. `elpsvet/nativepayload/nativepayload.go`'s header is the authority on the payload
tiers; this skill only condenses it.

## Value walker contracts

`lisp.ShapeOf` classifies values. Each walker selects its children and owns its
traversal. Child dispatch uses `ShapeOf` or an exhaustive `LType` switch without
`default`. A new walker needs an audited row in `valueWalkerFunctions`.
State its current traversal contract, including limits, paths and callback order.
Do not use an earlier implementation's benchmark result as the audit reason.

A downstream module that runs `elpsvalwalker` cannot edit elps's table, so it
records the same contract in its own source:

```go
// walk copies a request tree.
//
//elpsvet:allow-valwalker depth bounded by the request decoder's nesting limit
func walk(v *lisp.LVal) { ... }
```

The marker must sit in the doc comment of the declared function the
diagnostic names, with a reason of at least three words. It covers closures
that function owns. A marker in the body, trailing a line or above a closure
does not count, and neither does a prefix match (`allow-valwalkers`) or a
`/* */` comment. Text after ` // ` is not part of the reason; a URL is. Elps
is recognised by module path (`github.com/luthersystems/elps` exactly), so a
separate module under that prefix keeps its markers. Inside
the elps module the marker is ignored, so elps keeps one mechanism: the
table, whose rows `TestValueWalkerAllowlistReasons` classifies and counts.

## Run it

```bash
make elpsvet                     # what CI runs: two passes, see below
go test ./cmd/elpsvet/ ./elpsvet/...  # the analyzers' own tests and fixtures

# one analyzer, one subtree, while iterating:
go run ./cmd/elpsvet -test=false -elpsnativepayload ./lisp/...
GOFLAGS=-tags=elpscheck go run ./cmd/elpsvet -test=false -elpsnativepayload ./lisp/...
```

`make elpsvet` runs `go run ./cmd/elpsvet -test=false ./...` twice, the second
time under `GOFLAGS=-tags=elpscheck`. **elpsvet accepts `-tags` and silently
ignores it** (x/tools registers it as a no-op), so the build tag must travel in
`GOFLAGS`; without that pass every `*_elpscheck.go` file is invisible. The
package list is `./...` — there is no hand-scoped list to maintain.

## Read a diagnostic

Each message names the issue it guards against and ends with the fixes in
order of preference. Fix the code before reaching for a marker:

- **ownership** — construct the value per load. A shared builtin table whose
  formals are sealed by `libutil`/`elpsutil` is the accepted exception; copy the
  existing annotation, e.g. `lisp/lisplib/libbase64/libbase64.go`.
- **freshness** — construct locally or `Copy`/detach first.
- **escape** — store `copyLocation(...)` instead of the pointer.
- **native payload** — see the tiers below. The diagnostic variant tells you
  which case you hit: "not a known-safe value type" (ordinary report), "not
  statically known" (interface/type-parameter payload), "kernel
  representation slot" (an allowlist row used outside its header), "retained
  diagnostic stack" (`*lisp.CallStack`, banned outright by
  `checkDiagnosticPayload`, #629), or "address of LVal.Native taken".

- **builtin state** — make the state per-call (derive it from `env`/`args`),
  or move it onto the VM. Guarding it with a mutex or `sync.Once` does not make
  it per-VM; if sharing across every VM really is intended, justify it with
  `//elpsvet:allow-shared`.
- **frozen package** — route the write through `Package.ensureWritable` (the
  one write gate) or `Package.putSlot`; don't add a new direct write.
- **lazy read** — read through `Package.baseValue`, `Package.symbol` or
  `sortedmap.entry`, or force first with `materializeSymbols`/`forceAll`.

## Builtin shared state (`elpsbuiltinstate`)

A template approves a builtin by identity and shares its Go function value with
every VM it mints; publication never looks at what the function's receiver or
closure captures. #678 was a setter builtin registered as a method value
(`s.SetFooBuiltin`) that wrote `s.foo`, so every VM saw every other VM's writes.

- **What counts as a builtin:** any expression handed to a `lisp.LBuiltin`-typed
  slot — a call parameter (`libutil.Function*`, `elpsutil.Function*`,
  `lisp.Fun`, `FunInPackage`, `Macro*`, `SpecialOp*`, `RegisterDefault*`,
  `libschema.NewValidator*`), a struct field (keyed or positional), a map value
  or slice element, a variable of that type, or a `lisp.LBuiltin(f)` conversion.
  It matches by type, so there's no constructor-name list to drift.
- **What it reads:** a function literal's body (anything declared outside it is
  captured), a same-package method value's body with the receiver tracked, or a
  plain function's body for package-level writes.
- **Reported:** an assignment, `op=`, `++`/`--`, `x = append(x, ...)`,
  `m[k] = v` or `delete` rooted (through fields, indexing and derefs) at the
  receiver, a captured variable or a package-level var. A field write on a
  value receiver lands in the copy and isn't reported; a write through a map,
  slice or pointer reached from it is.
- **Exempt:** writes rooted at the builtin's own parameters and locals,
  `sync/atomic` types, and calls (`atomic.AddInt64(&s.n, 1)`, `s.mu.Lock()`).
- **Not seen, so a clean run is evidence, not proof:** builtins reached
  through a variable or reflection, methods declared in another package,
  helpers the body calls, mutating pointer-method calls (`s.buf.Reset()`),
  writes through a local alias, and callbacks not typed `lisp.LBuiltin` (e.g.
  libschema's internal validators).
- **Marker:** `//elpsvet:allow-shared <≥3-word reason>`, trailing, on the line
  above, or in the analysed method's doc. It's matched so neither
  `elpsvet:allow` nor `allow-native` collides with it.
- The runtime half of #680 (an opt-in shareability contract on
  `TemplateWithBuiltinPolicy`) isn't built. Fixtures:
  `testdata/src/builtinstate`.

## Frozen packages (`elpsfrozenpackage`)

A frozen package (`TemplateWithFrozenPackages`) is shared across VMs until its
first write. Every mutator passes one gate, `Package.ensureWritable`, which
thaws a private copy for that VM; `Package.thaw` is the only function allowed
to build private tables from a base. The one write that doesn't thaw is
rebinding a name the package already has (`set`/`set!` on an existing global,
`Package.putSlot`): it writes only that VM's `baseValues` slot, and a function
value's FID→name entry goes to the VM's `slotFunNames` overlay, which
`GetFunName` reads before the base and `thaw` merges. A new name, a doc, an
export or a `use-package` still thaws.

- The rule confines writes to `Package` tables and every `packageBase` field
  (including replacing `Package.base` or whole pointees) to the justified
  allowlist in `packageWriteFunctions`; `TestPackageWriteAllowlist` pins it.
- It checks field and index assignments, `append`, `delete`/`clear`/`copy`,
  `sort`/`slices` mutations, address-taking, and local map/slice aliases. It
  doesn't follow aliases across calls or aggregate containers, indirect calls,
  reflection or `unsafe`.
- The frozen tables' storage lives in `internal/packagetable`, whose types
  expose only scalar reads, iteration and copies. Checked builds fingerprint
  the base at publication and verify it in `Template.NewVM`.
- Fixtures: `testdata/frozenpackage`.

## Lazy instantiation (`elpslazyread`)

`Template.NewVM` builds only package shells and the root environment. Package
bindings, sorted-map entries and everything they reach are created on first
use, once per VM, memoized by plan index so identity and sharing match the
source, from an explicit work queue rather than recursion
(`lisp/template_lazy.go`). An unmaterialized base slot is nil in `baseValues`;
an unmaterialized thawed binding or sorted-map entry holds the `lazyPending`
marker. Every package gets a plan base under a lazy plan; `unfrozenBase` keeps
`Frozen()` false for the ones not named frozen.

- The rule reports every selector on `Package.symbols`, `Package.baseValues`
  or `sortedmap.m` outside the audited functions in `lazyTableFunctions`: the
  filling accessors (`Package.baseValue`, `Package.symbol`, `sortedmap.entry`),
  the sweeps that force first (`materializeSymbols`, `forceAll`), and writers
  or key/length-only readers. `TestLazyTableAllowlist` pins the list, and a new
  direct reader fails until it's audited.
- A VM is single-goroutine because a read can fill (write) a lazy slot;
  checked builds panic on an overlapping fill (`lazyGuard`).
- `TemplateWithEagerInstantiation` builds every value at `NewVM` instead.
  `VMWithPrewarm` builds, at `NewVM`, every value an earlier VM of the template
  used (a per-template atomic hot set), for hosts that mint VMs off the request
  path.
- A map or package with pending entries retains its whole VM (measured in
  `TestTemplateForkEscapedLeafDoesNotRetainVM`).
- Fixtures: `testdata/lazyread`.

## Native payload tiers (condensed)

Why it exists: a template publishes one value graph and instantiates a VM per
request, but an admitted native payload is **not** rebuilt — one Go value is
shared by every VM the template mints (`lisp/template_plan.go`). The runtime
half of the audit is `(*templateInventory).native` in `lisp/template.go`; this
rule is the static half, run at the construction site, and the tiers
**mirror** runtime admission exactly.

Constructions seen: `lisp.Native(x)`, `lisp.NativeOf[T](x)` (inferred or
explicit), a `lisp.Value(x)` that falls through to Native, a keyed literal
setting the `Native` field (`LVal{...}`, `ErrorVal{...}`), any write to that
field (matched by field object, so conversions and promoted fields count), and
`&v.Native` (always reported). Not seen: indirect calls, multi-value
assignment, positional literals, `reflect`, other modules.

A site is exempt only if one of:

1. **Scalar** — static type's underlying basic kind is in `runtimeScalarKinds`.
   `uintptr` and `unsafe.Pointer` are *not* scalars.
2. **Marked struct value** — a struct **value** (not pointer) embedding
   `internal/templatepolicy.Marker`. `*T` is reported even if `T` is marked
   (needs the embedder's `lisp.TemplateWithNativePolicy` + a site annotation).
   Examples: `libtime.ownedTime`, `libregexp.compiledRegexp`,
   `libschema.validatorTag`. `elpsmarkerfields` checks that its fields
   hold no mutable storage. Only packages under the module path can use it.
3. **Allowlist row** (`allowedPayloadTypes`: `*funData`/`LFun`,
   `*[]byte`/`LBytes`, `*MapData`/`LSortMap`) — only when (a) the site is in
   package `lisp`, (b) it is not a constructor call, and (c) a literal's
   `Type:` key resolves to that package-level `LType` constant. A `.Native`
   field write inside package `lisp` is exempt on (a) alone; everywhere else
   it is reported.
4. **Marker** — `//elpsvet:allow-native <justification>` (below).

Not tiers: `lisp.NativeCloner` (evidence a payload needs cloning, the opposite
of shareable), `time.Time`, `*regexp.Regexp` (wrap them in a marked struct
value instead). An interface-typed payload is reported, not skipped.

## Annotate with `//elpsvet:allow-native`

Prefer making the payload a marked immutable struct value. Annotate only when
the value provably never reaches a template, or when the site *is* a contract
(the constructors, the detach walker, the planner, the error-condition data
arm, libgolang's reflected fields):

```go
v := lisp.Native(h) //elpsvet:allow-native host handle is never published through a template
```

- The justification must be **at least three words**; a bare or shorter marker
  does not suppress.
- Placement: trailing on the reported line; standalone on the line above;
  for a multi-line literal, the opening line or the `Native:` line; or in the
  enclosing function's doc comment.
- One justification covers every construction on its line.
- `//elpsvet:allow` (the ownership marker) does **not** satisfy this rule, and
  vice versa.

## Durable natives (`elpsdurablenative`)

A durable dump (`libjson.DumpDurable`) saves a native only when its registry
has a codec for the payload's Go type. This rule moves a missing codec from
run time to the build. Each native payload type is one of:

| Class | How |
|---|---|
| Durable | A package-level `libjson.DurableCodec[T]` value (not a pointer) of the analysed module, declared in the package or exported by an import. A codec of another module counts only when the module re-declares it (`var C = libtime.DurableTimeCodec.WithName("m:time")`), because its registry lists only its own codecs. Example: `libtime.DurableTimeCodec`. |
| Transient | A `TransientNative()` method declared on the type itself, with a doc comment that says why it is never saved. A value receiver marks `T` and `*T`; a pointer receiver marks only `*T`. A promoted method does not count. Example: `libregexp.compiledRegexp`. |
| Transient at one site | `//elpsvet:transient <reason>`, trailing or on the line after the comment block above the construction. Only for a type declared outside the module (`time.Time`, `string`, `json.RawMessage`). One marker covers exactly one construction. Example: `internal/fuzzval`. |

- Not reported: interface-typed payloads, untyped nil, kernel slots
  (`*[]byte`, `*MapData`, `*funData`), a retained call stack, and in package
  `lisp` a literal of another header or the generic `NativeOf`.
- Reported too: a type-parameter payload, a type that is durable and
  transient, a pointer codec value, an unexported codec with no
  `NewFrozenDurableRegistry` call in its package, and a
  `NewFrozenDurableRegistry` call that leaves out a codec of the module.
- Never change a codec's `Name`, `Version` or payload type: registries of
  embedders pin their `Fingerprint`.
- Fixtures: `elpsvet/nativepayload/testdata` (`durableelps`, the lisp stub,
  and the embedder packages under `example.com/embed`).

## Embedder configuration

`elpsvet/nativepayload` is importable. `New(Config)` and
`NewDurable(DurableConfig)` build the two rules for another module; the zero
values are elps's (`Analyzer`, `DurableAnalyzer`). Embedder fixtures:
`elpsvet/nativepayload/testdata/src/example.com/embed`.

| Setting | Effect |
|---|---|
| `Config.AllowedTypes` | Audited rows, true at every site |
| `Config.ExemptCalls` | A function declaration that calls one is exempt |
| `Config.AllowMarker`, `AllowMinWords`, `Fix` | The module's marker, its minimum justification and the fix text |
| `Config.HideDynamic` | Interface-typed payloads are reported only with `-anypayload` |
| `DurableConfig.Module`, `TransientMarker`, `Registry` | The module path, the site marker and the fix text |

## Change a rule or a tier

A rule and its `analysistest` fixtures can drift away from runtime admission
together and stay green, so a tier change is checked against the runtime:

1. Edit the rule (and its header comment) in `cmd/elpsvet/<rule>.go`, or in
   `elpsvet/nativepayload/` for the two payload rules.
2. Allowlist row added, removed, or moved to another header → update
   `allowedPayloadTypes` **and** the audited inventory in
   `elpsvet/nativepayload/nativepayload_internal_test.go` (each row needs a readable
   justification and its `LType` header; dropped rows must stay dropped).
3. Add fixture cases with `// want` comments:
   - `testdata/src/nativepayload/` — spellings, allowlist, marker placements;
   - `testdata/src/github.com/luthersystems/elps/nativemarker/` — marker tier;
   - `testdata/nativelisp/` — in-kernel (import path is the `lisp` package).
     It is its own testdata root on purpose: the kernel-path stub under
     `testdata/src/github.com/luthersystems/elps/lisp` carries expectations
     for the escape rule, and `analysistest` checks every expectation in a
     package against the one analyzer it runs. Don't add native-payload
     cases to that stub.
4. Add a **paired case** to `TestNativePayloadAnalyzerMirrorsTemplateAdmission`
   (`cmd/elpsvet/nativepayload_runtime_test.go`): the same construction spelled
   in `testdata/src/github.com/luthersystems/elps/nativepaired/` (or
   `testdata/nativepairedkernel/` for in-kernel sites) and published through a
   real `lisp.NewTemplate`; both verdicts must agree. Keep the positive
   controls passing — "tighten until nothing passes" is not a fix.
5. Adding or removing an analyzer → update the `analyzers` slice in `main.go`
   and `TestRegisteredAnalyzers`; `scripts/ci-gates-test.sh` also checks the
   gate.
6. Run `go test ./cmd/elpsvet/ ./elpsvet/... && make elpsvet`, then `/verify`.

## Checklist

- [ ] `make elpsvet` clean (both passes)
- [ ] Every new marker carries a reason a reviewer can audit (≥3 words for `allow-native`, `allow-shared`, `allow-marker` and `allow-valwalker`)
- [ ] New `packageWriteFunctions` / `lazyTableFunctions` rows carry a justification
- [ ] Tier/allowlist change: inventory test, fixtures, and a paired runtime case updated
- [ ] New native payload type: a durable codec or a transient mark
- [ ] `go test ./cmd/elpsvet/ ./elpsvet/...` passes
