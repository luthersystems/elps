# Immutable VM templates

Load a program once, publish an immutable construction plan with
`lisp.NewTemplate`, and instantiate independent VMs with `Template.NewVM`.
A template is not an environment: it cannot evaluate, expose bindings, or
accumulate request state. The source environment may resume evaluation after
publication without changing the plan.

For standard-library initialization, use public `lisplib.LoadRuntimeLibrary`
before publication. `lisplib.LoadLibrary` intentionally also installs the
mutable testing registry; load that registry separately in each VM if needed.
The runtime loader does not approve arbitrary host callbacks or natives produced
by application initialization: explicit admission policies still apply.

`regexp:regexp-compile` is an audited exception: its native payload privately
owns a freshly compiled regexp, exposes no pointer or mutation methods, and is
safe to share without a native policy. Compilation occurs once, not per VM.
Lisp regexp predicates, pattern access, string/byte matching, and JSON encoding
are unchanged. Text marshaling returns fresh pattern bytes, never owned storage.
The Go payload type (also shown in native-value diagnostics) is now private;
Go embedders must not assert Lisp-produced payloads to `*regexp.Regexp`.
Host-supplied raw regexps remain accepted by the Lisp operations but require
explicit policy for template sharing. Wrapping a borrowed pointer would not
make it immutable, so no such constructor is provided.

`libtime.Time` likewise creates a private immutable wall-clock payload. It
copies the initialized Go timezone object on input; `libtime.Get` returns a
fresh timezone object on output. The private transition tables are read-only
after initialization and may be shared, preserving timezone names, offsets,
DST and calendar rules without copying them per VM. Tests pin the audited Go
time/zone layouts and public methods, and exercise aliases, lazy local-zone
initialization and calendar behavior. A future Go implementation still requires
review: shape checks alone cannot prove that its methods remain read-only.

`Time` and `Get` keep their Go signatures, but no longer expose a raw native
`time.Time` payload or preserve timezone pointer identity. Values have wall-clock
semantics: Go monotonic clock metadata is discarded, as it already is by Lisp's
UTC clock and RFC3339 parsers. JSON/text encoding is preserved. A caller must
not mutate the input timezone concurrently with capture; unsafe access to
private fields or rebinding Go's process-global timezone objects is outside
this ownership contract. Raw host `time.Time` natives remain usable by time
operations but still need explicit policy for template sharing. This does not
make request-dependent clock reads safe to snapshot during initialization.

## The invariant

For replayable initialization, two cold-loaded VMs and two template instances,
given identical requests, external initial state and per-request context, must
produce the same values, errors and external effects. Mutation in one VM must
not affect another VM or a later instance. Aliases *inside* a VM retain their
meaning, including aliases between different headers and overlapping slices.

This is an observational guarantee, not pointer identity across runtimes.
Independent VMs have distinct runtime identities. Request accounting, active
stacks, observers and evaluation context start fresh. Bind context and host
services per VM. A load that reads transaction time, randomness or ledger state
cannot simply be snapshotted and reused: run that initialization per instance.

## Construction model

The source is **quiescent** when no evaluation is running, its active call and
condition-handler stacks are empty, and no goroutine or host callback is
mutating its reachable state. That must remain true for the entire `NewTemplate`
call. The caller supplies this exclusion; checking evaluation depth and stacks
is an admission guard, not a lock or synchronization with another goroutine.

Publication validates a quiescent source and compiles its graph into private
descriptors. Mutable object references become indices. Slice views name an
indexed backing object plus offset, length and capacity. The plan owns copies
of mutable data and package metadata; source environments and runtime state
become descriptors. Immutable program code and approved host values may be shared.

This is a mutable-graph ownership guarantee, not an absolute garbage-collection
guarantee. A shared Go pointer or slice retains its enclosing allocation,
including inaccessible backing elements beyond a slice's capacity. Host-provided
code backing, frozen metadata, or shared services can consequently keep the source
VM alive indirectly. No accessible cross-VM mutation was demonstrated by the
synthetic retention cases in issue #630; eliminating this retention would require
owning shared syntax and metadata backing as well, not a zero-capacity special case.

Admission assigns the private object and environment indices. Compilation
consumes that same closed index, not a second discovery of the source graph.
A missing admitted identity, shared-code record or mutable-storage record is
an error; compilation cannot silently authorize a new edge or share its storage.

Instantiation allocates each mutable object once, then fills its indexed
references. There is no recursive graph discovery, per-instance object-identity
lookup table, native clone callback, or foreign map factory.

| State | Treatment |
| --- | --- |
| Transitively sealed code, read-only function formals/body vectors containing only sealed code, and the three singletons | Shared |
| Mutable value headers, lexical scopes and explicit builtin captures | Fresh objects; indexed references preserve aliases and cycles |
| List/array cells and byte storage | Fresh per backing group; exact overlapping views and capacity preserved |
| Stock and JSON maps | Interpreter-owned storage; key policies, backing aliases and cycles preserved |
| Mutable package metadata | Owned at publication, independently copied per instance |
| Retained diagnostic call-stack payloads, including empty stacks | Rejected at publication; the consumer cold-loads instead |
| Nonnil live `Runtime.Stack.GoStack`, or a retained pointer to its byte-slice header even while nil | Rejected before sharing policies; these can affect Lisp error handling |
| Function definition locations | Preserved as program metadata |
| Native scalars and explicitly immutable payloads | Shared, never cloned |
| Other natives and foreign map implementations | Rejected at publication |
| Limits and identifier counters | Copied; counters continue past published definitions |
| Reader, source library, sealed load cache, source metadata | Shared under their read-only/concurrency contracts |
| Context, current evaluation location, active stacks, step accounting, debugger and profiler | Not inherited |
| Stderr | Shared unless replaced with `VMWithStderr` |

Saved diagnostic stacks are not debugger-only state. Ordinary Lisp errors
capture call frames, and recovered Go panics additionally capture a Go trace.
The rejection applies only when a stack remains reachable from the state being
published (issue #629), not merely because an error occurred earlier. Errors
without stack payloads remain admissible; errors created after `NewVM` work
normally and do not modify or disable the template. Native approval policies
cannot override this known mutable diagnostic category.

The live runtime's `GoStack` must remain nil at publication. Recovered panics
populate an error's stack copy, not this live field. Host-created byte values
retaining the live field's slice header are rejected even when it is nil:
`append-bytes!` through that alias can otherwise change whether `ignore-errors`
catches an `internal-panic` condition. This is a Lisp-visible parity failure,
not merely debugger presentation. The rejection leaves the source unchanged;
ordinary unretained request errors do not prevent later publication.

Objects are allocated individually. A single escaped scalar must not keep
unrelated functions, scopes or byte buffers alive. Whole-VM allocation blocks
were tested and rejected because they violated this lifetime expectation.

Sealing remains an internal protection for shared program code. It is not
another VM construction mode. A sealed parent reaching a mutable child, a
forged seal on an unsupported type, or mutable storage overlapping shared
program storage is rejected. Slice overlap discovery uses numeric address
arithmetic only at publication and assumes the current Go runtime's non-moving
heap. Source slices retain their allocations throughout discovery. Numeric
addresses are never converted back into pointers or dereferenced; memory access
uses ordinary Go slices, and the plan retains only slices and integer offsets.

## Why this preserves the invariant

The argument is a graph reconstruction argument, not a formal verification of
the interpreter or arbitrary Go host code.

1. Admission establishes a closed graph: each edge points to an indexed mutable
   object, transitively immutable code, or explicitly approved immutable host
   state. Opaque mutable host graphs are not admitted.
2. Each mutable identity gets exactly one fresh destination per instance.
   Different value headers and shared underlying storage are indexed separately.
3. Every edge and view is reconstructed from the same descriptor. This preserves
   sharing, non-sharing, cycles, map behavior and slice bounds within each VM.
4. No mutable destination is shared with the source, plan or another instance.
   Consequently an interpreter mutation cannot cross the instance boundary.
5. With equivalent initial state and the same per-call host behavior, evaluation
   follows the same operations and observations as the corresponding cold VM.

The tests exercise both sides: independent cold loads plus literal expected
results, and direct identity/alias checks with adversarial cycles, overlapping
views, source mutations and concurrent instances. Neither a matching checksum
nor absence of races alone proves behavioral equivalence.

## Go embedding

The public lifecycle and sharing contract also live in the `lisp` package docs
(`lisp/doc.go`), with an executable `ExampleTemplate` checked by the Go test suite.
This document contains the fuller design argument and coverage matrix.

```go
// Source loading has finished and no goroutine is evaluating source.
tmpl, err := lisp.NewTemplate(source,
    lisp.TemplateWithBuiltinPolicy(approveAuditedBuiltin),
    lisp.TemplateWithNativePolicy(approveForeignImmutableValue),
)
if err != nil {
    return err // or use a safe per-VM initialization path
}

vm, err := tmpl.NewVM(lisp.VMWithContext(requestContext))
if err != nil {
    return err
}
// Install runtime-affine services here, then execute the request.
```

Do not approve callbacks by package name or Go function code address: neither
identifies what a closure captures. Approve registrations whose Go state you
have audited. Embedders attach mutable host state to each instance's context;
callbacks obtain it from the environment passed to them.

ELPS cannot inspect hidden Go closure or method-receiver state. An approve-all
predicate or census of every registration is an attestation about trusted host
code, not an automated audit. A registration's identity stops an unapproved
replacement from being admitted but does not prove that the approved callback
has no mutable captures. Ownership analyzers and review of every changed host
registration remain necessary; neither amounts to a whole-program proof.

ELPS's own libraries use an internal construction API for explicit Lisp captures:

```go
// Available only inside the ELPS repository (internal/funraw).
fn := funraw.NewCapturedBuiltin(funraw.CapturedBuiltin{
    Formals: lisp.Formals(),
    Captures: state,
    Eval: func(env *lisp.LEnv, args, captures *lisp.LVal) *lisp.LVal {
        return captures
    },
    Package: "example",
    FID: "state",
})
```

The callback must not also hide mutable VM state in its Go closure.
ELPS's internal native marker and the public `TemplateWithNativePolicy` assert transitive
immutability, including aliases retained by host code. They do not authorize
cloning mutable payloads. An approved immutable payload is shared unchanged
even if its type happens to implement `NativeCloner`.

The marker interface lives in `internal/templatepolicy`; downstream callers
approve their native payloads through `TemplateWithNativePolicy` instead.
Automatic marker admission is restricted to audited struct values. A pointer
inherits its value's marker methods, but Go can replace the whole pointee even
when every field is private. Pointer forms therefore require explicit policy;
they are not automatically admitted. The library's schema credential is a
zero-state value, and owned time/regexp payloads also use this value-only path.

Go host code remains trusted: a false immutability assertion or direct writes
to exported fields of shared sealed values or read-only function code vectors
can violate the contract. Function headers and lexical scopes remain private;
their formals/body vectors are immutable after construction. This is
not a sandbox for hostile Go plugins. Checked builds provide additional
ownership and runtime-affinity assertions, not a proof of arbitrary host code.

This warning is backed by several distinct CI checks, not one proof-producing
analyzer. `elpsvet` diagnoses direct LVal/whole-value writes and local aliases of
formals/body backing storage; `cmd/elpsvet/testdata/src/a/template_contract.go`
requires those diagnostics. Its documented intraprocedural blind spots include
slice parameters, aliases stored in existing structs, and shallow-copied headers.
Checked builds verify sealed-content fingerprints at guarded boundaries, while
isolation/parity tests and fuzzing exercise actual VM behavior. A clean static
pass does not establish that arbitrary host callbacks obey the sharing contract.
Consumers must run the tool over their own sources as well as run behavior tests.

Inside this repository the native census is mechanical: `elpsvet`'s
`elpsnativepayload` rule reports every native construction — `lisp.Native`,
`lisp.NativeOf`, a `lisp.Value` falling through, an `LVal{Native: x}` literal,
a `.Native` write — whose payload type publication would not admit. It mirrors
`(*templateInventory).native`: a struct VALUE carrying the internal marker
passes, an actual scalar passes, and everything else is reported until a human
either marks it, has the embedder approve it with `TemplateWithNativePolicy`,
or writes a justified `//elpsvet:allow-native` at the site saying why the
payload never reaches a template. A `NativeCloner` method is not an exemption,
for the reason above: an approved payload is shared unchanged and the clone
hook is never called. The rule audits elps's own sources only; an embedder's
payloads are the embedder's census to take (substrate runs the same rule over
its tree). See CLAUDE.md, "Go static analysis over elps's own sources".

A callback obtains request state from `env.Context()`; it must not fall back
to a captured load-time context. Shared caches, readers, writers and other
host services must obey their declared concurrency contracts and must not
smuggle source-VM state into future calls.

ELPS's template-based test runners use `internal/stdlib.Load(env, false)` during
bootstrap and load `libtesting` separately into each instance before loading
test definitions. A mutable test suite, including an empty one, is not
template state. `lisplib.LoadLibrary` still includes testing for ordinary
cold-loaded test/documentation environments.

## Breaking changes

- `LEnv.Fork` is removed. Publish once with `NewTemplate`, then reuse
  `Template.NewVM`. Recompiling the plan for each request defeats amortization.
- `ForkWithNativeReplacer` is removed. Create mutable native services per VM.
- `NativeCloner` no longer grants template admission and is never called by
  template construction or instantiation. Its within-value-copy contract is
  separate from VM construction.
- Foreign `Map` implementations cannot be published. JSON maps retain their
  existing string-only behavior through private interpreter-owned storage.
  The `libjson.SortedMap` alias is removed; `Serializer.GoMap` now returns an
  ordinary `map[string]any`. Decoder construction is repository-internal.
- The PoC's `TemplateMap` extension/factory protocol is removed.
- `LEnv.Copy`, an unused shallow environment-copy API, is removed. Lisp
  `copy` and `LVal.Copy` are value operations, not VM construction APIs.

Unsupported initialization is an explicit error, not silently changed
semantics. A preheater can instantiate a context-free base template and load
the remaining program per VM. If even bootstrap cannot meet admission, it
must use an ordinary independent cold load.

## Verification and performance

```sh
go test -tags elpscheck -race ./lisp ./lisp/lisplib ./elpstest \
  ./lisp/lisplib/libschema ./lisp/lisplib/libjson ./lisp/lisplib/libtesting \
  -run 'Test(Template|Fork|CapturedBuiltin|SchemaMutableCapture|JSONTemplate)' -count=1
go test ./lisp/lisplib -run '^$' -bench '^BenchmarkTemplateConstruction$' \
  -benchmem -benchtime=500ms -count=6
```

Publication is intentionally separate from steady-state construction in
benchmarks. Compare identical program/library sets, include publication
amortization, and measure escaped-value retention as well as allocation churn.
The full repository suite, race/checked configurations, fuzzing and benchmark
regression gates run in CI. Historical experiments and the public Substrate
sample harness are recorded in [the experiment archive](template-poc/README.md).

### Differential coverage and its limits

`elpstest.FuzzForkParity` compares multiple independent cold loads with instances
of one published plan, across lazy, interleaved and concurrent schedules. Each
instance executes multiple transactions. A bounded independent model checks
results, deliberate errors, state observations and host effects; the cold arm is
not the sole source of expected behavior. Generated graph edges vary aliasing
and cycles, with write-through and rewiring operations. Fixed controls verify
that these inputs change real graph structure rather than just scalar values.

`FuzzTemplateCancellationParity` varies nested definitions and cancellation
points, preserving exact diagnostic provenance as well as successful results.
Neither target skips a generated supported case when cold loading, publication,
instantiation or evaluation fails. Deliberately broken construction and state
comparison controls exercise the same runner used by fuzzing.

The independent isolation oracle observes mutable headers, actual full-capacity
cell/byte storage, map backing, lexical scopes and explicit builtin captures.
Sealed code is compared by content, with bounded DAG traversal, not parse-cache
interning identity. User strings and error messages are never normalized as
generated function identifiers. Native contents require a host-supplied
`RenderNative` observation in both result and reachable-state channels; direct
reference identity cannot reveal mutable references hidden inside arbitrary Go
structs or closures.

| Earlier PR | Relevant retained or equivalent controls |
| --- | --- |
| [#599](https://github.com/luthersystems/elps/pull/599), [#601](https://github.com/luthersystems/elps/pull/601) | Alias/isolation parity, bounded graph variation, multiple VMs and transactions, creation schedules and definition-location cancellation |
| [#602](https://github.com/luthersystems/elps/pull/602), [#616](https://github.com/luthersystems/elps/pull/616) | Empty-vector/view behavior, native sort/insert behavior and original minimized regressions |
| [#603](https://github.com/luthersystems/elps/pull/603) | Real shared/private/mixed load caches, late loads, exact hit controls and one/two publication hops |
| [#614](https://github.com/luthersystems/elps/pull/614), [#617](https://github.com/luthersystems/elps/pull/617) | Sealed-content/provenance fingerprints, DAG/cycle budgets, mutable descendants and independent oracle visibility controls |

Private cell-view layout assertions are replaced by their surviving behavioral
properties. An old expectation that function-definition locations disappear is
intentionally not retained: independent cold-run cancellation demonstrates that
those locations must be preserved. This work does **not** wholly subsume
[#604](https://github.com/luthersystems/elps/pull/604)'s proposed within-value Go
`LVal.Copy` semantics or [#605](https://github.com/luthersystems/elps/pull/605)'s
repository-wide native-constructor auditor. Those are distinct contracts.

Tracking: [#622](https://github.com/luthersystems/elps/issues/622),
[error-location parity](https://github.com/luthersystems/elps/issues/624),
[equivalent regression and fuzz coverage](https://github.com/luthersystems/elps/issues/625).
