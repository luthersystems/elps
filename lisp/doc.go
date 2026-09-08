// Copyright © 2026 The ELPS authors

// Package lisp implements ELPS values, environments, evaluation, and reusable
// templates for constructing independent VMs.
//
// # Loading and evaluating
//
// An [LEnv] is an executable environment. Configure its [Runtime], initialize
// the required packages, and load the program before executing requests. A VM
// must not be evaluated concurrently by multiple goroutines.
//
// # Reusable templates
//
// The supported lifecycle is initial load, [NewTemplate], then many calls to
// [Template.NewVM]. A [Template] is an immutable construction plan, not an
// executable environment. Publication validates and indexes the source graph
// once; NewVM allocates private mutable objects and reconnects their indexed
// references without rediscovering the graph. The Template example demonstrates
// minimal instance isolation.
//
// The source must be quiescent throughout NewTemplate: no evaluation is in
// progress, there are no active call frames or condition handlers, and no
// goroutine or host callback is changing its reachable state. Quiescence is a
// caller-held exclusion requirement, not a lock acquired by NewTemplate. The
// implementation rejects recorded active evaluation state, but that check
// cannot synchronize with a concurrent writer. After publication, the source
// may resume evaluation independently of the plan.
//
// For replayable initialization and equivalent request context and external
// state, separately cold-loaded VMs and template-constructed VMs must produce
// the same values, errors, and effects. Mutation in one VM must not affect the
// source, another VM, or a later instance. Aliases and slice views within a VM
// retain their meaning. Load-time clock, randomness, ledger reads, and effects
// must run per instance when a cold load would observe them per instance;
// NewTemplate cannot infer that requirement or replay those effects.
//
// A saved error with a diagnostic call-stack payload makes publication fail,
// even if the stack is empty; embedders can then cold-load the program instead.
// Ordinary errors can carry these stacks without a debugger attached. The
// boundary concerns diagnostics retained in the state being published, not
// errors generated during later requests: those remain supported and private
// to the instance. A stack-free error value is not rejected on this basis.
// Publication also requires the live Runtime.Stack.GoStack to be nil and rejects
// byte values retaining that field's slice header, even while nil. Otherwise a
// later byte mutation could change how Lisp classifies and catches errors.
// Ordinary unretained request errors do not prevent later publication.
//
// # Ownership and host code
//
// Mutable values, scopes, maps, byte/cell storage, and explicit builtin captures
// are private to each VM. Transitively sealed code and read-only function
// formals/body vectors may be shared. Embedders must not write their exported
// fields or backing storage. Use the supported value operations and copy
// mutable data before changing it.
//
// [TemplateWithBuiltinPolicy] and [TemplateWithNativePolicy] are explicit host
// contracts, not automatic proofs about arbitrary Go closures or native graphs.
// Approve only audited callbacks and transitively immutable native values.
// Request-bound services belong in the new VM's context via [VMWithContext];
// callbacks obtain them from the environment passed to each invocation. Readers,
// source libraries, load caches and diagnostic writers are shared services and
// must satisfy their read-only/concurrency contracts. [VMWithStderr] can install
// a per-instance writer.
//
// Shared Go pointers and slices keep their entire enclosing allocations alive,
// not just the fields or elements accessible through them. Host-provided code
// backing or frozen metadata can therefore indirectly retain a source VM even
// when the reconstructed mutable graph contains no source references. Isolation
// does not imply a garbage-collection guarantee for shared Go allocations.
//
// # Verification and limits
//
// ELPS CI runs elpsvet ownership, freshness/alias and location-escape checks,
// normal and checked-build tests, race detection, and differential fuzzing of
// cold-load parity and isolation. Negative controls deliberately corrupt sharing
// and observations to test the oracles themselves. These checks complement one
// another: elpsvet is not a complete alias analysis, checked builds cannot
// intercept arbitrary Go field stores, and bounded fuzzing is not a proof over
// every program. Consumers must also run the analyzers over their own Go code.
//
// The full design argument, admission rules, sharing contracts, migration guide,
// and executable coverage matrix are in the repository's [template design].
//
// [template design]: https://github.com/luthersystems/elps/blob/main/docs/fork.md
package lisp
