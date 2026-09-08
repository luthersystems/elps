// Copyright © 2026 The ELPS authors

//go:build elpscheck

package lisp

import (
	"strings"
	"testing"
)

// Declared runtime affinity for native payloads (issue #546).  RuntimeBound
// is production code — it compiles into every build — but its enforcement
// exists only under `-tags elpscheck`, which is why these tests are tagged.
// They pin both directions at both enforcement points: the sanctioned uses
// (same runtime, unbound payload, a clone that drops the binding) must pass
// untouched, and the violations (a foreign use, a bound native shared into
// a fork, a replacer handing one back) must panic.
//
// Conventions follow lisp/fork_ownership_elpscheck_test.go and
// lisp/detach_ownership_elpscheck_test.go: package lisp, environments from
// newForkTestEnv (a private Runtime each), panics captured by recover.

// boundNative is a native payload declaring an affinity to one Runtime.  A
// zero value is UNBOUND (nil Runtime), which is the protocol's "not tied to
// anything yet" answer and disables checking for that instance.
type boundNative struct {
	rt *Runtime
}

func (b *boundNative) BoundRuntime() *Runtime { return b.rt }

// rebindingNative is a bound payload that survives forking the sanctioned
// way: it implements NativeCloner and its clone comes back UNBOUND, ready
// for the destination runtime to claim.  A clone that copied b.rt would
// trip the fork-time check exactly as a shared payload does.
type rebindingNative struct {
	rt *Runtime
}

func (b *rebindingNative) BoundRuntime() *Runtime { return b.rt }

func (b *rebindingNative) CloneNative() interface{} { return &rebindingNative{} }

// expectAffinityPanic runs fn and fails the test unless fn panics with an
// ownershipViolation naming an affinity violation on a payload of the named
// Go type.  The panic type matters as much as the message: affinity
// violations reuse ownershipViolation so rethrowOwnershipViolation keeps
// them hard panics through env.eval's recover().
func expectAffinityPanic(t *testing.T, payloadType string, fn func()) {
	t.Helper()
	defer func() {
		t.Helper()
		r := recover()
		if r == nil {
			t.Fatal("expected a native-affinity panic, got none — the gate cannot fail")
		}
		v, ok := r.(ownershipViolation)
		if !ok {
			t.Fatalf("expected panic value of type ownershipViolation, got %T: %v", r, r)
		}
		if !strings.Contains(v.msg, "native affinity violation") ||
			!strings.Contains(v.msg, "bound runtime") ||
			!strings.Contains(v.msg, "using runtime") {
			t.Fatalf("panic message should identify both runtimes; got:\n%s", v.msg)
		}
		if !strings.Contains(v.msg, payloadType) {
			t.Fatalf("panic message should name the payload type %q; got:\n%s", payloadType, v.msg)
		}
	}()
	fn()
}

// TestRuntimeBound_SameRuntimeUseIsAllowed is the baseline: a payload bound
// to the environment's own Runtime is used through every instrumented point
// (PutGlobal, the eval entry by symbol lookup, and eval of the native value
// itself) and none of them may fire.
func TestRuntimeBound_SameRuntimeUseIsAllowed(t *testing.T) {
	env := newForkTestEnv(t)
	payload := &boundNative{rt: env.Runtime}

	if lerr := env.PutGlobal(Symbol("handle"), Native(payload)); lerr.Type == LError {
		t.Fatalf("put bound native: %v", lerr)
	}
	res := env.Eval(Symbol("handle"))
	if res.Type == LError {
		t.Fatalf("read bound native: %v", res)
	}
	if res.Native != interface{}(payload) {
		t.Fatalf("read back a different payload: %#v", res.Native)
	}
	if out := env.Eval(Native(payload)); out.Type == LError {
		t.Fatalf("eval bound native: %v", out)
	}
}

// TestRuntimeBound_UnboundPayloadCrossesFreely pins the nil answer: an
// unbound payload is not checked at all, so two runtimes may hold it.  Each
// runtime gets its own wrapper LVal — sharing one *LVal would be a genuine
// ownership violation and would prove nothing about affinity.
func TestRuntimeBound_UnboundPayloadCrossesFreely(t *testing.T) {
	env1 := newForkTestEnv(t)
	env2 := newForkTestEnv(t)
	payload := &boundNative{} // unbound: BoundRuntime returns nil

	if lerr := env1.PutGlobal(Symbol("handle"), Native(payload)); lerr.Type == LError {
		t.Fatalf("put in runtime 1: %v", lerr)
	}
	if lerr := env2.PutGlobal(Symbol("handle"), Native(payload)); lerr.Type == LError {
		t.Fatalf("put in runtime 2: %v", lerr)
	}
	if out := env2.Eval(Native(payload)); out.Type == LError {
		t.Fatalf("eval in runtime 2: %v", out)
	}
}

// TestRuntimeBound_ForeignUseAtPutTime is the use-time gate's load-bearing
// test.  The payload is bound to runtime 1 and then bound into runtime 2
// inside a FRESH lisp.Native wrapper: the per-*LVal ownership table has
// never seen that wrapper, so it cannot fire, and the only mechanism left
// that can produce a panic is the payload-level affinity check.
func TestRuntimeBound_ForeignUseAtPutTime(t *testing.T) {
	env1 := newForkTestEnv(t)
	env2 := newForkTestEnv(t)
	payload := &boundNative{rt: env1.Runtime}

	if lerr := env1.PutGlobal(Symbol("handle"), Native(payload)); lerr.Type == LError {
		t.Fatalf("put in the owning runtime: %v", lerr)
	}
	expectAffinityPanic(t, "boundNative", func() {
		env2.PutGlobal(Symbol("handle"), Native(payload))
	})
}

// TestRuntimeBound_ForkRejectsSharedBoundNative is the fork-time gate's
// load-bearing test, and it is deliberately nested: the bound native lives
// inside a sorted map, never at a binding's top level, so no use-time check
// can ever see it (checkOwnership is shallow by design).  Only the fork
// walker's deep visit reaches it.  A fork is a different Runtime, so
// sharing the payload by reference into one is the violation.
func TestRuntimeBound_ForkRejectsSharedBoundNative(t *testing.T) {
	env := newForkTestEnv(t)
	payload := &boundNative{rt: env.Runtime}

	m := SortedMap()
	if lerr := m.MapSet("handle", Native(payload)); lerr.Type == LError {
		t.Fatalf("map-set: %v", lerr)
	}
	if lerr := env.PutGlobal(Symbol("state"), m); lerr.Type == LError {
		t.Fatalf("put state map: %v", lerr)
	}

	if tmpl, err := NewTemplate(env, TemplateWithBuiltinPolicy(func(*LVal) bool { return true })); tmpl != nil || err == nil {
		t.Fatalf("nested runtime-bound native admitted: template=%v error=%v", tmpl, err)
	}
}

// Even a cloner that returns unbound data is not admitted into a Template.
// Create runtime-affine services only after VM instantiation.
func TestRuntimeBound_TemplateRejectsRebindingClone(t *testing.T) {
	env := newForkTestEnv(t)
	payload := &rebindingNative{rt: env.Runtime}
	env.PutGlobal(Symbol("handle"), Native(payload))
	if tmpl, err := NewTemplate(env, TemplateWithBuiltinPolicy(func(*LVal) bool { return true })); tmpl != nil || err == nil {
		t.Fatalf("cloning runtime-bound native admitted: template=%v error=%v", tmpl, err)
	}
	if payload.BoundRuntime() != env.Runtime {
		t.Fatal("rejection changed source affinity")
	}
}

// stickyNative is the bug class the detach-time check exists to catch: its
// CloneNative COPIES the receiver's binding, so every clone stays tethered
// to the runtime it came from.  Fork already rejects such a clone; detach
// must too.
type stickyNative struct {
	rt *Runtime
}

func (b *stickyNative) BoundRuntime() *Runtime { return b.rt }

func (b *stickyNative) CloneNative() interface{} { return &stickyNative{rt: b.rt} }

// TestRuntimeBound_DetachRejectsBindingRetainingClone: a strict detach is a
// cross-runtime transfer, and CloneNative cannot know the destination, so a
// detached clone that retains a binding is a violation however deep the
// native rides — here inside a list, below what any use-time check sees.
func TestRuntimeBound_DetachRejectsBindingRetainingClone(t *testing.T) {
	env := newForkTestEnv(t)
	payload := &stickyNative{rt: env.Runtime}
	orig := QExpr([]*LVal{Native(payload)})

	// Not expectAffinityPanic: a detach violation names only the bound
	// runtime, deliberately — there is no destination runtime to print.
	defer func() {
		r := recover()
		if r == nil {
			t.Fatal("expected a native-affinity panic, got none — the gate cannot fail")
		}
		v, ok := r.(ownershipViolation)
		if !ok {
			t.Fatalf("expected panic value of type ownershipViolation, got %T: %v", r, r)
		}
		if !strings.Contains(v.msg, "detached clone retains a runtime binding") ||
			!strings.Contains(v.msg, "bound runtime") ||
			!strings.Contains(v.msg, "stickyNative") {
			t.Fatalf("panic message should name the retained binding and payload type; got:\n%s", v.msg)
		}
	}()
	if _, err := orig.detach(); err != nil {
		t.Fatalf("detach returned an error instead of panicking: %v", err)
	}
}

// TestRuntimeBound_DetachAcceptsUnbindingClone is the sanctioned shape: a
// clone that arrives unbound detaches cleanly, carrying no tether for the
// destination to trip over.
func TestRuntimeBound_DetachAcceptsUnbindingClone(t *testing.T) {
	env := newForkTestEnv(t)
	payload := &rebindingNative{rt: env.Runtime}

	cp, err := Native(payload).detach()
	if err != nil {
		t.Fatalf("detach: %v", err)
	}
	clone, ok := cp.Native.(*rebindingNative)
	if !ok {
		t.Fatalf("detached payload has type %T, want *rebindingNative", cp.Native)
	}
	if clone == payload {
		t.Fatal("detach shared the template's payload; CloneNative was not honored")
	}
	if clone.BoundRuntime() != nil {
		t.Fatalf("detached clone carries a binding to %p; it must arrive unbound", clone.BoundRuntime())
	}
	if payload.BoundRuntime() != env.Runtime {
		t.Fatal("detach mutated the original payload's binding")
	}
}

// TestRuntimeBound_CopyKeepsBoundCloneUnchecked is the control that scopes
// the detach-time rule to strict detach: the lisp `copy` builtin runs the
// same walker WITHIN one runtime, where a clone keeping the copy's own
// runtime binding is correct, so a binding-copying payload must deep-copy
// without a panic.
func TestRuntimeBound_CopyKeepsBoundCloneUnchecked(t *testing.T) {
	env := newForkTestEnv(t)
	payload := &stickyNative{rt: env.Runtime}

	cp, err := Native(payload).deepCopy()
	if err != nil {
		t.Fatalf("deepCopy: %v", err)
	}
	clone, ok := cp.Native.(*stickyNative)
	if !ok {
		t.Fatalf("copied payload has type %T, want *stickyNative", cp.Native)
	}
	if clone == payload {
		t.Fatal("deepCopy shared the payload; CloneNative was not honored")
	}
	if clone.BoundRuntime() != env.Runtime {
		t.Fatal("the copy's clone lost its binding; within one runtime it should keep it")
	}
}

// An immutable approval is a trusted assertion, not an affinity override.
// Checked builds still reject a falsely approved runtime-bound payload.
func TestRuntimeBound_TemplateChecksApprovedNativeAffinity(t *testing.T) {
	env := newForkTestEnv(t)
	payload := &boundNative{}
	env.PutGlobal(Symbol("handle"), Native(payload))
	tmpl, err := NewTemplate(env, TemplateWithBuiltinPolicy(func(*LVal) bool { return true }),
		TemplateWithNativePolicy(func(value any) bool { return value == payload }))
	if err != nil {
		t.Fatal(err)
	}
	if _, err := tmpl.NewVM(); err != nil {
		t.Fatalf("unbound control: %v", err)
	}
	payload.rt = env.Runtime // deliberately break the host's immutability assertion
	expectAffinityPanic(t, "boundNative", func() { _, _ = tmpl.NewVM() })
}
