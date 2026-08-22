// Copyright © 2026 The ELPS authors

//go:build elpscheck

package lisp

import (
	"strings"
	"testing"
)

// newOwnershipTestEnv returns a root LEnv with its own private Runtime.
// Each call is a fully separate runtime — exactly the isolation boundary
// the ownership checker enforces.
func newOwnershipTestEnv() *LEnv {
	return NewEnvRuntime(StandardRuntime())
}

// expectOwnershipPanic runs fn and fails the test unless fn panics with an
// ownershipViolation whose message names both runtimes.
func expectOwnershipPanic(t *testing.T, fn func()) {
	t.Helper()
	defer func() {
		t.Helper()
		r := recover()
		if r == nil {
			t.Fatal("expected an ownership-violation panic, got none — the gate cannot fail")
		}
		v, ok := r.(ownershipViolation)
		if !ok {
			t.Fatalf("expected panic value of type ownershipViolation, got %T: %v", r, r)
		}
		if !strings.Contains(v.msg, "ownership violation") ||
			!strings.Contains(v.msg, "owner runtime") ||
			!strings.Contains(v.msg, "second runtime") {
			t.Fatalf("panic message should identify both runtimes; got:\n%s", v.msg)
		}
	}()
	fn()
}

// TestOwnershipCheck_CatchesCrossRuntimePut is the deliberate two-runtime
// test: a value adopted by runtime A is Put into runtime B, and the tag
// must catch it.  Run with:
//
//	go test -tags elpscheck -run TestOwnershipCheck_CatchesCrossRuntimePut ./lisp/
//
// House rule: a gate that has never been seen to fail proves nothing, so
// this test exists to prove the ownership gate CAN fail.
func TestOwnershipCheck_CatchesCrossRuntimePut(t *testing.T) {
	envA := newOwnershipTestEnv()
	envB := newOwnershipTestEnv()

	v := SExpr([]*LVal{Int(1), Int(2), Int(3)})
	v.quoted = true
	if lerr := envA.Put(Symbol("x"), v); lerr.Type == LError {
		t.Fatalf("put in runtime A failed: %v", lerr)
	}

	expectOwnershipPanic(t, func() {
		envB.Put(Symbol("y"), v)
	})
}

// TestOwnershipCheck_CatchesCrossRuntimeEval verifies the eval entry point
// both detects the violation AND that the panic punches through env.eval's
// recover() instead of being converted into a CondInternalPanic LError —
// rethrowOwnershipViolation is what keeps the finding loud.
func TestOwnershipCheck_CatchesCrossRuntimeEval(t *testing.T) {
	envA := newOwnershipTestEnv()
	envB := newOwnershipTestEnv()

	v := Int(42)
	if res := envA.Eval(v); res.Type == LError {
		t.Fatalf("eval in runtime A failed: %v", res)
	}

	expectOwnershipPanic(t, func() {
		envB.Eval(v)
	})
}

// TestOwnershipCheck_SameRuntimeReuse pins the negative case: repeated use
// of one value inside a single runtime — rebinding, re-evaluation, child
// scopes, and the quoted-AST handoff a macro performs — must never trip
// the checker.
func TestOwnershipCheck_SameRuntimeReuse(t *testing.T) {
	env := newOwnershipTestEnv()

	v := Int(7)
	for i := range 3 {
		if res := env.Eval(v); res.Type == LError {
			t.Fatalf("eval %d failed: %v", i, res)
		}
		if lerr := env.Put(Symbol("x"), v); lerr.Type == LError {
			t.Fatalf("put %d failed: %v", i, lerr)
		}
	}
	child := NewEnv(env)
	if lerr := child.Put(Symbol("x"), v); lerr.Type == LError {
		t.Fatalf("put in child scope failed: %v", lerr)
	}
}

// TestOwnershipCheck_SingletonsExempt pins the allowlist: Nil(), Bool(true)
// and Bool(false) are shared across every runtime by design and must never
// be adopted by any of them.
func TestOwnershipCheck_SingletonsExempt(t *testing.T) {
	envA := newOwnershipTestEnv()
	envB := newOwnershipTestEnv()

	for _, v := range []*LVal{Nil(), Bool(true), Bool(false)} {
		if lerr := envA.Put(Symbol("s"), v); lerr.Type == LError {
			t.Fatalf("put in runtime A failed: %v", lerr)
		}
		if lerr := envB.Put(Symbol("s"), v); lerr.Type == LError {
			t.Fatalf("put in runtime B failed: %v", lerr)
		}
		if res := envA.Eval(v); res.Type == LError {
			t.Fatalf("eval in runtime A failed: %v", res)
		}
		if res := envB.Eval(v); res.Type == LError {
			t.Fatalf("eval in runtime B failed: %v", res)
		}
	}
}

// TestOwnershipCheck_ResetForgets pins the documented detection gap: a
// reset drops every adoption, so a cross-runtime reuse that straddles a
// reset is NOT caught.  If this test starts failing, the reset semantics
// changed and the Memory section of the file comment is now lying.
func TestOwnershipCheck_ResetForgets(t *testing.T) {
	envA := newOwnershipTestEnv()
	envB := newOwnershipTestEnv()

	v := Int(9)
	if lerr := envA.Put(Symbol("x"), v); lerr.Type == LError {
		t.Fatalf("put in runtime A failed: %v", lerr)
	}
	resetOwnershipTable()
	// After the reset the table has no memory of envA's adoption, so envB
	// adopts v afresh and no panic occurs.
	if lerr := envB.Put(Symbol("y"), v); lerr.Type == LError {
		t.Fatalf("put in runtime B after reset failed: %v", lerr)
	}
}

// TestOwnershipCheck_SealedNodesExempt pins allowlist entry 2 in both
// directions (issue #372).
//
// The positive direction is the one with consequences: a SEALED node —
// what a parse cache hands to every environment that evaluates the same
// program — must cross runtimes freely.  Before the exemption this panicked
// on the first shared node, which meant an embedder running the supported
// substrate#375/#378 topology could not use checked mode at all.
//
// The negative direction is what keeps the checker worth having: an
// UNSEALED node with otherwise identical shape must still trip.  Copy and
// detach clear the flag on fresh storage, so the values that actually are
// per-runtime storage stay covered.
func TestOwnershipCheck_SealedNodesExempt(t *testing.T) {
	envA := newOwnershipTestEnv()
	envB := newOwnershipTestEnv()

	sealed := QExpr([]*LVal{Int(1), Int(2), Int(3)})
	sealed.SealAST()
	if !sealed.IsSealed() {
		t.Fatal("SealAST did not seal the node; this test would prove nothing")
	}
	if lerr := envA.Put(Symbol("shared"), sealed); lerr.Type == LError {
		t.Fatalf("put sealed node in runtime A: %v", lerr)
	}
	if lerr := envB.Put(Symbol("shared"), sealed); lerr.Type == LError {
		t.Fatalf("put sealed node in runtime B: %v", lerr)
	}
	if res := envA.Eval(sealed); res.Type == LError {
		t.Fatalf("eval sealed node in runtime A: %v", res)
	}
	if res := envB.Eval(sealed); res.Type == LError {
		t.Fatalf("eval sealed node in runtime B: %v", res)
	}
	for _, cell := range sealed.Cells {
		if res := envB.Eval(cell); res.Type == LError {
			t.Fatalf("eval sealed cell in runtime B: %v", res)
		}
	}

	// The control: the same shape UNSEALED must still trip the gate,
	// otherwise the pass above proves only that the checker is off.
	unsealed := QExpr([]*LVal{Int(1), Int(2), Int(3)})
	if lerr := envA.Put(Symbol("private"), unsealed); lerr.Type == LError {
		t.Fatalf("put unsealed node in runtime A: %v", lerr)
	}
	expectOwnershipPanic(t, func() {
		envB.Put(Symbol("stolen"), unsealed)
	})
}

// TestOwnershipCheck_CopyOfSealedIsChecked pins the boundary the exemption
// depends on: Copy() produces FRESH storage with the seal cleared, so a
// copy of a shared program node is ordinary per-runtime storage and is
// covered by the checker again.  If Copy ever started preserving the flag,
// the exemption above would silently widen to cover runtime values.
func TestOwnershipCheck_CopyOfSealedIsChecked(t *testing.T) {
	envA := newOwnershipTestEnv()
	envB := newOwnershipTestEnv()

	sealed := QExpr([]*LVal{Int(4), Int(5)})
	sealed.SealAST()
	cp := sealed.Copy()
	if cp.IsSealed() {
		t.Fatal("Copy preserved the seal flag; the ownership exemption now covers runtime storage")
	}
	if lerr := envA.Put(Symbol("c"), cp); lerr.Type == LError {
		t.Fatalf("put copy in runtime A: %v", lerr)
	}
	expectOwnershipPanic(t, func() {
		envB.Put(Symbol("c"), cp)
	})
}

// TestOwnershipCheck_ClosureFreeBuiltinExempt pins allowlist entry 3 in both
// directions.
//
// The exempt direction is the one libschema.NewValidator's doc comment
// promises: a Go-built function value with no captured environment may be
// bound into any number of runtimes.  The CONTROL is the half that matters,
// because an exemption keyed on "is a function" rather than on "captured
// nothing" would look identical here and would silently cover every ELPS
// lambda -- whose captured *LEnv is the deepest mutable aliasing channel in
// the interpreter.
func TestOwnershipCheck_ClosureFreeBuiltinExempt(t *testing.T) {
	envA := newOwnershipTestEnv()
	envB := newOwnershipTestEnv()

	shared := Fun("shared-builtin", Formals("x"), func(_ *LEnv, args *LVal) *LVal {
		return args.Cells[0]
	})
	if !isClosureFreeBuiltin(shared) {
		t.Fatal("premise: a Fun() value is not recognised as a closure-free builtin")
	}
	if lerr := envA.Put(Symbol("shared"), shared); lerr.Type == LError {
		t.Fatalf("put builtin in runtime A: %v", lerr)
	}
	if lerr := envB.Put(Symbol("shared"), shared); lerr.Type == LError {
		t.Fatalf("put builtin in runtime B: %v", lerr)
	}
	if res := envB.Eval(shared); res.Type == LError {
		t.Fatalf("eval builtin in runtime B: %v", res)
	}

	// CONTROL 1: a function value that DID capture an environment is not
	// exempt, and crossing runtimes with one still trips the gate.  It is
	// built in the shape LEnv.Lambda builds one (which needs a package this
	// bare test runtime has not got), so the only difference from `shared`
	// above is the captured *LEnv -- which is the whole point.
	closure := &LVal{
		Type:   LFun,
		Native: &funData{fid: "closure", env: NewEnv(envA)},
		Cells:  []*LVal{Formals("x"), Symbol("x")},
	}
	if isClosureFreeBuiltin(closure) {
		t.Fatal("a function that captured an environment is being treated as a" +
			" closure-free builtin; the exemption has widened to cover exactly" +
			" what it must not")
	}
	if lerr := envA.Put(Symbol("closure"), closure); lerr.Type == LError {
		t.Fatalf("put closure in runtime A: %v", lerr)
	}
	expectOwnershipPanic(t, func() {
		envB.Put(Symbol("stolen-closure"), closure)
	})

	// CONTROL 2: an ordinary non-function value of the same provenance still
	// trips the gate, so the pass above is not "the checker is off".
	private := QExpr([]*LVal{Int(1)})
	if lerr := envA.Put(Symbol("private"), private); lerr.Type == LError {
		t.Fatalf("put list in runtime A: %v", lerr)
	}
	expectOwnershipPanic(t, func() {
		envB.Put(Symbol("stolen-list"), private)
	})
}
