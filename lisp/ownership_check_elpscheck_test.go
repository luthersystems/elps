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
	v.Quoted = true
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
