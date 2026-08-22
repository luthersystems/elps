// Copyright © 2026 The ELPS authors

//go:build elpscheck

package lisp

import (
	"testing"
)

// Ownership-checker behavior for LEnv.Fork (issue #380).  The fork
// concurrency model is one Runtime per fork; what crosses the runtime
// boundary is exactly the checker's allowlist: singletons and sealed
// values.  These tests pin both directions — the sanctioned sharing does
// not trip the checker, and the checker still catches mutable values
// crossing between a template and its fork.

// TestForkOwnership_SealedSharedAcrossRuntimes evaluates one sealed
// program structure in the template and again in a fork.  The fork shares
// the sealed nodes (pointer-identical, per the fork sharing contract), so
// without the sealed-value allowlist entry the second evaluation would be
// a cross-runtime sighting and panic.  This is the exemption's load-bearing
// test: it goes red if the `v.sealed` clause is removed from
// checkOwnership.
func TestForkOwnership_SealedSharedAcrossRuntimes(t *testing.T) {
	env := newForkTestEnv(t)

	// A defun whose body will live, sealed, in both runtimes.  SealAST is
	// what the parser applies to every parse result; applying it by hand
	// keeps this test parser-free (package lisp cannot import the parser).
	expr := SExpr([]*LVal{
		Symbol("defun"),
		Symbol("double"),
		SExpr([]*LVal{Symbol("n")}),
		SExpr([]*LVal{Symbol("+"), Symbol("n"), Symbol("n")}),
	})
	expr.SealAST()
	if res := env.Eval(expr); res.Type == LError {
		t.Fatalf("template defun: %v", res)
	}
	call := SExpr([]*LVal{Symbol("double"), Int(21)})
	call.SealAST()
	if res := env.Eval(call); res.Type == LError || res.Int != 42 {
		t.Fatalf("template call: %v", res)
	}

	fork, err := env.Fork()
	if err != nil {
		t.Fatalf("fork: %v", err)
	}
	// The fork evaluates through the SAME sealed body nodes (its defun'd
	// function shares the sealed body with the template's).  Must not
	// panic under the checker.
	if res := fork.Eval(call); res.Type == LError || res.Int != 42 {
		t.Fatalf("fork call: %v", res)
	}
	// And the template still works after the fork ran the shared nodes.
	if res := env.Eval(call); res.Type == LError || res.Int != 42 {
		t.Fatalf("template call after fork: %v", res)
	}
}

// TestForkOwnership_MutableStillChecked is the anti-vacuity control for
// the sealed exemption: an UNSEALED value adopted by the template's
// runtime must still trip the checker when it crosses into a fork's
// runtime.  The fork walker never shares such values — this test smuggles
// one across by hand to prove the checker still would catch a fork
// implementation bug that leaked mutable state.
func TestForkOwnership_MutableStillChecked(t *testing.T) {
	env := newForkTestEnv(t)
	v := SExpr([]*LVal{Int(1), Int(2)})
	v.quoted = true
	if lerr := env.Put(Symbol("owned"), v); lerr.Type == LError {
		t.Fatalf("template put: %v", lerr)
	}
	fork, err := env.Fork()
	if err != nil {
		t.Fatalf("fork: %v", err)
	}
	expectOwnershipPanic(t, func() {
		fork.Put(Symbol("smuggled"), v)
	})
}

// TestForkOwnership_ForkServesIndependently pins the distinct-runtime
// model end to end under the checker: template and two forks each run
// their own mutations and reads with no cross-runtime findings.
func TestForkOwnership_ForkServesIndependently(t *testing.T) {
	env := newForkTestEnv(t)
	env.PutGlobal(Symbol("box"), Array(nil, []*LVal{Int(0)}))

	fork1, err := env.Fork()
	if err != nil {
		t.Fatalf("fork1: %v", err)
	}
	fork2, err := env.Fork()
	if err != nil {
		t.Fatalf("fork2: %v", err)
	}
	for _, e := range []*LEnv{env, fork1, fork2} {
		// Each side mints and evaluates its own values freely.
		expr := SExpr([]*LVal{Symbol("lambda"), SExpr([]*LVal{Symbol("x")}), Symbol("x")})
		if res := e.Eval(expr); res.Type != LFun {
			t.Fatalf("lambda: %v", res)
		}
		if lerr := e.Put(Symbol("local"), Int(1)); lerr.Type == LError {
			t.Fatalf("put: %v", lerr)
		}
	}
}
