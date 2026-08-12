// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
)

// TestSingletonCoWContainerOps drives the container-op edge shapes through
// the copy-on-write guards (lisp/seal.go) with a singleton operand.
// (rest '(x)) and (cdr '(x)) return the Nil() singleton, which is born
// sealed (issue #376), so each op below receives sealed singleton storage
// and must succeed by copying — never by writing the shared bytes.  Every
// case re-verifies the singleton snapshot afterwards.
func TestSingletonCoWContainerOps(t *testing.T) {
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	if rc := lisp.InitializeUserEnv(env); rc.Type == lisp.LError {
		t.Fatalf("InitializeUserEnv: %v", rc)
	}
	snap := lisp.TakeSingletonSnapshot()
	for _, tc := range []struct {
		src  string
		want string
	}{
		// stable-sort of the Nil singleton: the CoW guard sorts a fresh
		// copy and returns it (empty either way, but the guard path runs).
		{`(stable-sort < (rest '(1)))`, `()`},
		{`(stable-sort > (cdr '(1)))`, `()`},
		// append with the Nil singleton as base sequence, both output
		// types; 'vector is the spare-capacity write path (#369 shape).
		{`(append 'list (rest '(1)) 1 2)`, `'(1 2)`},
		{`(append 'vector (rest '(1)) 1 2)`, `(vector 1 2)`},
		// slice over the Nil singleton, both output types; 'vector must
		// hand back mutable storage, never a window onto the singleton.
		{`(slice 'list (rest '(1)) 0 0)`, `'()`},
		{`(slice 'vector (rest '(1)) 0 0)`, `(vector)`},
		// A vector sliced out of the singleton must be independently
		// mutable: append! writes its backing in place.
		{`(let ([v (slice 'vector (rest '(1)) 0 0)]) (append! v 9) v)`, `(vector 9)`},
		// reverse over the singleton allocates fresh output already;
		// included to pin the edge.
		{`(reverse 'list (rest '(1)))`, `'()`},
	} {
		got := env.LoadString("singleton-cow.lisp", tc.src)
		if got.Type == lisp.LError {
			t.Fatalf("eval %q: %v", tc.src, got)
		}
		want := env.LoadString("singleton-cow-want.lisp", tc.want)
		if want.Type == lisp.LError {
			t.Fatalf("eval %q: %v", tc.want, want)
		}
		if got.String() != want.String() {
			t.Errorf("%s = %s, want %s", tc.src, got, want)
		}
		if drift := snap.Verify(); drift != "" {
			t.Fatalf("%s drifted singleton %s", tc.src, drift)
		}
	}

	// The identity claim behind the lisp cases: stable-sort's CoW guard
	// returns fresh storage, not the singleton, so downstream code holding
	// the result cannot write singleton memory.
	res := env.LoadString("singleton-cow-id.lisp", `(stable-sort < (rest '(1)))`)
	if res.Type == lisp.LError {
		t.Fatalf("stable-sort: %v", res)
	}
	if res == lisp.Nil() {
		t.Fatal("stable-sort over Nil() returned the singleton itself; the CoW guard must return a fresh copy")
	}
	if res.IsSealed() {
		t.Fatal("stable-sort over Nil() returned sealed storage; the CoW copy must be mutable")
	}
	if drift := snap.Verify(); drift != "" {
		t.Fatalf("identity checks drifted singleton %s", drift)
	}
}
