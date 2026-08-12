// Copyright © 2026 The ELPS authors

package lisp

import (
	"testing"
)

// These tests pin the singleton half of the seal design (issue #376): the
// three process-wide singletons are born sealed, IsSealed communicates the
// do-not-mutate contract to embedders, and the copy-on-write mutation-site
// guards treat a singleton exactly like a shared program literal when a
// container op reaches one (stable-sort of a Nil result, append/slice
// 'vector over an empty sequence).  Every lisp-level case re-verifies the
// singleton bytes afterwards — the ops must succeed by copying, never by
// touching singleton storage.

func TestSingletonsBornSealed(t *testing.T) {
	for name, v := range map[string]*LVal{
		"Nil()":       Nil(),
		"Bool(true)":  Bool(true),
		"Bool(false)": Bool(false),
	} {
		if !v.IsSealed() {
			t.Errorf("%s: IsSealed() = false, want true (issue #376: singletons are born sealed)", name)
		}
	}
	// The sanctioned mutable spelling of the empty list is unaffected.
	if SExpr(nil).IsSealed() {
		t.Error("SExpr(nil): fresh empty list reports sealed; runtime storage must stay mutable")
	}
	// Copy-then-mutate remains the sanctioned pattern: the copy's fresh
	// storage is unsealed and writable without corrupting the singleton.
	snap := TakeSingletonSnapshot()
	cp := Nil().Copy()
	if cp.IsSealed() {
		t.Fatal("Copy() of Nil() reports sealed; copy-on-write depends on fresh storage being mutable")
	}
	if cp == singletonNil {
		t.Fatal("Copy() of Nil() returned the singleton itself")
	}
	cp.Cells = append(cp.Cells, Int(1))
	cp.quoted = true
	if drift := snap.Verify(); drift != "" {
		t.Fatalf("mutating a Copy() of Nil() drifted singleton %s", drift)
	}
}

// TestSingletonSetSourceNoOp pins the metadata edge: SetSource on a sealed
// value is a no-op, and the singletons are sealed, so no embedder can
// stamp a location onto shared singleton storage.
func TestSingletonSetSourceNoOp(t *testing.T) {
	snap := TakeSingletonSnapshot()
	loc := nativeLocation()
	Nil().SetSource(&loc)
	Bool(true).SetSource(&loc)
	Bool(false).SetSource(&loc)
	if drift := snap.Verify(); drift != "" {
		t.Fatalf("SetSource wrote to singleton %s; sealed values must ignore SetSource", drift)
	}
}
