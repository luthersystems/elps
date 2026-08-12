// Copyright © 2026 The ELPS authors

package lisp

import (
	"testing"

	"github.com/luthersystems/elps/parser/token"
)

// Unit tests for the canonical sealed-AST fingerprint (lisp/sealfp.go).
// The three verification tools — the fuzz corruption oracle, the
// checked-mode inspector, and the watchdog's red-proof tests — all stand
// on the properties pinned here: the fingerprint moves when sealed bytes
// move, stays put when they are restored, ignores unsealed (runtime)
// storage, and terminates deterministically on adversarial graphs.
//
// Deliberate sealed-node mutation in this file happens under
// pauseSealWatchdog (the tree could be sampled by the -race watchdog in
// elpscheck builds) and every mutation is restored before the test ends
// (the tree is recorded by the checked-mode inspector at SealAST time,
// and end-of-suite verification must still hold).

// sealedFixture builds and seals a small tree with a known shape:
// (fixture 7 3.5 "lit" (inner qsym)) with a source location on the inner
// symbol.
func sealedFixture(t *testing.T) *LVal {
	t.Helper()
	inner := Symbol("qsym")
	loc := &token.Location{File: "fixture.lisp", Path: "fixture.lisp", Pos: 21, Line: 2, Col: 3}
	inner.SetSource(loc)
	root := SExpr([]*LVal{
		Symbol("fixture"),
		Int(7),
		Float(3.5),
		String("lit"),
		SExpr([]*LVal{Symbol("inner"), inner}),
	})
	root.SealAST()
	if !root.IsSealed() || !inner.IsSealed() {
		t.Fatal("anti-vacuity: fixture did not seal")
	}
	return root
}

func fingerprintOne(v *LVal) uint64 {
	return SealedASTFingerprint([]*LVal{v})
}

// TestSealedASTFingerprintSensitivity mutates each covered field class of
// a sealed node in turn and asserts the digest moves, then restores the
// bytes and asserts the digest returns to its original value.
func TestSealedASTFingerprintSensitivity(t *testing.T) {
	root := sealedFixture(t)
	fp0 := fingerprintOne(root)
	if fp0 != fingerprintOne(root) {
		t.Fatal("fingerprint is not deterministic on an untouched tree")
	}

	resume := pauseSealWatchdog()
	defer resume()

	mutations := []struct {
		name    string
		mutate  func()
		restore func()
	}{
		{
			name:    "scalar Str",
			mutate:  func() { root.Cells[0].Str = "corrupted" }, //elps:mutates -- deliberate, paused, restored
			restore: func() { root.Cells[0].Str = "fixture" },   //elps:mutates -- restore
		},
		{
			name:    "scalar Int",
			mutate:  func() { root.Cells[1].Int = 8 }, //elps:mutates -- deliberate, paused, restored
			restore: func() { root.Cells[1].Int = 7 }, //elps:mutates -- restore
		},
		{
			name:    "quote flag",
			mutate:  func() { root.Cells[3].quoted = true },  //elps:mutates -- deliberate, paused, restored
			restore: func() { root.Cells[3].quoted = false }, //elps:mutates -- restore
		},
		{
			name: "cells swap",
			mutate: func() {
				root.Cells[0], root.Cells[1] = root.Cells[1], root.Cells[0] //elps:mutates -- deliberate, paused, restored
			},
			restore: func() {
				root.Cells[0], root.Cells[1] = root.Cells[1], root.Cells[0] //elps:mutates -- restore
			},
		},
	}
	for _, m := range mutations {
		m.mutate()
		if fp := fingerprintOne(root); fp == fp0 {
			t.Errorf("%s: fingerprint did not move on a sealed-byte mutation", m.name)
		}
		m.restore()
		if fp := fingerprintOne(root); fp != fp0 {
			t.Errorf("%s: fingerprint did not return after restoring the bytes", m.name)
		}
	}
}

// TestSealedASTFingerprintSourceMetadata pins the .source decision
// documented in lisp/sealfp.go: location CONTENTS are covered (issue
// #370's class changes them), pointer identity is not — an equal-content
// pointer swap is invisible here by design and is the watchdog's to
// catch.
func TestSealedASTFingerprintSourceMetadata(t *testing.T) {
	root := sealedFixture(t)
	inner := root.Cells[4].Cells[1]
	origLoc := SourceLocForTest(inner)
	if origLoc == nil {
		t.Fatal("anti-vacuity: fixture's inner symbol has no source")
	}
	fp0 := fingerprintOne(root)

	resume := pauseSealWatchdog()
	defer resume()

	// Content change: a #370-shaped restamp to a different call site.
	inner.source = &token.Location{File: "elsewhere.lisp", Pos: 99, Line: 9, Col: 9} //elps:mutates -- deliberate, paused, restored
	if fp := fingerprintOne(root); fp == fp0 {
		t.Error("fingerprint did not move when a sealed node's source location was restamped")
	}

	// Clearing the location entirely must also move the digest.
	inner.source = nil //elps:mutates -- deliberate, paused, restored
	if fp := fingerprintOne(root); fp == fp0 {
		t.Error("fingerprint did not move when a sealed node's source location was cleared")
	}

	// Equal-content pointer swap: same bytes, different allocation.  The
	// fingerprint must NOT move — this is the documented split with the
	// -race watchdog, which sees the write itself.
	clone := *origLoc
	inner.source = &clone //elps:mutates -- deliberate, paused, restored below
	if fp := fingerprintOne(root); fp != fp0 {
		t.Error("fingerprint moved on an equal-content source pointer swap; it must hash contents, not identity")
	}

	inner.source = origLoc //elps:mutates -- restore
	if fp := fingerprintOne(root); fp != fp0 {
		t.Fatal("fingerprint did not return after restoring the original source pointer")
	}
}

// TestSealedASTFingerprintIgnoresRuntimeHoles pins the hole semantics: a
// runtime-only value (here LBytes) under a sealed parent is not sealed by
// SealAST, contributes only a hole marker, and its contents may change
// freely without moving the digest.
func TestSealedASTFingerprintIgnoresRuntimeHoles(t *testing.T) {
	payload := Bytes([]byte{1, 2, 3})
	root := SExpr([]*LVal{Symbol("holder"), payload})
	root.SealAST()
	if !root.IsSealed() {
		t.Fatal("anti-vacuity: root did not seal")
	}
	if payload.IsSealed() {
		t.Fatal("anti-vacuity: SealAST sealed a runtime-only LBytes value")
	}
	fp0 := fingerprintOne(root)
	payload.Bytes()[0] = 200
	if fp := fingerprintOne(root); fp != fp0 {
		t.Error("fingerprint moved when unsealed runtime storage under a sealed parent changed")
	}
}

// TestSealedASTFingerprintCycleTerminates pins the termination bounds: an
// embedder can seal a hand-built cyclic graph (SealAST's already-sealed
// check terminates the seal walk), and fingerprinting it must terminate
// deterministically rather than recurse forever.
func TestSealedASTFingerprintCycleTerminates(t *testing.T) {
	a := SExpr([]*LVal{Symbol("a")})
	b := SExpr([]*LVal{Symbol("b"), a})
	a.Cells = append(a.Cells, b) // cycle a -> b -> a, built before sealing
	a.SealAST()
	if !a.IsSealed() || !b.IsSealed() {
		t.Fatal("anti-vacuity: cyclic graph did not seal")
	}
	fp1 := fingerprintOne(a)
	fp2 := fingerprintOne(a)
	if fp1 != fp2 {
		t.Fatal("truncated fingerprint of a cyclic graph is not deterministic")
	}
}
