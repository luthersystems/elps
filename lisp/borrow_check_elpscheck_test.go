// Copyright © 2026 The ELPS authors

//go:build elpscheck

package lisp

import (
	"strings"
	"testing"
)

// TestBorrowCheckFiresOnRawMint is the red-proof for the mint-time
// provenance detector.  It reproduces exactly what rangePath.Get did
// before #392 — take a live subslice of a sealed value's cells and hand it
// to a raw constructor — and asserts the detector reports it.  The second
// half is the anti-vacuity control: the SAME subslice through the borrow
// constructor is silent, so the detector is reporting the missing
// constraint and not merely the sharing.
func TestBorrowCheckFiresOnRawMint(t *testing.T) {
	lit := QExpr([]*LVal{Int(10), Int(20), Int(30), Int(40)})
	lit.SealAST()
	if !lit.IsSealed() {
		t.Fatal("anti-vacuity: the literal was not sealed")
	}

	before := len(BorrowFaults())

	// The #392 shape: a raw mint over borrowed sealed backing.
	bad := SExpr(lit.Cells[0:2])
	if bad.IsSealed() {
		t.Fatal("anti-vacuity: the raw mint inherited the constraint; the probe is not reproducing #392")
	}
	afterBad := BorrowFaults()
	if len(afterBad) != before+1 {
		t.Fatalf("detector did not fire on a raw mint over sealed backing: %d faults, want %d", len(afterBad), before+1)
	}
	if !strings.Contains(afterBad[len(afterBad)-1], "CONSTRAINED backing storage") {
		t.Errorf("unexpected fault text: %s", afterBad[len(afterBad)-1])
	}

	// The sanctioned route over the SAME storage: silent, and constrained.
	good := lit.SliceCells(0, 2)
	if !good.IsSealed() {
		t.Error("the borrow constructor did not transfer the constraint")
	}
	if got := len(BorrowFaults()); got != before+1 {
		t.Errorf("the borrow constructor tripped the detector: %d faults, want %d", got, before+1)
	}
}

// TestBorrowCheckIgnoresFreshStorage pins that ordinary construction — the
// overwhelming majority of QExpr/SExpr calls — is never reported.
func TestBorrowCheckIgnoresFreshStorage(t *testing.T) {
	lit := QExpr([]*LVal{Int(1), Int(2), Int(3)})
	lit.SealAST()
	before := len(BorrowFaults())
	for range 100 {
		_ = QExpr([]*LVal{Int(1), Int(2), Int(3)})
		_ = SExpr(make([]*LVal, 3))
	}
	if got := len(BorrowFaults()); got != before {
		t.Errorf("fresh storage was reported as borrowed: %d faults, want %d", got, before)
	}
}
