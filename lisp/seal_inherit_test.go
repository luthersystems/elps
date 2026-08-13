// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// sealedList returns a list sealed exactly as the parser seals a quoted
// program literal.
func sealedList(t *testing.T, n int) *lisp.LVal {
	t.Helper()
	cells := make([]*lisp.LVal, n)
	for i := range cells {
		cells[i] = lisp.Int(i)
	}
	v := lisp.QExpr(cells)
	v.SealAST()
	if !v.IsSealed() {
		t.Fatal("anti-vacuity: SealAST left the list unsealed")
	}
	return v
}

// TestInheritSealPropagates covers the case InheritSeal exists for: a fresh
// header minted over a sealed value's backing array.
//
// This is the shape every copy-on-write guard in the kernel is blind to
// unless the flag travels. builtinSlice, builtinCdr and builtinRest do it
// by assigning the unexported field; InheritSeal is that assignment made
// available to the value-restructuring libraries outside package lisp --
// libelpspath, which shipped without it (issue #392).
func TestInheritSealPropagates(t *testing.T) {
	src := sealedList(t, 4)

	// A two-index window: the exact shape the defect produced.
	window := lisp.QExpr(src.Cells[1:3])
	if window.IsSealed() {
		t.Fatal("anti-vacuity: a freshly minted header is already sealed")
	}
	window.InheritSeal(src)
	if !window.IsSealed() {
		t.Error("InheritSeal did not carry a sealed source's constraint onto a window over its backing")
	}

	// Idempotent: sealing is monotone.
	window.InheritSeal(src)
	if !window.IsSealed() {
		t.Error("InheritSeal is not idempotent")
	}
}

// TestInheritSealCannotInventAConstraint pins the half of the contract that
// keeps InheritSeal from becoming a general "seal this" setter. A caller can
// only propagate a constraint that already exists on the source; it cannot
// manufacture one, so no amount of misuse can freeze ordinary runtime data
// and start every mutating builtin copying it.
func TestInheritSealCannotInventAConstraint(t *testing.T) {
	runtimeList := lisp.QExpr([]*lisp.LVal{lisp.Int(1), lisp.Int(2)})
	if runtimeList.IsSealed() {
		t.Fatal("anti-vacuity: a hand-built list is sealed")
	}
	out := lisp.QExpr(runtimeList.Cells)
	out.InheritSeal(runtimeList)
	if out.IsSealed() {
		t.Error("InheritSeal sealed a value whose source was not sealed")
	}
	out.InheritSeal(nil)
	if out.IsSealed() {
		t.Error("InheritSeal sealed a value from a nil source")
	}
}

// TestInheritSealRefusesArrays is the rule that makes the array branch of a
// borrowed-backing constructor safe to leave alone.
//
// An array must never be marked, because nothing honours the mark: append!
// and assoc! write vector backing in place without consulting the flag
// (lisp/seal.go -- "maps/vectors/bytes cannot be produced by the parser, so
// a sealed value can never legally reach their mutation path"). A sealed
// vector would therefore be a lie that reads as protection, which is worse
// than no flag at all. Code that needs to wrap sealed backing in a vector
// must COPY, as builtinSlice does for (slice 'vector ...) -- and
// TestSealPropagation pins that behaviour on the kernel side.
func TestInheritSealRefusesArrays(t *testing.T) {
	// The "vector" row below deliberately mints a vector over a SEALED
	// list's live cells -- the undischargeable form of the borrowed-backing
	// fault, since InheritSeal refuses arrays by design and the only
	// correct production response is to copy. The read-half detector
	// (lisp/borrow_check_elpscheck.go) reports it, correctly, and this test
	// is its own subject, so the notes are dropped afterwards rather than
	// left to fail the package's TestMain -- the same hygiene
	// TestCheckSingleton_DetectsCorruption applies to the singleton it
	// deliberately corrupts. A no-op in untagged builds.
	defer lisp.DropBorrowNotesForTest()

	src := sealedList(t, 4)
	for _, tc := range []struct {
		name string
		val  *lisp.LVal
	}{
		{"vector", lisp.Vector(src.Cells[0:2])},
		{"sorted-map", lisp.SortedMap()},
		{"bytes", lisp.Bytes([]byte{1, 2})},
	} {
		t.Run(tc.name, func(t *testing.T) {
			tc.val.InheritSeal(src)
			if tc.val.IsSealed() {
				t.Errorf("InheritSeal marked a %s; only parser-producible types may carry the flag, "+
					"because only they are covered by the kernel's copy-on-write sites", tc.name)
			}
		})
	}
}

// TestInheritSealNilReceiver keeps the method usable in the same
// unconditional style as IsSealed, which tolerates a nil receiver. Path
// libraries build values in branches that can produce a nil on an error
// route; a helper that panics there would be a worse hazard than the one it
// closes.
func TestInheritSealNilReceiver(t *testing.T) {
	var v *lisp.LVal
	v.InheritSeal(sealedList(t, 2)) // must not panic
}

// TestInheritSealSingletonsUntouched guards the one write InheritSeal must
// never make. Nil/true/false are shared process-wide and are born sealed
// (lisp/singleton.go, issue #376), so writing even a flag to one would be a
// data race on a value every environment holds.
func TestInheritSealSingletonsUntouched(t *testing.T) {
	src := sealedList(t, 2)
	before := lisp.TakeSingletonSnapshot()
	lisp.Nil().InheritSeal(src)
	lisp.Bool(true).InheritSeal(src)
	lisp.Bool(false).InheritSeal(src)
	if drift := before.Verify(); drift != "" {
		t.Errorf("InheritSeal disturbed the %s singleton", drift)
	}
}
