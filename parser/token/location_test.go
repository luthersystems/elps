// Copyright © 2026 The ELPS authors

package token

import "testing"

// TestNativeLocationReturnsAValue pins the property that makes it safe to
// call from anywhere: NativeLocation returns a Location, not a *Location, so
// no two callers can end up writing through one object (issues #362, #366).
func TestNativeLocationReturnsAValue(t *testing.T) {
	a := NativeLocation()
	b := NativeLocation()
	if a != b {
		t.Fatalf("NativeLocation is not stable: %+v vs %+v", a, b)
	}
	if a.File != NativeFile {
		t.Errorf("File = %q, want %q", a.File, NativeFile)
	}
	if a.Pos != -1 {
		t.Errorf("Pos = %d, want -1: a negative Pos is how the tree spells \"no real position\"", a.Pos)
	}
	if got, want := a.String(), NativeFile; got != want {
		t.Errorf("String() = %q, want %q", got, want)
	}

	a.Pos = 7
	if b.Pos == 7 {
		t.Error("writing to one NativeLocation value reached another")
	}
	if NativeLocation().Pos == 7 {
		t.Error("writing to a NativeLocation value reached the next call's result")
	}
}

func TestLocationCopy(t *testing.T) {
	orig := &Location{
		File: "f.lisp", Path: "/tmp/f.lisp",
		Pos: 1, Line: 2, Col: 3, EndPos: 4, EndLine: 5, EndCol: 6,
	}

	cp := orig.Copy()
	if cp == orig {
		t.Fatal("Copy returned the same pointer")
	}
	if *cp != *orig {
		t.Errorf("Copy changed the value: got %+v, want %+v", *cp, *orig)
	}

	// Independent in both directions.
	cp.Line = 99
	if orig.Line == 99 {
		t.Error("a write to the copy reached the original")
	}
	orig.Col = 42
	if cp.Col == 42 {
		t.Error("a write to the original reached the copy")
	}
}

// TestLocationCopyPreservesNil pins the nil convention.  A nil Source means
// "no position recorded" throughout the tree and is checked for explicitly
// (lisp.LEnv.ErrorAssociate, every lsp/ and analysis/ walker); materialising
// nil into a zero Location would turn those checks into false positives and
// print positions as ":0:0".
func TestLocationCopyPreservesNil(t *testing.T) {
	var loc *Location
	if got := loc.Copy(); got != nil {
		t.Errorf("(*Location)(nil).Copy() = %+v, want nil", got)
	}
}
