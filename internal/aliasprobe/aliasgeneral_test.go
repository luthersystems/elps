// Copyright © 2026 The ELPS authors
//
// Generalisation probes: for each LVal payload that has Go backing
// storage, is the borrowed-backing class REACHABLE, and does the arm's
// change cover it?  Each probe attempts the alias by construction and
// reports what happened; the table in the design packet is read off these.

package aliasprobe_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// TestGeneralisationLBytes: an LBytes minted over another's backing.
func TestGeneralisationLBytes(t *testing.T) {
	src := lisp.Bytes([]byte("ABCDEF"))
	view := lisp.Bytes(src.Bytes()[0:2])
	// Write past the view's length, into src's retained capacity.
	b := view.Bytes()
	b = append(b, 'z', 'z')
	_ = b
	if got := string(src.Bytes()); got != "ABCDEF" {
		t.Logf("REACHABLE via the raw constructor lisp.Bytes: src=%q (want ABCDEF)", got)
	} else {
		t.Logf("not reachable through raw lisp.Bytes on this arm: src=%q", got)
	}
}

// TestGeneralisationLSortMap: two LSortMap headers over one *MapData.
func TestGeneralisationLSortMap(t *testing.T) {
	src := lisp.SortedMap()
	if lerr := src.MapSet("k", lisp.Int(1)); lerr.Type == lisp.LError {
		t.Fatalf("MapSet: %v", lerr)
	}
	// SortedMapFromData is the exported mint-over-borrowed-backing
	// constructor for maps.  It takes NO provenance argument.
	alias := lisp.SortedMapFromData(src.Map())
	if lerr := alias.MapSet("k", lisp.Int(2)); lerr.Type == lisp.LError {
		t.Fatalf("MapSet: %v", lerr)
	}
	got := src.MapGet("k")
	if got.Int != 1 {
		t.Logf("REACHABLE through lisp.SortedMapFromData: src[k]=%d after writing the alias", got.Int)
	} else {
		t.Logf("not reachable: src[k]=%d", got.Int)
	}
}

// TestGeneralisationLArrayDims: dims are copied by Array(), so two arrays
// never share a dims list.
func TestGeneralisationLArrayDims(t *testing.T) {
	dims := lisp.QExpr([]*lisp.LVal{lisp.Int(2)})
	a := lisp.Array(dims, []*lisp.LVal{lisp.Int(1), lisp.Int(2)})
	bArr := lisp.Array(dims, []*lisp.LVal{lisp.Int(3), lisp.Int(4)})
	if a.Cells[0] == bArr.Cells[0] {
		t.Logf("REACHABLE: two arrays share one dims LVal")
	}
	if a.Cells[0].Cells[0] == dims.Cells[0] {
		t.Logf("REACHABLE: the array dims cell aliases the caller's")
	}
	t.Logf("dims: Array() copies (a.dims=%p caller=%p)", a.Cells[0], dims)
}

// TestGeneralisationLArrayBacking: two vectors over one cells array.
func TestGeneralisationLArrayBacking(t *testing.T) {
	cells := []*lisp.LVal{lisp.Int(1), lisp.Int(2), lisp.Int(3), lisp.Int(4)}
	src := lisp.Vector(cells)
	view := lisp.Vector(src.Cells[1].Cells[0:2])
	view.Cells[1].Cells[0] = lisp.Int(99)
	if src.Cells[1].Cells[0].Int != 1 {
		t.Logf("REACHABLE through lisp.Vector over a borrowed subslice: src[0]=%d",
			src.Cells[1].Cells[0].Int)
	}
}

// TestGeneralisationLString: Go strings are immutable, so a string view
// cannot write its source.  This is a proof of absence, not a repro.
func TestGeneralisationLString(t *testing.T) {
	src := lisp.String("ABCDEF")
	view := lisp.String(src.Str[0:2])
	if view.Str != "AB" || src.Str != "ABCDEF" {
		t.Fatalf("unexpected: %q %q", view.Str, src.Str)
	}
	t.Log("LString: Str is a Go string; the language provides no write path through a view")
}

// TestGeneralisationLNative: two LVals over one Go object.
func TestGeneralisationLNative(t *testing.T) {
	backing := []int{1, 2, 3}
	src := lisp.Native(backing)
	alias := lisp.Native(src.Native)
	alias.Native.([]int)[0] = 99
	if backing[0] == 99 {
		t.Logf("REACHABLE through lisp.Native: the kernel cannot know a Go value's aliasing rules")
	}
}
