// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// The contract these tests pin: each of the three iterative value walkers --
// (*LVal).Copy (lisp/copier.go), detach (lisp/detach.go) and GoValue
// (lisp/embed.go) -- converts the children a container held AT THE MOMENT
// THE WALKER ENTERED IT.  A host hook that runs during the walk and mutates
// a container the walk is part-way through does not change what that walk
// produces: no appended cell appears in the output, and no overwritten cell
// replaces the child that occupied the slot when the container was entered.
// The mutation is of course visible in the SOURCE afterwards; it is simply
// not half-visible in the result.
//
// Snapshot rather than "the walk follows the mutation" because a walker's
// output is a copy of a value, and a value it never held is not a copy of
// anything.  Following the mutation is not even expressible: each walker
// sizes the container's output slice when it enters the container, so a
// walk that grew with the source would have to reallocate the output it has
// already handed to its parent, and one that shrank would leave holes.
//
// Each walker held the container's cells as a SLICE HEADER for as long as
// the container's frame was live (`f.cells = v.Cells`, `f.children =
// v.Cells`), and read a child back out of it at each resume.  A header is
// not a snapshot: it shares its backing array with the source, so a write
// into a slot the walk had not reached yet was picked up, and the walk
// produced a value that was neither the container as it was nor the
// container as it became.  Each walker now copies the children into the
// output slice it was already allocating and reads them back out of that,
// which is a true snapshot at no extra allocation (the allocation pins in
// walker_allocation_test.go hold the "no extra" half).
//
// One hook drives all three: a custom Map's Entries.  It is the only piece
// of host code every walk runs -- GoValue hands native payloads back by
// reference and never calls CloneNative, so a NativeCloner reaches only two
// of the three -- and running one fixture through all three is the point,
// since the claim is that they agree.

// walkerSourceMutation is what the hook does to the list being walked: it
// overwrites a cell the walk has not reached yet, THEN appends one.  The
// order matters.  An append that reallocates leaves the old backing array
// untouched, so an append alone is invisible to a stale header by accident;
// the overwrite lands in the array the stale header still points at, which
// is the half that was actually observable.
func walkerSourceMutation(parent *lisp.LVal) {
	parent.Cells[2] = lisp.Int(99)
	parent.Cells = append(parent.Cells, lisp.Int(9))
}

// walkerMutatingEntries is a custom Map whose Entries mutates a list it is
// a cell of.  A Map implementation is embedder code (NewMapData /
// SortedMapFromData are the documented extension point) and every walker
// calls Entries through sortedMapEntries while the enclosing list's frame
// is live.
type walkerMutatingEntries struct {
	*copierStringMap
	parent *lisp.LVal
}

func (m *walkerMutatingEntries) Entries(buf []*lisp.LVal) *lisp.LVal {
	walkerSourceMutation(m.parent)
	return m.copierStringMap.Entries(buf)
}

// walkerMutationFixture builds `(map 1 2)`, where the map's Entries mutates
// the list.  A fresh one per walker: the hook mutates the source, so a
// second walk of the same value would not start from the same list.
func walkerMutationFixture() *lisp.LVal {
	m := &walkerMutatingEntries{
		copierStringMap: newCopierStringMap(map[string]*lisp.LVal{"k": lisp.Int(7)}),
	}
	parent := lisp.QExpr([]*lisp.LVal{
		lisp.SortedMapFromData(lisp.NewMapData(m)),
		lisp.Int(1),
		lisp.Int(2),
	})
	m.parent = parent
	return parent
}

// assertWalkerSawTheSource checks the source really was mutated during the
// walk.  Without it a walker that never reached the map -- or a fixture
// whose hook stopped firing -- would pass every assertion below by doing
// nothing.
func assertWalkerSawTheSource(t *testing.T, parent *lisp.LVal) {
	t.Helper()
	if len(parent.Cells) != 4 || parent.Cells[2].Int != 99 {
		t.Fatalf("anti-vacuity: the hook did not mutate the source during the walk"+
			" (len %d, Cells[2] %v)", len(parent.Cells), parent.Cells[2])
	}
}

func TestCopySnapshotsCellsAgainstAHostHook(t *testing.T) {
	parent := walkerMutationFixture()
	cp := parent.Copy()
	if cp.Type == lisp.LError {
		t.Fatalf("copy: %v", cp)
	}
	assertWalkerSawTheSource(t, parent)
	if len(cp.Cells) != 3 {
		t.Fatalf("the copy has %d cells, want the 3 the list held when Copy entered it", len(cp.Cells))
	}
	if cp.Cells[2].Int != 2 {
		t.Errorf("the copy's third cell is %v; the list held 2 there when Copy entered it, and a"+
			" mid-walk overwrite must not reach the copy", cp.Cells[2])
	}
}

func TestDetachSnapshotsCellsAgainstAHostHook(t *testing.T) {
	parent := walkerMutationFixture()
	cp, err := lisp.Detach(parent)
	if err != nil {
		t.Fatalf("detach: %v", err)
	}
	assertWalkerSawTheSource(t, parent)
	if len(cp.Cells) != 3 {
		t.Fatalf("the detached copy has %d cells, want the 3 the list held when detach entered it", len(cp.Cells))
	}
	if cp.Cells[2].Int != 2 {
		t.Errorf("the detached copy's third cell is %v; the list held 2 there when detach entered it", cp.Cells[2])
	}
}

func TestGoValueSnapshotsCellsAgainstAHostHook(t *testing.T) {
	parent := walkerMutationFixture()
	out := lisp.GoValue(parent)
	assertWalkerSawTheSource(t, parent)
	got, ok := out.([]any)
	if !ok {
		t.Fatalf("GoValue returned %T, want []interface{}", out)
	}
	if len(got) != 3 {
		t.Fatalf("GoValue returned %d elements, want the 3 the list held when the walk entered it", len(got))
	}
	if got[2] != 2 {
		t.Errorf("GoValue's third element is %v; the list held 2 there when the walk entered it", got[2])
	}
}
