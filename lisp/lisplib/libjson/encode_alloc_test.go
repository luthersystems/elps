// Copyright © 2018 The ELPS authors

// The allocation assertions for the JSON encoder's cycle guard live behind
// !race because the race detector allocates on its own account: it changes
// both the counts testing.AllocsPerRun reports and the ones the encoder makes,
// so an exact assertion under -race would be pinning the detector's behaviour
// rather than the encoder's.  Every other build runs them, including the plain
// `go test ./...` the CI gate uses.

//go:build !race

package libjson

import (
	"fmt"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
)

// TestEncodeDoesNotAllocateForCycleTracking pins the guard's stated contract:
// a document that is not cyclic pays nothing for it.  Serializing costs
// exactly the allocations it cost before the guard existed, because the pass
// that writes every real document carries a single int down the stack and
// touches no map at all.
//
// The counts below were measured on the commit before the guard (origin/main
// at f5d3d9e).  They are equalities, not bounds: this is the assertion that
// the encoder is walking the same code it always did.
//
// They each dropped by exactly one when the encoder was pooled (issue #379,
// item 6): the encoder struct itself is no longer allocated per document.
// Nothing else here moved, which is the point of keeping these as equalities
// -- the map batching that landed in the same change saves 2 allocations per
// entry, and every map in this test has exactly ONE entry, where batching
// three objects into three arrays is a wash.  A count that fell further than
// one would mean the shapes below stopped being the shapes described.
//
// Red-proof: replacing encodeGuard{} in encode with a guard whose path set is
// made up front fails every case here.
func TestEncodeDoesNotAllocateForCycleTracking(t *testing.T) {
	shared := lisp.SortedMap()
	shared.MapSet("a", lisp.Int(1))

	tests := []struct {
		name string
		v    *lisp.LVal
		want int
	}{
		{"leaf", lisp.Int(1), 1},
		{"flat object", shared, 8},
		// One map short of the depth that abandons the counting pass: the
		// value at the bottom sits at encodeGuardDepth-1.
		{"nested to the last depth the counting pass writes", nestMaps(encodeGuardDepth-2, lisp.Int(1)), 438},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			got := testing.AllocsPerRun(50, func() {
				if _, err := Dump(test.v, false); err != nil {
					t.Fatal(err)
				}
			})
			// AllocsPerRun averages over its runs but the encoder is
			// deterministic, so the mean is the count itself.
			assert.Equal(t, test.want, int(got))
		})
	}
}

// TestGuardIsFlatAcrossTheGuardDepth is the other half of the cost claim.  A
// document that nests past the guard depth is written twice -- once by the
// counting pass, which abandons it, and once by the pass that carries a path
// set -- and that second pass must allocate one set for the whole document,
// not one per value at the depth where tracking begins.
//
// The shapes compared are the same document at three widths, each measured
// just below the guard depth and just past it.  Crossing the guard costs
// whatever a second pass over that document costs; what must not happen is for
// that cost to grow with the width of the layer sitting at the boundary.
//
// Red-proof: making the path set inside encodeGuard.enter, so each of the
// width values at the boundary inherits a nil set from its parent and makes
// its own, turns the flat difference below into one that tracks width.
func TestGuardIsFlatAcrossTheGuardDepth(t *testing.T) {
	// wideAtDepth nests its wide layer under n maps, so the values in that
	// layer sit at depth n+2.  At below that is the last depth the counting
	// pass writes; at onGuard the document is abandoned and rewritten.
	const below = encodeGuardDepth - 3
	const onGuard = encodeGuardDepth - 2

	// Whatever crossing the guard costs, it is a fixed number of allocations
	// for the document and not a number per value in the widest layer.
	const constantCost = 32

	wideAtDepth := func(depth, width int) *lisp.LVal {
		cells := make([]*lisp.LVal, width)
		for i := range cells {
			cells[i] = lisp.Int(i)
		}
		return nestMaps(depth, lisp.SExpr(cells))
	}
	crossing := func(width int) float64 {
		lo := wideAtDepth(below, width)
		hi := wideAtDepth(onGuard, width)
		base := testing.AllocsPerRun(20, func() { _, _ = Dump(lo, false) })
		return testing.AllocsPerRun(20, func() { _, _ = Dump(hi, false) }) - base
	}

	narrow := crossing(2)
	for _, width := range []int{64, 1024} {
		t.Run(fmt.Sprintf("width=%d", width), func(t *testing.T) {
			got := crossing(width)
			assert.Less(t, got-narrow, float64(constantCost),
				"crossing the guard depth allocated per value in a layer of %d: %v vs %v",
				width, got, narrow)
		})
	}
}
