// Copyright © 2026 The ELPS authors

//go:build !race

package libelpspath

import (
	"strconv"
	"testing"
)

// A whole-range replacement never copies an off-path value. Its guard state
// must stay on the stack even though the general copier can grow heap frames.
func TestRangePathNilAllocations(t *testing.T) {
	for _, n := range presizeSizes {
		t.Run(strconv.Itoa(n), func(t *testing.T) {
			p := Root(Chain(Range(0, n, false)))
			in := benchIntList(n)
			allocs := testing.AllocsPerRun(100, func() {
				out, err := p.Nil(in)
				if err != nil {
					t.Fatal(err)
				}
				if len(out.Cells) != n || !out.Cells[0].IsNil() || in.Cells[0].IsNil() {
					t.Fatal("range replacement changed the source or lost nils")
				}
			})
			if allocs != 4 {
				t.Fatalf("got %g allocations, want 4", allocs)
			}
		})
	}
}
