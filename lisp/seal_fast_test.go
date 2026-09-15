// Copyright © 2026 The ELPS authors

package lisp

import "testing"

func TestSealWideLeavesNoScratchAllocations(t *testing.T) {
	for _, width := range []int{64, 512} {
		nodes := make([]LVal, width)
		cells := make([]*LVal, width)
		for i := range nodes {
			nodes[i].Type = LInt
			cells[i] = &nodes[i]
		}
		v := SExpr(cells)
		allocations := testing.AllocsPerRun(20, func() {
			v.sealed = false
			for i := range nodes {
				nodes[i].sealed = false
			}
			v.sealAST()
		})
		if allocations != 0 {
			t.Fatalf("width %d: sealing allocated %g times", width, allocations)
		}
		for _, cell := range cells {
			if !cell.IsSealed() {
				t.Fatal("sealing skipped a leaf")
			}
		}
	}
}

func TestSealContinuationCycleAndSiblings(t *testing.T) {
	cycle := SExpr([]*LVal{nil, Int(7)})
	cycle.Cells[0] = cycle
	v := cycle
	for i := range 80 {
		v = SExpr([]*LVal{Int(i), v})
	}
	hidden := Int(9)
	opaque := Vector([]*LVal{hidden})
	root := SExpr([]*LVal{v, v, opaque, Nil()})
	root.sealAST()
	for range 80 {
		if !v.IsSealed() || !v.Cells[0].IsSealed() {
			t.Fatal("sealing lost a continuation sibling")
		}
		v = v.Cells[1]
	}
	if !cycle.IsSealed() || !cycle.Cells[1].IsSealed() || cycle.Cells[0] != cycle {
		t.Fatal("sealing lost the cycle or its leaf")
	}
	if opaque.IsSealed() || hidden.IsSealed() {
		t.Fatal("sealing descended into an opaque value")
	}
}
