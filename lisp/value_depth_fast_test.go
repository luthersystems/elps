// Copyright © 2026 The ELPS authors

package lisp

import (
	"context"
	"errors"
	"testing"
)

func TestCheckValueDepthWideLeaves(t *testing.T) {
	for _, width := range []int{64, 512} {
		cells := make([]*LVal, width)
		for i := range cells {
			cells[i] = Int(i)
		}
		v := SExpr(cells)
		allocations := testing.AllocsPerRun(20, func() {
			if err := checkValueDepth(v, 1024, nil); err != nil {
				t.Fatal(err)
			}
		})
		if allocations != 0 {
			t.Fatalf("width %d: depth validation allocated %g times", width, allocations)
		}
	}
}

func TestCheckValueDepthLeafBoundary(t *testing.T) {
	v := nestList(1023, Int(7))
	if err := checkValueDepth(v, 1024, nil); err != nil {
		t.Fatal(err)
	}
	var depthErr ValueDepthError
	if err := checkValueDepth(SExpr([]*LVal{v}), 1024, nil); !errors.As(err, &depthErr) {
		t.Fatalf("expected depth error at scalar boundary, got %v", err)
	}
	// Host-created scalar headers can carry cells; they still need traversal.
	scalar := Int(7)
	scalar.Cells = []*LVal{v}
	if err := checkValueDepth(scalar, 1024, nil); !errors.As(err, &depthErr) {
		t.Fatalf("scalar header hid its cells: %v", err)
	}
	ctx, cancel := context.WithCancel(context.Background())
	cancel()
	if err := checkValueDepth(Int(7), 1024, ctx); !errors.Is(err, context.Canceled) {
		t.Fatalf("leaf hid cancellation: %v", err)
	}
}
