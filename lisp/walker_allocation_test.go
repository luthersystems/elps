// Copyright © 2026 The ELPS authors

package lisp

import "testing"

func TestGoValueLeafAllocations(t *testing.T) {
	for _, tc := range []struct {
		name string
		v    *LVal
		want float64
	}{
		{"native", Native(make([]byte, 1<<20)), 0},
		{"int", Int(1 << 20), 1},
		{"string", String("here I stand"), 1},
		{"bytes", Bytes(make([]byte, 16)), 2},
	} {
		t.Run(tc.name, func(t *testing.T) {
			got := testing.AllocsPerRun(100, func() { goValueSink = GoValue(tc.v) })
			if got != tc.want {
				t.Fatalf("got %g allocations, want %g", got, tc.want)
			}
		})
	}
}

// Width must not spill traversal scratch: only output headers and the one
// backing slice are allocated, however many scalar siblings a list has.
func TestCopyWideLeafAllocations(t *testing.T) {
	for _, n := range []int{16, 64, 512} {
		cells := make([]*LVal, n)
		for i := range cells {
			cells[i] = Int(i)
		}
		v := QExpr(cells)
		got := testing.AllocsPerRun(100, func() { v.Copy() })
		if got != float64(n+2) {
			t.Fatalf("width %d: got %g allocations, want %d", n, got, n+2)
		}
	}
}

func TestDetachLeafAllocations(t *testing.T) {
	v := Int(7)
	got := testing.AllocsPerRun(100, func() {
		cp, err := v.detach()
		if err != nil || cp == v || cp.Int != 7 {
			t.Fatal("invalid detached leaf")
		}
	})
	if got != 1 {
		t.Fatalf("got %g allocations, want only the copied header", got)
	}
}
