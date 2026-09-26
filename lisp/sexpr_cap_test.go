// Copyright © 2026 The ELPS authors

package lisp

import "testing"

// TestNewSExprCap pins the contract evalSExprCells relies on: a fresh
// unquoted s-expression header and an empty cells slice whose capacity is
// exactly n, across every co-allocation size class and past it, so an append
// beyond n reallocates rather than writing into the co-allocated array.
func TestNewSExprCap(t *testing.T) {
	for n := 1; n <= 10; n++ {
		call, cells := newSExprCap(n)
		if call.Type != LSExpr || call.quoted || call.Cells != nil {
			t.Fatalf("n=%d: header %#v, want a fresh unquoted s-expression", n, call)
		}
		if len(cells) != 0 || cap(cells) != n {
			t.Fatalf("n=%d: len %d cap %d, want 0 and %d", n, len(cells), cap(cells), n)
		}
		for i := range n {
			cells = append(cells, Int(i))
		}
		full := cells
		grown := append(cells, Int(n))
		if &grown[0] == &full[0] {
			t.Fatalf("n=%d: append past capacity reused the co-allocated array", n)
		}
		call.Cells = full
		for i, c := range call.Cells {
			if c.Int != i {
				t.Fatalf("n=%d: cell %d = %v", n, i, c)
			}
		}
	}
}
