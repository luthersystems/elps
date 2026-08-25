// Copyright © 2026 The ELPS authors

package lisp

import (
	"testing"
)

// In-package tests for the admission walk's size arithmetic: the two counts
// are unexported, and the point of the round-four blocker fix is that they
// differ, so nothing outside the package can see it directly.

// TestLoaderWalkQuoteDiscount pins the two counts the round-four blocker fix
// separated, on the four shapes that must differ.  raw is what the walks
// downstream of admission cost (Copy, firstUnsealed, the fingerprint); eval is
// what the EVALUATOR costs, and only eval feeds the Unbounded hard failure.
func TestLoaderWalkQuoteDiscount(t *testing.T) {
	// A depth-10 doubling DAG: 11 distinct nodes, 2^11-1 = 2047 unfolded.
	dag := func() *LVal {
		node := Int(7)
		for range 10 {
			node = SExpr([]*LVal{node, node})
		}
		return node
	}
	const dagRaw = 2047

	tests := []struct {
		name          string
		build         func() *LVal
		wantRaw, want int64
	}{
		{"bare", func() *LVal { return dag() }, dagRaw, dagRaw},
		{"quote form", func() *LVal {
			return SExpr([]*LVal{Symbol("quote"), dag()})
		}, dagRaw + 2, 3},
		{"qualified quote form", func() *LVal {
			return SExpr([]*LVal{Symbol("lisp:quote"), dag()})
		}, dagRaw + 2, 3},
		{"quoted flag", func() *LVal { return Quote(dag()) }, dagRaw, 1},
		{"quasiquote form", func() *LVal {
			return SExpr([]*LVal{Symbol("quasiquote"), dag()})
		}, dagRaw + 2, dagRaw + 2},
		{"quasiquote over a quoted payload", func() *LVal {
			// No discount survives beneath a quasiquote: findAndUnquote
			// descends through quote levels, so the payload is charged its
			// quote-blind size.
			return SExpr([]*LVal{Symbol("quasiquote"), Quote(dag())})
		}, dagRaw + 2, dagRaw + 2},
		{"a quote the head symbol does not name", func() *LVal {
			return SExpr([]*LVal{Symbol("list"), dag()})
		}, dagRaw + 2, dagRaw + 2},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			w := newLoaderWalk(true)
			info, err := w.check(tt.build(), 0)
			if err != nil {
				t.Fatalf("check: %v", err)
			}
			if info.raw != tt.wantRaw {
				t.Errorf("raw = %d, want %d", info.raw, tt.wantRaw)
			}
			if info.eval != tt.want {
				t.Errorf("eval = %d, want %d", info.eval, tt.want)
			}
		})
	}
}
