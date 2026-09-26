// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// BenchmarkEqualLarge compares values past the budgets equal? switches
// strategy at (lisp/sharing.go): a flat tree past the shallow pass's budget,
// a million-cell tree, and a chain far deeper than cycleGuardDepth.  The
// small cases are BenchmarkEqual's.
func BenchmarkEqualLarge(b *testing.B) {
	flat := func(n int) *lisp.LVal {
		cells := make([]*lisp.LVal, n)
		for i := range cells {
			cells[i] = lisp.SExpr([]*lisp.LVal{lisp.Int(i), lisp.String("x")})
		}
		return lisp.SExpr(cells)
	}
	deep := func(n int) *lisp.LVal {
		v := lisp.Int(7)
		for range n {
			v = lisp.SExpr([]*lisp.LVal{v, lisp.Int(1)})
		}
		return v
	}
	for _, c := range []struct {
		name string
		mk   func() *lisp.LVal
	}{
		{"flat70k", func() *lisp.LVal { return flat(70_000 / 3) }},
		// 600k cells stays under equalShallowBudget; 1M cells does not,
		// and pays the shallow pass's restart.
		{"flat600k", func() *lisp.LVal { return flat(600_000 / 3) }},
		{"flat1M", func() *lisp.LVal { return flat(1_000_000 / 3) }},
		{"chain2000", func() *lisp.LVal { return deep(2000) }},
	} {
		a, o := c.mk(), c.mk()
		b.Run(c.name, func(b *testing.B) {
			b.ReportAllocs()
			for b.Loop() {
				if !lisp.True(a.Equal(o)) {
					b.Fatal("unequal")
				}
			}
		})
	}
}
