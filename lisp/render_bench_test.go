// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// The rendering and equality walks are hot -- every error path in the
// interpreter formats its operands, and equal? is an ordinary builtin -- so
// any bound placed on them has to be paid for out of nothing.  These
// benchmarks exist to measure that.  They deliberately cover the shallow
// shapes real values have, since that is the path a guard must not tax.

var benchStringSink string

type benchValue struct {
	value *lisp.LVal
	name  string
}

// benchValues returns the corpus in a fixed order, so two runs of the same
// binary report their sub-benchmarks in the same sequence.
func benchValues() []benchValue {
	nest := func(depth int, inner *lisp.LVal) *lisp.LVal {
		v := inner
		for i := range depth {
			v = lisp.SExpr([]*lisp.LVal{lisp.Int(i), v})
		}
		return v
	}
	m := lisp.SortedMap()
	m.MapSet("alpha", lisp.Int(1))
	m.MapSet("beta", lisp.String("two"))
	m.MapSet("gamma", lisp.SExpr([]*lisp.LVal{lisp.Int(1), lisp.Int(2), lisp.Int(3)}))
	wide := make([]*lisp.LVal, 64)
	for i := range wide {
		wide[i] = lisp.Int(i)
	}
	return []benchValue{
		{lisp.Int(42), "int"},
		{lisp.String("a moderately sized string value"), "string"},
		{lisp.SExpr([]*lisp.LVal{lisp.Int(1), lisp.String("two"), lisp.Symbol("three")}), "list"},
		{lisp.SExpr(wide), "wide"},
		{m, "map"},
		{lisp.Vector([]*lisp.LVal{lisp.Int(1), lisp.String("two"), m}), "vector"},
		{nest(8, lisp.SExpr([]*lisp.LVal{lisp.Int(0), m})), "nested"},
		{nest(32, lisp.Int(0)), "deep"},
	}
}

func BenchmarkString(b *testing.B) {
	for _, bv := range benchValues() {
		b.Run(bv.name, func(b *testing.B) {
			b.ReportAllocs()
			for range b.N {
				benchStringSink = bv.value.String()
			}
		})
	}
}

func BenchmarkEqual(b *testing.B) {
	for _, bv := range benchValues() {
		b.Run(bv.name, func(b *testing.B) {
			b.ReportAllocs()
			for range b.N {
				_ = bv.value.Equal(bv.value)
			}
		})
	}
}

// BenchmarkEqualDistinct compares two values built separately, so no
// comparison short-circuits on pointer identity anywhere.
func BenchmarkEqualDistinct(b *testing.B) {
	left, right := benchValues(), benchValues()
	for i, bv := range left {
		other := right[i].value
		b.Run(bv.name, func(b *testing.B) {
			b.ReportAllocs()
			for range b.N {
				_ = bv.value.Equal(other)
			}
		})
	}
}
