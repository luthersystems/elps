// Copyright © 2026 The ELPS authors

package libjson

import (
	"strconv"
	"testing"

	"github.com/luthersystems/elps/lisp"
)

func valwalkBenchRecords(n int) *lisp.LVal {
	cells := make([]*lisp.LVal, n)
	for i := range cells {
		m := lisp.SortedMap()
		m.MapSet("id", lisp.Int(i))
		m.MapSet("label", lisp.String("record-"+strconv.Itoa(i)))
		m.MapSet("rate", lisp.Float(0.125))
		m.MapSet("active", lisp.Symbol("true"))
		m.MapSet("items", lisp.Vector([]*lisp.LVal{lisp.Int(1), lisp.Int(2)}))
		cells[i] = m
	}
	return lisp.Vector(cells)
}

func BenchmarkValwalkJSON(b *testing.B) {
	for _, size := range []struct {
		name string
		n    int
	}{{"small", 1}, {"records400", 400}} {
		v := valwalkBenchRecords(size.n)
		tagged, err := Tag(v)
		if err != nil {
			b.Fatal(err)
		}
		for _, op := range []struct {
			name string
			fn   func() error
		}{
			{"Tag", func() error { _, err := Tag(v); return err }},
			{"Untag", func() error { _, err := Untag(tagged); return err }},
			{"Canonize", func() error { _, err := Canonize(v); return err }},
			{"Typed", func() error { _, err := DumpTyped(v); return err }},
			{"Dump", func() error { _, err := Dump(v, false); return err }},
		} {
			b.Run(op.name+"/"+size.name, func(b *testing.B) {
				b.ReportAllocs()
				for b.Loop() {
					if err := op.fn(); err != nil {
						b.Fatal(err)
					}
				}
			})
		}
	}
}
