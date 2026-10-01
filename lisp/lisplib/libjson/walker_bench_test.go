// Copyright © 2026 The ELPS authors

package libjson

import (
	"errors"
	"strconv"
	"testing"

	"github.com/luthersystems/elps/lisp"
)

func jsonWalkerBenchRecords(n int) *lisp.LVal {
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

func BenchmarkJSONWalkers(b *testing.B) {
	b.Run("Tag/rejected-array", func(b *testing.B) {
		v := lisp.Array(lisp.QExpr([]*lisp.LVal{lisp.Int(1024), lisp.Int(1024)}), nil)
		opts := []TypedOption{WithTypedMaxValues(3)}
		b.ReportAllocs()
		for b.Loop() {
			if _, err := Tag(v, opts...); !errors.Is(err, ErrTypedLimit) {
				b.Fatalf("Tag error = %v, want value limit", err)
			}
		}
	})
	for _, size := range []struct {
		name string
		n    int
	}{{"small", 1}, {"records400", 400}} {
		v := jsonWalkerBenchRecords(size.n)
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

func BenchmarkCanonize(b *testing.B) { benchmarkJSONTransform(b, Canonize) }

func BenchmarkTag(b *testing.B) { benchmarkJSONTransform(b, Tag) }

func benchmarkJSONTransform(b *testing.B, transform func(*lisp.LVal, ...TypedOption) (*lisp.LVal, error)) {
	for _, fixture := range []struct {
		name string
		v    *lisp.LVal
	}{{"frame", typedBenchFrame()}, {"records400", typedBenchRecords(400)}} {
		b.Run(fixture.name, func(b *testing.B) {
			b.ReportAllocs()
			for b.Loop() {
				if _, err := transform(fixture.v); err != nil {
					b.Fatal(err)
				}
			}
		})
	}
}
