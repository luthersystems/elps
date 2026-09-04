// Copyright © 2026 The ELPS authors

package libelpspath

import (
	"fmt"
	"strconv"
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// Benchmarks for the sites that build a result of known final size one
// entry at a time: the copy behind the non-mutating map operations, and
// the result slices of the iterator and range paths.
//
//	go test -run '^$' -bench 'CopyMapWide|IterPathGet|RangePathNil' -benchmem -count=10 -cpu 1 ./lisp/lisplib/libelpspath/

func benchWideMap(n int) *lisp.LVal {
	m := lisp.SortedMap()
	for i := range n {
		m.MapSet(fmt.Sprintf("key%04d", i), lisp.Int(i))
	}
	return m
}

func benchRecords(n int) *lisp.LVal {
	cells := make([]*lisp.LVal, n)
	for i := range cells {
		m := lisp.SortedMap()
		m.MapSet("id", lisp.Int(i))
		m.MapSet("name", lisp.String("name"))
		cells[i] = m
	}
	return lisp.Vector(cells)
}

func benchIntList(n int) *lisp.LVal {
	cells := make([]*lisp.LVal, n)
	for i := range cells {
		cells[i] = lisp.Int(i)
	}
	return lisp.QExpr(cells)
}

var presizeSizes = []int{10, 100, 1000}

// BenchmarkCopyMapWide is ?set! on a flat map of n scalar entries: one
// copyMapOffPath of the whole map per call.
func BenchmarkCopyMapWide(b *testing.B) {
	for _, n := range presizeSizes {
		b.Run(strconv.Itoa(n), func(b *testing.B) {
			env := lisp.NewEnv(nil)
			env.Runtime.Reader = nil
			call := lisp.QExpr([]*lisp.LVal{benchWideMap(n), lisp.String("key0000"), lisp.String("v")})
			b.ReportAllocs()
			for b.Loop() {
				if v := BuiltinQuerySet(env, call); v.Type == lisp.LError {
					b.Fatal(v)
				}
			}
		})
	}
}

func benchPath(b *testing.B, in *lisp.LVal, op func(*lisp.LVal) (*lisp.LVal, error)) {
	b.Helper()
	b.ReportAllocs()
	for b.Loop() {
		if _, err := op(in); err != nil {
			b.Fatal(err)
		}
	}
}

// BenchmarkIterPathGet covers the iterator's Get over a vector of n
// records.
func BenchmarkIterPathGet(b *testing.B) {
	for _, n := range presizeSizes {
		p := Root(Chain(Iter(Dot("id"))))
		b.Run(strconv.Itoa(n), func(b *testing.B) {
			benchPath(b, benchRecords(n), p.Get)
		})
	}
}

// BenchmarkRangePathNil covers the range path's copying Nil, which
// rebuilds the window as fresh nils.
func BenchmarkRangePathNil(b *testing.B) {
	for _, n := range presizeSizes {
		p := Root(Chain(Range(0, n, false)))
		b.Run(strconv.Itoa(n), func(b *testing.B) {
			benchPath(b, benchIntList(n), p.Nil)
		})
	}
}
