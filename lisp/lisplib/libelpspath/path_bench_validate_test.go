// Copyright © 2026 The ELPS authors

package libelpspath

import (
	"strconv"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
)

// Every elpspath builtin validates the whole document before it runs
// (okSimpleType), so a read of one field costs a walk of every record.
// These benchmarks hold that walk on a document of many records, the shape
// in which it dominates the call (luthersystems/elps#944).

// validateBenchRecords returns a vector of n records.  A record has scalar fields,
// a vector and a nested map, like the records of benchDoc.
func validateBenchRecords(n int) *lisp.LVal {
	cells := make([]*lisp.LVal, n)
	for i := range cells {
		meta := lisp.SortedMap()
		meta.MapSetString("x", lisp.Int(1))
		meta.MapSetString("y", lisp.Int(2))
		rec := lisp.SortedMap()
		rec.MapSetString("a", lisp.Int(i))
		rec.MapSetString("id", lisp.String("rec-"+strconv.Itoa(i)))
		rec.MapSetString("active", lisp.Bool(true))
		rec.MapSetString("tags", lisp.Vector([]*lisp.LVal{lisp.String("t1"), lisp.String("t2")}))
		rec.MapSetString("meta", meta)
		cells[i] = rec
	}
	return lisp.Vector(cells)
}

// validateBenchJSON is validateBenchRecords decoded from JSON text, whose maps have
// the decoder's backing and not the sorted-map builtin's.
func validateBenchJSON(b *testing.B, n int) *lisp.LVal {
	b.Helper()
	var sb strings.Builder
	sb.WriteString("[")
	for i := range n {
		if i > 0 {
			sb.WriteString(",")
		}
		sb.WriteString(`{"a":` + strconv.Itoa(i) + `,"id":"rec-` + strconv.Itoa(i) +
			`","active":true,"tags":["t1","t2"],"meta":{"x":1,"y":2}}`)
	}
	sb.WriteString("]")
	doc := libjson.Load([]byte(sb.String()), false)
	if doc.Type == lisp.LError {
		b.Fatal(doc)
	}
	return doc
}

// BenchmarkTypeCheckRecords measures the validation walk alone.
func BenchmarkTypeCheckRecords(b *testing.B) {
	for _, n := range []int{10, 1000} {
		b.Run("sorted-map/"+strconv.Itoa(n), func(b *testing.B) {
			doc := validateBenchRecords(n)
			b.ReportAllocs()
			for b.Loop() {
				if err := okSimpleType(doc); err != nil {
					b.Fatal(err)
				}
			}
		})
		b.Run("json/"+strconv.Itoa(n), func(b *testing.B) {
			doc := validateBenchJSON(b, n)
			b.ReportAllocs()
			for b.Loop() {
				if err := okSimpleType(doc); err != nil {
					b.Fatal(err)
				}
			}
		})
	}
}

// BenchmarkBuiltinGetRecords measures (elpspath:? doc 0 "a"): the whole
// call a program pays to read one field of the first record.
func BenchmarkBuiltinGetRecords(b *testing.B) {
	for _, n := range []int{10, 1000} {
		b.Run(strconv.Itoa(n), func(b *testing.B) {
			env := lisp.NewEnv(nil)
			call := lisp.QExpr([]*lisp.LVal{validateBenchRecords(n), lisp.Int(0), lisp.String("a")})
			b.ReportAllocs()
			for b.Loop() {
				if v := BuiltinQueryGet(env, call); v.Type == lisp.LError {
					b.Fatal(v)
				}
			}
		})
	}
}

// BenchmarkBuiltinSetMutateRecords measures (elpspath:?set! doc 0 "a" 1),
// which validates the document and the new value and copies nothing.
func BenchmarkBuiltinSetMutateRecords(b *testing.B) {
	env := lisp.NewEnv(nil)
	call := lisp.QExpr([]*lisp.LVal{validateBenchRecords(1000), lisp.Int(0), lisp.String("a"), lisp.Int(1)})
	b.ReportAllocs()
	for b.Loop() {
		if v := BuiltinQuerySetMutate(env, call); v.Type == lisp.LError {
			b.Fatal(v)
		}
	}
}
