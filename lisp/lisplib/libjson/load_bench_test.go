// Copyright © 2026 The ELPS authors

package libjson_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
)

// The package had benchmarks for the ENCODE direction only (BenchmarkEncode,
// BenchmarkEncode_stringNumbers). Decode had none, which is why the cost of
// the :exact-integers option could not be stated before it was added.
//
// The option matters here specifically: it replaces json.Unmarshal's
// float64 fast path with json.Decoder in UseNumber mode, which materialises
// every number as a json.Number -- a string -- before it is parsed. That is a
// different allocation profile on a path that decodes replicated state on
// every read.
//
// The "default" arm is the one to watch across a base/PR comparison: it is the
// path every existing caller is on, and it must not move.

const benchLoadDocument = `{
	"id": 9007199254740993,
	"seq": 421,
	"name": "a record with a few fields",
	"active": true,
	"score": 1.5,
	"tags": ["alpha", "beta", "gamma"],
	"counts": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
	"nested": {
		"a": {"b": {"c": 9223372036854775807}},
		"list": [{"x": 1, "y": 2}, {"x": 3, "y": 4}],
		"null": null
	}
}`

func benchLoad(b *testing.B, load func([]byte) *lisp.LVal) {
	b.Helper()
	doc := []byte(benchLoadDocument)
	if v := load(doc); v.Type == lisp.LError {
		b.Fatalf("benchmark document does not load: %s", v)
	}
	b.ReportAllocs()
	b.ResetTimer()
	for range b.N {
		if v := load(doc); v.Type == lisp.LError {
			b.Fatal(v)
		}
	}
}

func BenchmarkLoad(b *testing.B) {
	b.Run("default", func(b *testing.B) {
		benchLoad(b, func(doc []byte) *lisp.LVal { return libjson.Load(doc, false) })
	})
	b.Run("stringNumbers", func(b *testing.B) {
		benchLoad(b, func(doc []byte) *lisp.LVal { return libjson.Load(doc, true) })
	})
	b.Run("exactIntegers", func(b *testing.B) {
		benchLoad(b, func(doc []byte) *lisp.LVal {
			return libjson.LoadWith(doc, libjson.LoadOpts{ExactIntegers: true})
		})
	})
}

// BenchmarkLoadIntegers isolates the number path from the container walk, so a
// change in the cost of decoding a number is visible rather than diluted.
func BenchmarkLoadIntegers(b *testing.B) {
	doc := `[1,2,3,4,5,6,7,8,9,10,9007199254740993,9223372036854775807,-9223372036854775808]`
	b.Run("default", func(b *testing.B) {
		benchLoad(b, func(_ []byte) *lisp.LVal { return libjson.Load([]byte(doc), false) })
	})
	b.Run("exactIntegers", func(b *testing.B) {
		benchLoad(b, func(_ []byte) *lisp.LVal {
			return libjson.LoadWith([]byte(doc), libjson.LoadOpts{ExactIntegers: true})
		})
	})
}
