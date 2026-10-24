// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// BenchmarkMapOf and BenchmarkSortedMapOf build the same four-key map. The
// difference is the cost of MapOf's conversion of Go keys and values.
func BenchmarkMapOf(b *testing.B) {
	env := benchEnv(b)
	id, desc := lisp.String("id-1"), lisp.String("a description")
	b.ReportAllocs()
	for b.Loop() {
		if _, err := env.MapOf("id", id, "description", desc, "count", 3, "ok", true); err != nil {
			b.Fatal(err)
		}
	}
}

func BenchmarkSortedMapOf(b *testing.B) {
	env := benchEnv(b)
	id, desc := lisp.String("id-1"), lisp.String("a description")
	b.ReportAllocs()
	for b.Loop() {
		if _, err := env.SortedMapOf(lisp.String("id"), id, lisp.String("description"), desc,
			lisp.String("count"), lisp.Int(3), lisp.String("ok"), lisp.Bool(true)); err != nil {
			b.Fatal(err)
		}
	}
}
