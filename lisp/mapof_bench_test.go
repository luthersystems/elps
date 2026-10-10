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
		if m := env.MapOf("id", id, "description", desc, "count", 3, "ok", true); m.IsError() {
			b.Fatal(m)
		}
	}
}

func BenchmarkSortedMapOf(b *testing.B) {
	env := benchEnv(b)
	id, desc := lisp.String("id-1"), lisp.String("a description")
	b.ReportAllocs()
	for b.Loop() {
		m := env.SortedMapOf(lisp.String("id"), id, lisp.String("description"), desc,
			lisp.String("count"), lisp.Int(3), lisp.String("ok"), lisp.Bool(true))
		if m.IsError() {
			b.Fatal(m)
		}
	}
}
