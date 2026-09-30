// Copyright © 2026 The ELPS authors

package libjson

import (
	"github.com/luthersystems/elps/lisp"
	"testing"
)

// BenchmarkPlainCorpus uses the same values as BenchmarkTypedJSON.
func BenchmarkPlainCorpus(b *testing.B) {
	for _, p := range []struct {
		name string
		v    func() *lisp.LVal
	}{
		{"frame", typedBenchFrame}, {"records400", func() *lisp.LVal { return typedBenchRecords(400) }},
	} {
		v := p.v()
		enc, err := Dump(v, false)
		if err != nil {
			b.Fatal(err)
		}
		b.Run("dump/"+p.name, func(b *testing.B) {
			b.ReportAllocs()
			for b.Loop() {
				if _, err := Dump(v, false); err != nil {
					b.Fatal(err)
				}
			}
		})
		b.Run("load/"+p.name, func(b *testing.B) {
			b.ReportAllocs()
			for b.Loop() {
				if v := Load(enc, false); v.Type == lisp.LError {
					b.Fatal(v)
				}
			}
		})
	}
}
