// Copyright © 2026 The ELPS authors

package lisp

import (
	"fmt"
	"testing"
)

// benchStringKeys returns n distinct LString keys built up front, so the
// benchmarks below measure the map operation and not the key construction.
func benchStringKeys(n int) []*LVal {
	keys := make([]*LVal, n)
	for i := range keys {
		keys[i] = String(fmt.Sprintf("key-%04d", i))
	}
	return keys
}

// BenchmarkSortedMapInsert isolates the insert cost of the stock sorted
// map: a fresh map filled with 1000 string keys through Set, the way a
// map literal, assoc! or a JSON decode fills one.  It pins that storing a
// string key costs the map growth and nothing else (no per-key boxing).
func BenchmarkSortedMapInsert(b *testing.B) {
	keys := benchStringKeys(1000)
	val := Int(1)
	b.ReportAllocs()
	for b.Loop() {
		m := newmap()
		for _, k := range keys {
			if lerr := m.Set(k, val); lerr.Type == LError {
				b.Fatalf("set: %v", lerr)
			}
		}
	}
}

// BenchmarkSortedMapGet isolates the lookup cost of the stock sorted map:
// 1000 Get hits on a 1000-entry string-keyed map.
func BenchmarkSortedMapGet(b *testing.B) {
	keys := benchStringKeys(1000)
	m := newmap()
	for i, k := range keys {
		if lerr := m.Set(k, Int(i)); lerr.Type == LError {
			b.Fatalf("set: %v", lerr)
		}
	}
	b.ReportAllocs()
	for b.Loop() {
		for _, k := range keys {
			if _, ok := m.Get(k); !ok {
				b.Fatalf("get %s: missing", k.Str)
			}
		}
	}
}
