// Copyright © 2026 The ELPS authors

package lisp

import (
	"fmt"
	"testing"
)

// benchKeysMaps returns a 100-entry stock sorted map (string keys, with
// every tenth key a symbol) and a 100-entry jsonMap, the inputs of the
// Keys/Entries benchmarks below.
func benchKeysMaps() (sortedmap, jsonMap) {
	sm := newmap()
	jm := make(jsonMap, 100)
	for i := 0; i < 100; i++ {
		k := fmt.Sprintf("key-%04d", i)
		kv := String(k)
		if i%10 == 0 {
			kv = Symbol(k)
		}
		sm.Set(kv, Int(i))
		jm[k] = Int(i)
	}
	return sm, jm
}

func BenchmarkSortedMapKeys(b *testing.B) {
	sm, _ := benchKeysMaps()
	b.ReportAllocs()
	for b.Loop() {
		_ = sm.Keys()
	}
}

func BenchmarkSortedMapEntries(b *testing.B) {
	sm, _ := benchKeysMaps()
	buf := make([]*LVal, sm.Len())
	b.ReportAllocs()
	for b.Loop() {
		_ = sm.Entries(buf)
	}
}

func BenchmarkJSONMapKeys(b *testing.B) {
	_, jm := benchKeysMaps()
	b.ReportAllocs()
	for b.Loop() {
		_ = jm.Keys()
	}
}

func BenchmarkJSONMapEntries(b *testing.B) {
	_, jm := benchKeysMaps()
	buf := make([]*LVal, jm.Len())
	b.ReportAllocs()
	for b.Loop() {
		_ = jm.Entries(buf)
	}
}
