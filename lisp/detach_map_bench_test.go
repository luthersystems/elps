// Copyright © 2026 The ELPS authors

package lisp

import (
	"fmt"
	"testing"
)

// detachBenchMap builds a map of n entries for BenchmarkDeepCopySortedMap.
// The stock shape holds string keys and int values, the common one.  The
// nested shape holds a list under each key, so the walk descends.  The json
// shape is the map json:load builds.
func detachBenchMap(shape string, n int) *LVal {
	if shape == "json" {
		data := make(map[string]any, n)
		for i := range n {
			data[fmt.Sprintf("k%05d", i)] = Int(i)
		}
		return SortedMapFromData(NewMapData(jsonMap(data)))
	}
	m := SortedMap()
	for i := range n {
		v := Int(i)
		if shape == "nested" {
			v = QExpr([]*LVal{Int(i), String("x")})
		}
		m.Map().Set(String(fmt.Sprintf("k%05d", i)), v)
	}
	return m
}

// BenchmarkDeepCopySortedMap measures the copy builtin on a sorted-map
// (luthersystems/elps#944).  BenchmarkCopySortedMap covers LVal.Copy, a
// different walker.
func BenchmarkDeepCopySortedMap(b *testing.B) {
	env := NewEnv(nil)
	for _, shape := range []string{"stock", "nested", "json"} {
		for _, n := range []int{8, 64, 1024} {
			b.Run(fmt.Sprintf("%s/%d", shape, n), func(b *testing.B) {
				m := detachBenchMap(shape, n)
				b.ReportAllocs()
				b.ResetTimer()
				for range b.N {
					if cp := deepCopyValue(env, m); cp.Type == LError {
						b.Fatal(cp)
					}
				}
			})
		}
	}
}
