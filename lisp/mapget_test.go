// Copyright © 2026 The ELPS authors

package lisp

// testMapGet reads key k of the sorted-map m, or nil when m has no such key.
func testMapGet[K Key](m *LVal, k K) *LVal {
	v, _ := Lookup[*LVal](MapView{v: m}, k)
	return v
}
