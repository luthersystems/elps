// Copyright © 2026 The ELPS authors

package lisplib_test

import "github.com/luthersystems/elps/lisp"

// mapGet reads key k of the sorted-map m, or nil when m has no such key or
// m is not a sorted-map.
func mapGet[K lisp.Key](m *lisp.LVal, k K) *lisp.LVal {
	mv, _ := lisp.AsMap(m)
	v, _ := lisp.Lookup[*lisp.LVal](mv, k)
	return v
}
