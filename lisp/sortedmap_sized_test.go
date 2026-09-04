// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// TestSortedMapSizedIsAHintOnly pins the two properties SortedMapSized
// promises beyond SortedMap: the size is only a hint, and no argument can
// make it panic.
//
// The negative-n rows pin the DOCUMENTED clamp, not a panic: gc ignores a
// negative map hint, so deleting the clamp in SortedMapSized would leave
// this test green.  They are here because the clamp is a contract an
// exported constructor owes a caller who computes a size from a
// subtraction, and because the language does not oblige an implementation
// to be as forgiving as gc is.
func TestSortedMapSizedIsAHintOnly(t *testing.T) {
	for _, n := range []int{-1 << 40, -1, 0, 1, 64} {
		m := lisp.SortedMapSized(n)
		require.Equal(t, lisp.LSortMap, m.Type)
		// Sized or not, the map starts empty and grows past the hint.
		assert.Equal(t, 0, m.Len(), "hint %d", n)
		for i := range 100 {
			m.MapSet(string(rune('a'+i%26))+string(rune('a'+i/26)), lisp.Int(i))
		}
		assert.Equal(t, 100, m.Len(), "hint %d", n)
	}
}
