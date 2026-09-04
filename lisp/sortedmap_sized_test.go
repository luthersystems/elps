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
// make it panic.  make(map, n) panics on a negative n, so an exported
// constructor that forwards its argument would hand a caller computing a
// size from a subtraction a panic instead of an empty map.
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
