// Copyright © 2026 The ELPS authors

//go:build elpscheck

package lisp

import (
	"testing"

	"github.com/stretchr/testify/require"
)

// Under elpscheck a returned call list is poisoned and never reused, and a
// second return panics.
func TestCallListPoison(t *testing.T) {
	var p callLists
	v, back := p.get(3)
	cells := append(back[:0:3], Symbol("f"), Int(1), Int(2))
	v.Cells = cells[1:]
	p.put(v, back)

	require.Equal(t, LInvalid, v.Type)
	require.Nil(t, v.Cells)
	require.Equal(t, 4, cap(back))
	for i, c := range back[:cap(back)] {
		require.Same(t, callListPoison, c, "cell %d was not poisoned", i)
	}
	for _, c := range cells {
		require.Same(t, callListPoison, c, "the list's own cells were not poisoned")
	}
	for _, free := range p.free {
		require.Empty(t, free, "a checked build kept a list for reuse")
	}
	require.PanicsWithValue(t, "lisp: a call list was returned twice", func() { p.put(v, back) })

	w, _ := p.get(3)
	require.NotSame(t, v, w)
	require.Equal(t, LSExpr, w.Type)
}
