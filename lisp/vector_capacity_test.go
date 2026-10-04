// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// A vector's capacity is the language's: a new vector's capacity is its
// length, and append! grows it by lisp.GrowCap, max(2c, n, 4).  These
// capacities are the same on every platform and Go version; Go's own
// append growth is not (it rounds to allocation size classes, which differ
// between 32-bit and 64-bit builds).
func TestVectorCapacityIsLanguageOwned(t *testing.T) {
	env := templateTestEnv(t)
	capOf := func(src string) int {
		t.Helper()
		v := env.LoadString("test", src)
		require.NoError(t, lisp.GoError(v), src)
		require.Equal(t, lisp.LArray, v.Type, src)
		return cap(v.Cells[1].Cells)
	}
	require.NoError(t, lisp.GoError(env.LoadString("test", `(set 'v (vector))`)))
	for _, step := range []struct {
		src      string
		len, cap int
	}{
		{`v`, 0, 0},
		{`(append! v 1)`, 1, 4},
		{`(append! v 2 3 4)`, 4, 4},
		{`(append! v 5)`, 5, 8},
		{`(append! v 6 7 8 9 10 11 12 13 14 15)`, 15, 16},
		{`(append! v 16)`, 16, 16},
		{`(append! v 17)`, 17, 32},
	} {
		v := env.LoadString("test", step.src)
		require.NoError(t, lisp.GoError(v), step.src)
		assert.Len(t, v.Cells[1].Cells, step.len, step.src)
		assert.Equal(t, step.cap, cap(v.Cells[1].Cells), step.src)
	}
	for _, c := range []struct {
		src string
		cap int
	}{
		{`(vector 1 2 3)`, 3},
		{`(append 'vector (vector 1 2 3) 4)`, 4},
		{`(append 'vector (vector 1 2 3))`, 3},
		{`(select 'vector (lambda (x) true) (vector 1 2 3 4 5))`, 5},
		{`(reject 'vector (lambda (x) false) (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))`, 20},
		{`(copy v)`, 17},
		{`(let ((w (vector 1 2))) (append! w 3) w)`, 4},
	} {
		assert.Equal(t, c.cap, capOf(c.src), c.src)
	}
	assert.Equal(t, 4, lisp.GrowCap(0, 1))
	assert.Equal(t, 10, lisp.GrowCap(5, 10))
	assert.Equal(t, 12, lisp.GrowCap(6, 7))
}

// append! never grows a vector's capacity past the allocation cap, and
// never below the new length.
func TestVectorCapacityAtAllocationCap(t *testing.T) {
	env := templateTestEnv(t)
	env.Runtime.MaxAlloc = 10
	for _, step := range []struct {
		src      string
		len, cap int
	}{
		{`(set 'v (vector 1 2 3 4 5 6))`, 6, 6},
		{`(append! v 7)`, 7, 10},
		{`(append! v 8 9 10)`, 10, 10},
	} {
		v := env.LoadString("test", step.src)
		require.NoError(t, lisp.GoError(v), step.src)
		assert.Len(t, v.Cells[1].Cells, step.len, step.src)
		assert.Equal(t, step.cap, cap(v.Cells[1].Cells), step.src)
	}
	assert.Equal(t, lisp.LError, env.LoadString("test", `(append! v 11)`).Type, "past the cap")
}
