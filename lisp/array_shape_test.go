// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"strconv"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func shapeDims(sizes ...int) *lisp.LVal {
	cells := make([]*lisp.LVal, len(sizes))
	for i, size := range sizes {
		cells[i] = lisp.Int(size)
	}
	return lisp.QExpr(cells)
}

func TestArrayRejectsInvalidShape(t *testing.T) {
	maxInt := int(^uint(0) >> 1)
	wrapToZero := 1 << (strconv.IntSize / 2)
	for _, tc := range []struct {
		name string
		dims *lisp.LVal
	}{
		{"product wraps to zero", shapeDims(wrapToZero, wrapToZero)},
		{"product wraps to positive", shapeDims(maxInt, maxInt)},
		{"negative dimension after zero", shapeDims(0, -1)},
		{"negative dimension before zero", shapeDims(-1, 0)},
		// These cannot have a representable slice backing; the old make
		// immediately panics instead of attempting an enormous allocation.
		{"backing byte count overflows", shapeDims(maxInt/(strconv.IntSize/8) + 1)},
		{"maximum dimension", shapeDims(maxInt)},
	} {
		t.Run(tc.name, func(t *testing.T) {
			before := tc.dims.String()
			var got *lisp.LVal
			require.NotPanics(t, func() { got = lisp.Array(tc.dims, nil) })
			require.NotNil(t, got)
			assert.Equal(t, lisp.LError, got.Type, "invalid shape must be refused by the constructor")
			assert.False(t, lisp.IsInternalPanic(got))
			assert.Equal(t, before, tc.dims.String(), "input dimensions changed")
			if got.Type == lisp.LArray && tc.name == "product wraps to zero" {
				// Prove the accepted malformed shape reaches an internal panic
				// through ordinary Lisp. No large backing is allocated here.
				env := newLimitTestEnv(t)
				require.NoError(t, lisp.GoError(env.PutGlobal(lisp.Symbol("a"), got)))
				result := env.LoadString("shape.lisp", `(aref a 0 0)`)
				assert.False(t, lisp.IsInternalPanic(result), "%v", result)
			}
		})
	}
}

func TestArrayZeroSizedShape(t *testing.T) {
	maxInt := int(^uint(0) >> 1)
	for _, dims := range []*lisp.LVal{
		shapeDims(0), shapeDims(0, 3), shapeDims(3, 0),
		shapeDims(maxInt, 2, 0), shapeDims(0, maxInt, 2),
	} {
		t.Run(dims.String(), func(t *testing.T) {
			got := lisp.Array(dims, nil)
			require.Equal(t, lisp.LArray, got.Type, "%v", got)
			assert.Empty(t, got.Cells[1].Cells)
			assert.True(t, lisp.True(got.ArrayDims().Equal(dims)))
			indices := make([]*lisp.LVal, len(dims.Cells))
			for i := range indices {
				indices[i] = lisp.Int(0)
			}
			result := got.ArrayIndex(indices...)
			assert.Equal(t, lisp.LError, result.Type, "zero-sized array has no valid index")
			assert.False(t, lisp.IsInternalPanic(result))
		})
	}
}

func TestArrayScalarIndexReturnsElement(t *testing.T) {
	for _, value := range []*lisp.LVal{lisp.Int(7), lisp.Vector([]*lisp.LVal{lisp.Int(9)}), lisp.Nil()} {
		t.Run(value.String(), func(t *testing.T) {
			array := lisp.Array(shapeDims(), []*lisp.LVal{value})
			require.Equal(t, lisp.LArray, array.Type)
			assert.Same(t, value, array.ArrayIndex(), "zero-dimensional index returns its element")
			env := newLimitTestEnv(t)
			require.NoError(t, lisp.GoError(env.PutGlobal(lisp.Symbol("a"), array)))
			got := env.LoadString("shape.lisp", `(aref a)`)
			require.False(t, lisp.IsInternalPanic(got), "%v", got)
			assert.Same(t, value, got, "Lisp aref must preserve the stored element")
			assert.Equal(t, lisp.LError, array.ArrayIndex(lisp.Int(0)).Type)
		})
	}
	// An omitted backing initializes the scalar element to Lisp nil.
	array := lisp.Array(shapeDims(), nil)
	assert.True(t, array.ArrayIndex().IsNil())
}

func TestArrayRowMajorShapeControl(t *testing.T) {
	cells := []*lisp.LVal{lisp.Int(0), lisp.Int(1), lisp.Int(2), lisp.Int(3), lisp.Int(4), lisp.Int(5)}
	array := lisp.Array(shapeDims(2, 3), cells)
	require.Equal(t, lisp.LArray, array.Type)
	for row := range 2 {
		for col := range 3 {
			assert.Same(t, cells[3*row+col], array.ArrayIndex(lisp.Int(row), lisp.Int(col)))
		}
	}
	for _, indices := range [][]*lisp.LVal{
		{lisp.Int(2), lisp.Int(0)}, {lisp.Int(0), lisp.Int(3)},
		{lisp.Int(-1), lisp.Int(0)}, {lisp.Float(0), lisp.Int(0)}, {lisp.Int(0)},
	} {
		got := array.ArrayIndex(indices...)
		assert.Equal(t, lisp.LError, got.Type)
		assert.False(t, lisp.IsInternalPanic(got))
	}
}
