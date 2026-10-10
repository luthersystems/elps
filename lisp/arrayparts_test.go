// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// TestEmptyVector: Vector(nil) is an empty vector with dimensions (0), the
// same value as Array(nil, nil).
func TestEmptyVector(t *testing.T) {
	v := lisp.Vector(nil)
	require.Equal(t, lisp.LArray, v.Type)
	assert.Equal(t, 0, v.Len())
	assert.Equal(t, "'(0)", v.ArrayDims().String())
	assert.True(t, lisp.True(v.Equal(lisp.Array(nil, nil))))
	dims, data := v.ArrayParts()
	assert.Equal(t, "'(0)", dims.String())
	assert.Empty(t, data.Cells)
}

// TestArrayParts: ArrayParts returns the stored lists, not copies.
func TestArrayParts(t *testing.T) {
	v := lisp.Vector([]*lisp.LVal{lisp.Int(1), lisp.Int(2)})
	dims, data := v.ArrayParts()
	assert.Equal(t, "'(2)", dims.String())
	assert.Equal(t, "'(1 2)", data.String())
	data.Cells[0] = lisp.Int(7)
	assert.Equal(t, 7, v.ArrayIndex(lisp.Int(0)).Int, "data is v's own list")
	assert.PanicsWithValue(t, "not array: list", func() { lisp.QExpr(nil).ArrayParts() })
	dims, data = (&lisp.LVal{Type: lisp.LArray}).ArrayParts()
	assert.Nil(t, dims, "a malformed array has no lists")
	assert.Nil(t, data, "a malformed array has no lists")
}

// TestSetArrayData: SetArrayData fills an array in place and keeps its
// pointer.
func TestSetArrayData(t *testing.T) {
	v := lisp.Vector(nil)
	holder := lisp.Cells{v}.List() // a value that already holds v
	v.SetArrayData(lisp.Cells{lisp.Int(1), lisp.Int(2), lisp.Int(3)}.List())
	assert.Same(t, v, holder.Cells[0])
	assert.Equal(t, 3, v.Len())
	assert.Equal(t, "'(3)", v.ArrayDims().String())
	assert.Equal(t, 3, v.ArrayIndex(lisp.Int(2)).Int)

	// A new rank stores a new dimension list.
	m := lisp.Array(nil, nil)
	data := lisp.Cells{lisp.Int(1), lisp.Int(2), lisp.Int(3), lisp.Int(4), lisp.Int(5), lisp.Int(6)}.List()
	m.SetArrayData(data, 2, 3)
	assert.Equal(t, "'(2 3)", m.ArrayDims().String())
	assert.Equal(t, 6, m.ArrayIndex(lisp.Int(1), lisp.Int(2)).Int)
	_, got := m.ArrayParts()
	assert.Same(t, data, got, "data is kept, not copied")

	// The same rank writes the dimensions in place.
	m.SetArrayData(data, 3, 2)
	assert.Equal(t, "'(3 2)", m.ArrayDims().String())
	assert.Equal(t, 6, m.ArrayIndex(lisp.Int(2), lisp.Int(1)).Int)

	// Back to a vector.
	m.SetArrayData(data)
	assert.Equal(t, "'(6)", m.ArrayDims().String())
	assert.True(t, lisp.True(m.Equal(lisp.Vector(data.Cells))))

	assert.PanicsWithValue(t, "not array: list", func() { lisp.QExpr(nil).SetArrayData(lisp.QExpr(nil)) })
}

// TestSetArrayCells: SetArrayCells fills the array's own data list.
func TestSetArrayCells(t *testing.T) {
	v := lisp.Vector(nil)
	_, data := v.ArrayParts()
	v.SetArrayCells([]*lisp.LVal{lisp.Int(4), lisp.Int(5)})
	_, got := v.ArrayParts()
	assert.Same(t, data, got, "the data list is v's own")
	assert.Equal(t, "'(2)", v.ArrayDims().String())
	assert.Equal(t, 5, v.ArrayIndex(lisp.Int(1)).Int)

	m := lisp.Array(nil, nil)
	m.SetArrayCells([]*lisp.LVal{lisp.Int(1), lisp.Int(2), lisp.Int(3), lisp.Int(4)}, 2, 2)
	assert.Equal(t, "'(2 2)", m.ArrayDims().String())
	assert.Equal(t, 4, m.ArrayIndex(lisp.Int(1), lisp.Int(1)).Int)

	assert.PanicsWithValue(t, "not array: list", func() { lisp.QExpr(nil).SetArrayCells(nil) })
}
