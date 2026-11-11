// Copyright © 2026 The ELPS authors

package lisp

import (
	"testing"

	"github.com/luthersystems/elps/parser/token"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// SharedInt returns one shared value for each number in [smallIntMin,
// smallIntMax) and a new value for any other number.
func TestSharedIntSharesSmallValues(t *testing.T) {
	for x := smallIntMin; x < smallIntMax; x++ {
		v := SharedInt(x)
		require.Same(t, v, SharedInt(x), "SharedInt(%d) allocated", x)
		require.Equal(t, LInt, v.Type)
		require.Equal(t, x, v.Int)
		require.True(t, isSmallInt(v))
	}
	for _, x := range []int{smallIntMin - 1, smallIntMax, -1 << 62, 1 << 62} {
		v := SharedInt(x)
		require.NotSame(t, v, SharedInt(x), "SharedInt(%d) is shared", x)
		require.Equal(t, x, v.Int)
		require.False(t, isSmallInt(v))
		require.False(t, v.sealed)
	}
	assert.Zero(t, testing.AllocsPerRun(100, func() { _ = SharedInt(7) }))
}

// Int always allocates, as Float does, even for a small number, and its
// result takes writes and a location.
func TestIntIsTheCallersOwn(t *testing.T) {
	v := Int(7)
	require.NotSame(t, Int(7), v)
	require.NotSame(t, SharedInt(7), v)
	require.False(t, isSmallInt(v))
	require.False(t, v.IsSealed())
	loc := &token.Location{File: "f.lisp", Line: 3, Col: 1}
	v.SetSource(loc)
	require.NotNil(t, v.loc())
	require.Equal(t, 3, v.loc().Line)
	v.Int = 8
	require.Equal(t, 7, SharedInt(7).Int)
	assert.InDelta(t, 1.0, testing.AllocsPerRun(100, func() { intSink = Int(7) }), 0)
}

// The two writes a caller could make on a shared integer through the API do
// not land, and the API does not report it as a program literal.
func TestSharedIntRefusesWrites(t *testing.T) {
	v := SharedInt(7)
	v.SetSource(&token.Location{File: "f.lisp", Line: 3, Col: 1})
	require.Nil(t, v.loc(), "SetSource wrote a location to the shared table")
	require.True(t, v.IsSealed(), "IsSealed must tell a caller not to write a shared integer")
	require.Empty(t, verifySmallInts(smallInts))

	cp := v.Copy()
	require.NotSame(t, v, cp)
	require.False(t, cp.sealed, "the copy inherited the table's seal")
	cp.Int = 99
	require.Equal(t, 7, SharedInt(7).Int)
}

// An array writes its dimension in place (append! does on every call).  The
// dimension an array builds for itself is its own integer.  A dimension that
// is a shared integer (a list built by hand, or by a decoder) is replaced.
func TestArrayDimensionNeverWritesASharedInt(t *testing.T) {
	vec := Vector([]*LVal{SharedInt(1)})
	require.False(t, isSmallInt(vec.Cells[0].Cells[0]), "Vector stored a shared integer as its dimension")

	vec.Cells[0].Cells[0] = SharedInt(1)
	setArrayDim(vec.Cells[0], 0, 2)
	require.Equal(t, 2, vec.Cells[0].Cells[0].Int)
	require.Equal(t, 1, SharedInt(1).Int, "resizing the vector wrote the shared integer 1")
	require.False(t, isSmallInt(vec.Cells[0].Cells[0]))

	own := vec.Cells[0].Cells[0]
	setArrayDim(vec.Cells[0], 0, 3)
	require.Same(t, own, vec.Cells[0].Cells[0], "an integer the array owns is written in place")
	require.Equal(t, 3, own.Int)
	require.Empty(t, verifySmallInts(smallInts))
}

// An integer a macro body computed has no location, so the stamp gives the
// expansion a located copy, as it did when Int allocated.  It does not take
// the shared integer for a sealed parser node and skip it.
func TestMacroStampLocatesAComputedSmallInt(t *testing.T) {
	callSite := &token.Location{File: "test.lisp", Line: 5, Col: 1}
	rt := StandardRuntime()

	got := stampMacroExpansion(SharedInt(3), callSite, nil, rt)
	require.NotSame(t, SharedInt(3), got)
	require.Equal(t, 3, got.Int)
	require.Equal(t, callSite, got.loc())
	require.False(t, got.sealed, "the located copy inherited the table's seal")

	expr := SExpr([]*LVal{Symbol("+"), SharedInt(1), SharedInt(2)})
	tree := stampMacroExpansion(expr, callSite, nil, rt)
	for i, c := range tree.Cells {
		require.Equal(t, callSite, c.loc(), "cell %d is not located", i)
		require.False(t, isSmallInt(c), "cell %d is the shared integer", i)
	}
	require.Nil(t, SharedInt(1).loc())
	require.Empty(t, verifySmallInts(smallInts))
}

// The integrity check names the entry that changed.  It runs on a private
// table: writing the shared one would race with every other test.
func TestVerifySmallIntsDetectsAWrite(t *testing.T) {
	table := newSmallInts()
	require.Empty(t, verifySmallInts(table))

	table[7-smallIntMin].Int = 8
	require.Equal(t, "SharedInt(7)", verifySmallInts(table))
	require.True(t, smallIntDrift(table[7-smallIntMin], 7-smallIntMin))
	table[7-smallIntMin].Int = 7

	table[0].sealed = false
	require.Equal(t, "SharedInt(-16)", verifySmallInts(table))
	table[0].sealed = true

	table[1039].source = locWord(&token.Location{File: "f.lisp"})
	require.Equal(t, "SharedInt(1023)", verifySmallInts(table))
}

// intSink keeps the result of Int on the heap in TestIntIsTheCallersOwn.
var intSink *LVal
