// Copyright © 2026 The ELPS authors

package lisp

import (
	"testing"
	"unsafe"

	"github.com/luthersystems/elps/internal/fmtmeta"
	"github.com/luthersystems/elps/parser/token"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// These tests pin the tooling record's semantics (see nodeAux).  The record
// used to be two pointer fields of LVal; it moved onto the node's Location
// to take LVal down a size class (luthersystems/elps#944).  Each test names
// the behavior of the two fields that the record must keep.

// TestLValSizeClass pins the point of the move: a third pointer in LVal
// puts every value in the 112-byte class.
func TestLValSizeClass(t *testing.T) {
	assert.Equal(t, uintptr(96), unsafe.Sizeof(LVal{}))
}

func TestNodeAuxAbsentInProduction(t *testing.T) {
	v := Int(1)
	assert.Nil(t, v.fmtMeta())
	assert.Nil(t, v.macroExp())
	loc := &token.Location{File: "a.lisp", Pos: 3}
	v.SetSource(loc)
	assert.Same(t, loc, v.source, "SetSource stores the pointer it is given")
	assert.False(t, v.hasAux)
}

// A header copy copied the two pointers, so it shared the record.
func TestNodeAuxHeaderCopyShares(t *testing.T) {
	v := Symbol("x")
	v.SetSource(&token.Location{File: "a.lisp", Pos: 3})
	m := &fmtmeta.Meta{OriginalText: "x"}
	v.setFmtMeta(m, true)
	cp := Quote(v) // a header copy
	assert.Same(t, m, cp.fmtMeta())
}

// A node that borrowed another node's Location did not get its record.
func TestNodeAuxBorrowedLocationDoesNotShare(t *testing.T) {
	v := Symbol("x")
	v.SetSource(&token.Location{File: "a.lisp", Pos: 3})
	v.setMacroExp(&macroExpansionInfo{ID: 7})
	w := Symbol("y")
	w.source = v.source
	assert.Nil(t, w.macroExp())
	_, ok := w.MacroExpansion()
	assert.False(t, ok)
}

// A write to one header's field did not reach a header copy.
func TestNodeAuxWriteDoesNotReachHeaderCopy(t *testing.T) {
	v := Symbol("x")
	v.SetSource(&token.Location{File: "a.lisp", Pos: 3})
	first := &macroExpansionInfo{ID: 1}
	v.setMacroExp(first)
	cp := Quote(v) // a header copy
	second := &macroExpansionInfo{ID: 2}
	cp.setMacroExp(second)
	assert.Same(t, v.macroExp(), first)
	assert.Same(t, cp.macroExp(), second)
	cp.setMacroExp(nil)
	assert.Nil(t, cp.macroExp())
	assert.Same(t, v.macroExp(), first)
	want, _ := v.Source()
	got, ok := cp.Source()
	require.True(t, ok)
	assert.Equal(t, want, got, "dropping the record keeps the location")
}

// The two halves of the record are independent, as the two fields were.
func TestNodeAuxHalvesAreIndependent(t *testing.T) {
	v := Symbol("x")
	m := &fmtmeta.Meta{OriginalText: "x"}
	info := &macroExpansionInfo{ID: 1}
	v.setFmtMeta(m, false)
	v.setMacroExp(info)
	assert.Same(t, m, v.fmtMeta())
	assert.Same(t, v.macroExp(), info)
	v.setFmtMeta(nil, false)
	assert.Nil(t, v.fmtMeta())
	assert.Same(t, v.macroExp(), info)
	v.setMacroExp(nil)
	assert.False(t, v.hasAux)
	assert.Nil(t, v.source)
}

// A node without a location could carry either field.  It still reports no
// location, although its source pointer is now the record's carrier.
func TestNodeAuxWithoutLocation(t *testing.T) {
	v := Symbol("x")
	v.setMacroExp(&macroExpansionInfo{ID: 1})
	require.NotNil(t, v.macroExp())
	assert.Nil(t, v.loc())
	_, ok := v.Source()
	assert.False(t, ok)
	assert.True(t, needsStamp(v))

	loc := &token.Location{File: "a.lisp", Pos: 3}
	v.SetSource(loc)
	got, ok := v.Source()
	require.True(t, ok)
	assert.Equal(t, *loc, got)
	require.NotNil(t, v.macroExp(), "SetSource keeps the record")

	v.SetSource(nil)
	_, ok = v.Source()
	assert.False(t, ok)
	require.NotNil(t, v.macroExp(), "clearing the location keeps the record")
}

// The format-preserving parser hands a node a Location, stamps the node,
// and then writes end positions through the pointer it kept.
func TestNodeAuxParserKeepsItsLocationPointer(t *testing.T) {
	v := Symbol("x")
	loc := &token.Location{File: "a.lisp", Pos: 3}
	v.SetSource(loc)
	v.setFmtMeta(&fmtmeta.Meta{}, true)
	assert.Same(t, loc, v.source)
	loc.EndPos = 9
	got, _ := v.Source()
	assert.Equal(t, 9, got.EndPos)

	// The parser's Quote path: a header copy gets a Location of its own
	// and still shares the operand's metadata.
	outer := Quote(v) // a header copy
	loc2 := &token.Location{File: "a.lisp", Pos: 2}
	outer.SetSource(loc2)
	assert.Same(t, loc2, outer.source)
	assert.Same(t, v.fmtMeta(), outer.fmtMeta())
	got, _ = v.Source()
	assert.Equal(t, 3, got.Pos, "the operand keeps its own location")
}

// A Location that leaves the package is a plain position.
func TestNodeAuxDoesNotLeaveThePackage(t *testing.T) {
	v := Symbol("x")
	v.SetSource(&token.Location{File: "a.lisp", Pos: 3})
	v.setMacroExp(&macroExpansionInfo{ID: 1})
	got, ok := v.Source()
	require.True(t, ok)
	assert.Equal(t, token.Location{File: "a.lisp", Pos: 3}, got)

	w := Symbol("y")
	w.SetSource(&got)
	assert.Nil(t, w.macroExp())
	assert.Nil(t, locAux(v.source.Copy()))
}

// SetSource must not write a debugger record onto a Location it was handed:
// that Location may be shared with nodes other goroutines read.
func TestNodeAuxSetSourceCopiesForDebuggerRecord(t *testing.T) {
	v := Symbol("x")
	v.setMacroExp(&macroExpansionInfo{ID: 1})
	shared := &token.Location{File: "a.lisp", Pos: 3}
	v.SetSource(shared)
	assert.NotSame(t, shared, v.source)
	assert.Nil(t, locAux(shared))
	got, _ := v.Source()
	assert.Equal(t, *shared, got)
}

// Copy and detach kept a private copy of the formatter metadata and dropped
// the macro-expansion record.
func TestNodeAuxCopyAndDetach(t *testing.T) {
	v := Symbol("x")
	v.SetSource(&token.Location{File: "a.lisp", Pos: 3})
	m := &fmtmeta.Meta{OriginalText: "x"}
	v.setFmtMeta(m, true)
	v.setMacroExp(&macroExpansionInfo{ID: 1})

	cp := v.Copy()
	require.NotNil(t, cp.fmtMeta())
	assert.NotSame(t, m, cp.fmtMeta())
	assert.Equal(t, "x", cp.fmtMeta().OriginalText)
	assert.Nil(t, cp.macroExp())
	assert.NotSame(t, v.source, cp.source)

	plain := Symbol("y")
	plain.SetSource(&token.Location{File: "a.lisp", Pos: 3})
	plain.setMacroExp(&macroExpansionInfo{ID: 2})
	pc := plain.Copy()
	assert.False(t, pc.hasAux)
	got, ok := pc.Source()
	require.True(t, ok)
	assert.Equal(t, 3, got.Pos)

	bare := Symbol("z")
	bare.setMacroExp(&macroExpansionInfo{ID: 3})
	bc := bare.Copy()
	assert.False(t, bc.hasAux)
	assert.Nil(t, bc.source, "a carrier is not a location and is not copied")
}

// MacroExpansion hands out the call site and the definition site.  The
// definition site is the Location of the macro's own node, which can hold
// that node's tooling record.  The snapshot must not carry it out.
func TestNodeAuxDoesNotLeaveInMacroExpansion(t *testing.T) {
	// A macro's node as the format-preserving parser leaves it: metadata
	// on the node's own Location.
	macro := Symbol("m")
	defSite := &token.Location{File: "def.lisp", Pos: 7, Line: 2, Col: 1}
	macro.SetSource(defSite)
	macro.setFmtMeta(&fmtmeta.Meta{}, true)
	require.Same(t, defSite, macro.loc())
	require.NotNil(t, locAux(defSite))

	callSite := &token.Location{File: "call.lisp", Pos: 3, Line: 1, Col: 4}
	setLocAux(callSite, &nodeAux{meta: &fmtmeta.Meta{}})

	v := Symbol("x")
	v.setMacroExp(&macroExpansionInfo{ID: 1, macroExpansionContext: &macroExpansionContext{
		Name: "user:m", CallSite: callSite, DefSite: macro.loc(),
	}})
	m, ok := v.MacroExpansion()
	require.True(t, ok)
	assert.Equal(t, token.Location{File: "def.lisp", Pos: 7, Line: 2, Col: 1}, *m.DefSite)
	assert.Equal(t, token.Location{File: "call.lisp", Pos: 3, Line: 1, Col: 4}, *m.CallSite)
	assert.Nil(t, locAux(m.DefSite))
	assert.Nil(t, locAux(m.CallSite))
	assert.NotSame(t, defSite, m.DefSite)

	// No site: no Location.
	w := Symbol("y")
	w.setMacroExp(&macroExpansionInfo{ID: 2, macroExpansionContext: &macroExpansionContext{Name: "user:b"}})
	m, ok = w.MacroExpansion()
	require.True(t, ok)
	assert.Nil(t, m.CallSite)
	assert.Nil(t, m.DefSite)
}

// The other Locations that package lisp hands out are copies made by
// Location.Copy, which drops the record: an environment's location, a
// parsed node's inspection record and an error's location.
func TestNodeAuxDoesNotLeaveOnOtherCopies(t *testing.T) {
	loc := &token.Location{File: "a.lisp", Pos: 3, Line: 1, Col: 4}
	node := Symbol("x")
	node.SetSource(loc)
	node.setFmtMeta(&fmtmeta.Meta{}, true)
	require.NotNil(t, locAux(node.loc()))
	plain := token.Location{File: "a.lisp", Pos: 3, Line: 1, Col: 4}

	env := NewEnv(nil)
	env.loc = node.loc()
	assert.Equal(t, plain, *env.Source())

	lerr := env.ErrorfAt(node, "boom")
	got, ok := lerr.Source()
	require.True(t, ok)
	assert.Equal(t, plain, got)
	got, ok = (*ErrorVal)(lerr).Source()
	require.True(t, ok)
	assert.Equal(t, plain, got)

	lerr = env.Errorf("boom")
	got, ok = lerr.Source()
	require.True(t, ok)
	assert.Equal(t, plain, got)
	assert.Nil(t, locAux(lerr.source))
}
