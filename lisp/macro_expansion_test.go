package lisp

import (
	"testing"

	"github.com/luthersystems/elps/parser/token"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestStampMacroExpansion_NoContext(t *testing.T) {
	// Without a context (no debugger), MacroExpansion should remain nil.
	callSite := &token.Location{File: "test.lisp", Line: 5, Col: 1}
	rt := StandardRuntime()

	inner := Symbol("+")
	// Give it a synthetic source (Pos < 0).
	inner.source = nil
	expr := SExpr([]*LVal{inner, Int(1), Int(2)})
	expr.source = nil

	got := stampMacroExpansion(expr, callSite, nil, rt)

	// The stamp is copy-on-write (issue #582): the returned tree is
	// stamped, the input is untouched.
	require.NotSame(t, expr, got)
	assert.Equal(t, callSite, got.source)
	assert.Equal(t, callSite, got.Cells[0].source)
	assert.Nil(t, expr.source)
	assert.Nil(t, inner.source)

	// MacroExpansion should remain nil.
	assert.Nil(t, got.macroExpansion)
	assert.Nil(t, got.Cells[0].macroExpansion)
}

func TestStampMacroExpansion_WithContext(t *testing.T) {
	// With a context (debugger attached), MacroExpansion should be populated
	// on nodes that get stamped (synthetic source) and have unique IDs.
	callSite := &token.Location{File: "test.lisp", Line: 5, Col: 1}
	rt := StandardRuntime()
	ctx := &macroExpansionContext{
		CallSite: callSite,
		Name:     "lisp:defun",
		Args:     []*LVal{Symbol("my-fn"), SExpr([]*LVal{Symbol("x")})},
	}

	inner := Symbol("+")
	inner.source = nil
	arg1 := Int(1)
	arg1.source = nil
	arg2 := Int(2)
	arg2.source = nil
	expr := SExpr([]*LVal{inner, arg1, arg2})
	expr.source = nil

	got := stampMacroExpansion(expr, callSite, ctx, rt)

	// Copy-on-write (issue #582): the metadata lands on the returned tree
	// and the input is untouched.
	for _, orig := range []*LVal{expr, inner, arg1, arg2} {
		assert.Nil(t, orig.macroExpansion, "the input was written to")
		assert.Nil(t, orig.source, "the input was written to")
	}
	expr, inner, arg1, arg2 = got, got.Cells[0], got.Cells[1], got.Cells[2]

	// All nodes should have MacroExpansion set.
	require.NotNil(t, expr.macroExpansion)
	require.NotNil(t, inner.macroExpansion)
	require.NotNil(t, arg1.macroExpansion)
	require.NotNil(t, arg2.macroExpansion)

	// All nodes should share the same context.
	assert.Equal(t, ctx, expr.macroExpansion.macroExpansionContext)
	assert.Equal(t, ctx, inner.macroExpansion.macroExpansionContext)

	// All IDs should be unique and monotonically increasing.
	ids := []int64{
		expr.macroExpansion.ID,
		inner.macroExpansion.ID,
		arg1.macroExpansion.ID,
		arg2.macroExpansion.ID,
	}
	for i := 1; i < len(ids); i++ {
		assert.Greater(t, ids[i], ids[i-1], "IDs should be monotonically increasing")
	}
}

func TestStampMacroExpansion_PreservesRealSource(t *testing.T) {
	// Nodes with valid source locations (from parser) should not be stamped.
	callSite := &token.Location{File: "test.lisp", Line: 5, Col: 1}
	realSource := &token.Location{File: "test.lisp", Line: 10, Col: 3, Pos: 42}
	rt := StandardRuntime()
	ctx := &macroExpansionContext{
		CallSite: callSite,
		Name:     "lisp:defun",
	}

	// Node with real source (from unquote).
	node := Symbol("x")
	node.source = realSource

	// Node with synthetic source.
	synth := Symbol("+")
	synth.source = nil

	expr := SExpr([]*LVal{synth, node})
	expr.source = nil

	got := stampMacroExpansion(expr, callSite, ctx, rt)

	// Real source node is shared, keeps its source and has NO
	// MacroExpansion.
	assert.Same(t, node, got.Cells[1])
	assert.Equal(t, realSource, node.source)
	assert.Nil(t, node.macroExpansion)

	// Synthetic nodes are stamped on the copy; the input is untouched
	// (issue #582).
	stamped := got.Cells[0]
	require.NotSame(t, synth, stamped)
	assert.Equal(t, callSite, stamped.source)
	require.NotNil(t, stamped.macroExpansion)
	assert.Equal(t, "lisp:defun", stamped.macroExpansion.Name)
	assert.Nil(t, synth.source)
	assert.Nil(t, synth.macroExpansion)
}

func TestStampMacroExpansion_SkipsSingletonNil(t *testing.T) {
	// Singleton nil (empty SExpr) must not be mutated.
	callSite := &token.Location{File: "test.lisp", Line: 5, Col: 1}
	rt := StandardRuntime()
	ctx := &macroExpansionContext{
		CallSite: callSite,
		Name:     "lisp:defun",
	}

	nilVal := Nil() // singleton

	stampMacroExpansion(nilVal, callSite, ctx, rt)

	// Should NOT have been stamped.
	assert.Nil(t, nilVal.macroExpansion)
}

// TestStampMacroExpansion_SkipsSingletonTrue verifies that Bool(true)
// — an LSymbol singleton with Source.Pos == -1 — is not mutated by
// macro expansion stamping. A type-based guard catches only singletonNil
// (the empty LSExpr); identity-based guarding is required for the two
// Bool singletons. See issue #274.
func TestStampMacroExpansion_SkipsSingletonTrue(t *testing.T) {
	callSite := &token.Location{File: "test.lisp", Line: 5, Col: 1, Pos: 0}
	rt := StandardRuntime()
	ctx := &macroExpansionContext{CallSite: callSite, Name: "lisp:defun"}

	origSource := Bool(true).source

	trueVal := Bool(true) // singleton
	stampMacroExpansion(trueVal, callSite, ctx, rt)

	assert.Nil(t, trueVal.macroExpansion, "Bool(true) singleton was mutated (MacroExpansion)")
	assert.Equal(t, origSource, trueVal.source, "Bool(true) singleton was mutated (Source)")
	assert.Nil(t, Bool(true).macroExpansion, "Bool(true) singleton corruption is shared")
	assert.Equal(t, origSource, Bool(true).source, "Bool(true) singleton corruption is shared (Source)")
}

// TestStampMacroExpansion_SkipsSingletonFalse mirrors the Bool(true)
// case. See TestStampMacroExpansion_SkipsSingletonTrue.
func TestStampMacroExpansion_SkipsSingletonFalse(t *testing.T) {
	callSite := &token.Location{File: "test.lisp", Line: 5, Col: 1, Pos: 0}
	rt := StandardRuntime()
	ctx := &macroExpansionContext{CallSite: callSite, Name: "lisp:defun"}

	origSource := Bool(false).source

	falseVal := Bool(false) // singleton
	stampMacroExpansion(falseVal, callSite, ctx, rt)

	assert.Nil(t, falseVal.macroExpansion, "Bool(false) singleton was mutated (MacroExpansion)")
	assert.Equal(t, origSource, falseVal.source, "Bool(false) singleton was mutated (Source)")
	assert.Nil(t, Bool(false).macroExpansion, "Bool(false) singleton corruption is shared")
	assert.Equal(t, origSource, Bool(false).source, "Bool(false) singleton corruption is shared (Source)")
}

func TestRuntimeMacroExpSeq(t *testing.T) {
	rt := StandardRuntime()
	id1 := rt.nextMacroExpID()
	id2 := rt.nextMacroExpID()
	id3 := rt.nextMacroExpID()

	assert.Equal(t, int64(1), id1)
	assert.Equal(t, int64(2), id2)
	assert.Equal(t, int64(3), id3)
}

// TestMacroExpansionAccessor exercises the exported read-only snapshot
// (issue #382): the metadata storage is unexported, so external packages
// (the debugger, downstream tooling) observe expansion metadata only
// through (*LVal).MacroExpansion.
func TestMacroExpansionAccessor(t *testing.T) {
	// Nil receiver and unstamped values report false.
	var nilVal *LVal
	_, ok := nilVal.MacroExpansion()
	assert.False(t, ok)
	_, ok = Symbol("x").MacroExpansion()
	assert.False(t, ok)

	// Info with a nil context reports false: the in-kernel stamp never
	// creates that state (it embeds the context it was handed), so the
	// accessor treats it as no-metadata rather than exposing a snapshot
	// with an empty name.
	broken := Symbol("+")
	broken.macroExpansion = &macroExpansionInfo{ID: 1}
	_, ok = broken.MacroExpansion()
	assert.False(t, ok)

	// A stamped node yields a copy of the metadata.
	callSite := &token.Location{File: "test.lisp", Line: 5, Col: 1, Pos: -1}
	args := []*LVal{Symbol("my-fn")}
	rt := StandardRuntime()
	ctx := &macroExpansionContext{CallSite: callSite, Name: "lisp:defun", Args: args}
	expr := SExpr([]*LVal{Symbol("+")})
	expr.source = nil
	expr = stampMacroExpansion(expr, callSite, ctx, rt)

	m, ok := expr.MacroExpansion()
	require.True(t, ok)
	assert.Equal(t, "lisp:defun", m.Name)
	assert.Equal(t, int64(1), m.ID)
	require.NotNil(t, m.CallSite)
	assert.Equal(t, *callSite, *m.CallSite)
	assert.NotSame(t, callSite, m.CallSite, "CallSite must be a copy")
	assert.Nil(t, m.DefSite)
	require.Len(t, m.Args, 1)
	assert.Same(t, args[0], m.Args[0], "arg nodes are the shared originals")

	// The snapshot is detached: mutating it must not touch the stored
	// metadata.
	m.CallSite.Line = 999
	m.Args[0] = nil
	m2, ok := expr.MacroExpansion()
	require.True(t, ok)
	assert.Equal(t, 5, m2.CallSite.Line)
	assert.Same(t, args[0], m2.Args[0])
}
