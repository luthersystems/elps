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
	inner.source = nativeSource()
	expr := SExpr([]*LVal{inner, Int(1), Int(2)})
	expr.source = nativeSource()

	stampMacroExpansion(expr, callSite, nil, rt)

	// Source should be stamped.
	assert.Equal(t, callSite, expr.source)
	assert.Equal(t, callSite, inner.source)

	// MacroExpansion should remain nil.
	assert.Nil(t, expr.MacroExpansion)
	assert.Nil(t, inner.MacroExpansion)
}

func TestStampMacroExpansion_WithContext(t *testing.T) {
	// With a context (debugger attached), MacroExpansion should be populated
	// on nodes that get stamped (synthetic source) and have unique IDs.
	callSite := &token.Location{File: "test.lisp", Line: 5, Col: 1}
	rt := StandardRuntime()
	ctx := &MacroExpansionContext{
		CallSite: callSite,
		Name:     "lisp:defun",
		Args:     []*LVal{Symbol("my-fn"), SExpr([]*LVal{Symbol("x")})},
	}

	inner := Symbol("+")
	inner.source = nativeSource()
	arg1 := Int(1)
	arg1.source = nativeSource()
	arg2 := Int(2)
	arg2.source = nativeSource()
	expr := SExpr([]*LVal{inner, arg1, arg2})
	expr.source = nativeSource()

	stampMacroExpansion(expr, callSite, ctx, rt)

	// All nodes should have MacroExpansion set.
	require.NotNil(t, expr.MacroExpansion)
	require.NotNil(t, inner.MacroExpansion)
	require.NotNil(t, arg1.MacroExpansion)
	require.NotNil(t, arg2.MacroExpansion)

	// All nodes should share the same context.
	assert.Equal(t, ctx, expr.MacroExpansion.MacroExpansionContext)
	assert.Equal(t, ctx, inner.MacroExpansion.MacroExpansionContext)

	// All IDs should be unique and monotonically increasing.
	ids := []int64{
		expr.MacroExpansion.ID,
		inner.MacroExpansion.ID,
		arg1.MacroExpansion.ID,
		arg2.MacroExpansion.ID,
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
	ctx := &MacroExpansionContext{
		CallSite: callSite,
		Name:     "lisp:defun",
	}

	// Node with real source (from unquote).
	node := Symbol("x")
	node.source = realSource

	// Node with synthetic source.
	synth := Symbol("+")
	synth.source = nativeSource()

	expr := SExpr([]*LVal{synth, node})
	expr.source = nativeSource()

	stampMacroExpansion(expr, callSite, ctx, rt)

	// Real source node should keep its source and have NO MacroExpansion.
	assert.Equal(t, realSource, node.source)
	assert.Nil(t, node.MacroExpansion)

	// Synthetic nodes should be stamped.
	assert.Equal(t, callSite, synth.source)
	require.NotNil(t, synth.MacroExpansion)
	assert.Equal(t, "lisp:defun", synth.MacroExpansion.Name)
}

func TestStampMacroExpansion_SkipsSingletonNil(t *testing.T) {
	// Singleton nil (empty SExpr) must not be mutated.
	callSite := &token.Location{File: "test.lisp", Line: 5, Col: 1}
	rt := StandardRuntime()
	ctx := &MacroExpansionContext{
		CallSite: callSite,
		Name:     "lisp:defun",
	}

	nilVal := Nil() // singleton

	stampMacroExpansion(nilVal, callSite, ctx, rt)

	// Should NOT have been stamped.
	assert.Nil(t, nilVal.MacroExpansion)
}

// TestStampMacroExpansion_SkipsSingletonTrue verifies that Bool(true)
// — an LSymbol singleton with Source.Pos == -1 — is not mutated by
// macro expansion stamping. A type-based guard catches only singletonNil
// (the empty LSExpr); identity-based guarding is required for the two
// Bool singletons. See issue #274.
func TestStampMacroExpansion_SkipsSingletonTrue(t *testing.T) {
	callSite := &token.Location{File: "test.lisp", Line: 5, Col: 1, Pos: 0}
	rt := StandardRuntime()
	ctx := &MacroExpansionContext{CallSite: callSite, Name: "lisp:defun"}

	origSource := Bool(true).source

	trueVal := Bool(true) // singleton
	stampMacroExpansion(trueVal, callSite, ctx, rt)

	assert.Nil(t, trueVal.MacroExpansion, "Bool(true) singleton was mutated (MacroExpansion)")
	assert.Equal(t, origSource, trueVal.source, "Bool(true) singleton was mutated (Source)")
	assert.Nil(t, Bool(true).MacroExpansion, "Bool(true) singleton corruption is shared")
	assert.Equal(t, origSource, Bool(true).source, "Bool(true) singleton corruption is shared (Source)")
}

// TestStampMacroExpansion_SkipsSingletonFalse mirrors the Bool(true)
// case. See TestStampMacroExpansion_SkipsSingletonTrue.
func TestStampMacroExpansion_SkipsSingletonFalse(t *testing.T) {
	callSite := &token.Location{File: "test.lisp", Line: 5, Col: 1, Pos: 0}
	rt := StandardRuntime()
	ctx := &MacroExpansionContext{CallSite: callSite, Name: "lisp:defun"}

	origSource := Bool(false).source

	falseVal := Bool(false) // singleton
	stampMacroExpansion(falseVal, callSite, ctx, rt)

	assert.Nil(t, falseVal.MacroExpansion, "Bool(false) singleton was mutated (MacroExpansion)")
	assert.Equal(t, origSource, falseVal.source, "Bool(false) singleton was mutated (Source)")
	assert.Nil(t, Bool(false).MacroExpansion, "Bool(false) singleton corruption is shared")
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
