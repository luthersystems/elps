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
	inner.Source = nativeSource()
	expr := SExpr([]*LVal{inner, Int(1), Int(2)})
	expr.Source = nativeSource()

	stampMacroExpansion(expr, callSite, nil, rt)

	// Source should be stamped.
	assert.Equal(t, callSite, expr.Source)
	assert.Equal(t, callSite, inner.Source)

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
	inner.Source = nativeSource()
	arg1 := Int(1)
	arg1.Source = nativeSource()
	arg2 := Int(2)
	arg2.Source = nativeSource()
	expr := SExpr([]*LVal{inner, arg1, arg2})
	expr.Source = nativeSource()

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
	node.Source = realSource

	// Node with synthetic source.
	synth := Symbol("+")
	synth.Source = nativeSource()

	expr := SExpr([]*LVal{synth, node})
	expr.Source = nativeSource()

	stampMacroExpansion(expr, callSite, ctx, rt)

	// Real source node should keep its source and have NO MacroExpansion.
	assert.Equal(t, realSource, node.Source)
	assert.Nil(t, node.MacroExpansion)

	// Synthetic nodes should be stamped.
	assert.Equal(t, callSite, synth.Source)
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

func TestRuntimeMacroExpSeq(t *testing.T) {
	rt := StandardRuntime()
	id1 := rt.nextMacroExpID()
	id2 := rt.nextMacroExpID()
	id3 := rt.nextMacroExpID()

	assert.Equal(t, int64(1), id1)
	assert.Equal(t, int64(2), id2)
	assert.Equal(t, int64(3), id3)
}
