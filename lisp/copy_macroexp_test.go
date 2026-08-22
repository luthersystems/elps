// Copyright © 2026 The ELPS authors

package lisp

import (
	"testing"

	"github.com/luthersystems/elps/parser/token"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// The macro-expansion half of the elps#466 regression suite.
//
// It lives in package lisp rather than alongside the Meta half in
// copy_meta_test.go because the properties it pins are about the IDENTITY of
// the unexported macroExpansionInfo and macroExpansionContext structs (issue
// #382) -- which node's write reaches which other node -- and a snapshot
// accessor cannot express that.  MacroExpansion() deliberately hands out a
// copy; asking it whether two nodes share a struct would always answer no.
// The Meta half stays external because it needs parser/rdparser, which
// package lisp cannot import.

// newExpansionNode builds a node in the state stampMacroExpansion leaves an
// expansion node in: a MacroExpansionInfo with an ID, wrapping a context
// shared with the rest of the expansion.
func newExpansionNode(id int64, ctx *macroExpansionContext) *LVal {
	v := Symbol("expanded")
	v.macroExpansion = &macroExpansionInfo{macroExpansionContext: ctx, ID: id}
	return v
}

// TestCopyDoesNotAliasMacroExpansionInfo is the second half of issue #466.
// The struct is per node -- its ID is the thing that tells one expansion node
// from another -- so a copy, which is a second node, must not write through
// the original's.
// CATCH: failed on 95e2e1a.
func TestCopyDoesNotAliasMacroExpansionInfo(t *testing.T) {
	loc := &token.Location{File: "m.lisp", Line: 3, Col: 5}
	ctx := &macroExpansionContext{CallSite: loc, Name: "lisp:defun"}
	orig := newExpansionNode(7, ctx)

	cp := orig.Copy()
	require.NotNil(t, cp.macroExpansion, "the copy lost its expansion info")
	assert.NotSame(t, orig.macroExpansion, cp.macroExpansion,
		"a copy shares the original's *MacroExpansionInfo (#466)")

	cp.macroExpansion.ID = 99
	assert.Equal(t, int64(7), orig.macroExpansion.ID,
		"a write through the copy moved the original's expansion ID (#466)")
}

// TestCopyKeepsSharingTheMacroExpansionContext pins the pointer that is
// deliberately NOT separated, and is the reason this issue is not simply
// "copy everything".
//
// MacroExpansionContext describes the macro CALL, not the node.  It is
// documented shared across every node of one expansion; #456 already made its
// CallSite an object the expansion owns rather than one borrowed from a live
// parse tree, so there is no third party to separate it from.  Copying it
// would separate nothing and would make that documented sharing false for
// copied nodes.
//
// GUARD: passes before the fix (everything was shared then) and pins that the
// fix did not over-correct.
func TestCopyKeepsSharingTheMacroExpansionContext(t *testing.T) {
	loc := &token.Location{File: "m.lisp", Line: 3, Col: 5}
	ctx := &macroExpansionContext{CallSite: loc, Name: "lisp:defun"}
	a, b := newExpansionNode(1, ctx), newExpansionNode(2, ctx)
	require.Same(t, a.macroExpansion.macroExpansionContext, b.macroExpansion.macroExpansionContext,
		"premise: one expansion's nodes share one context")

	cp := a.Copy()
	assert.Same(t, ctx, cp.macroExpansion.macroExpansionContext,
		"copying an expansion node allocated a private MacroExpansionContext; the context"+
			" is documented shared across an expansion and has only one owner")
	assert.Same(t, loc, cp.macroExpansion.CallSite,
		"the call site moved; it belongs to the expansion (#456), not to the node")
}

// TestCopyDuplicatesTheMacroExpansionID is the half of #466 where the DOC
// COMMENT was what was wrong rather than the code.
//
// ID was documented "unique per node".  LVal.Copy cannot honour that under
// any implementation: it takes no *Runtime, so it has no counter to draw a
// fresh value from, and drawing one from anywhere else would defeat the
// point -- the value exists to come from the runtime that did the expanding.
// So the behaviour stands and the comment now says what is true, and names
// the consumer that has to know: lisp/x/debugger's stepper steps on
// `loc.MacroID != s.start.MacroID`, so two nodes carrying one ID read to it
// as one node.
//
// GUARD: passes before the fix.  It is here so that a later change which
// starts renumbering copies fails against a stated decision instead of
// quietly contradicting the field comment.
func TestCopyDuplicatesTheMacroExpansionID(t *testing.T) {
	ctx := &macroExpansionContext{Name: "lisp:defun"}
	orig := newExpansionNode(7, ctx)
	cp := orig.Copy()
	require.NotNil(t, cp.macroExpansion)
	assert.Equal(t, int64(7), cp.macroExpansion.ID,
		"a copy no longer carries the expansion ID of the node it came from;"+
			" if that is intended, MacroExpansionInfo.ID's comment needs updating with it")
}

// TestCopyPreservesNilMacroExpansion pins the nil case, which is every node in
// a process with no debugger attached -- i.e. the hot path.
// GUARD: passes before the fix.
func TestCopyPreservesNilMacroExpansion(t *testing.T) {
	v := SExpr([]*LVal{Symbol("a")})
	require.Nil(t, v.macroExpansion)
	cp := v.Copy()
	assert.Nil(t, cp.macroExpansion)
	require.Len(t, cp.Cells, 1)
	assert.Nil(t, cp.Cells[0].macroExpansion)
}
