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

// TestCopyDoesNotAliasMacroExpansionInfo is the second half of issue #466,
// under the contract lisp/copier.go settled on.  The struct is per node --
// its ID is the thing that tells one expansion node from another -- so a
// copy, which is a second node, must not write through the original's.  It
// was pinned as "the copy gets its own record"; Copy now DROPS the record
// outright, as Fork and detach do, because the record's shared context
// points at the tree the copy was made from
// (TestCopyDropsMacroExpansionMetadata in copier_test.go is the control
// for that).  The property this test pins is unchanged: nothing about the
// original's record moves when the copy is written to.
// CATCH: failed on 95e2e1a (the copy shared the original's struct).
func TestCopyDoesNotAliasMacroExpansionInfo(t *testing.T) {
	loc := &token.Location{File: "m.lisp", Line: 3, Col: 5}
	ctx := &macroExpansionContext{CallSite: loc, Name: "lisp:defun"}
	orig := newExpansionNode(7, ctx)

	cp := orig.Copy()
	assert.Nil(t, cp.macroExpansion,
		"a copy carries an expansion record; Copy drops it (lisp/copier.go), as Fork and detach do")
	require.NotNil(t, orig.macroExpansion, "copying moved the original's expansion info")
	assert.Equal(t, int64(7), orig.macroExpansion.ID, "copying changed the original's expansion ID")
}

// TestCopyKeepsSharingTheMacroExpansionContext pins that the shared half
// stays the ORIGINAL'S, untouched.  MacroExpansionContext describes the
// macro CALL, not the node; it is documented shared across every node of
// one expansion, and #456 made its CallSite an object the expansion owns.
// A copy has no record at all now, so the only thing left to hold is that
// copying neither replaces nor edits the context the original's nodes
// share.
//
// GUARD: passed before #466's fix (everything was shared then), pinned that
// the fix did not over-correct into a private context, and now pins that
// dropping the record on the copy leaves the original's sharing intact.
func TestCopyKeepsSharingTheMacroExpansionContext(t *testing.T) {
	loc := &token.Location{File: "m.lisp", Line: 3, Col: 5}
	ctx := &macroExpansionContext{CallSite: loc, Name: "lisp:defun"}
	a, b := newExpansionNode(1, ctx), newExpansionNode(2, ctx)
	require.Same(t, a.macroExpansion.macroExpansionContext, b.macroExpansion.macroExpansionContext,
		"premise: one expansion's nodes share one context")

	cp := a.Copy()
	assert.Nil(t, cp.macroExpansion, "a copy carries an expansion record; Copy drops it")
	assert.Same(t, ctx, a.macroExpansion.macroExpansionContext,
		"copying replaced the original's MacroExpansionContext")
	assert.Same(t, ctx, b.macroExpansion.macroExpansionContext,
		"copying one node moved the context its sibling shares")
	assert.Same(t, loc, ctx.CallSite, "copying moved the call site; it belongs to the expansion (#456)")
}

// TestCopyCarriesNoMacroExpansionID is the half of #466 where the DOC
// COMMENT was what was wrong rather than the code, under the decision
// lisp/copier.go settled on.
//
// ID was documented "unique per node".  (*LVal).Copy could not honour that
// while it carried the record across: it takes no *Runtime, so it had no
// counter to draw a fresh value from, and two nodes carrying one ID read to
// lisp/x/debugger's stepper (which steps on `loc.MacroID != s.start.MacroID`)
// as one node.  The decision then was to keep duplicating and say so
// (this test was TestCopyDuplicatesTheMacroExpansionID).  The decision now
// is that a copy carries NO record -- the record's shared context points at
// the tree the copy came from, and Fork and detach already drop it -- so the
// field comment's "unique per stamped node" is true again and the stepper
// hazard has no path.
//
// GUARD: a later change that starts carrying the record across, renumbered
// or not, fails here against a stated decision instead of quietly
// contradicting the field comment.
func TestCopyCarriesNoMacroExpansionID(t *testing.T) {
	ctx := &macroExpansionContext{Name: "lisp:defun"}
	orig := newExpansionNode(7, ctx)
	cp := orig.Copy()
	assert.Nil(t, cp.macroExpansion,
		"a copy carries an expansion record (and so an ID); Copy drops it (lisp/copier.go)."+
			" If that is intended to change, MacroExpansionInfo.ID's comment and this decision go with it")
	assert.Equal(t, int64(7), orig.macroExpansion.ID, "copying changed the original's expansion ID")
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
