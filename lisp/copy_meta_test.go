// Copyright © 2026 The ELPS authors

// Regression tests for elps#466 -- LVal.Copy sharing Meta and MacroExpansion.
//
// #446 (PR #467) separated LVal.Source, and the comment it left on Copy now
// says "the copy owns its positions".  The same `*cp = *v` carried the other
// two metadata pointers across untouched, so that sentence was true of the
// node and false one level down: SourceMeta holds []*token.Token, every token
// holds a *token.Location, and both trees reached the same objects.
//
// WHY THIS IS THE CODE AND NOT THE DOC COMMENT.  #466 leaves that open --
// "these are metadata ABOUT a node which a copy legitimately shares" is a
// coherent reading, and if it were the right one then Copy's doc comment and
// MacroExpansionInfo.ID's would be what needed fixing.  The evidence says it
// is the right reading for exactly one of the three pointers:
//
//   - SourceMeta is not a description of a node, it is per-node MUTABLE state
//     that the parser writes in place at a dozen sites, and that
//     rdparser.hoistOperandComments MOVES between nodes (append onto outer,
//     `= nil` on inner).  The parser already special-cases two LVals holding
//     one *SourceMeta -- the `outer.Meta == inner.Meta` guard, whose comment
//     explains that moving the comments would move them onto themselves.  An
//     in-tree guard against a state means the state is an anomaly, and
//     LVal.Copy manufactured it deliberately.  The comment-hoist test below
//     performs that exact move through a copy and watches the ORIGINAL's
//     rendered output change.
//
//   - MacroExpansionInfo is per node too (the ID is what distinguishes one
//     node from another), so the struct is separated.
//
//   - Its embedded *MacroExpansionContext genuinely is shared metadata: it
//     describes the macro CALL, it is documented "shared across all nodes in
//     one expansion", and #456 already made its CallSite an object the
//     expansion owns.  It stays shared, and the context test below pins that.
//
// The one place the doc comment WAS the thing that was wrong is
// MacroExpansionInfo.ID, documented "unique per node".  LVal.Copy cannot
// honour that under any implementation -- it takes no *Runtime and so has no
// counter to draw a fresh ID from -- so the comment now says what is true and
// names the consumer that cares (lisp/x/debugger's stepper, which steps on
// MacroID changing).  TestCopyDuplicatesTheMacroExpansionID pins it.
//
// LIVE OR LATENT: latent, on the same population argument #467 records.  The
// parser's Meta writes all happen on nodes it has just built, before anything
// can have copied them, and no non-test caller of LVal.Copy writes through
// Meta or MacroExpansion afterwards.  So the writes these tests perform are
// the TESTS' writes; they demonstrate the mechanism.  What is observed rather
// than demonstrated is the SHARING, which the tests assert directly and which
// failed on 95e2e1a.
//
// Tests marked GUARD pass before the fix as well as after.  They pin
// behaviour the fix must not break; they are not catches.

package lisp_test

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/formatter"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/rdparser"
	"github.com/luthersystems/elps/parser/token"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// readFormatting parses src in format-preserving mode, which is the only mode
// that populates Meta at all.
func readFormatting(t *testing.T, src string) []*lisp.LVal {
	t.Helper()
	sc := token.NewScanner("copy-meta.lisp", strings.NewReader(src))
	exprs, err := rdparser.NewFormatting(sc).ParseProgram()
	require.NoError(t, err)
	require.NotEmpty(t, exprs)
	return exprs
}

// metaSource is the probe from issue #466: one form with a leading comment and
// an inline trailing comment, so Meta carries both a []*token.Token and a
// single *token.Token.
const metaSource = "; lead\n(+ 1 2) ; trail\n"

// TestCopyDoesNotAliasSourceMeta is the reproduction from issue #466.
// CATCH: both assertions failed on 95e2e1a.
func TestCopyDoesNotAliasSourceMeta(t *testing.T) {
	orig := readFormatting(t, metaSource)[0]
	require.NotNil(t, orig.Meta, "premise: format-preserving parsing populates Meta")

	cp := orig.Copy()
	require.NotNil(t, cp.Meta, "the copy lost its formatting metadata")
	assert.NotSame(t, orig.Meta, cp.Meta,
		"a copy shares the original's *SourceMeta (#466)")

	was := orig.Meta.BlankLinesBefore
	cp.Meta.BlankLinesBefore = was + 99
	assert.Equal(t, was, orig.Meta.BlankLinesBefore,
		"a write through the copy moved the original's formatting metadata (#466)")
}

// TestCopyDoesNotAliasMetaCommentTokens is the half that reopens #446.  Each
// comment token holds a *token.Location, so a shared token means the copy and
// the original reach one mutable position object -- exactly what #446/#467
// separated on the node, still shared one level down through Meta.
// CATCH: every assertion failed on 95e2e1a.
func TestCopyDoesNotAliasMetaCommentTokens(t *testing.T) {
	orig := readFormatting(t, metaSource)[0]
	require.NotEmpty(t, orig.Meta.LeadingComments, "premise: the leading comment was recorded")
	require.NotNil(t, orig.Meta.TrailingComment, "premise: the trailing comment was recorded")

	cp := orig.Copy()
	require.Len(t, cp.Meta.LeadingComments, len(orig.Meta.LeadingComments))

	for i := range orig.Meta.LeadingComments {
		a, b := orig.Meta.LeadingComments[i], cp.Meta.LeadingComments[i]
		assert.NotSame(t, a, b, "LeadingComments[%d]: copy shares the original's *token.Token (#466)", i)
		assert.Equal(t, a.Text, b.Text, "LeadingComments[%d]: text differs", i)
		if a.Source != nil {
			require.NotNil(t, b.Source)
			assert.NotSame(t, a.Source, b.Source,
				"LeadingComments[%d]: copy shares the original's *token.Location, reopening #446 through Meta", i)
			assert.Equal(t, *a.Source, *b.Source, "LeadingComments[%d]: position differs", i)
		}
	}

	assert.NotSame(t, orig.Meta.TrailingComment, cp.Meta.TrailingComment,
		"TrailingComment: copy shares the original's *token.Token (#466)")
	assert.Equal(t, orig.Meta.TrailingComment.Text, cp.Meta.TrailingComment.Text)
	if orig.Meta.TrailingComment.Source != nil {
		assert.NotSame(t, orig.Meta.TrailingComment.Source, cp.Meta.TrailingComment.Source,
			"TrailingComment: copy shares the original's *token.Location (#446 through Meta)")
	}

	// The corruption the sharing enables.  The write is this test's.
	cp.Meta.LeadingComments[0].Source.Line = 9999
	assert.NotEqual(t, 9999, orig.Meta.LeadingComments[0].Source.Line,
		"a write through the copy's comment token moved the original's recorded position (#466)")
}

// TestCopyDoesNotAliasMetaAtDepth pins that the separation reaches every node
// Copy reaches, not just the root -- the shape #446's depth test had.
// CATCH: failed on 95e2e1a at every node that carried a Meta.
func TestCopyDoesNotAliasMetaAtDepth(t *testing.T) {
	const src = `; top
(defun f (x) ; on the formals
  ; on the body
  (let ([y (+ x 1)])
    (* y y)))
`
	orig := readFormatting(t, src)[0]
	cp := orig.Copy()

	origNodes := flatten(orig)
	cpNodes := flatten(cp)
	require.Len(t, cpNodes, len(origNodes), "copy has a different shape")
	require.Greater(t, len(origNodes), 10, "test program is too small to be interesting")

	shared, withMeta := 0, 0
	for i := range origNodes {
		if origNodes[i].Meta == nil {
			assert.Nil(t, cpNodes[i].Meta, "node %d: nil Meta became non-nil", i)
			continue
		}
		withMeta++
		if origNodes[i].Meta == cpNodes[i].Meta {
			shared++
		}
	}
	require.Positive(t, withMeta, "no node carried a Meta; the test observed nothing")
	assert.Zero(t, shared,
		"%d of %d copied nodes with metadata share the original's *SourceMeta (#466)",
		shared, withMeta)
}

// TestCopyMetaSurvivesACommentHoist is the consequence, stated as OUTPUT
// rather than as a pointer comparison.
//
// rdparser.hoistOperandComments moves a node's LeadingComments onto another
// node: append onto the destination, `= nil` on the source.  That is a write
// the parser really performs -- it is the fix for the comment loss described
// in its own doc comment -- and it is destructive.  Performed through a copy
// while a shared *SourceMeta is in place, it deletes the comment from the
// ORIGINAL tree, and the original then formats without it.
//
// The move here is this test's, not the parser's (the parser runs before
// anything can copy).  What the test shows is what the shared object COSTS
// when a writer does reach it: not a debugging curiosity, a comment silently
// dropped from a file `elps fmt` rewrites.
//
// CATCH: on 95e2e1a the original's formatted output lost its leading comment.
func TestCopyMetaSurvivesACommentHoist(t *testing.T) {
	exprs := readFormatting(t, metaSource)
	orig := exprs[0]
	before := string(formatter.FormatProgram([]*lisp.LVal{orig}, nil, nil))
	require.Contains(t, before, "; lead", "premise: the comment is in the formatted output")

	cp := orig.Copy()
	// Exactly what hoistOperandComments does, performed on the copy.
	moved := cp.Meta.LeadingComments
	require.NotEmpty(t, moved)
	cp.Meta.LeadingComments = nil

	after := string(formatter.FormatProgram([]*lisp.LVal{orig}, nil, nil))
	assert.Equal(t, before, after,
		"moving comments off the COPY changed how the ORIGINAL formats (#466):\nbefore:\n%s\nafter:\n%s",
		before, after)
	assert.Contains(t, after, "; lead",
		"the original tree lost a source comment because a copy hoisted it (#466)")
}

// TestCopyMetaCommentSlicesArePrivate is the slice-header half of the same
// property: even without touching a token, appending to one tree's comment
// list must not be visible in the other's.  A shared backing array can make
// an append visible through both headers.
// CATCH: failed on 95e2e1a (one header, so the append was simply shared).
func TestCopyMetaCommentSlicesArePrivate(t *testing.T) {
	orig := readFormatting(t, metaSource)[0]
	cp := orig.Copy()

	origLen := len(orig.Meta.LeadingComments)
	require.Positive(t, origLen)

	cp.Meta.LeadingComments = append(cp.Meta.LeadingComments,
		&token.Token{Type: token.COMMENT, Text: "; added"})
	assert.Len(t, orig.Meta.LeadingComments, origLen,
		"appending to the copy's LeadingComments changed the original's (#466)")
}

// TestCopyPreservesMetaContent pins that separating the objects does not
// change what they say.  A fix that dropped Meta, or that materialised an
// empty one, would pass every NotSame assertion above.
// GUARD: passes before the fix.
func TestCopyPreservesMetaContent(t *testing.T) {
	orig := readFormatting(t, "; lead\n\n[foo 1] ; trail\n")[0]
	cp := orig.Copy()
	require.NotNil(t, cp.Meta)

	assert.Equal(t, orig.Meta.OriginalText, cp.Meta.OriginalText)
	assert.Equal(t, orig.Meta.BracketType, cp.Meta.BracketType)
	assert.Equal(t, orig.Meta.BlankLinesBefore, cp.Meta.BlankLinesBefore)
	assert.Equal(t, orig.Meta.BlankLinesAfterComments, cp.Meta.BlankLinesAfterComments)
	assert.Equal(t, orig.Meta.PrecedingSpaces, cp.Meta.PrecedingSpaces)
	assert.Equal(t, orig.Meta.NewlineBefore, cp.Meta.NewlineBefore)
	assert.Equal(t, orig.Meta.ClosingBracketNewline, cp.Meta.ClosingBracketNewline)
	assert.Len(t, cp.Meta.LeadingComments, len(orig.Meta.LeadingComments))
	assert.Len(t, cp.Meta.InnerTrailingComments, len(orig.Meta.InnerTrailingComments))

	// And the copy formats identically, which is the property a reader of
	// Meta actually depends on.
	assert.Equal(t,
		string(formatter.FormatProgram([]*lisp.LVal{orig}, nil, nil)),
		string(formatter.FormatProgram([]*lisp.LVal{cp}, nil, nil)),
		"a copy formats differently from the tree it copied")
}

// TestCopyPreservesNilMeta pins that a nil Meta stays nil.  Nil means "not
// parsed in format-preserving mode" and every reader branches on it;
// materialising a zero SourceMeta would make an ordinary parse tree look
// format-preserving and would put an allocation on the hot path.
// GUARD: passes before the fix.
func TestCopyPreservesNilMeta(t *testing.T) {
	v := lisp.SExpr([]*lisp.LVal{lisp.Symbol("a"), lisp.Int(1)})
	require.Nil(t, v.Meta, "premise: a natively-built value has no Meta")
	cp := v.Copy()
	assert.Nil(t, cp.Meta)
	require.Len(t, cp.Cells, 2)
	for i, c := range cp.Cells {
		assert.Nil(t, c.Meta, "cell %d gained a Meta", i)
	}
}

// TestCopyPreservesNilComments pins that nil comment slices stay nil rather
// than becoming empty non-nil ones.  Not cosmetic: it is what keeps a copy of
// a Meta with no comments from allocating two slice headers per node on the
// formatting path.
// GUARD: passes before the fix (there was one Meta, so its nil slices were
// trivially the original's).  It is here so the fix cannot buy separation by
// materialising empty slices per node.
func TestCopyPreservesNilComments(t *testing.T) {
	v := lisp.Symbol("a")
	v.Meta = &lisp.SourceMeta{OriginalText: "a"}
	cp := v.Copy()
	require.NotNil(t, cp.Meta)
	assert.Nil(t, cp.Meta.LeadingComments)
	assert.Nil(t, cp.Meta.InnerTrailingComments)
	assert.Nil(t, cp.Meta.TrailingComment)
}

// newExpansionNode builds a node in the state stampMacroExpansion leaves an
// expansion node in: a MacroExpansionInfo with an ID, wrapping a context
// shared with the rest of the expansion.
func newExpansionNode(id int64, ctx *lisp.MacroExpansionContext) *lisp.LVal {
	v := lisp.Symbol("expanded")
	v.MacroExpansion = &lisp.MacroExpansionInfo{MacroExpansionContext: ctx, ID: id}
	return v
}

// TestCopyDoesNotAliasMacroExpansionInfo is the second half of issue #466.
// The struct is per node -- its ID is the thing that tells one expansion node
// from another -- so a copy, which is a second node, must not write through
// the original's.
// CATCH: failed on 95e2e1a.
func TestCopyDoesNotAliasMacroExpansionInfo(t *testing.T) {
	loc := &token.Location{File: "m.lisp", Line: 3, Col: 5}
	ctx := &lisp.MacroExpansionContext{CallSite: loc, Name: "lisp:defun"}
	orig := newExpansionNode(7, ctx)

	cp := orig.Copy()
	require.NotNil(t, cp.MacroExpansion, "the copy lost its expansion info")
	assert.NotSame(t, orig.MacroExpansion, cp.MacroExpansion,
		"a copy shares the original's *MacroExpansionInfo (#466)")

	cp.MacroExpansion.ID = 99
	assert.Equal(t, int64(7), orig.MacroExpansion.ID,
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
	ctx := &lisp.MacroExpansionContext{CallSite: loc, Name: "lisp:defun"}
	a, b := newExpansionNode(1, ctx), newExpansionNode(2, ctx)
	require.Same(t, a.MacroExpansion.MacroExpansionContext, b.MacroExpansion.MacroExpansionContext,
		"premise: one expansion's nodes share one context")

	cp := a.Copy()
	assert.Same(t, ctx, cp.MacroExpansion.MacroExpansionContext,
		"copying an expansion node allocated a private MacroExpansionContext; the context"+
			" is documented shared across an expansion and has only one owner")
	assert.Same(t, loc, cp.MacroExpansion.CallSite,
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
	ctx := &lisp.MacroExpansionContext{Name: "lisp:defun"}
	orig := newExpansionNode(7, ctx)
	cp := orig.Copy()
	require.NotNil(t, cp.MacroExpansion)
	assert.Equal(t, int64(7), cp.MacroExpansion.ID,
		"a copy no longer carries the expansion ID of the node it came from;"+
			" if that is intended, MacroExpansionInfo.ID's comment needs updating with it")
}

// TestCopyPreservesNilMacroExpansion pins the nil case, which is every node in
// a process with no debugger attached -- i.e. the hot path.
// GUARD: passes before the fix.
func TestCopyPreservesNilMacroExpansion(t *testing.T) {
	v := lisp.SExpr([]*lisp.LVal{lisp.Symbol("a")})
	require.Nil(t, v.MacroExpansion)
	cp := v.Copy()
	assert.Nil(t, cp.MacroExpansion)
	require.Len(t, cp.Cells, 1)
	assert.Nil(t, cp.Cells[0].MacroExpansion)
}

// TestTokenCopy covers parser/token.Token.Copy directly, since SourceMeta.Copy
// leans on it for the position separation.  A Token whose Copy shared its
// Location would put #446 straight back through Meta.
//
// NEITHER a catch nor a guard: Token.Copy did not exist on 95e2e1a, so there
// was nothing for it to fail against.  It is unit cover for new API, and it is
// labelled so rather than left to read as a catch.
func TestTokenCopy(t *testing.T) {
	assert.Nil(t, (*token.Token)(nil).Copy(), "nil must stay nil")

	loc := &token.Location{File: "t.lisp", Pos: 4, Line: 2, Col: 3, EndPos: 10, EndLine: 2, EndCol: 9}
	tok := &token.Token{Type: token.COMMENT, Text: "; c", Source: loc, PrecedingNewlines: 2, PrecedingSpaces: 1}

	cp := tok.Copy()
	require.NotNil(t, cp)
	assert.NotSame(t, tok, cp)
	assert.Equal(t, tok.Type, cp.Type)
	assert.Equal(t, tok.Text, cp.Text)
	assert.Equal(t, tok.PrecedingNewlines, cp.PrecedingNewlines)
	assert.Equal(t, tok.PrecedingSpaces, cp.PrecedingSpaces)

	require.NotNil(t, cp.Source)
	assert.NotSame(t, loc, cp.Source, "a copied Token shares the original's *token.Location")
	assert.Equal(t, *loc, *cp.Source)

	cp.Source.Line = 99
	assert.Equal(t, 2, loc.Line, "a write through the copy moved the original's position")

	// A Token with no position keeps none rather than gaining a zero one.
	bare := (&token.Token{Type: token.COMMENT, Text: "; c"}).Copy()
	assert.Nil(t, bare.Source)
}
