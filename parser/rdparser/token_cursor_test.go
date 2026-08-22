// Copyright © 2026 The ELPS authors

// Tests for the token-under-the-cursor invariant, elps#430.
//
// Two halves, with different status:
//
//   - TestAccessorsBeforeFirstScan is a CATCH.  Parser.Location(),
//     TokenText() and TokenType() dereferenced p.src.Token unconditionally,
//     and TokenSource.Token starts nil, so an embedder constructing a Parser
//     through the exported API and asking it anything before parsing got
//     "invalid memory address or nil pointer dereference".  Red on the parent
//     commit -- it panics there -- green after.
//
//   - TestTokenLValEndPositionGuard is a GUARD.  The `p.src.Token != nil`
//     conjunct removed from tokenLVal could not fail: Location() had already
//     dereferenced that same pointer one line above it, so the guard was
//     unreachable-as-false and its only effect was to advertise an invariant
//     ("this may be nil here") that the rest of the file does not honour.
//     Deleting dead code fixes no behaviour; this pins that the end positions
//     it appeared to guard are still written for every parsed node, so the
//     deletion is visibly inert.

package rdparser

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/token"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// unstartedParser is the shape from elps#430: a Parser built through the
// exported API, driven by an embedder's own TokenStream, on which nothing has
// been scanned yet.  This is what NewFromSource, TokenStream and TokenGenerator
// are exported for.
func unstartedParser() *Parser {
	stream := TokenGenerator(func() []*token.Token {
		return []*token.Token{{
			Type:   token.EOF,
			Source: &token.Location{File: "embedder", Pos: 0, Line: 1, Col: 1},
		}}
	})
	return NewFromSource(NewTokenStreamSource(stream))
}

// TestAccessorsBeforeFirstScan is the CATCH.
//
// On the parent commit each of these panicked with a nil pointer dereference.
// An accessor whose whole job is to report the parser's state must be able to
// report "not started" -- the embedder has no other way to ask, and
// token.Source's own contract says "Token returns nil if Scan has not been
// called."
func TestAccessorsBeforeFirstScan(t *testing.T) {
	t.Parallel()

	t.Run("Location", func(t *testing.T) {
		t.Parallel()
		p := unstartedParser()
		require.NotPanics(t, func() {
			assert.Nil(t, p.Location(),
				"Location must report no position before the first scan (#430)")
		})
	})

	t.Run("TokenText", func(t *testing.T) {
		t.Parallel()
		p := unstartedParser()
		require.NotPanics(t, func() {
			assert.Empty(t, p.TokenText())
		})
	})

	t.Run("TokenType", func(t *testing.T) {
		t.Parallel()
		p := unstartedParser()
		require.NotPanics(t, func() {
			assert.Equal(t, token.INVALID, p.TokenType())
		})
	})

	// The accessors must still answer correctly once a token IS under the
	// cursor: the nil branch states the invariant, it does not replace it.
	t.Run("after-scan", func(t *testing.T) {
		t.Parallel()
		p := New(token.NewScanner("embedder.lisp", strings.NewReader("(foo)")))
		require.NotNil(t, p.ReadToken())
		assert.Equal(t, token.PAREN_L, p.TokenType())
		assert.Equal(t, "(", p.TokenText())
		loc := p.Location()
		require.NotNil(t, loc)
		assert.Equal(t, "embedder.lisp", loc.File)
		assert.Equal(t, 1, loc.Line)
		assert.Equal(t, 1, loc.Col)
	})
}

// TestUnstartedParserParsesNormally checks the nil branch is genuinely a
// pre-start state and not a broken parser: the same unstarted Parser, once
// driven, behaves.
func TestUnstartedParserParsesNormally(t *testing.T) {
	t.Parallel()

	p := New(token.NewScanner("embedder.lisp", strings.NewReader("(+ 1 2)")))
	assert.Nil(t, p.Location(), "nothing scanned yet")
	assert.Equal(t, token.INVALID, p.TokenType())

	exprs, err := p.ParseProgram()
	require.NoError(t, err)
	require.Len(t, exprs, 1)
	loc, ok := exprs[0].Source()
	require.True(t, ok)
	assert.Equal(t, "embedder.lisp:1:1", loc.String())
}

// TestTokenLValEndPositionGuard is the GUARD for the deleted conjunct.
//
// tokenLVal writes EndLine/EndCol/EndPos under `loc != nil`, which is now the
// whole condition.  That is sound because Location() returns nil whenever
// p.src.Token is nil -- so a non-nil loc is itself proof there is a token
// under the cursor, which is what the deleted `p.src.Token != nil` conjunct was
// pretending to establish one line after Location() had dereferenced it.
//
// The property to hold onto is that every parsed node still gets a complete
// span.  Nothing here failed before the deletion; this fails if the deletion
// ever turns out to have mattered.
func TestTokenLValEndPositionGuard(t *testing.T) {
	t.Parallel()

	const src = "(defun f (x)\n  (+ x 1))"
	exprs, err := NewReader().Read("span.lisp", strings.NewReader(src))
	require.NoError(t, err)
	require.Len(t, exprs, 1)

	var walk func(v *lisp.LVal)
	n := 0
	walk = func(v *lisp.LVal) {
		if v == nil {
			return
		}
		n++
		loc, ok := v.Source()
		require.Truef(t, ok, "node %v %q has no source location", v.Type, v.Str)
		assert.NotZerof(t, loc.EndPos, "node %v %q has no end position", v.Type, v.Str)
		assert.NotZerof(t, loc.EndLine, "node %v %q has no end line", v.Type, v.Str)
		assert.NotZerof(t, loc.EndCol, "node %v %q has no end column", v.Type, v.Str)
		assert.GreaterOrEqualf(t, loc.EndPos, loc.Pos,
			"node %v %q ends before it starts", v.Type, v.Str)
		for _, c := range v.Cells {
			walk(c)
		}
	}
	walk(exprs[0])
	assert.Positive(t, n)
}
