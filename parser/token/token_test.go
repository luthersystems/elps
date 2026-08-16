// Copyright © 2018 The ELPS authors

package token

import (
	"errors"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestTypeString(t *testing.T) {
	used := make(map[string]bool)
	for tok := Type(0); tok < numTokenTypes; tok++ {
		str := tok.String()
		t.Log(str)
		if str == "" {
			t.Errorf("token type %x has empty string value", tok)
			continue
		}
		if used[str] {
			t.Errorf("token type string used twice: %v", tok)
		}
		used[str] = true
	}
}

func TestLocationError_Unwrap(t *testing.T) {
	inner := errors.New("inner error")
	lerr := &LocationError{
		Err:    inner,
		Source: &Location{File: "test.lisp", Line: 1, Col: 1},
	}
	assert.Equal(t, inner, lerr.Unwrap())
	assert.ErrorIs(t, lerr, inner)
}

func TestLocationError_Code(t *testing.T) {
	lerr := &LocationError{
		Err:    errors.New("bad syntax"),
		Source: &Location{File: "test.lisp", Line: 1, Col: 1},
		Code:   "parse-error",
	}
	assert.Equal(t, "parse-error", lerr.Code)
	assert.Contains(t, lerr.Error(), "bad syntax")
}

func TestLocationError_Error(t *testing.T) {
	lerr := &LocationError{
		Err:    errors.New("something"),
		Source: &Location{File: "test.lisp", Line: 5, Col: 3},
	}
	assert.Equal(t, "test.lisp:5:3: something", lerr.Error())
}

func TestLocation_EndFields_ZeroDefault(t *testing.T) {
	loc := &Location{File: "test.lisp", Line: 1, Col: 1}
	assert.Equal(t, 0, loc.EndPos)
	assert.Equal(t, 0, loc.EndLine)
	assert.Equal(t, 0, loc.EndCol)
	// String() is unchanged — only shows start position.
	assert.Equal(t, "test.lisp:1:1", loc.String())
}

func TestLocation_EndFields_Populated(t *testing.T) {
	loc := &Location{
		File: "test.lisp", Line: 1, Col: 1, Pos: 0,
		EndLine: 1, EndCol: 8, EndPos: 7,
	}
	assert.Equal(t, 1, loc.EndLine)
	assert.Equal(t, 8, loc.EndCol)
	assert.Equal(t, 7, loc.EndPos)
	// String() still only shows start position.
	assert.Equal(t, "test.lisp:1:1", loc.String())
}

func TestLocation_String_AllBranches(t *testing.T) {
	// Pos < 0 → just file name
	assert.Equal(t, "test.lisp", (&Location{File: "test.lisp", Pos: -1}).String())
	// Line == 0 → file[pos] format
	assert.Equal(t, "test.lisp[5]", (&Location{File: "test.lisp", Pos: 5}).String())
	// Col == 0 → file:line format
	assert.Equal(t, "test.lisp:3", (&Location{File: "test.lisp", Line: 3}).String())
	// All set → file:line:col format
	assert.Equal(t, "test.lisp:3:7", (&Location{File: "test.lisp", Line: 3, Col: 7}).String())
}

func TestLocationError_Error_ExactFormat(t *testing.T) {
	// Code field is NOT included in Error() output — it's metadata only.
	lerr := &LocationError{
		Err:    errors.New("bad syntax"),
		Source: &Location{File: "test.lisp", Line: 1, Col: 1},
		Code:   "parse-error",
	}
	assert.Equal(t, "test.lisp:1:1: bad syntax", lerr.Error())
}

// TestTokenEndCountsBytes is the unit-level RED test for elps#463.
//
// TokenEnd advanced its column one per RUNE (`for _, ch := range tok.Text`)
// onto the byte-valued Col that Scanner.LocStart computes, so the end column
// of any token holding a multi-byte rune was short by
// len(text)-utf8.RuneCountInString(text).  The corruption that came out of
// that is pinned end-to-end in lsp.TestRenameNonASCIIIdentifierRewritesWholeName;
// this is the arithmetic underneath it.
//
// THE INVARIANT is the last assertion in each row and the point of the whole
// exercise: on a single-line token the column span must equal the byte span,
// endCol-Col == endPos-Pos.  It says the two ends of a Location are counted in
// the same unit without saying which unit that is, so it stays true under
// whatever elps#464 decides the server should put on the wire.
//
// Rows marked GUARD passed before the fix -- a byte and a rune are the same
// width in ASCII, which is exactly why this survived so long -- and are here
// only to pin that the common case did not move.
func TestTokenEndCountsBytes(t *testing.T) {
	for _, tc := range []struct {
		name                       string
		text                       string
		line, col, pos             int
		wantLine, wantCol, wantPos int
		guard                      bool
	}{
		{name: "ascii symbol", text: "add", line: 1, col: 8, pos: 7,
			wantLine: 1, wantCol: 11, wantPos: 10, guard: true},
		{name: "empty text", text: "", line: 1, col: 8, pos: 7,
			wantLine: 1, wantCol: 8, wantPos: 7, guard: true},
		// éx is 3 bytes / 2 runes.  Before the fix: endCol 10, one short.
		{name: "two-byte lead rune", text: "éx", line: 1, col: 8, pos: 7,
			wantLine: 1, wantCol: 11, wantPos: 10},
		// 加算 is 6 bytes / 2 runes.  Before the fix: endCol 10, four short.
		{name: "three-byte runes", text: "加算", line: 1, col: 8, pos: 7,
			wantLine: 1, wantCol: 14, wantPos: 13},
		// 𝛼 is 4 bytes / 1 rune.  Before the fix: endCol 9, three short.
		{name: "astral rune", text: "𝛼", line: 1, col: 8, pos: 7,
			wantLine: 1, wantCol: 12, wantPos: 11},
		// A multi-line token restarts its column at the last newline, so the
		// start column drops out and only the bytes after it count.
		{name: "multi-line ascii", text: "\"\"\"a\nbc\"\"\"", line: 1, col: 3, pos: 2,
			wantLine: 2, wantCol: 6, wantPos: 12, guard: true},
		// The multi-byte rune is AFTER the last newline, so it is inside the
		// span the column measures.  Before the fix: endCol 5, one short.
		{name: "multi-line non-ascii after the newline", text: "\"\"\"a\né\"\"\"", line: 1, col: 3, pos: 2,
			wantLine: 2, wantCol: 6, wantPos: 12},
		// GUARD, and the mirror image: the multi-byte rune is BEFORE the last
		// newline, so it drops out of the column entirely and the old rune
		// count and the new byte count agree.  It pins that bytes on earlier
		// lines are not counted -- the plausible over-correction of just
		// swapping the loop for len(text).
		{name: "non-ascii before the newline only", text: "\"\"\"é\nb\"\"\"", line: 1, col: 3, pos: 2,
			wantLine: 2, wantCol: 5, wantPos: 12, guard: true},
		// GUARD.  Invalid UTF-8, where a byte scan is exact and `range` yields
		// RuneError with a width that need not describe the input.  It passed
		// before the fix too: on THIS input `range` happens to yield three
		// one-byte steps, so the two counts coincide.  It is here because the
		// byte scan is a deliberate choice in TokenEnd and something should
		// hold it, not because it caught anything.
		{name: "invalid utf-8", text: "a\xffb", line: 1, col: 1, pos: 0,
			wantLine: 1, wantCol: 4, wantPos: 3, guard: true},
	} {
		name := tc.name
		if tc.guard {
			name += " (GUARD)"
		}
		t.Run(name, func(t *testing.T) {
			tok := &Token{
				Type:   SYMBOL,
				Text:   tc.text,
				Source: &Location{File: "t.lisp", Line: tc.line, Col: tc.col, Pos: tc.pos},
			}
			endLine, endCol, endPos := TokenEnd(tok)
			assert.Equal(t, tc.wantLine, endLine, "endLine")
			assert.Equal(t, tc.wantCol, endCol, "endCol")
			assert.Equal(t, tc.wantPos, endPos, "endPos")

			// THE INVARIANT.  Only meaningful on a single-line token: across a
			// newline the column restarts and the two spans measure different
			// things by design.
			if endLine == tc.line {
				assert.Equal(t, endPos-tc.pos, endCol-tc.col,
					"column span (%d-%d) disagrees with byte span (%d-%d): Location's fields are in different units",
					endCol, tc.col, endPos, tc.pos)
			}
		})
	}
}

// TestTokenEndNilSafe pins the two nil guards, which have no bearing on
// elps#463 but are the only other behaviour TokenEnd has.  GUARD.
func TestTokenEndNilSafe(t *testing.T) {
	l, c, p := TokenEnd(nil)
	assert.Equal(t, [3]int{0, 0, 0}, [3]int{l, c, p})
	l, c, p = TokenEnd(&Token{Text: "abc"})
	assert.Equal(t, [3]int{0, 0, 0}, [3]int{l, c, p})
}
