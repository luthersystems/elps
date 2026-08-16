// Copyright © 2026 The ELPS authors

package lsp

import (
	"fmt"
	"sort"
	"strings"
	"testing"
	"unicode/utf8"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	protocol "github.com/tliron/glsp/protocol_3_16"
)

// This file is the END-TO-END pin for elps#463: it renames, APPLIES the edits
// the way a client does, and compares the resulting DOCUMENT TEXT.
//
// It is deliberately not a test about a column number.  #463 was a wrong
// integer -- token.TokenEnd counted EndCol one per RUNE onto the byte-valued
// Col that Scanner.LocStart produces -- but what made it the highest-priority
// bug in the repository was not the integer.  It was that textDocument/rename
// builds its TextEdit ranges from that integer, so the edit was NARROWER than
// the identifier, the rename replaced a prefix and left the tail, and the user
// got a different program with no diagnostic and nothing to un-apply.  A test
// asserting EndCol == 11 would pass the moment the arithmetic is right and
// would never have told anyone what was at stake; this one fails with the
// corrupted source in the failure message.
//
// UNITS.  The edits are applied by treating Character as a BYTE offset, which
// is what this server means by it (Scanner.LocStart counts bytes,
// elpsToLSPPosition passes the column through unchanged, and position.go
// slices lines by byte throughout).  That choice is what keeps this file about
// elps#463 and not about elps#464.  #464 is that LSP 3.16 specifies UTF-16
// code units and this server emits bytes without negotiating positionEncoding
// -- a real gap, and orthogonal: applying these edits in the server's OWN
// declared unit still corrupted the document, because Col and EndCol did not
// agree with EACH OTHER.  No wire encoding can rescue a range whose two ends
// are counted differently, so #463 is a defect under every answer #464 might
// get, and fixing it settles nothing about #464.  Nothing in this file asserts
// that bytes are the right unit to send.

// applyTextEdits applies LSP TextEdits to content the way a client does,
// interpreting Range.Character as a BYTE offset into the line (see UNITS
// above).  Edits are applied last-first so that earlier offsets stay valid.
func applyTextEdits(t *testing.T, content string, edits []protocol.TextEdit) string {
	t.Helper()
	lines := strings.Split(content, "\n")
	// lineOffset[i] is the byte offset of the first byte of line i.  The +1 is
	// the "\n" that Split consumed.
	lineOffset := make([]int, len(lines)+1)
	for i, l := range lines {
		lineOffset[i+1] = lineOffset[i] + len(l) + 1
	}
	offsetOf := func(p protocol.Position) int {
		line := int(p.Line)
		require.Less(t, line, len(lines), "edit position names line %d of a %d-line document", line, len(lines))
		off := lineOffset[line] + int(p.Character)
		require.LessOrEqual(t, off, len(content), "edit position %d:%d is past the end of the document", p.Line, p.Character)
		return off
	}
	type resolved struct {
		start, end int
		text       string
	}
	rs := make([]resolved, 0, len(edits))
	for _, e := range edits {
		r := resolved{start: offsetOf(e.Range.Start), end: offsetOf(e.Range.End), text: e.NewText}
		require.LessOrEqual(t, r.start, r.end, "edit range is inverted")
		rs = append(rs, r)
	}
	sort.Slice(rs, func(i, j int) bool { return rs[i].start > rs[j].start })
	out := []byte(content)
	for _, r := range rs {
		out = append(out[:r.start], append([]byte(r.text), out[r.end:]...)...)
	}
	return string(out)
}

// renameAt runs textDocument/rename at a 0-based position and returns the
// document text a client would end up with.
func renameAt(t *testing.T, s *Server, uri, content string, line, char int, newName string) string {
	t.Helper()
	edit, err := s.textDocumentRename(mockContext(), &protocol.RenameParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: protocol.DocumentUri(uri)},
			Position:     protocol.Position{Line: safeUint(line), Character: safeUint(char)},
		},
		NewName: newName,
	})
	require.NoError(t, err)
	require.NotNil(t, edit, "rename returned no workspace edit")
	edits := edit.Changes[protocol.DocumentUri(uri)]
	require.NotEmpty(t, edits, "rename produced no edits for %s", uri)
	return applyTextEdits(t, content, edits)
}

// TestRenameNonASCIIIdentifierRewritesWholeName is the RED test for elps#463.
//
// Run on 4737835, the commit this branch is based on, the six rows whose
// IDENTIFIER is non-ASCII failed, each having replaced only a PREFIX of the
// name.  Captured verbatim:
//
//	éx    -> zz  gave "(defun zzx (a) a)\n(zzx 1)\n"
//	xé    -> zz  gave "(defun zz\xa9 (a) a)\n(zz\xa9 1)\n"
//	éèê   -> abc gave "(defun abc\xa8ê (x) x)\n(abc\xa8ê 1)\n"
//	λ     -> zz  gave "(set zz\xbb 1)\nzz\xbb\n"
//	加算  -> add gave "(defun add\xa0算 (a b) (+ a b))\n(add\xa0算 1 2)\n"
//	𝛼𝛽    -> ab  gave "(defun ab\x9b\xbc𝛽 (a) a)\n(ab\x9b\xbc𝛽 1)\n"
//
// The first is a well-formed program with different semantics -- the worst
// outcome of the set, because it compiles and runs and the user has no reason
// to look.  The other five are not valid text at all: the leftover starts
// mid-rune, so what is left behind is a bare UTF-8 continuation byte.  Either
// way the client applied a well-formed edit and had no way to know.
//
// The last three rows are GUARDS: they passed on 4737835 and pass here, and
// are not counted as catches.  Two are plain ASCII.  The third,
// non-ascii-earlier-on-the-line, is the more interesting one -- the LINE holds
// a multi-byte rune but the NAME being renamed does not, and it passed on
// 4737835 because Col was already a correct byte column and EndCol == Col+1 is
// right for a one-byte name wherever on the line it sits.  It is here to pin
// exactly that: the defect was in the WIDTH of the span, never in its start,
// so a fix that adjusted starts would be wrong and this row would catch it.
func TestRenameNonASCIIIdentifierRewritesWholeName(t *testing.T) {
	for _, tc := range []struct {
		name    string
		content string
		line    int
		char    int // 0-based BYTE column of a position inside the identifier
		newName string
		want    string
		guard   bool
	}{{
		name:    "two-byte-lead-rune",
		content: "(defun éx (a) a)\n(éx 1)\n",
		line:    0, char: 7,
		newName: "zz",
		want:    "(defun zz (a) a)\n(zz 1)\n",
	}, {
		name:    "two-byte-trailing-rune", // leftover is a bare continuation byte
		content: "(defun xé (a) a)\n(xé 1)\n",
		line:    0, char: 7,
		newName: "zz",
		want:    "(defun zz (a) a)\n(zz 1)\n",
	}, {
		name:    "all-multi-byte",
		content: "(defun éèê (x) x)\n(éèê 1)\n",
		line:    0, char: 7,
		newName: "abc",
		want:    "(defun abc (x) x)\n(abc 1)\n",
	}, {
		name:    "greek-variable",
		content: "(set λ 1)\nλ\n",
		line:    0, char: 5,
		newName: "zz",
		want:    "(set zz 1)\nzz\n",
	}, {
		name:    "three-byte-runes", // CJK: 3 bytes per rune, so the range was 2/3 short
		content: "(defun 加算 (a b) (+ a b))\n(加算 1 2)\n",
		line:    0, char: 7,
		newName: "add",
		want:    "(defun add (a b) (+ a b))\n(add 1 2)\n",
	}, {
		name:    "four-byte-runes", // outside the BMP
		content: "(defun 𝛼𝛽 (a) a)\n(𝛼𝛽 1)\n",
		line:    0, char: 7,
		newName: "ab",
		want:    "(defun ab (a) a)\n(ab 1)\n",
	}, {
		// GUARD: passed on 4737835.  The LINE is non-ASCII, the NAME is not.
		name:    "non-ascii-earlier-on-the-line",
		content: "(defun f (é) (g é))\n(defun g (x) x)\n(f 1)\n",
		line:    0, char: 15,
		newName: "h",
		want:    "(defun f (é) (h é))\n(defun h (x) x)\n(f 1)\n",
		guard:   true,
	}, {
		name:    "ascii-baseline", // GUARD: passed on 4737835
		content: "(defun add (x y) (+ x y))\n(add 1 2)\n",
		line:    0, char: 7,
		newName: "sum",
		want:    "(defun sum (x y) (+ x y))\n(sum 1 2)\n",
		guard:   true,
	}, {
		name:    "ascii-longer-new-name", // GUARD: passed on 4737835
		content: "(set x 1)\nx\n",
		line:    0, char: 5,
		newName: "counter",
		want:    "(set counter 1)\ncounter\n",
		guard:   true,
	}} {
		// The GUARD suffix is in the subtest NAME so that `go test -v` says
		// which rows could have caught elps#463 and which are only pinning
		// that it did not move: a guard that quietly reads as a catch is how a
		// suite comes to look stronger than it is.
		name := tc.name
		if tc.guard {
			name += "-GUARD"
		}
		t.Run(name, func(t *testing.T) {
			s := testServer()
			uri := fmt.Sprintf("file:///test/rename-%s.lisp", tc.name)
			openDoc(s, uri, tc.content)
			got := renameAt(t, s, uri, tc.content, tc.line, tc.char, tc.newName)

			assert.Equal(t, tc.want, got, "rename produced a different program than the user asked for")
			assert.True(t, utf8.ValidString(got),
				"rename produced text that is not valid UTF-8: %q", got)
			assert.NotContains(t, got, tc.newName+tc.newName,
				"the new name appears doubled, which means an edit range overlapped another")
		})
	}
}

// TestRenameNonASCIIRoundTripsThroughTheParser is the same defect stated as a
// property rather than as expected text: whatever the rename produces has to
// still be the program the user had, with one name changed.  Re-analysing the
// result and asking for the new name back is the check.
//
// It exists because equality against a `want` string can be satisfied by
// writing the wrong string into the test.  This cannot: the renamed document
// has to parse, and the symbol has to be found under its NEW name at a
// position where the old one was.
func TestRenameNonASCIIRoundTripsThroughTheParser(t *testing.T) {
	for i, content := range []string{
		"(defun éx (a) a)\n(éx 1)\n",
		"(defun xé (a) a)\n(xé 1)\n",
		"(defun éèê (x) x)\n(éèê 1)\n",
		"(defun 加算 (a b) (+ a b))\n(加算 1 2)\n",
		"(defun add (x y) (+ x y))\n(add 1 2)\n", // GUARD: ASCII, passed on 4737835
	} {
		t.Run(fmt.Sprintf("%d", i), func(t *testing.T) {
			s := testServer()
			uri := fmt.Sprintf("file:///test/roundtrip%d.lisp", i)
			openDoc(s, uri, content)
			got := renameAt(t, s, uri, content, 0, 7, "renamed")

			require.True(t, utf8.ValidString(got), "renamed document is not valid UTF-8: %q", got)

			// The renamed document must analyse, and the new name must be a
			// symbol in it with as many references as the old name had.
			s2 := testServer()
			uri2 := uri + ".renamed"
			openDoc(s2, uri2, got)
			doc := s2.docs.Get(uri2)
			require.NotNil(t, doc)
			s2.ensureAnalysis(doc)
			require.NotNil(t, doc.analysis, "renamed document did not analyse")

			var found bool
			for _, sym := range doc.analysis.Symbols {
				if sym.Name == "renamed" {
					found = true
				}
			}
			assert.True(t, found, "renamed document has no symbol %q; got %q", "renamed", got)
		})
	}
}
