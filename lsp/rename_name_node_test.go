// Copyright © 2026 The ELPS authors

package lsp

import (
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	protocol "github.com/tliron/glsp/protocol_3_16"
)

// elps#577 was about a definition span that reached one byte past the name, so
// rename deleted a reader quote.  Both cases in this file are the same defect
// in a different node type: the span analysis records for the definition is
// the span of the whole FORM the name is written in rather than of the name,
// and rename -- the one request whose answer is applied to the user's file
// unread -- therefore replaces the form's delimiters along with the name.
//
// They predate elps#577 and were found while reviewing its fix.  Neither is
// about multi-byte text; both are wrong by whole delimiters.

// TestRenameStringDeftypeNameExcludesStringDelimiters pins the LString half.
//
// (s:deftype "myint" ...) binds a global named by the STRING LITERAL, and
// analyzeStringDeftype records the literal's node as the definition.  Its span
// covers the quotes, so renaming myint to NEW produced
//
//	(s:deftype NEW s:int (s:positive))
//
// -- a bare symbol where the form requires a string, i.e. a program that no
// longer parses the way its author wrote it, emitted silently.
func TestRenameStringDeftypeNameExcludesStringDelimiters(t *testing.T) {
	// Byte layout:
	//
	//	line 0: ( s : d e f t y p e _ " m y i n t " ...   " at 11, myint 12..16, " at 17
	//	line 1: ( + _ m y i n t _ 1 )                     myint at 3..7
	const content = "(s:deftype \"myint\" s:int (s:positive))\n(+ myint 1)\n"
	const uri = "file:///test/string-deftype.lisp"

	want := []protocol.Range{
		{Start: protocol.Position{Line: 0, Character: 12}, End: protocol.Position{Line: 0, Character: 17}},
		{Start: protocol.Position{Line: 1, Character: 3}, End: protocol.Position{Line: 1, Character: 8}},
	}
	const wantText = "(s:deftype \"NEW\" s:int (s:positive))\n(+ NEW 1)\n"

	s := renameTestServer(encodingUTF8)
	openDoc(s, uri, content)

	pos := protocol.Position{Line: 0, Character: 12}
	edit, err := s.textDocumentRename(mockContext(), &protocol.RenameParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: uri},
			Position:     pos,
		},
		NewName: "NEW",
	})
	require.NoError(t, err)
	require.NotNil(t, edit)
	got := edit.Changes[uri]
	require.Len(t, got, len(want))
	assert.Equal(t, want, sortedRanges(got),
		"the edit must cover the string's INTERIOR and leave its delimiters in place")

	pr, err := s.textDocumentPrepareRename(mockContext(), &protocol.PrepareRenameParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: uri},
			Position:     pos,
		},
	})
	require.NoError(t, err)
	rwp, ok := pr.(*protocol.RangeWithPlaceholder)
	require.True(t, ok, "prepareRename returned %T", pr)
	assert.Equal(t, want[0], rwp.Range, "prepareRename range must match the definition edit")

	for _, e := range renameEncodings {
		t.Run(e.name, func(t *testing.T) {
			s := renameTestServer(e.enc)
			openDoc(s, uri, content)
			assert.Equal(t, wantText, renameAt(t, s, uri, content, 0, 12, "NEW"),
				"rename produced a different program than the user asked for")
		})
	}
}

// TestRenameQuotedListSetTargetDefinesNothing pins the quoted-list half.
//
// (set '(a b) 1) does not bind a; set takes a symbol.  extractSetSymbolName
// nevertheless reached into the quoted list and returned its first element,
// a leftover from when a quoted symbol parsed as a one-cell LSExpr -- it has
// not since rdparser started folding the quote into the symbol's own node --
// so analysis recorded a definition named "a" whose Source was the whole
// '(a b).  Renaming a to NEW then rewrote the list:
//
//	(set NEW 1)
//
// dropping b entirely.  There is no definition here to rename, so the correct
// answer is that there is no symbol at that position at all.
func TestRenameQuotedListSetTargetDefinesNothing(t *testing.T) {
	const content = "(set '(a b) 1)\n(+ a 1)\n"

	for _, tc := range []struct {
		name string
		line int
		char int
	}{
		{"inside-the-quoted-list", 0, 6},
		{"at-the-later-use", 1, 3},
	} {
		t.Run(tc.name, func(t *testing.T) {
			for _, e := range renameEncodings {
				t.Run(e.name, func(t *testing.T) {
					s := renameTestServer(e.enc)
					uri := "file:///test/quoted-list-set.lisp"
					openDoc(s, uri, content)

					edit, err := s.textDocumentRename(mockContext(), &protocol.RenameParams{
						TextDocumentPositionParams: protocol.TextDocumentPositionParams{
							TextDocument: protocol.TextDocumentIdentifier{URI: uri},
							Position: protocol.Position{
								Line:      safeUint(tc.line),
								Character: safeUint(wireChar(s, content, tc.line, tc.char)),
							},
						},
						NewName: "NEW",
					})
					if err != nil {
						assert.Nil(t, edit)
						return
					}
					// If a future change does resolve something here, it
					// still must not rewrite the quoted list.  Byte columns
					// on line 0: ' at 5, ( at 6, a at 7, b at 9, ) at 10, so
					// anything an edit may touch lies inside [7, 10).
					require.NotNil(t, edit)
					for _, te := range edit.Changes[uri] {
						if te.Range.Start.Line != 0 {
							continue
						}
						assert.GreaterOrEqual(t, te.Range.Start.Character, protocol.UInteger(7),
							"an edit on the set line must not reach the quoted list's delimiters")
						assert.LessOrEqual(t, te.Range.End.Character, protocol.UInteger(10),
							"an edit on the set line must not reach the quoted list's delimiters")
					}
				})
			}
		})
	}
}
