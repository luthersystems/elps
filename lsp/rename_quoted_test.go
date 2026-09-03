// Copyright © 2026 The ELPS authors

package lsp

import (
	"fmt"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	protocol "github.com/tliron/glsp/protocol_3_16"
)

// This file pins elps#577: the span analysis records for a symbol written with
// a reader quote used to start at the QUOTE rather than at the name.
//
// rdparser.ParseQuote gives the whole 'x form one node -- lisp.Quote copies the
// symbol and sets its quoted flag, so there is no separate node standing for
// the quote -- and applyPrefixLocation then moves that node's start back onto
// the ' so the form reports the position a reader would point at.  Recording
// THAT span as the location of the NAME is what made every LSP range built
// from it one byte too wide on the left: rename replaced "'é" with the new
// name and deleted the quote with it, turning (set 'é 1) into (set zz 1) --
// a different program, applied to the user's file unread.
//
// The defect is NOT about multi-byte text, and the ASCII rows below are here to
// say so.  FuzzLSPSession surfaced it on "'é" only because checkRenameSpans
// compares the covered text against prepareRename's placeholder and the
// non-ASCII corpus entry was the one the fuzzer reached first; "'e" was broken
// in exactly the same way and by exactly the same byte.  That makes it distinct
// from elps#463, which really was a rune-vs-byte width defect in
// token.TokenEnd: this one is a WRONG START, and it is wrong by one byte
// whatever the encoding of the name.
//
// The unquoted rows are the guards.  They passed before the fix and pin that
// the correction narrows only the spans that carried a reader prefix.

// TestRenameQuotedSymbolExcludesReaderQuote asserts the exact edit ranges, not
// just the resulting text, because the range is the artefact the client acts
// on and a text comparison can be satisfied by two compensating errors.
func TestRenameQuotedSymbolExcludesReaderQuote(t *testing.T) {
	// Byte layout, which every column below is counted in:
	//
	//	line 0: ( s e t _ ' é. .é _ 1 )      é at bytes 6..8
	//	line 1: ( s e t _ ' e _ 2 )          e at byte  6
	//	line 2: ( s e t _ ú. .ú _ 3 )        ú at bytes 5..7
	//	line 3: ( + _ é. .é _ e _ ú. .ú )    é 3..5, e 6, ú 8..10
	const content = "(set 'é 1)\n(set 'e 2)\n(set ú 3)\n(+ é e ú)\n"

	for _, tc := range []struct {
		name     string
		line     int
		char     int // 0-based BYTE column of a position inside the name
		newName  string
		want     []protocol.Range // in source order: definition, then reference
		wantText string
		unquoted bool
	}{{
		name: "quoted-non-ascii", // the elps#577 corpus entry, minimised
		line: 0, char: 6,
		newName: "zz",
		want: []protocol.Range{
			{Start: protocol.Position{Line: 0, Character: 6}, End: protocol.Position{Line: 0, Character: 8}},
			{Start: protocol.Position{Line: 3, Character: 3}, End: protocol.Position{Line: 3, Character: 5}},
		},
		wantText: "(set 'zz 1)\n(set 'e 2)\n(set ú 3)\n(+ zz e ú)\n",
	}, {
		name: "quoted-ascii", // broken identically before the fix
		line: 1, char: 6,
		newName: "ee",
		want: []protocol.Range{
			{Start: protocol.Position{Line: 1, Character: 6}, End: protocol.Position{Line: 1, Character: 7}},
			{Start: protocol.Position{Line: 3, Character: 6}, End: protocol.Position{Line: 3, Character: 7}},
		},
		wantText: "(set 'é 1)\n(set 'ee 2)\n(set ú 3)\n(+ é ee ú)\n",
	}, {
		name: "unquoted-non-ascii-GUARD",
		line: 2, char: 5,
		newName: "uu",
		want: []protocol.Range{
			{Start: protocol.Position{Line: 2, Character: 5}, End: protocol.Position{Line: 2, Character: 7}},
			{Start: protocol.Position{Line: 3, Character: 8}, End: protocol.Position{Line: 3, Character: 10}},
		},
		wantText: "(set 'é 1)\n(set 'e 2)\n(set uu 3)\n(+ é e uu)\n",
		unquoted: true,
	}} {
		t.Run(tc.name, func(t *testing.T) {
			s := renameTestServer(encodingUTF8)
			uri := fmt.Sprintf("file:///test/quoted-%s.lisp", tc.name)
			openDoc(s, uri, content)

			pos := protocol.Position{Line: safeUint(tc.line), Character: safeUint(tc.char)}
			edit, err := s.textDocumentRename(mockContext(), &protocol.RenameParams{
				TextDocumentPositionParams: protocol.TextDocumentPositionParams{
					TextDocument: protocol.TextDocumentIdentifier{URI: uri},
					Position:     pos,
				},
				NewName: tc.newName,
			})
			require.NoError(t, err)
			require.NotNil(t, edit)
			got := edit.Changes[uri]
			require.Len(t, got, len(tc.want))
			assert.Equal(t, tc.want, sortedRanges(got),
				"edit ranges must cover the NAME and not the reader quote in front of it")

			// prepareRename is the oracle checkRenameSpans uses; it has to
			// move with the edits or the fuzz property is vacuous.
			pr, err := s.textDocumentPrepareRename(mockContext(), &protocol.PrepareRenameParams{
				TextDocumentPositionParams: protocol.TextDocumentPositionParams{
					TextDocument: protocol.TextDocumentIdentifier{URI: uri},
					Position:     pos,
				},
			})
			require.NoError(t, err)
			rwp, ok := pr.(*protocol.RangeWithPlaceholder)
			require.True(t, ok, "prepareRename returned %T", pr)
			assert.Equal(t, tc.want[0], rwp.Range, "prepareRename range must match the definition edit")

			// And the document a client ends up with, under both encodings.
			for _, e := range renameEncodings {
				t.Run(e.name, func(t *testing.T) {
					s := renameTestServer(e.enc)
					openDoc(s, uri, content)
					assert.Equal(t, tc.wantText,
						renameAt(t, s, uri, content, tc.line, tc.char, tc.newName),
						"rename produced a different program than the user asked for")
				})
			}
		})
	}
}

// sortedRanges returns the ranges of edits in source order.  The server builds
// the definition edit first and the reference edits after it, but nothing in
// the protocol promises an order and a map iteration sits in between.
func sortedRanges(edits []protocol.TextEdit) []protocol.Range {
	out := make([]protocol.Range, 0, len(edits))
	for _, e := range edits {
		out = append(out, e.Range)
	}
	for i := 1; i < len(out); i++ {
		for j := i; j > 0 && less(out[j], out[j-1]); j-- {
			out[j], out[j-1] = out[j-1], out[j]
		}
	}
	return out
}

func less(a, b protocol.Range) bool {
	if a.Start.Line != b.Start.Line {
		return a.Start.Line < b.Start.Line
	}
	return a.Start.Character < b.Start.Character
}
