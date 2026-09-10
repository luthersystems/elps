// Copyright © 2026 The ELPS authors

package lsp

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/luthersystems/elps/analysis"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	protocol "github.com/tliron/glsp/protocol_3_16"
)

// This file pins the interaction between the two things elps#642 added: the
// bounded workspace read (elps#611) and the outbound column conversion on the
// rename path (elps#464).
//
// A cross-file rename converts each edit range against the text of the file it
// points into, and for a file that is not open that text comes off disk. When
// the bounded read was introduced, an over-limit file started yielding no text
// -- and the caller LEFT THE RANGE IN BYTE COLUMNS, which a UTF-16 client then
// reads as UTF-16 columns. On any line holding a non-ASCII character before
// the identifier the two differ, so the client applies a shifted edit to a file
// nobody has open and nobody is looking at.
//
// The godoc on documentTexts.get used to argue that leaving the range
// unconverted "cannot be a regression, [because] unconverted is what every
// range on main is". That is true of a MISSING file, which main also could not
// read. It is false of an over-limit one: main read it with an unbounded
// os.ReadFile and converted correctly, so the bound turned a correct edit into
// a corrupting one. Hence: an over-limit file on the rename path fails the
// WHOLE rename, loudly, rather than shipping byte columns as UTF-16.

// overLimitPadding returns comment lines that push src past the workspace
// scan's per-file limit without changing the program's first line -- the file
// is indexed while it is small and then GROWS, which is the sequence that
// leaves a stale, correct-looking range pointing into a file the server will
// no longer read.
func overLimitPadding(t *testing.T, src string) string {
	t.Helper()
	const unit = ";; padding to push this file past the workspace scan limit\n"
	n := int(analysis.DefaultMaxFileBytes)/len(unit) + 1
	out := src + strings.Repeat(unit, n)
	require.Greater(t, int64(len(out)), int64(analysis.DefaultMaxFileBytes))
	return out
}

// closedFileSource is the closed file's first line: a non-ASCII character sits
// to the LEFT of the identifier being renamed, so its byte column and its
// UTF-16 column differ by exactly one. "é" is two bytes and one UTF-16 unit.
//
//	(list "é" (target))
//	 byte columns   : target spans [12, 18)
//	 UTF-16 columns : target spans [11, 17)
//
// Applying the byte range to the file as a UTF-16 client counts it replaces
// "arget)" instead of "target", which is what the assertions below show.
const closedFileSource = "(list \"é\" (target))\n(defun target () 1)\n"

const (
	closedU16Start, closedU16End   = 11, 17
	closedByteStart, closedByteEnd = 12, 18
)

// renameOverLimitFixture stands up the reviewer's sequence: a closed file on
// disk indexed while small, and an open document that defines the same symbol
// and from which the rename is driven.
func renameOverLimitFixture(t *testing.T, enc positionEncoding) (s *Server, openURI, closedURI, closedPath string) {
	t.Helper()
	s = renameTestServer(enc)
	setTestAnalysisCfg(s, &analysis.Config{})

	dir := t.TempDir()
	closedPath = filepath.Join(dir, "closed.lisp")
	require.NoError(t, os.WriteFile(closedPath, []byte(closedFileSource), 0o600))
	closedURI = pathToURI(closedPath)

	// Index it while it is under the limit: this is the real scan path, so
	// the cross-file range below is one the server itself produced rather
	// than one the test forged.
	s.setTestWorkspaceRefs(map[string][]analysis.FileReference{})
	s.updateFileRefs(closedURI)
	targetKey := analysis.SymbolKey{Package: "user", Name: "target", Kind: analysis.SymFunction}.String()
	s.workspaceRefsMu.RLock()
	indexed := len(s.workspaceRefs[targetKey])
	s.workspaceRefsMu.RUnlock()
	require.Equal(t, 1, indexed, "the small closed file must contribute exactly one workspace reference")

	openURI = pathToURI(filepath.Join(dir, "open.lisp"))
	doc := openDoc(s, openURI, "(defun target () 1)\n(target)\n")
	s.ensureAnalysis(doc)
	return s, openURI, closedURI, closedPath
}

// renameTargetFrom drives textDocument/rename from the open document.
func renameTargetFrom(s *Server, openURI string) (*protocol.WorkspaceEdit, error) {
	return s.textDocumentRename(mockContext(), &protocol.RenameParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: openURI},
			Position:     protocol.Position{Line: 0, Character: 7}, // inside "target"
		},
		NewName: "renamed",
	})
}

// TestRenameOverLimitClosedFileFailsRatherThanEmittingByteColumns is the RED
// test for the finding.
//
// On the head of elps#642 the second rename SUCCEEDED and returned
// {{0 12} {0 18}} for the closed file -- byte columns, shipped to a UTF-16
// client -- so applying the workspace edit produced
//
//	(list "é" (trenamed
//
// The edit replaced "arget)" rather than "target": it ate the closing paren
// and left a "t" behind, silently corrupting a file the user never opened and
// leaving a program that no longer parses.
func TestRenameOverLimitClosedFileFailsRatherThanEmittingByteColumns(t *testing.T) {
	s, openURI, closedURI, closedPath := renameOverLimitFixture(t, encodingUTF16)

	// Positive control: while the file is under the limit the range is
	// converted and the applied edit rewrites the whole identifier.
	edit, err := renameTargetFrom(s, openURI)
	require.NoError(t, err)
	require.NotNil(t, edit)
	closedEdits := edit.Changes[closedURI]
	require.Len(t, closedEdits, 1)
	assert.Equal(t, protocol.UInteger(closedU16Start), closedEdits[0].Range.Start.Character,
		"an under-limit closed file gets UTF-16 columns")
	assert.Equal(t, protocol.UInteger(closedU16End), closedEdits[0].Range.End.Character)
	assert.Equal(t, "(list \"é\" (renamed))\n(defun target () 1)\n",
		applyTextEdits(t, s, closedFileSource, closedEdits))

	// The file grows past the scan's per-file limit -- appended comments, so
	// the indexed range on line 0 is still exactly right -- and no watcher or
	// didSave has refreshed the index yet.
	require.NoError(t, os.WriteFile(closedPath, []byte(overLimitPadding(t, closedFileSource)), 0o600))

	edit, err = renameTargetFrom(s, openURI)

	// Show the corruption rather than only the integers, when it happens.
	if edit != nil {
		if got := edit.Changes[closedURI]; len(got) > 0 {
			assert.NotEqual(t, protocol.UInteger(closedByteStart), got[0].Range.Start.Character,
				"byte columns were emitted as UTF-16; applying this edit gives %q",
				applyTextEdits(t, s, closedFileSource, got))
		}
	}

	require.Error(t, err, "a rename that cannot convert one of its edits must fail")
	assert.Nil(t, edit, "a failed rename must not return a partial workspace edit")
	assert.Contains(t, err.Error(), closedPath, "the error must name the file that could not be read")
	assert.Contains(t, err.Error(), "size limit", "the error must name the limit that stopped the read")
}

// TestRenameOverLimitClosedFileIsUnaffectedUnderUTF8 pins the other half: a
// client that negotiated utf-8 is owed the server's own byte columns, no
// conversion is required, so nothing needs the file's text and the same rename
// still succeeds.
func TestRenameOverLimitClosedFileIsUnaffectedUnderUTF8(t *testing.T) {
	s, openURI, closedURI, closedPath := renameOverLimitFixture(t, encodingUTF8)
	require.NoError(t, os.WriteFile(closedPath, []byte(overLimitPadding(t, closedFileSource)), 0o600))

	edit, err := renameTargetFrom(s, openURI)
	require.NoError(t, err, "utf-8 needs no conversion, so the size limit cannot block a rename")
	require.NotNil(t, edit)
	closedEdits := edit.Changes[closedURI]
	require.Len(t, closedEdits, 1)
	assert.Equal(t, protocol.UInteger(closedByteStart), closedEdits[0].Range.Start.Character)
	assert.Equal(t, protocol.UInteger(closedByteEnd), closedEdits[0].Range.End.Character)
	assert.Equal(t, "(list \"é\" (renamed))\n(defun target () 1)\n",
		applyTextEdits(t, s, closedFileSource, closedEdits))
}

// TestRenameMissingClosedFileFailsRatherThanEmittingByteColumns is the other
// unconvertible case: the index saw the file, and by the time of the rename
// the disk no longer offers it. The server cannot convert the range it owes a
// UTF-16 client, so the whole rename fails and names the file, exactly as for
// the over-limit case -- an unconverted range is never emitted, whatever took
// the text away.
func TestRenameMissingClosedFileFailsRatherThanEmittingByteColumns(t *testing.T) {
	s, openURI, closedURI, closedPath := renameOverLimitFixture(t, encodingUTF16)
	require.NoError(t, os.Remove(closedPath))

	edit, err := renameTargetFrom(s, openURI)
	if edit != nil {
		if got := edit.Changes[closedURI]; len(got) > 0 {
			assert.NotEqual(t, protocol.UInteger(closedByteStart), got[0].Range.Start.Character,
				"byte columns were emitted as UTF-16; applying this edit gives %q",
				applyTextEdits(t, s, closedFileSource, got))
		}
	}
	require.Error(t, err, "a rename that cannot convert one of its edits must fail")
	assert.Nil(t, edit, "a failed rename must not return a partial workspace edit")
	assert.Contains(t, err.Error(), closedPath, "the error must name the file that could not be read")
	assert.Contains(t, err.Error(), "cannot be read")
}
