// Copyright © 2026 The ELPS authors

package lsp

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	protocol "github.com/tliron/glsp/protocol_3_16"
)

// A rename must keep a package qualifier and must rename the export
// (luthersystems/elps#940).
//
// The edit for a reference written helpers:add-one replaced the whole token,
// so the qualifier went and the call resolved in the wrong package.  And
// (export 'add-one) got no edit, so the package exported a name with no
// definition.  The second case below starts the rename from another file than
// the one that has the definition and the export.
func TestRenameKeepsQualifierAndRenamesExport(t *testing.T) {
	const helpers = `(in-package 'helpers)
(export 'add-one "add-two")
(defun add-one (x) (+ x 1))
(defun add-two (x) (helpers:add-one (add-one x)))
`
	const consumer = `(in-package 'consumer)
(defun do-work () (helpers:add-one 42))
`
	wantHelpers := strings.ReplaceAll(helpers, "add-one", "inc")
	wantConsumer := strings.ReplaceAll(consumer, "add-one", "inc")

	for _, tc := range []struct {
		name string
		open string // the file the rename starts from
		pos  protocol.Position
	}{
		{"from the definition", "helpers.lisp", protocol.Position{Line: 2, Character: 7}},
		{"from a qualified call in another file", "consumer.lisp", protocol.Position{Line: 1, Character: 28}},
	} {
		for _, enc := range []positionEncoding{encodingUTF16, encodingUTF8} {
			t.Run(tc.name+"/"+positionEncodingName(enc), func(t *testing.T) {
				dir := t.TempDir()
				files := map[string]string{"helpers.lisp": helpers, "consumer.lisp": consumer}
				for name, src := range files {
					require.NoError(t, os.WriteFile(filepath.Join(dir, name), []byte(src), 0600))
				}
				s := renameTestServer(enc)
				s.rootPath = dir
				s.buildWorkspaceIndex()

				uri := pathToURI(filepath.Join(dir, tc.open))
				s.ensureAnalysis(openDoc(s, uri, files[tc.open]))
				edit, err := s.textDocumentRename(mockContext(), &protocol.RenameParams{
					TextDocumentPositionParams: protocol.TextDocumentPositionParams{
						TextDocument: protocol.TextDocumentIdentifier{URI: uri},
						Position:     tc.pos,
					},
					NewName: "inc",
				})
				require.NoError(t, err)
				require.NotNil(t, edit)

				got := func(name string) string {
					return applyTextEdits(t, s, files[name], edit.Changes[pathToURI(filepath.Join(dir, name))])
				}
				assert.Equal(t, wantHelpers, got("helpers.lisp"))
				assert.Equal(t, wantConsumer, got("consumer.lisp"))
			})
		}
	}
}

// Find-references lists the export of a symbol with its other uses.
func TestReferencesIncludeTheExport(t *testing.T) {
	s := testServer()
	const src = "(in-package 'helpers)\n(export 'add-one)\n(defun add-one (x) (+ x 1))\n(add-one 1)\n"
	doc := openDoc(s, "file:///helpers.lisp", src)
	s.ensureAnalysis(doc)
	locs, err := s.textDocumentReferences(mockContext(), &protocol.ReferenceParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: doc.URI},
			Position:     protocol.Position{Line: 2, Character: 7},
		},
		Context: protocol.ReferenceContext{IncludeDeclaration: false},
	})
	require.NoError(t, err)
	var lines []uint32
	for _, loc := range locs {
		lines = append(lines, loc.Range.Start.Line)
	}
	assert.ElementsMatch(t, []uint32{1, 3}, lines)
	for _, loc := range locs {
		if loc.Range.Start.Line == 1 {
			// The name only, without the reader quote.
			assert.Equal(t, uint32(9), loc.Range.Start.Character)
			assert.Equal(t, uint32(16), loc.Range.End.Character)
		}
	}
}

// A rename can start with the cursor on the name in an export form
// (luthersystems/elps#940).
func TestRenameStartsFromTheExport(t *testing.T) {
	s := testServer()
	const src = "(in-package 'helpers)\n(export 'add-one)\n(defun add-one (x) (+ x 1))\n(add-one 1)\n"
	doc := openDoc(s, "file:///helpers.lisp", src)
	s.ensureAnalysis(doc)
	edit, err := s.textDocumentRename(mockContext(), &protocol.RenameParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: doc.URI},
			Position:     protocol.Position{Line: 1, Character: 10},
		},
		NewName: "inc",
	})
	require.NoError(t, err)
	require.NotNil(t, edit)
	assert.Equal(t, strings.ReplaceAll(src, "add-one", "inc"), applyTextEdits(t, s, src, edit.Changes[doc.URI]))
}

// Linked editing has the ranges of a rename in the file: the name without
// its qualifier, and the name in the export form (luthersystems/elps#940).
func TestLinkedEditingKeepsQualifierAndCoversExport(t *testing.T) {
	const src = "(in-package 'helpers)\n(export 'add-one)\n(defun add-one (x) (+ x 1))\n(defun add-two (x) (helpers:add-one (add-one x)))\n"
	dir := t.TempDir()
	path := filepath.Join(dir, "helpers.lisp")
	require.NoError(t, os.WriteFile(path, []byte(src), 0600))
	s := testServer()
	s.rootPath = dir
	s.buildWorkspaceIndex()
	doc := openDoc(s, pathToURI(path), src)
	result, err := s.textDocumentLinkedEditingRange(mockContext(), &protocol.LinkedEditingRangeParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: doc.URI},
			Position:     protocol.Position{Line: 2, Character: 7},
		},
	})
	require.NoError(t, err)
	require.NotNil(t, result)
	lines := strings.Split(src, "\n")
	var got []uint32
	for _, rng := range result.Ranges {
		got = append(got, rng.Start.Line)
		assert.Equal(t, "add-one", lines[rng.Start.Line][rng.Start.Character:rng.End.Character])
	}
	assert.ElementsMatch(t, []uint32{1, 2, 3, 3}, got)
}
