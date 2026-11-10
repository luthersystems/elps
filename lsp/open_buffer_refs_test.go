// Copyright © 2026 The ELPS authors

package lsp

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/luthersystems/elps/analysis"
	"github.com/luthersystems/elps/parser/token"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	protocol "github.com/tliron/glsp/protocol_3_16"
)

// unsavedWorkspace is a library file with a definition and its export,
// and a consumer in another file.
var unsavedWorkspace = map[string]string{
	"lib.lisp":      "(in-package 'helpers)\n(export 'old)\n(defun old (x) (+ x 1))\n(defun twice (x) (old (old x)))\n",
	"consumer.lisp": "(in-package 'consumer)\n(defun do-work () (helpers:old 42))\n",
}

// unsavedEdit is the text an editor holds for a file after an edit that
// is not saved: two new lines at the top move every form down, and the
// indent moves the columns.
func unsavedEdit(src string) string {
	first, rest, _ := strings.Cut(src, "\n")
	return first + "\n; a note\n\n   " + strings.ReplaceAll(rest, "\n(", "\n  (")
}

// renameToNew renames the symbol at pos of the open document name to "new".
func renameToNew(t *testing.T, s *Server, dir, name string, pos protocol.Position) *protocol.WorkspaceEdit {
	t.Helper()
	edit, err := s.textDocumentRename(mockContext(), &protocol.RenameParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: pathToURI(filepath.Join(dir, name))},
			Position:     pos,
		},
		NewName: "new",
	})
	require.NoError(t, err)
	require.NotNil(t, edit)
	return edit
}

// TestRenameEditsUnsavedTextOfOpenDocument checks that a rename edits the
// text an open document holds, not the text of its file on disk
// (luthersystems/elps#943). The positions of the workspace index follow
// the disk. An edit at one of them lands on other text of the buffer, and
// the export form keeps the old name.
func TestRenameEditsUnsavedTextOfOpenDocument(t *testing.T) {
	s := testServer()
	dir := writeWorkspace(t, s, unsavedWorkspace)
	libText := unsavedEdit(unsavedWorkspace["lib.lisp"])
	require.NotEqual(t, unsavedWorkspace["lib.lisp"], libText)
	openDoc(s, pathToURI(filepath.Join(dir, "lib.lisp")), libText)
	openDoc(s, pathToURI(filepath.Join(dir, "consumer.lisp")), unsavedWorkspace["consumer.lisp"])

	edit := renameToNew(t, s, dir, "consumer.lisp", protocol.Position{Line: 1, Character: 28})
	got := applyTextEdits(t, s, libText, edit.Changes[pathToURI(filepath.Join(dir, "lib.lisp"))])
	assert.Equal(t, strings.ReplaceAll(libText, "old", "new"), got, "lib.lisp")
	got = applyTextEdits(t, s, unsavedWorkspace["consumer.lisp"], edit.Changes[pathToURI(filepath.Join(dir, "consumer.lisp"))])
	assert.Equal(t, strings.ReplaceAll(unsavedWorkspace["consumer.lisp"], "old", "new"), got, "consumer.lisp")
}

// TestReferencesUseUnsavedTextOfOpenDocument is the same check for
// find-references: each location is a place the name is written in the
// text of the open document.
func TestReferencesUseUnsavedTextOfOpenDocument(t *testing.T) {
	s := testServer()
	dir := writeWorkspace(t, s, unsavedWorkspace)
	libText := unsavedEdit(unsavedWorkspace["lib.lisp"])
	libURI := pathToURI(filepath.Join(dir, "lib.lisp"))
	consumerURI := pathToURI(filepath.Join(dir, "consumer.lisp"))
	openDoc(s, libURI, libText)
	openDoc(s, consumerURI, unsavedWorkspace["consumer.lisp"])

	locs, err := s.textDocumentReferences(mockContext(), &protocol.ReferenceParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: consumerURI},
			Position:     protocol.Position{Line: 1, Character: 28},
		},
		Context: protocol.ReferenceContext{IncludeDeclaration: true},
	})
	require.NoError(t, err)
	lines := strings.Split(libText, "\n")
	inLib := 0
	for _, loc := range locs {
		if loc.URI != libURI {
			continue
		}
		inLib++
		line := lines[loc.Range.Start.Line]
		assert.Equal(t, "old", line[loc.Range.Start.Character:loc.Range.End.Character],
			"location %v of lib.lisp", loc.Range)
	}
	// The export, the definition and the two calls in twice.
	assert.Equal(t, 4, inLib, "locations in lib.lisp")
}

// TestRenameEditsUnsavedExportInThirdFile checks the export form of a
// third file that is open with text that is not saved.
func TestRenameEditsUnsavedExportInThirdFile(t *testing.T) {
	s := testServer()
	dir := writeWorkspace(t, s, thirdFileWorkspace)
	exportsText := unsavedEdit(thirdFileWorkspace["exports.lisp"])
	openDoc(s, pathToURI(filepath.Join(dir, "exports.lisp")), exportsText)
	openDoc(s, pathToURI(filepath.Join(dir, "consumer.lisp")), thirdFileWorkspace["consumer.lisp"])

	edit, err := s.textDocumentRename(mockContext(), &protocol.RenameParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: pathToURI(filepath.Join(dir, "consumer.lisp"))},
			Position:     protocol.Position{Line: 1, Character: 28},
		},
		NewName: "inc",
	})
	require.NoError(t, err)
	require.NotNil(t, edit)
	texts := map[string]string{
		"defs.lisp":     thirdFileWorkspace["defs.lisp"],
		"exports.lisp":  exportsText,
		"consumer.lisp": thirdFileWorkspace["consumer.lisp"],
	}
	for name, src := range texts {
		got := applyTextEdits(t, s, src, edit.Changes[pathToURI(filepath.Join(dir, name))])
		assert.Equal(t, strings.ReplaceAll(src, "add-one", "inc"), got, name)
	}
}

// TestRenameEditsExportWithoutWorkspaceIndex checks a rename on a server
// that has no workspace reference index. The file that defines the symbol
// is then read for its export forms, open or not.
func TestRenameEditsExportWithoutWorkspaceIndex(t *testing.T) {
	for _, tc := range []struct {
		name    string
		libOpen bool
	}{
		{"definition file not open", false},
		{"definition file open and not saved", true},
	} {
		t.Run(tc.name, func(t *testing.T) {
			s := testServer()
			dir := t.TempDir()
			for name, src := range unsavedWorkspace {
				require.NoError(t, os.WriteFile(filepath.Join(dir, name), []byte(src), 0o600))
			}
			libPath := filepath.Join(dir, "lib.lisp")
			old := analysis.ExternalSymbol{
				Name:    "old",
				Package: "helpers",
				Kind:    analysis.SymFunction,
				Source:  &token.Location{File: libPath, Line: 3, Col: 8, Pos: 43},
			}
			// The definitions are known, but no reference index exists.
			setTestAnalysisCfg(s, &analysis.Config{
				ExtraGlobals:   []analysis.ExternalSymbol{old},
				PackageExports: map[string][]analysis.ExternalSymbol{"helpers": {old}},
				PackageSymbols: map[string][]analysis.ExternalSymbol{"helpers": {old}},
			})
			libText := unsavedWorkspace["lib.lisp"]
			if tc.libOpen {
				libText = unsavedEdit(libText)
				openDoc(s, pathToURI(libPath), libText)
			}
			openDoc(s, pathToURI(filepath.Join(dir, "consumer.lisp")), unsavedWorkspace["consumer.lisp"])

			edit := renameToNew(t, s, dir, "consumer.lisp", protocol.Position{Line: 1, Character: 28})
			got := applyTextEdits(t, s, libText, edit.Changes[pathToURI(libPath)])
			// The export and the definition. Without an index, the
			// calls of a file that is not open are not known.
			want := strings.Replace(libText, "(export 'old)", "(export 'new)", 1)
			want = strings.Replace(want, "(defun old ", "(defun new ", 1)
			if tc.libOpen {
				want = strings.ReplaceAll(libText, "old", "new")
			}
			assert.Equal(t, want, got)
		})
	}
}
