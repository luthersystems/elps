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

// thirdFileWorkspace is a package whose export form is in another file
// than the definition, and a consumer in a third file.
var thirdFileWorkspace = map[string]string{
	"defs.lisp":     "(in-package 'helpers)\n(defun add-one (x) (+ x 1))\n",
	"exports.lisp":  "(in-package 'helpers)\n(export 'add-one)\n",
	"consumer.lisp": "(in-package 'consumer)\n(defun do-work () (helpers:add-one 42))\n",
}

// writeWorkspace writes files to a new directory, builds the workspace
// index of s over it and returns the directory.
func writeWorkspace(t testing.TB, s *Server, files map[string]string) string {
	t.Helper()
	dir := t.TempDir()
	for name, src := range files {
		require.NoError(t, os.WriteFile(filepath.Join(dir, name), []byte(src), 0o600))
	}
	s.rootPath = dir
	s.buildWorkspaceIndex()
	return dir
}

// TestRenameRenamesExportInThirdFile checks that a rename edits an export
// form that is in neither the current file nor the file that defines the
// symbol (luthersystems/elps#943). An export that keeps the old name
// exports a name with no definition, and the package fails to load.
func TestRenameRenamesExportInThirdFile(t *testing.T) {
	for _, tc := range []struct {
		name string
		open string // the file the rename starts from
		pos  protocol.Position
	}{
		{"from the definition", "defs.lisp", protocol.Position{Line: 1, Character: 7}},
		{"from a call in another file", "consumer.lisp", protocol.Position{Line: 1, Character: 28}},
		{"from the export", "exports.lisp", protocol.Position{Line: 1, Character: 10}},
	} {
		t.Run(tc.name, func(t *testing.T) {
			s := testServer()
			dir := writeWorkspace(t, s, thirdFileWorkspace)
			uri := pathToURI(filepath.Join(dir, tc.open))
			openDoc(s, uri, thirdFileWorkspace[tc.open])
			edit, err := s.textDocumentRename(mockContext(), &protocol.RenameParams{
				TextDocumentPositionParams: protocol.TextDocumentPositionParams{
					TextDocument: protocol.TextDocumentIdentifier{URI: uri},
					Position:     tc.pos,
				},
				NewName: "inc",
			})
			require.NoError(t, err)
			require.NotNil(t, edit)
			for name, src := range thirdFileWorkspace {
				got := applyTextEdits(t, s, src, edit.Changes[pathToURI(filepath.Join(dir, name))])
				assert.Equal(t, strings.ReplaceAll(src, "add-one", "inc"), got, name)
			}
		})
	}
}

// TestReferencesIncludeExportInThirdFile checks that find-references
// lists an export form of a third file one time, from each file.
func TestReferencesIncludeExportInThirdFile(t *testing.T) {
	for _, tc := range []struct {
		open string
		pos  protocol.Position
	}{
		{"defs.lisp", protocol.Position{Line: 1, Character: 7}},
		{"consumer.lisp", protocol.Position{Line: 1, Character: 28}},
		{"exports.lisp", protocol.Position{Line: 1, Character: 10}},
	} {
		t.Run(tc.open, func(t *testing.T) {
			s := testServer()
			dir := writeWorkspace(t, s, thirdFileWorkspace)
			uri := pathToURI(filepath.Join(dir, tc.open))
			openDoc(s, uri, thirdFileWorkspace[tc.open])
			locs, err := s.textDocumentReferences(mockContext(), &protocol.ReferenceParams{
				TextDocumentPositionParams: protocol.TextDocumentPositionParams{
					TextDocument: protocol.TextDocumentIdentifier{URI: uri},
					Position:     tc.pos,
				},
				Context: protocol.ReferenceContext{IncludeDeclaration: true},
			})
			require.NoError(t, err)
			var got []string
			for _, loc := range locs {
				got = append(got, filepath.Base(uriToPath(loc.URI)))
			}
			assert.ElementsMatch(t, []string{"defs.lisp", "exports.lisp", "consumer.lisp"}, got)
		})
	}
}

// TestCallHierarchyIgnoresExports checks that an export form is not an
// incoming call. The workspace reference index holds the names in export
// forms for rename and find-references. The call hierarchy reads the same
// index, and an export is not a call.
func TestCallHierarchyIgnoresExports(t *testing.T) {
	files := map[string]string{
		"defs.lisp": thirdFileWorkspace["defs.lisp"],
		// An export form in a function body is not a call from it.
		"exports.lisp":  "(in-package 'helpers)\n(export 'add-one)\n(defun also () (export 'add-one))\n",
		"consumer.lisp": thirdFileWorkspace["consumer.lisp"],
	}
	s := testServer()
	dir := writeWorkspace(t, s, files)
	uri := pathToURI(filepath.Join(dir, "defs.lisp"))
	openDoc(s, uri, files["defs.lisp"])
	ctx := mockContext()
	items, err := s.textDocumentPrepareCallHierarchy(ctx, &protocol.CallHierarchyPrepareParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: uri},
			Position:     protocol.Position{Line: 1, Character: 7},
		},
	})
	require.NoError(t, err)
	require.Len(t, items, 1)
	calls, err := s.callHierarchyIncomingCalls(ctx, &protocol.CallHierarchyIncomingCallsParams{Item: items[0]})
	require.NoError(t, err)
	var callers []string
	for _, call := range calls {
		callers = append(callers, call.From.Name)
	}
	assert.Equal(t, []string{"do-work"}, callers)
}
