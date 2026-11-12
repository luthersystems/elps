// Copyright © 2026 The ELPS authors

package lsp

import (
	"fmt"
	"os"
	"path/filepath"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	protocol "github.com/tliron/glsp/protocol_3_16"
)

// unopenedWorkspace defines gamma in lib.lisp and calls it in use.lisp.
// The tests open neither file.
var unopenedWorkspace = map[string]string{
	"lib.lisp": "(defun gamma (x) \"Gamma doc.\" (+ x 1))\n(gamma 2)\n",
	"use.lisp": "(defun caller () (gamma 3))\n",
}

func unopenedPos(dir, name string, line, char uint32) protocol.TextDocumentPositionParams {
	return protocol.TextDocumentPositionParams{
		TextDocument: protocol.TextDocumentIdentifier{URI: pathToURI(filepath.Join(dir, name))},
		Position:     protocol.Position{Line: line, Character: char},
	}
}

// TestRequestsOnUnopenedFile checks that a position request for a
// workspace file the editor has not opened reads the file from disk.
// Before, references, hover, definition, rename and call hierarchy found
// no document and returned nothing (found while testing elps#952).
func TestRequestsOnUnopenedFile(t *testing.T) {
	s := testServer()
	dir := writeWorkspace(t, s, unopenedWorkspace)
	use := unopenedPos(dir, "use.lisp", 0, 19) // gamma in caller

	locs, err := s.textDocumentReferences(mockContext(), &protocol.ReferenceParams{
		TextDocumentPositionParams: use,
		Context:                    protocol.ReferenceContext{IncludeDeclaration: true},
	})
	require.NoError(t, err)
	var got []string
	for _, l := range locs {
		got = append(got, fmt.Sprintf("%s:%d", filepath.Base(uriToPath(l.URI)), l.Range.Start.Line))
	}
	assert.ElementsMatch(t, []string{"lib.lisp:0", "lib.lisp:1", "use.lisp:0"}, got, "references")

	hover, err := s.textDocumentHover(mockContext(), &protocol.HoverParams{TextDocumentPositionParams: use})
	require.NoError(t, err)
	require.NotNil(t, hover, "hover")
	markup, ok := hover.Contents.(protocol.MarkupContent)
	require.True(t, ok, "hover contents are %T", hover.Contents)
	assert.Contains(t, markup.Value, "Gamma doc.")

	def, err := s.textDocumentDefinition(mockContext(), &protocol.DefinitionParams{TextDocumentPositionParams: use})
	require.NoError(t, err)
	require.NotNil(t, def, "definition")

	edit, err := s.textDocumentRename(mockContext(), &protocol.RenameParams{TextDocumentPositionParams: use, NewName: "delta"})
	require.NoError(t, err)
	require.NotNil(t, edit, "rename")
	assert.Len(t, edit.Changes[pathToURI(filepath.Join(dir, "lib.lisp"))], 2)
	assert.Len(t, edit.Changes[pathToURI(filepath.Join(dir, "use.lisp"))], 1)

	items, err := s.textDocumentPrepareCallHierarchy(mockContext(), &protocol.CallHierarchyPrepareParams{
		TextDocumentPositionParams: unopenedPos(dir, "lib.lisp", 0, 8),
	})
	require.NoError(t, err)
	require.Len(t, items, 1, "call hierarchy")
	calls, err := s.callHierarchyIncomingCalls(mockContext(), &protocol.CallHierarchyIncomingCallsParams{Item: items[0]})
	require.NoError(t, err)
	require.Len(t, calls, 1)
	assert.Equal(t, "caller", calls[0].From.Name)
}

// TestRequestDocLimits checks that requestDoc reads only the files the
// workspace scan indexes: not one outside the workspace (also through a
// link), in a directory the scan skips, excluded, not a .lisp file or
// over the per-file size limit.
func TestRequestDocLimits(t *testing.T) {
	s := testServer()
	dir := writeWorkspace(t, s, unopenedWorkspace)
	assert.NotNil(t, s.requestDoc(pathToURI(filepath.Join(dir, "use.lisp"))))

	outside := filepath.Join(t.TempDir(), "out.lisp")
	require.NoError(t, os.WriteFile(outside, []byte("(defun out () 1)\n"), 0o600))
	assert.Nil(t, s.requestDoc(pathToURI(outside)), "outside the workspace")
	sibling := dir + "x"
	require.NoError(t, os.Mkdir(sibling, 0o700))
	t.Cleanup(func() { _ = os.RemoveAll(sibling) })
	require.NoError(t, os.WriteFile(filepath.Join(sibling, "a.lisp"), []byte("(defun a () 1)\n"), 0o600))
	assert.Nil(t, s.requestDoc(pathToURI(filepath.Join(sibling, "a.lisp"))),
		"a sibling directory whose name starts with the root's")

	link := filepath.Join(dir, "link.lisp")
	require.NoError(t, os.Symlink(outside, link))
	assert.Nil(t, s.requestDoc(pathToURI(link)), "a link out of the workspace")

	vendor := filepath.Join(dir, "vendor")
	require.NoError(t, os.Mkdir(vendor, 0o700))
	require.NoError(t, os.WriteFile(filepath.Join(vendor, "v.lisp"), []byte("(defun v () 1)\n"), 0o600))
	assert.Nil(t, s.requestDoc(pathToURI(filepath.Join(vendor, "v.lisp"))), "a directory the scan skips")
	s.includeDirs = []string{"vendor"}
	assert.NotNil(t, s.requestDoc(pathToURI(filepath.Join(vendor, "v.lisp"))), "a skipped directory an include names")
	s.excludePatterns = []string{"v.lisp"}
	assert.Nil(t, s.requestDoc(pathToURI(filepath.Join(vendor, "v.lisp"))), "an excluded file")

	txt := filepath.Join(dir, "notes.txt")
	require.NoError(t, os.WriteFile(txt, []byte("(defun n () 1)\n"), 0o600))
	assert.Nil(t, s.requestDoc(pathToURI(txt)), "not a .lisp file")

	s.maxWorkspaceFiles = 0
	big := filepath.Join(dir, "big.lisp")
	require.NoError(t, os.WriteFile(big, make([]byte, s.scanConfig().EffectiveMaxFileBytes()+1), 0o600))
	assert.Nil(t, s.requestDoc(pathToURI(big)), "over the size limit")

	assert.Nil(t, s.requestDoc("untitled:Untitled-1"), "not a file URI")

	s.rootPath = ""
	assert.Nil(t, s.requestDoc(pathToURI(filepath.Join(dir, "use.lisp"))), "no workspace root")
}
