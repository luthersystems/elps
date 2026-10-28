// Copyright © 2026 The ELPS authors

package lsp

import (
	"os"
	"path/filepath"
	"testing"

	"github.com/luthersystems/elps/analysis"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// TestWorkspaceRecordMethods checks that the workspace index carries the
// s:defmethod methods of other files into a document's analysis, and that
// a save of the other file replaces them.  The record lint checks read
// them there, so a method added in another file is not an unknown member
// in the editor, and a method removed from it is one again.
func TestWorkspaceRecordMethods(t *testing.T) {
	dir := t.TempDir()
	orderPath := filepath.Join(dir, "order.lisp")
	methodsPath := filepath.Join(dir, "methods.lisp")
	src := "(set 'order (s:make-record \"order\" (s:method big (o) (> (o 'size) 1))))\n"
	require.NoError(t, os.WriteFile(orderPath, []byte(src), 0o600))
	require.NoError(t, os.WriteFile(methodsPath, []byte("(s:defmethod order size (o) 0)\n"), 0o600))

	s := testServer()
	s.rootPath = dir
	s.buildWorkspaceIndex()
	doc := openDoc(s, pathToURI(orderPath), src)
	methods := func() map[string]*analysis.Signature {
		doc.mu.Lock()
		doc.analysis = nil
		doc.mu.Unlock()
		s.ensureAnalysis(doc)
		require.NotNil(t, doc.analysis)
		return doc.analysis.WorkspaceRecordMethods()["order"]
	}
	assert.Contains(t, methods(), "size")

	// A save that renames the method replaces the file's entry.
	require.NoError(t, os.WriteFile(methodsPath, []byte("(s:defmethod order sz (o) 0)\n"), 0o600))
	s.updateFileDefinitions(pathToURI(methodsPath))
	got := methods()
	assert.Contains(t, got, "sz")
	assert.NotContains(t, got, "size")

	// A deleted file adds no method.
	s.removeFileDefinitions(methodsPath)
	assert.Empty(t, methods())
}

// TestWorkspaceRecordMethodsOwnFile checks that a document's analysis
// leaves out its own file's entries from the disk.  The document's text
// declares its methods itself; a method the unsaved text removed must not
// stay known.
func TestWorkspaceRecordMethodsOwnFile(t *testing.T) {
	dir := t.TempDir()
	orderPath := filepath.Join(dir, "order.lisp")
	onDisk := "(set 'order (s:make-record \"order\"))\n(s:defmethod order own (o) 0)\n"
	require.NoError(t, os.WriteFile(orderPath, []byte(onDisk), 0o600))

	s := testServer()
	s.rootPath = dir
	s.buildWorkspaceIndex()
	doc := openDoc(s, pathToURI(orderPath), "(set 'order (s:make-record \"order\"))\n")
	s.ensureAnalysis(doc)
	require.NotNil(t, doc.analysis)
	assert.NotContains(t, doc.analysis.WorkspaceRecordMethods()["order"], "own")
}
