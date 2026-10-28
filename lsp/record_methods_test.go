// Copyright © 2026 The ELPS authors

package lsp

import (
	"os"
	"path/filepath"
	"strings"
	"sync"
	"testing"
	"time"

	"github.com/luthersystems/elps/analysis"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	protocol "github.com/tliron/glsp/protocol_3_16"
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

// TestRecordMethodsSaveReanalyzesOpenDocs checks that a save that changes
// a file's s:defmethod methods publishes new diagnostics for the other
// open documents that make a record type, not only for the saved one.
func TestRecordMethodsSaveReanalyzesOpenDocs(t *testing.T) {
	old := recordReanalyzeDelay
	recordReanalyzeDelay = time.Millisecond
	t.Cleanup(func() { recordReanalyzeDelay = old })

	dir := t.TempDir()
	orderPath := filepath.Join(dir, "order.lisp")
	methodsPath := filepath.Join(dir, "methods.lisp")
	src := "(set 'order (s:make-record \"order\" (s:has-key \"n\" s:int) (s:method use (o) (o 'sz))))\n"
	require.NoError(t, os.WriteFile(orderPath, []byte(src), 0o600))
	require.NoError(t, os.WriteFile(methodsPath, []byte("(s:defmethod order size (o) 0)\n"), 0o600))

	s := testServer()
	s.rootPath = dir
	s.buildWorkspaceIndex()
	ctx, captured := capturingContext()
	var mu sync.Mutex
	notify := ctx.Notify
	ctx.Notify = func(method string, params any) {
		mu.Lock()
		defer mu.Unlock()
		notify(method, params)
	}
	s.captureNotify(ctx)
	orderURI := pathToURI(orderPath)
	unknownSz := func() (bool, bool) { // published, unknown
		mu.Lock()
		defer mu.Unlock()
		for i := len(*captured) - 1; i >= 0; i-- {
			p := (*captured)[i]
			if p.URI != orderURI {
				continue
			}
			for _, d := range p.Diagnostics {
				if strings.Contains(d.Message, "sz is not a field or method") {
					return true, true
				}
			}
			return true, false
		}
		return false, false
	}
	require.NoError(t, s.textDocumentDidOpen(ctx, &protocol.DidOpenTextDocumentParams{
		TextDocument: protocol.TextDocumentItem{URI: orderURI, Version: 1, Text: src},
	}))
	published, unknown := unknownSz()
	require.True(t, published)
	require.True(t, unknown, "sz is not known before the save")

	require.NoError(t, os.WriteFile(methodsPath, []byte("(s:defmethod order sz (o) 0)\n"), 0o600))
	require.NoError(t, s.textDocumentDidSave(ctx, &protocol.DidSaveTextDocumentParams{
		TextDocument: protocol.TextDocumentIdentifier{URI: pathToURI(methodsPath)},
	}))
	require.Eventually(t, func() bool {
		_, unknown := unknownSz()
		return !unknown
	}, 5*time.Second, 5*time.Millisecond, "the open document is analyzed again with sz known")
}

// TestWorkspaceRecordMethodsTestFile checks that an open test file knows
// the methods of the workspace's test helpers, and a production file does
// not.
func TestWorkspaceRecordMethodsTestFile(t *testing.T) {
	dir := t.TempDir()
	require.NoError(t, os.WriteFile(filepath.Join(dir, "order_testhelpers.lisp"), []byte("(s:defmethod order helper (o) 0)\n"), 0o600))
	s := testServer()
	s.rootPath = dir
	s.buildWorkspaceIndex()
	src := "(set 'order (s:make-record \"order\"))\n"
	for name, want := range map[string]bool{"order_test.lisp": true, "order.lisp": false} {
		doc := openDoc(s, pathToURI(filepath.Join(dir, name)), src)
		s.ensureAnalysis(doc)
		require.NotNil(t, doc.analysis)
		_, ok := doc.analysis.WorkspaceRecordMethods()["order"]["helper"]
		assert.Equal(t, want, ok, name)
	}
}
