// Copyright © 2026 The ELPS authors

package lsp

import (
	"os"
	"path/filepath"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	protocol "github.com/tliron/glsp/protocol_3_16"
)

// TestCompletionListsGlobalsOfThePackage pins that completion lists the
// definitions other files make in the current package. The analysis of a
// document uses a GlobalIndex, with which the root scope's PackageSymbols holds
// only the globals the file looked up, so completion must list them through
// Scope.EachPackageSymbol (luthersystems/elps#944).
func TestCompletionListsGlobalsOfThePackage(t *testing.T) {
	files := map[string]string{
		"defs.lisp": "(in-package 'helpers)\n(defun add-one (x) (+ x 1))\n(defun add-two (x) (+ x 2))\n",
		"use.lisp":  "(in-package 'helpers)\n(defun work () (add-one 1))\n(add",
	}
	s := testServer()
	dir := writeWorkspace(t, s, files)

	uri := pathToURI(filepath.Join(dir, "use.lisp"))
	doc := openDoc(s, uri, files["use.lisp"])
	view := s.ensureAnalysis(doc)
	require.NotNil(t, view.analysis.GlobalIndex, "the analysis of a document must use a GlobalIndex")
	result, err := s.textDocumentCompletion(mockContext(), &protocol.CompletionParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: uri},
			Position:     protocol.Position{Line: 2, Character: 4},
		},
	})
	require.NoError(t, err)
	labels := completionLabels(t, result)
	assert.Contains(t, labels, "add-one", "a global the file calls")
	assert.Contains(t, labels, "add-two", "a global the file never looked up")
}

// TestSaveKeepsTheGlobalIndexCurrent pins that the index, which the server
// builds on the first analysis that needs it and not on a save, follows a
// save: the next analysis finds a definition the save adds and not one it
// removes (luthersystems/elps#944).
func TestSaveKeepsTheGlobalIndexCurrent(t *testing.T) {
	files := map[string]string{
		"defs.lisp": "(in-package 'helpers)\n(defun add-one (x) (+ x 1))\n",
		"use.lisp":  "(in-package 'helpers)\n(defun work () (add-one 1))\n",
	}
	s := testServer()
	dir := writeWorkspace(t, s, files)
	use := pathToURI(filepath.Join(dir, "use.lisp"))
	count := func(name string) int {
		n := 0
		for range s.getAnalysisConfig(use).GlobalIndex.Named(name) {
			n++
		}
		return n
	}
	before := s.getAnalysisConfig(use).GlobalIndex
	require.NotNil(t, before)
	assert.Equal(t, 1, count("add-one"))
	assert.Same(t, before, s.getAnalysisConfig(use).GlobalIndex, "analyses with the same globals share one index")

	defs := filepath.Join(dir, "defs.lisp")
	require.NoError(t, os.WriteFile(defs, []byte("(in-package 'helpers)\n(defun add-two (x) (+ x 2))\n"), 0o600))
	s.updateFileDefinitions(pathToURI(defs))
	assert.Equal(t, 0, count("add-one"), "a definition the save removed")
	assert.Equal(t, 1, count("add-two"), "a definition the save added")
}
