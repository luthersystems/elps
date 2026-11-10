// Copyright © 2026 The ELPS authors

package lsp

import (
	"sync"
	"testing"

	"github.com/stretchr/testify/require"
	protocol "github.com/tliron/glsp/protocol_3_16"
)

// TestHandlersDuringReanalysis runs each request handler that reads a
// document's analysis while the re-analysis pass of a timer drops that
// analysis and builds it again (luthersystems/elps#943).
//
// The timers (the didChange debounce, the record method pass) run on
// their own goroutines and write Document.analysis under Document.mu. A
// handler that reads the field without the lock races with them. A
// handler that reads it two times can also see the result of one read
// and nil on the next, and dereference nil. Under -race the detector
// reports the first case. The iteration count makes the two goroutines
// overlap on each run.
func TestHandlersDuringReanalysis(t *testing.T) {
	s := testServer()
	uri := "file:///race.lisp"
	// "make-record" in the text selects the document for
	// reanalyzeRecordDocuments.
	src := "(in-package 'race)\n" +
		"(export 'helper)\n" +
		"(defun helper (x) (+ x 1))\n" +
		"(defun caller (y) (helper (helper y)))\n" +
		"(set 'order (s:make-record \"order\"))\n"
	doc := openDoc(s, uri, src)
	s.ensureAnalysis(doc)

	const rounds = 300
	stop := make(chan struct{})
	var wg sync.WaitGroup
	wg.Add(1)
	go func() {
		defer wg.Done()
		for {
			select {
			case <-stop:
				return
			default:
			}
			// The body of the record method timer.
			s.reanalyzeRecordDocuments("")
			// The state a didChange leaves for its debounce timer.
			s.docs.Change(uri, 1, src)
		}
	}()

	id := protocol.TextDocumentIdentifier{URI: uri}
	// The cursor is on the first helper of the caller body (line 3).
	at := protocol.TextDocumentPositionParams{
		TextDocument: id,
		Position:     protocol.Position{Line: 3, Character: 19},
	}
	ctx := mockContext()
	for range rounds {
		_, err := s.textDocumentHover(ctx, &protocol.HoverParams{TextDocumentPositionParams: at})
		require.NoError(t, err)
		_, err = s.textDocumentDefinition(ctx, &protocol.DefinitionParams{TextDocumentPositionParams: at})
		require.NoError(t, err)
		_, err = s.textDocumentReferences(ctx, &protocol.ReferenceParams{
			TextDocumentPositionParams: at,
			Context:                    protocol.ReferenceContext{IncludeDeclaration: true},
		})
		require.NoError(t, err)
		_, err = s.textDocumentDocumentHighlight(ctx, &protocol.DocumentHighlightParams{TextDocumentPositionParams: at})
		require.NoError(t, err)
		_, err = s.textDocumentLinkedEditingRange(ctx, &protocol.LinkedEditingRangeParams{TextDocumentPositionParams: at})
		require.NoError(t, err)
		_, err = s.textDocumentDocumentSymbol(ctx, &protocol.DocumentSymbolParams{TextDocument: id})
		require.NoError(t, err)
		_, err = s.textDocumentCompletion(ctx, &protocol.CompletionParams{TextDocumentPositionParams: at})
		require.NoError(t, err)
		_, err = s.textDocumentPrepareRename(ctx, &protocol.PrepareRenameParams{TextDocumentPositionParams: at})
		require.NoError(t, err)
		// A rename fails when the timer has dropped the analysis and the
		// cursor has no symbol yet. Only the race matters here.
		_, _ = s.textDocumentRename(ctx, &protocol.RenameParams{
			TextDocumentPositionParams: at,
			NewName:                    "helper2",
		})
		_, err = s.textDocumentPrepareCallHierarchy(ctx, &protocol.CallHierarchyPrepareParams{TextDocumentPositionParams: at})
		require.NoError(t, err)
		_, err = s.workspaceSymbol(ctx, &protocol.WorkspaceSymbolParams{Query: "help"})
		require.NoError(t, err)
	}
	close(stop)
	wg.Wait()
}
