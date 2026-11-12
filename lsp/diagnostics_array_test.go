// Copyright © 2026 The ELPS authors

package lsp

import (
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	protocol "github.com/tliron/glsp/protocol_3_16"
)

// TestCleanDocumentPublishesEmptyArray checks that a document with no
// problems gets an empty diagnostics array, not null. LSP defines no null
// there, and a client that ignores it keeps showing the diagnostics of a
// problem the user has just fixed (found while testing elps#952).
func TestCleanDocumentPublishesEmptyArray(t *testing.T) {
	s := testServer()
	setTestAnalysisCfg(s, nil)
	var captured []*protocol.PublishDiagnosticsParams
	s.notify = func(method string, params any) {
		if method == protocol.ServerTextDocumentPublishDiagnostics {
			captured = append(captured, publishedDiagnostics(params))
		}
	}
	s.analyzeAndPublish(openDoc(s, "file:///clean.lisp", "(defun f () 1)\n(f)\n"))
	require.Len(t, captured, 1)
	assert.NotNil(t, captured[0].Diagnostics, "diagnostics must be an array, not null")
	assert.Empty(t, captured[0].Diagnostics)
}
