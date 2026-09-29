// Copyright © 2026 The ELPS authors

package lsp

import (
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	protocol "github.com/tliron/glsp/protocol_3_16"
)

func hoverAt(t *testing.T, s *Server, line, char uint32) string {
	t.Helper()
	hover, err := s.textDocumentHover(mockContext(), &protocol.HoverParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: "file:///test.lisp"},
			Position:     protocol.Position{Line: line, Character: char},
		},
	})
	require.NoError(t, err)
	if hover == nil {
		return ""
	}
	return hover.Contents.(protocol.MarkupContent).Value
}

// Hovering over lambda lists the enclosing local variables the closure
// captures (astutil.FreeVarsIn).
func TestHoverLambdaCaptures(t *testing.T) {
	s := testServer()
	openDoc(s, "file:///test.lisp", `(defun make-counter (start step)
  (let ((n start) (unused 0))
    (lambda () (set! n (+ n step)) n)))
(set 'k (lambda (x) (* x 2)))`)

	got := hoverAt(t, s, 2, 6) // on "lambda" in line 3
	assert.Contains(t, got, "**Captures:** `n`, `step`")
	assert.NotContains(t, got, "unused")

	// A closure that uses no local variable says so.
	assert.Contains(t, hoverAt(t, s, 3, 10), "Captures no local variables")
}
