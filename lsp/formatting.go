// Copyright © 2024 The ELPS authors

package lsp

import (
	"github.com/luthersystems/elps/formatter"
	"github.com/tliron/glsp"
	protocol "github.com/tliron/glsp/protocol_3_16"
)

// textDocumentFormatting handles textDocument/formatting requests.
// It formats the document content using the ELPS formatter and returns
// a single whole-document text edit, or nil if no changes are needed.
func (s *Server) textDocumentFormatting(_ *glsp.Context, params *protocol.DocumentFormattingParams) ([]protocol.TextEdit, error) {
	doc := s.docs.Get(params.TextDocument.URI)
	if doc == nil {
		return nil, nil
	}

	doc.mu.Lock()
	content := doc.Content
	uri := doc.URI
	doc.mu.Unlock()

	if content == "" {
		return nil, nil
	}

	cfg := formatter.DefaultConfig()
	if tabSize, ok := params.Options["tabSize"]; ok {
		switch v := tabSize.(type) {
		case float64:
			if v > 0 {
				cfg.IndentSize = int(v)
			}
		case int:
			if v > 0 {
				cfg.IndentSize = v
			}
		}
	}

	formatted, err := formatter.FormatFile([]byte(content), uriToPath(uri), cfg)
	if err != nil {
		// Parse error — return nil edits (not an error) so the editor
		// doesn't show an error dialog for incomplete code.
		return nil, nil
	}

	// No changes needed.
	if string(formatted) == content {
		return nil, nil
	}

	// Return a single edit replacing the entire document.
	lines := countLines(content)
	return []protocol.TextEdit{
		{
			Range: protocol.Range{
				Start: protocol.Position{Line: 0, Character: 0},
				End:   protocol.Position{Line: safeUint(lines), Character: 0},
			},
			NewText: string(formatted),
		},
	}, nil
}

// countLines returns the number of lines in s (0-indexed end line for LSP).
func countLines(s string) int {
	n := 0
	for _, c := range s {
		if c == '\n' {
			n++
		}
	}
	return n
}
