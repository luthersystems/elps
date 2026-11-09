// Copyright © 2024 The ELPS authors

package lsp

import (
	"strings"

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
	// formatter.FormatFile parses the whole document itself, so it is the
	// one request that would still cost a full parse of an over-limit
	// document after Document.parse started honouring the limit. Same
	// answer as for incomplete code: no edits, no error dialog.
	if doc.OverLimit() {
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

	// Return a single edit replacing the entire document.  The edit ends
	// after the last character.  With no final newline that is inside the
	// last line, and an edit that ended at the line's start would leave the
	// old last line after the formatted text.
	lastLine := strings.Count(content, "\n")
	tail := content[strings.LastIndexByte(content, '\n')+1:]
	endCol := len(tail)
	if s.positionEncoding() != encodingUTF8 {
		endCol = utf16LenOf(tail)
	}
	return []protocol.TextEdit{
		{
			Range: protocol.Range{
				Start: protocol.Position{Line: 0, Character: 0},
				End:   protocol.Position{Line: safeUint(lastLine), Character: safeUint(endCol)},
			},
			NewText: string(formatted),
		},
	}, nil
}
