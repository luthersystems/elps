// Copyright © 2024 The ELPS authors

package lsp

import (
	"github.com/tliron/glsp"
	protocol "github.com/tliron/glsp/protocol_3_16"
)

// textDocumentDocumentHighlight handles textDocument/documentHighlight.
// It highlights all occurrences of the symbol under the cursor within the
// current document — definitions with Write kind, references with Read kind.
func (s *Server) textDocumentDocumentHighlight(_ *glsp.Context, params *protocol.DocumentHighlightParams) ([]protocol.DocumentHighlight, error) {
	doc := s.docs.Get(params.TextDocument.URI)
	if doc == nil {
		return nil, nil
	}
	s.ensureAnalysis(doc)

	line := int(params.Position.Line)
	col := int(params.Position.Character)

	sym, _ := symbolAtPosition(doc, line, col)
	if sym == nil || doc.analysis == nil {
		return nil, nil
	}

	currentFile := uriToPath(params.TextDocument.URI)

	var highlights []protocol.DocumentHighlight

	// Definition highlight (Write kind) — only if defined in the current file.
	if sym.Source != nil && sym.Source.Pos >= 0 {
		defFile := sym.Source.File
		if defFile == "" || defFile == currentFile {
			writeKind := protocol.DocumentHighlightKindWrite
			highlights = append(highlights, protocol.DocumentHighlight{
				Range: elpsToLSPRange(sym.Source, len(sym.Name)),
				Kind:  &writeKind,
			})
		}
	}

	// Reference highlights (Read kind) — current file only.
	readKind := protocol.DocumentHighlightKindRead
	for _, ref := range doc.analysis.References {
		if ref.Symbol != sym || ref.Source == nil {
			continue
		}
		highlights = append(highlights, protocol.DocumentHighlight{
			Range: elpsToLSPRange(ref.Source, len(sym.Name)),
			Kind:  &readKind,
		})
	}

	return highlights, nil
}
