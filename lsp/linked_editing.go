// Copyright © 2024 The ELPS authors

package lsp

import (
	"github.com/tliron/glsp"
	protocol "github.com/tliron/glsp/protocol_3_16"
)

// elpsSymbolPattern constrains linked editing to valid ELPS symbol characters.
var elpsSymbolPattern = `[a-zA-Z0-9\-_!?+*/<>=:.#^]+`

// textDocumentLinkedEditingRange handles textDocument/linkedEditingRange.
// It returns the ranges of all same-file occurrences of the symbol under
// the cursor, enabling simultaneous rename in supporting editors.
func (s *Server) textDocumentLinkedEditingRange(_ *glsp.Context, params *protocol.LinkedEditingRangeParams) (*protocol.LinkedEditingRanges, error) {
	doc := s.docs.Get(params.TextDocument.URI)
	if doc == nil {
		return nil, nil
	}
	s.ensureAnalysis(doc)

	// elps#464: the client counts Character in the negotiated encoding
	// (UTF-16 unless it asked for utf-8); everything below counts bytes.
	line, col := s.cursorAt(doc, params.Position)

	sym, _ := symbolAtPosition(doc, line, col)
	if sym == nil || doc.analysis == nil {
		return nil, nil
	}

	// Skip builtins and special ops — they cannot be renamed.
	if sym.Source == nil || sym.Source.Pos < 0 {
		return nil, nil
	}

	currentFile := uriToPath(params.TextDocument.URI)
	var ranges []protocol.Range

	// Definition range — only if in the current file.
	defFile := sym.Source.File
	if defFile == "" || defFile == currentFile {
		ranges = append(ranges, nameRange(sym.Source, sym.Name))
	}

	// Reference ranges — current file only.  Each range is that of the name
	// alone, as in a rename: a range that covered the qualifier of
	// pkg:name would have the edit remove it.
	for _, ref := range doc.analysis.References {
		if ref.Symbol != sym || ref.Source == nil {
			continue
		}
		ranges = append(ranges, nameRange(ref.Source, sym.Name))
	}

	// The names in export forms, so that the export follows the edit.
	for _, ref := range doc.analysis.ExportRefs {
		if ref.Symbol != sym || ref.Source == nil {
			continue
		}
		ranges = append(ranges, nameRange(ref.Source, sym.Name))
	}

	if len(ranges) == 0 {
		return nil, nil
	}

	return &protocol.LinkedEditingRanges{
		Ranges:      ranges,
		WordPattern: &elpsSymbolPattern,
	}, nil
}
