// Copyright © 2024 The ELPS authors

package lsp

import (
	"fmt"

	"github.com/luthersystems/elps/analysis"
	"github.com/tliron/glsp"
	protocol "github.com/tliron/glsp/protocol_3_16"
)

// textDocumentPrepareRename validates that the symbol under the cursor
// is renameable and returns its range.
func (s *Server) textDocumentPrepareRename(_ *glsp.Context, params *protocol.PrepareRenameParams) (any, error) {
	doc := s.docs.Get(params.TextDocument.URI)
	if doc == nil {
		return nil, nil // no document — rename not applicable
	}
	s.ensureAnalysis(doc)

	line := int(params.Position.Line)
	col := int(params.Position.Character)

	sym, ref := symbolAtPosition(doc, line, col)
	if sym == nil {
		return nil, nil // no symbol at position — rename not applicable
	}

	// Reject renaming builtins, special ops, and external symbols.
	// Per LSP spec, prepareRename returns null (not error) for non-renameable symbols.
	if sym.Kind == analysis.SymBuiltin || sym.Kind == analysis.SymSpecialOp {
		return nil, nil
	}
	if sym.External {
		return nil, nil
	}

	// Determine the range to highlight.
	var loc = sym.Source
	if ref != nil && ref.Source != nil {
		loc = ref.Source
	}
	if loc == nil {
		return nil, nil
	}

	return &protocol.RangeWithPlaceholder{
		Range:       elpsToLSPRange(loc, len(sym.Name)),
		Placeholder: sym.Name,
	}, nil
}

// textDocumentRename handles the textDocument/rename request.
func (s *Server) textDocumentRename(_ *glsp.Context, params *protocol.RenameParams) (*protocol.WorkspaceEdit, error) {
	doc := s.docs.Get(params.TextDocument.URI)
	if doc == nil {
		return nil, fmt.Errorf("document not found")
	}
	s.ensureAnalysis(doc)

	line := int(params.Position.Line)
	col := int(params.Position.Character)

	sym, _ := symbolAtPosition(doc, line, col)
	if sym == nil {
		return nil, fmt.Errorf("no symbol at position")
	}

	if sym.Kind == analysis.SymBuiltin || sym.Kind == analysis.SymSpecialOp {
		return nil, fmt.Errorf("cannot rename %s: %s", symbolKindLabel(sym.Kind), sym.Name)
	}
	if sym.External {
		return nil, fmt.Errorf("cannot rename external symbol: %s", sym.Name)
	}

	edits := make(map[protocol.DocumentUri][]protocol.TextEdit)
	docURI := params.TextDocument.URI

	// Rename at the definition site.
	if sym.Source != nil && sym.Source.Pos >= 0 && sym.Source.Line > 0 {
		defURI := s.resolveURI(docURI, sym.Source.File)
		edits[defURI] = append(edits[defURI], protocol.TextEdit{
			Range:   elpsToLSPRange(sym.Source, len(sym.Name)),
			NewText: params.NewName,
		})
	}

	// Rename at all reference sites.
	if doc.analysis != nil {
		for _, ref := range doc.analysis.References {
			if ref.Symbol != sym || ref.Source == nil {
				continue
			}
			refURI := s.resolveURI(docURI, ref.Source.File)
			edits[refURI] = append(edits[refURI], protocol.TextEdit{
				Range:   elpsToLSPRange(ref.Source, len(sym.Name)),
				NewText: params.NewName,
			})
		}
	}

	return &protocol.WorkspaceEdit{Changes: edits}, nil
}
