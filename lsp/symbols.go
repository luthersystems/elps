// Copyright © 2024 The ELPS authors

package lsp

import (
	"github.com/luthersystems/elps/analysis"
	"github.com/tliron/glsp"
	protocol "github.com/tliron/glsp/protocol_3_16"
)

// textDocumentDocumentSymbol handles the textDocument/documentSymbol request.
func (s *Server) textDocumentDocumentSymbol(_ *glsp.Context, params *protocol.DocumentSymbolParams) (any, error) {
	doc := s.docs.Get(params.TextDocument.URI)
	if doc == nil {
		return nil, nil
	}
	s.ensureAnalysis(doc)

	if doc.analysis == nil {
		return nil, nil
	}

	docPath := uriToPath(params.TextDocument.URI)
	var symbols []protocol.DocumentSymbol

	for _, sym := range doc.analysis.Symbols {
		// Skip external symbols (from workspace scan or imports).
		if sym.External {
			continue
		}
		// Skip symbols not defined in this file.
		if sym.Source == nil || sym.Source.Line == 0 {
			continue
		}
		if sym.Source.File != "" && sym.Source.File != docPath {
			continue
		}
		// Skip parameters and let bindings — only report top-level defs.
		if sym.Scope != nil && sym.Scope.Kind != analysis.ScopeGlobal {
			continue
		}

		r := elpsToLSPRange(sym.Source, len(sym.Name))
		detail := symbolDetail(sym)
		symbols = append(symbols, protocol.DocumentSymbol{
			Name:           sym.Name,
			Detail:         detail,
			Kind:           mapSymbolKind(sym.Kind),
			Range:          r,
			SelectionRange: r,
		})
	}

	// Return as []DocumentSymbol (the preferred hierarchical form).
	return symbols, nil
}

// symbolDetail builds a short detail string (e.g., function signature).
func symbolDetail(sym *analysis.Symbol) *string {
	if sym.Signature == nil {
		return nil
	}
	s := formatSignature(sym.Signature)
	return &s
}
