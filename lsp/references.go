// Copyright © 2024 The ELPS authors

package lsp

import (
	"path/filepath"

	"github.com/tliron/glsp"
	protocol "github.com/tliron/glsp/protocol_3_16"
)

// textDocumentReferences handles the textDocument/references request.
func (s *Server) textDocumentReferences(_ *glsp.Context, params *protocol.ReferenceParams) ([]protocol.Location, error) {
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

	var locs []protocol.Location

	// Optionally include the declaration.
	if params.Context.IncludeDeclaration && sym.Source != nil && sym.Source.Pos >= 0 {
		defURI := s.resolveURI(params.TextDocument.URI, sym.Source.File)
		locs = append(locs, protocol.Location{
			URI:   defURI,
			Range: elpsToLSPRange(sym.Source, len(sym.Name)),
		})
	}

	// Find all references to this symbol in the current file.
	for _, ref := range doc.analysis.References {
		if ref.Symbol != sym || ref.Source == nil {
			continue
		}
		refURI := s.resolveURI(params.TextDocument.URI, ref.Source.File)
		locs = append(locs, protocol.Location{
			URI:   refURI,
			Range: elpsToLSPRange(ref.Source, len(sym.Name)),
		})
	}

	// Cross-file references from workspace index.
	key := symbolToKey(sym)
	currentFile := uriToPath(params.TextDocument.URI)
	for _, wref := range s.getWorkspaceRefs(key, currentFile) {
		refURI := s.resolveURI(params.TextDocument.URI, wref.File)
		locs = append(locs, protocol.Location{
			URI:   refURI,
			Range: elpsToLSPRange(wref.Source, len(sym.Name)),
		})
	}

	return locs, nil
}

// resolveURI resolves a file path from analysis into a document URI.
// If the file matches the current document, the original URI is returned.
func (s *Server) resolveURI(currentURI, file string) string {
	if file == "" || file == uriToPath(currentURI) {
		return currentURI
	}
	path := file
	if !filepath.IsAbs(path) && s.rootPath != "" {
		path = filepath.Join(s.rootPath, path)
	}
	return pathToURI(path)
}
