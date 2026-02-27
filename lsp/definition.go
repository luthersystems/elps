// Copyright © 2024 The ELPS authors

package lsp

import (
	"path/filepath"

	"github.com/tliron/glsp"
	protocol "github.com/tliron/glsp/protocol_3_16"
)

// textDocumentDefinition handles the textDocument/definition request.
func (s *Server) textDocumentDefinition(_ *glsp.Context, params *protocol.DefinitionParams) (any, error) {
	doc := s.docs.Get(params.TextDocument.URI)
	if doc == nil {
		return nil, nil
	}
	s.ensureAnalysis(doc)

	line := int(params.Position.Line)
	col := int(params.Position.Character)

	sym, _ := symbolAtPosition(doc, line, col)
	if sym == nil || sym.Source == nil {
		return nil, nil
	}

	// Builtins and special ops have no navigable source.
	if sym.Source.Pos < 0 {
		return nil, nil
	}

	// Build URI for the definition location.
	defURI := params.TextDocument.URI
	if sym.Source.File != "" && sym.Source.File != uriToPath(params.TextDocument.URI) {
		defPath := sym.Source.File
		// Resolve relative paths against workspace root.
		if !filepath.IsAbs(defPath) && s.rootPath != "" {
			defPath = filepath.Join(s.rootPath, defPath)
		}
		defURI = pathToURI(defPath)
	}

	return protocol.Location{
		URI:   defURI,
		Range: elpsToLSPRange(sym.Source, len(sym.Name)),
	}, nil
}
