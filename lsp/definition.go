// Copyright © 2024 The ELPS authors

package lsp

import (
	"path/filepath"

	"github.com/luthersystems/elps/analysis"
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
	word := wordAtPosition(doc.Content, line, col)

	if sym == nil || sym.Source == nil {
		if sym != nil && isBuiltin(sym) {
			// Core builtin with no source — return virtual URI.
			return builtinLocationForWord(word, sym.Name), nil
		}
		// Fallback: check for qualified symbol (e.g. "math:sin").
		if loc := s.qualifiedSymbolDefinition(word); loc != nil {
			return *loc, nil
		}
		return nil, nil
	}

	// Builtins and special ops with synthetic source (Pos < 0).
	if sym.Source.Pos < 0 {
		return builtinLocationForWord(word, sym.Name), nil
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

// builtinLocation builds an LSP Location with a virtual URI and zero range.
func builtinLocation(pkg, name string) protocol.Location {
	return protocol.Location{
		URI:   builtinURI(pkg, name),
		Range: protocol.Range{},
	}
}

// builtinLocationForWord builds a builtin location, deriving the package
// from a package-qualified word at the cursor (e.g. "math:sin" → pkg "math").
// Falls back to "lisp" for unqualified symbols.
func builtinLocationForWord(word, symName string) protocol.Location {
	pkg := "lisp"
	if pkgName, _, ok := splitPackageQualified(word); ok && pkgName != "" {
		pkg = pkgName
	}
	return builtinLocation(pkg, symName)
}

// qualifiedSymbolDefinition looks up a qualified symbol (e.g. "math:sin")
// in the workspace config and returns its definition location.
func (s *Server) qualifiedSymbolDefinition(word string) *protocol.Location {
	pkgName, symName, ok := splitPackageQualified(word)
	if !ok || symName == "" {
		return nil
	}

	s.ensureWorkspaceIndex()

	s.analysisCfgMu.RLock()
	cfg := s.analysisCfg
	s.analysisCfgMu.RUnlock()

	if cfg == nil || cfg.PackageExports == nil {
		return nil
	}

	exports, ok := cfg.PackageExports[pkgName]
	if !ok {
		return nil
	}

	for _, ext := range exports {
		if ext.Name != symName {
			continue
		}
		sym := externalToSymbol(&ext)
		if isBuiltin(sym) {
			loc := builtinLocation(pkgName, symName)
			return &loc
		}
		// User-defined symbol with real source.
		if sym.Source != nil && sym.Source.File != "" {
			defPath := sym.Source.File
			if !filepath.IsAbs(defPath) && s.rootPath != "" {
				defPath = filepath.Join(s.rootPath, defPath)
			}
			loc := protocol.Location{
				URI:   pathToURI(defPath),
				Range: elpsToLSPRange(sym.Source, len(sym.Name)),
			}
			return &loc
		}
		return nil
	}
	return nil
}

// externalToSymbol converts an ExternalSymbol to a Symbol for common helpers.
func externalToSymbol(ext *analysis.ExternalSymbol) *analysis.Symbol {
	return &analysis.Symbol{
		Name:      ext.Name,
		Kind:      ext.Kind,
		Source:    ext.Source,
		Signature: ext.Signature,
		DocString: ext.DocString,
		External:  true,
	}
}
