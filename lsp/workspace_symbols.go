// Copyright © 2024 The ELPS authors

package lsp

import (
	"strings"

	"github.com/luthersystems/elps/analysis"
	"github.com/tliron/glsp"
	protocol "github.com/tliron/glsp/protocol_3_16"
)

// workspaceSymbol handles the workspace/symbol request.
// It returns all top-level definitions across the workspace that match the
// query string. An empty query returns all symbols.
func (s *Server) workspaceSymbol(_ *glsp.Context, params *protocol.WorkspaceSymbolParams) ([]protocol.SymbolInformation, error) {
	s.ensureWorkspaceIndex()

	s.analysisCfgMu.RLock()
	cfg := s.analysisCfg
	s.analysisCfgMu.RUnlock()

	if cfg == nil {
		return nil, nil
	}

	query := strings.ToLower(params.Query)
	var results []protocol.SymbolInformation

	// Collect symbols from workspace globals (top-level defs across files).
	for _, sym := range cfg.ExtraGlobals {
		if !matchesQuery(sym.Name, query) {
			continue
		}
		si, ok := externalSymbolToInfo(sym, "")
		if !ok {
			continue
		}
		results = append(results, si)
	}

	// Collect symbols from package exports.
	for pkg, syms := range cfg.PackageExports {
		for _, sym := range syms {
			if !matchesQuery(sym.Name, query) && !matchesQuery(pkg+":"+sym.Name, query) {
				continue
			}
			si, ok := externalSymbolToInfo(sym, pkg)
			if !ok {
				continue
			}
			results = append(results, si)
		}
	}

	// Also include symbols from open documents (definitions the user is
	// actively editing that may not yet be in the workspace index).
	seen := make(map[string]bool)
	for _, r := range results {
		seen[r.Name+"|"+r.Location.URI] = true
	}
	for _, doc := range s.docs.All() {
		s.ensureAnalysis(doc)
		if doc.analysis == nil {
			continue
		}
		docPath := uriToPath(doc.URI)
		for _, sym := range doc.analysis.Symbols {
			if sym.External {
				continue
			}
			if sym.Source == nil || sym.Source.Line == 0 {
				continue
			}
			if sym.Source.File != "" && sym.Source.File != docPath {
				continue
			}
			// Only top-level definitions.
			if sym.Scope != nil && sym.Scope.Kind != analysis.ScopeGlobal {
				continue
			}
			if !matchesQuery(sym.Name, query) {
				continue
			}
			key := sym.Name + "|" + doc.URI
			if seen[key] {
				continue
			}
			seen[key] = true
			r := elpsToLSPRange(sym.Source, len(sym.Name))
			results = append(results, protocol.SymbolInformation{
				Name: sym.Name,
				Kind: mapSymbolKind(sym.Kind),
				Location: protocol.Location{
					URI:   doc.URI,
					Range: r,
				},
			})
		}
	}

	return results, nil
}

// externalSymbolToInfo converts an analysis.ExternalSymbol to a
// protocol.SymbolInformation. Returns false if the symbol has no usable
// source location.
func externalSymbolToInfo(sym analysis.ExternalSymbol, pkg string) (protocol.SymbolInformation, bool) {
	if sym.Source == nil || sym.Source.Line == 0 {
		return protocol.SymbolInformation{}, false
	}
	r := elpsToLSPRange(sym.Source, len(sym.Name))
	uri := pathToURI(sym.Source.File)

	name := sym.Name
	var containerName *string
	if pkg != "" {
		containerName = &pkg
	}

	return protocol.SymbolInformation{
		Name:          name,
		Kind:          mapSymbolKind(sym.Kind),
		Location:      protocol.Location{URI: uri, Range: r},
		ContainerName: containerName,
	}, true
}

// matchesQuery performs case-insensitive substring matching. An empty query
// matches everything (per LSP spec: empty string requests all symbols).
func matchesQuery(name, lowerQuery string) bool {
	if lowerQuery == "" {
		return true
	}
	return strings.Contains(strings.ToLower(name), lowerQuery)
}
