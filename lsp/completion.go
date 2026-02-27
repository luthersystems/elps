// Copyright © 2024 The ELPS authors

package lsp

import (
	"strings"

	"github.com/tliron/glsp"
	protocol "github.com/tliron/glsp/protocol_3_16"
)

// textDocumentCompletion handles the textDocument/completion request.
func (s *Server) textDocumentCompletion(_ *glsp.Context, params *protocol.CompletionParams) (any, error) {
	doc := s.docs.Get(params.TextDocument.URI)
	if doc == nil {
		return nil, nil
	}
	s.ensureAnalysis(doc)

	line := int(params.Position.Line)
	col := int(params.Position.Character)

	prefix := wordAtPosition(doc.Content, line, col)

	var items []protocol.CompletionItem

	// Check for package-qualified completion (prefix contains ':').
	if pkgName, partial, ok := splitPackageQualified(prefix); ok {
		items = s.packageCompletions(doc, pkgName, partial)
	} else {
		items = s.scopeCompletions(doc, line, col, prefix)
	}

	return items, nil
}

// splitPackageQualified checks if a prefix looks like "pkg:partial" and
// splits it. Returns false if there's no package qualifier.
func splitPackageQualified(prefix string) (pkg, partial string, ok bool) {
	// Keywords start with ':' — don't treat as package qualifier.
	if strings.HasPrefix(prefix, ":") {
		return "", "", false
	}
	idx := strings.LastIndex(prefix, ":")
	if idx < 0 {
		return "", "", false
	}
	return prefix[:idx], prefix[idx+1:], true
}

// packageCompletions returns completion items from a package's exports.
func (s *Server) packageCompletions(doc *Document, pkgName, partial string) []protocol.CompletionItem {
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

	var items []protocol.CompletionItem
	for _, ext := range exports {
		if partial != "" && !strings.HasPrefix(ext.Name, partial) {
			continue
		}
		kind := mapCompletionItemKind(ext.Kind)
		label := pkgName + ":" + ext.Name
		item := protocol.CompletionItem{
			Label: label,
			Kind:  &kind,
		}
		if ext.Signature != nil {
			detail := formatSignature(ext.Signature)
			item.Detail = &detail
		}
		items = append(items, item)
	}
	return items
}

// scopeCompletions returns completion items from the scope chain at the
// cursor position.
func (s *Server) scopeCompletions(doc *Document, line, col int, prefix string) []protocol.CompletionItem {
	if doc.analysis == nil {
		return nil
	}

	// Find the innermost scope at cursor.
	elpsLine := line + 1
	elpsCol := col + 1
	scope := scopeAtPosition(doc.analysis.RootScope, elpsLine, elpsCol)

	syms := collectVisibleSymbols(scope)

	var items []protocol.CompletionItem
	for _, sym := range syms {
		if prefix != "" && !strings.HasPrefix(sym.Name, prefix) {
			continue
		}
		kind := mapCompletionItemKind(sym.Kind)
		item := protocol.CompletionItem{
			Label: sym.Name,
			Kind:  &kind,
		}
		if sym.Signature != nil {
			detail := formatSignature(sym.Signature)
			item.Detail = &detail
		}
		if sym.DocString != "" {
			item.Documentation = &protocol.MarkupContent{
				Kind:  protocol.MarkupKindMarkdown,
				Value: sym.DocString,
			}
		}
		items = append(items, item)
	}

	// Also add package-qualified completions from workspace exports.
	s.analysisCfgMu.RLock()
	cfg := s.analysisCfg
	s.analysisCfgMu.RUnlock()
	if cfg != nil && cfg.PackageExports != nil {
		for pkgName := range cfg.PackageExports {
			if prefix != "" && !strings.HasPrefix(pkgName+":", prefix) {
				continue
			}
			kind := protocol.CompletionItemKindModule
			items = append(items, protocol.CompletionItem{
				Label: pkgName + ":",
				Kind:  &kind,
			})
		}
	}

	return items
}

