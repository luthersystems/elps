// Copyright © 2024 The ELPS authors

package lsp

import (
	"fmt"
	"strings"

	"github.com/luthersystems/elps/analysis"
	"github.com/tliron/glsp"
	protocol "github.com/tliron/glsp/protocol_3_16"
)

// textDocumentCodeAction handles the textDocument/codeAction request.
// It returns quick-fix actions for diagnostics in the requested range.
func (s *Server) textDocumentCodeAction(_ *glsp.Context, params *protocol.CodeActionParams) (any, error) {
	doc := s.docs.Get(params.TextDocument.URI)
	if doc == nil {
		return nil, nil
	}

	// If the client only wants specific kinds, check we support them.
	if len(params.Context.Only) > 0 {
		if !slicesContains(params.Context.Only, protocol.CodeActionKindQuickFix) {
			return nil, nil
		}
	}

	s.ensureAnalysis(doc)

	doc.mu.Lock()
	content := doc.Content
	docAnalysis := doc.analysis
	doc.mu.Unlock()

	s.analysisCfgMu.RLock()
	cfg := s.analysisCfg
	s.analysisCfgMu.RUnlock()

	var actions []protocol.CodeAction

	for _, diag := range params.Context.Diagnostics {
		// Only handle diagnostics from our lint source.
		if diag.Source == nil {
			continue
		}

		switch *diag.Source {
		case "elps-lint":
			analyzerName := ""
			if diag.Code != nil {
				analyzerName = fmt.Sprintf("%v", diag.Code.Value)
			}

			switch analyzerName {
			case "undefined-symbol":
				actions = append(actions,
					fixUndefinedSymbol(params.TextDocument.URI, diag, content, cfg)...)
			case "unused-variable", "unused-function":
				// Offer nolint suppression for unused warnings.
				actions = append(actions,
					suppressLintAction(params.TextDocument.URI, diag, analyzerName, content))
			default:
				// All lint diagnostics can be suppressed with nolint.
				if analyzerName != "" {
					actions = append(actions,
						suppressLintAction(params.TextDocument.URI, diag, analyzerName, content))
				}
			}
		case "elps":
			// Parse errors — no quick fixes available.
		}
	}

	// Also offer "add use-package" for unresolved refs from analysis
	// that overlap with the requested range.
	actions = append(actions,
		unresolvedRefActions(params.TextDocument.URI, params.Range, docAnalysis, content, cfg)...)

	if len(actions) == 0 {
		return nil, nil
	}
	return actions, nil
}

// fixUndefinedSymbol creates code actions to add use-package for an undefined symbol.
func fixUndefinedSymbol(uri string, diag protocol.Diagnostic, content string, cfg *analysis.Config) []protocol.CodeAction {
	if cfg == nil {
		return nil
	}
	// Extract the symbol name from the diagnostic message.
	// Messages look like: "undefined symbol: foo"
	symName := extractSymbolFromMessage(diag.Message)
	if symName == "" {
		return nil
	}

	return usePackageActions(uri, diag.Range, symName, content, cfg, &diag)
}

// unresolvedRefActions creates code actions for unresolved references that
// overlap with the requested range.
func unresolvedRefActions(uri string, rng protocol.Range, res *analysis.Result, content string, cfg *analysis.Config) []protocol.CodeAction {
	if res == nil || cfg == nil {
		return nil
	}
	var actions []protocol.CodeAction
	for _, ref := range res.Unresolved {
		if ref.Source == nil || ref.Source.Line == 0 {
			continue
		}
		refLine := safeUint(ref.Source.Line - 1)
		if refLine < rng.Start.Line || refLine > rng.End.Line {
			continue
		}
		actions = append(actions,
			usePackageActions(uri, elpsToLSPRange(ref.Source, len(ref.Name)), ref.Name, content, cfg, nil)...)
	}
	return actions
}

// usePackageActions searches all package exports for a symbol name and returns
// code actions that insert (use-package 'pkg) at the top of the file.
func usePackageActions(uri string, _ protocol.Range, symName, content string, cfg *analysis.Config, diag *protocol.Diagnostic) []protocol.CodeAction {
	var actions []protocol.CodeAction
	for pkg, exports := range cfg.PackageExports {
		for _, sym := range exports {
			if sym.Name == symName {
				title := fmt.Sprintf("Add (use-package '%s)", pkg)
				insertText := fmt.Sprintf("(use-package '%s)\n", pkg)
				insertPos := usePackageInsertPosition(content)
				kind := protocol.CodeActionKindQuickFix
				action := protocol.CodeAction{
					Title: title,
					Kind:  &kind,
					Edit: &protocol.WorkspaceEdit{
						Changes: map[string][]protocol.TextEdit{
							uri: {
								{
									Range:   protocol.Range{Start: insertPos, End: insertPos},
									NewText: insertText,
								},
							},
						},
					},
				}
				if diag != nil {
					action.Diagnostics = []protocol.Diagnostic{*diag}
				}
				actions = append(actions, action)
				break // one action per package
			}
		}
	}
	return actions
}

// suppressLintAction creates a code action that adds a ; nolint:analyzer-name
// comment to the end of the diagnostic line.
func suppressLintAction(uri string, diag protocol.Diagnostic, analyzer, content string) protocol.CodeAction {
	line := int(diag.Range.Start.Line)
	lines := strings.Split(content, "\n")
	lineEnd := 0
	if line >= 0 && line < len(lines) {
		lineEnd = len(lines[line])
	}

	kind := protocol.CodeActionKindQuickFix
	insertPos := protocol.Position{Line: diag.Range.Start.Line, Character: safeUint(lineEnd)}
	return protocol.CodeAction{
		Title:       fmt.Sprintf("Suppress with ; nolint:%s", analyzer),
		Kind:        &kind,
		Diagnostics: []protocol.Diagnostic{diag},
		Edit: &protocol.WorkspaceEdit{
			Changes: map[string][]protocol.TextEdit{
				uri: {
					{
						Range:   protocol.Range{Start: insertPos, End: insertPos},
						NewText: " ; nolint:" + analyzer,
					},
				},
			},
		},
	}
}

// usePackageInsertPosition finds the best position to insert a use-package form.
// It inserts after the last existing use-package or in-package at the top of
// the file, or at the very beginning if none exist.
func usePackageInsertPosition(content string) protocol.Position {
	lines := strings.Split(content, "\n")
	lastPkgLine := -1
	for i, line := range lines {
		trimmed := strings.TrimSpace(line)
		if strings.HasPrefix(trimmed, "(in-package ") ||
			strings.HasPrefix(trimmed, "(use-package ") {
			lastPkgLine = i
		}
	}
	if lastPkgLine >= 0 {
		// Insert after the last package-related form.
		return protocol.Position{
			Line:      safeUint(lastPkgLine + 1),
			Character: 0,
		}
	}
	// Insert at the very beginning.
	return protocol.Position{Line: 0, Character: 0}
}

// extractSymbolFromMessage extracts the symbol name from a lint message.
// Expected formats: "undefined symbol: foo", "undefined symbol: foo (did you mean ...)"
// extractSymbolFromMessage extracts the symbol name from a lint message.
// Expected formats: "undefined symbol: foo", "undefined symbol: foo (did you mean ...)"
func extractSymbolFromMessage(msg string) string {
	const prefix = "undefined symbol: "
	_, after, found := strings.Cut(msg, prefix)
	if !found {
		return ""
	}
	// Trim any parenthetical suffix.
	if rest, _, ok := strings.Cut(after, " ("); ok {
		return strings.TrimSpace(rest)
	}
	return strings.TrimSpace(after)
}

// slicesContains checks if a string slice contains a value.
func slicesContains(ss []string, v string) bool {
	for _, s := range ss {
		if s == v {
			return true
		}
	}
	return false
}
