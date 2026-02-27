// Copyright © 2024 The ELPS authors

package lsp

import (
	"fmt"
	"strings"

	"github.com/luthersystems/elps/analysis"
	"github.com/tliron/glsp"
	protocol "github.com/tliron/glsp/protocol_3_16"
)

// textDocumentHover handles the textDocument/hover request.
func (s *Server) textDocumentHover(_ *glsp.Context, params *protocol.HoverParams) (*protocol.Hover, error) {
	doc := s.docs.Get(params.TextDocument.URI)
	if doc == nil {
		return nil, nil
	}
	s.ensureAnalysis(doc)

	line := int(params.Position.Line)
	col := int(params.Position.Character)

	sym, _ := symbolAtPosition(doc, line, col)
	if sym == nil {
		return nil, nil
	}

	content := buildHoverContent(sym)
	if content == "" {
		return nil, nil
	}

	return &protocol.Hover{
		Contents: protocol.MarkupContent{
			Kind:  protocol.MarkupKindMarkdown,
			Value: content,
		},
	}, nil
}

// buildHoverContent builds Markdown hover text for a symbol.
func buildHoverContent(sym *analysis.Symbol) string {
	var sb strings.Builder

	// Kind label.
	kindLabel := symbolKindLabel(sym.Kind)

	// Header: **kind** `name`
	fmt.Fprintf(&sb, "**%s** `%s`", kindLabel, sym.Name)

	// Signature for callables.
	if sym.Signature != nil {
		sig := formatSignature(sym.Signature)
		// Strip outer parens from "(x y)" to get "x y" for inline display.
		inner := sig[1 : len(sig)-1]
		if inner != "" {
			fmt.Fprintf(&sb, "\n\n```lisp\n(%s %s)\n```", sym.Name, inner)
		} else {
			fmt.Fprintf(&sb, "\n\n```lisp\n(%s)\n```", sym.Name)
		}
	}

	// Docstring.
	if sym.DocString != "" {
		fmt.Fprintf(&sb, "\n\n%s", sym.DocString)
	}

	// Source location.
	if sym.Source != nil && sym.Source.File != "" {
		fmt.Fprintf(&sb, "\n\n*Defined in %s:%d*", sym.Source.File, sym.Source.Line)
	}

	return sb.String()
}

func symbolKindLabel(kind analysis.SymbolKind) string {
	switch kind {
	case analysis.SymFunction:
		return "function"
	case analysis.SymMacro:
		return "macro"
	case analysis.SymVariable:
		return "variable"
	case analysis.SymParameter:
		return "parameter"
	case analysis.SymSpecialOp:
		return "special operator"
	case analysis.SymBuiltin:
		return "builtin"
	case analysis.SymType:
		return "type"
	default:
		return "symbol"
	}
}
