// Copyright © 2024 The ELPS authors

package lsp

import (
	"fmt"
	"strings"

	"github.com/luthersystems/elps/analysis"
	"github.com/luthersystems/elps/astutil"
	"github.com/luthersystems/elps/lisp"
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
	if sym != nil {
		content := buildHoverContent(sym)
		if content != "" {
			return &protocol.Hover{
				Contents: protocol.MarkupContent{
					Kind:  protocol.MarkupKindMarkdown,
					Value: content,
				},
			}, nil
		}
	}

	// Fallback: check for qualified symbol (e.g. "string:join") in package exports.
	word := wordAtPosition(doc.Content, line, col)
	if content := s.qualifiedSymbolHover(word); content != "" {
		return &protocol.Hover{
			Contents: protocol.MarkupContent{
				Kind:  protocol.MarkupKindMarkdown,
				Value: content,
			},
		}, nil
	}

	// Fallback: check builtins, special ops, macros, and use-package imports
	// in the registry's package exports.
	if content := s.registryHover(word, doc.ast, line+1); content != "" {
		return &protocol.Hover{
			Contents: protocol.MarkupContent{
				Kind:  protocol.MarkupKindMarkdown,
				Value: content,
			},
		}, nil
	}

	// Fallback: language keywords (not callable, but used in special contexts).
	if content, ok := keywordDocs[word]; ok {
		return &protocol.Hover{
			Contents: protocol.MarkupContent{
				Kind:  protocol.MarkupKindMarkdown,
				Value: content,
			},
		}, nil
	}

	return nil, nil
}

// keywordDocs provides hover documentation for language keywords that are
// not registered as callable symbols but appear in source code.
var keywordDocs = map[string]string{
	"&rest":             "**&rest** — variadic parameter marker\n\nMarks the start of a variadic parameter in a formal argument list. The parameter after `&rest` collects all remaining arguments into a list.\n\n```lisp\n(defun f (x &rest args) args)\n```",
	"&optional":         "**&optional** — optional parameter marker\n\nMarks the start of optional parameters in a formal argument list. Optional parameters are bound to `()` if not provided.\n\n```lisp\n(defun f (x &optional y) y)\n```",
	"&key":              "**&key** — keyword parameter marker\n\nMarks the start of keyword parameters in a formal argument list. Keyword arguments are passed as `:name value` pairs.\n\n```lisp\n(defun f (&key x y) (list x y))\n```",
	"unquote":           "**unquote** — evaluate inside quasiquote\n\nEvaluates an expression inside a `quasiquote` template. The result replaces the `unquote` form. Only valid inside `(quasiquote ...)`.\n\n```lisp\n(quasiquote (list (unquote x)))\n```",
	"unquote-splicing":  "**unquote-splicing** — evaluate and splice inside quasiquote\n\nEvaluates an expression inside a `quasiquote` template and splices the result list into the parent. The expression must evaluate to a list.\n\n```lisp\n(quasiquote (list (unquote-splicing items)))\n```",
	"condition":         "**condition** — catch-all error type\n\nUsed as the error type in a `handler-bind` clause to match any error.\n\n```lisp\n(handler-bind ((condition (lambda (e) e))) body)\n```",
	"else":              "**else** — catch-all cond clause\n\nWhen used as the test in the last `cond` clause, it always matches.\n\n```lisp\n(cond ((= x 1) \"one\") (else \"other\"))\n```",
}

// qualifiedSymbolHover looks up a qualified symbol (e.g. "string:join") in
// the workspace analysis config's PackageExports and builds hover content.
func (s *Server) qualifiedSymbolHover(word string) string {
	pkgName, symName, ok := splitPackageQualified(word)
	if !ok || symName == "" {
		return ""
	}

	s.ensureWorkspaceIndex()

	s.analysisCfgMu.RLock()
	cfg := s.analysisCfg
	s.analysisCfgMu.RUnlock()

	if cfg == nil || cfg.PackageExports == nil {
		return ""
	}

	exports, ok := cfg.PackageExports[pkgName]
	if !ok {
		return ""
	}

	for _, ext := range exports {
		if ext.Name == symName {
			return buildHoverContent(externalToSymbol(&ext))
		}
	}
	return ""
}

// registryHover looks up an unqualified symbol in the registry's package
// exports. It checks the core "lisp" package (builtins, special ops, macros)
// and any packages imported via use-package before the given 1-based line.
func (s *Server) registryHover(word string, ast []*lisp.LVal, elpsLine int) string {
	if word == "" {
		return ""
	}

	s.ensureWorkspaceIndex()

	s.analysisCfgMu.RLock()
	cfg := s.analysisCfg
	s.analysisCfgMu.RUnlock()

	if cfg == nil || cfg.PackageExports == nil {
		return ""
	}

	// Check the core "lisp" package (builtins, special ops, macros).
	if exports, ok := cfg.PackageExports[lisp.DefaultLangPackage]; ok {
		for _, ext := range exports {
			if ext.Name == word {
				return buildHoverContent(externalToSymbol(&ext))
			}
		}
	}

	// Check packages imported via use-package.
	for _, pkg := range usedPackagesAtLine(ast, elpsLine) {
		if exports, ok := cfg.PackageExports[pkg]; ok {
			for _, ext := range exports {
				if ext.Name == word {
					return buildHoverContent(externalToSymbol(&ext))
				}
			}
		}
	}

	return ""
}

// usedPackagesAtLine returns the package names imported via use-package
// forms that appear before the given 1-based line in the AST.
func usedPackagesAtLine(ast []*lisp.LVal, line int) []string {
	var pkgs []string
	for _, expr := range ast {
		if expr == nil || expr.Type != lisp.LSExpr || expr.Quoted || len(expr.Cells) == 0 {
			continue
		}
		if expr.Source != nil && expr.Source.Line > line {
			break
		}
		head := expr.Cells[0]
		if head.Type != lisp.LSymbol || head.Str != "use-package" || len(expr.Cells) < 2 {
			continue
		}
		if name := astutil.PackageNameArg(expr.Cells[1]); name != "" {
			pkgs = append(pkgs, name)
		}
	}
	return pkgs
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
	} else if isBuiltin(sym) {
		sb.WriteString("\n\n*Built-in*")
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
