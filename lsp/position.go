// Copyright © 2024 The ELPS authors

package lsp

import (
	"strings"

	"github.com/luthersystems/elps/analysis"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/token"
	protocol "github.com/tliron/glsp/protocol_3_16"
)

// elpsToLSPPosition converts a 1-based ELPS location to a 0-based LSP position.
func elpsToLSPPosition(loc *token.Location) protocol.Position {
	line := loc.Line
	col := loc.Col
	if line > 0 {
		line--
	}
	if col > 0 {
		col--
	}
	return protocol.Position{
		Line:      safeUint(line),
		Character: safeUint(col),
	}
}

// safeUint converts a non-negative int to protocol.UInteger, clamping
// negative values to zero.
func safeUint(n int) protocol.UInteger {
	if n < 0 {
		return 0
	}
	return protocol.UInteger(n) // #nosec G115 -- line/col are always small positive ints
}

// elpsToLSPRange converts an ELPS location to an LSP range.
// If the location has end information, it is used; otherwise nameLen
// characters are used for the range width.
func elpsToLSPRange(loc *token.Location, nameLen int) protocol.Range {
	start := elpsToLSPPosition(loc)
	var end protocol.Position
	if loc.EndLine > 0 && loc.EndCol > 0 {
		end = protocol.Position{
			Line:      safeUint(loc.EndLine - 1),
			Character: safeUint(loc.EndCol - 1),
		}
	} else {
		end = protocol.Position{
			Line:      start.Line,
			Character: start.Character + safeUint(nameLen),
		}
	}
	return protocol.Range{Start: start, End: end}
}

// symbolAtPosition finds the analysis symbol at the given 0-based LSP
// position in the document's analysis result. It returns both the symbol
// (definition) and the specific reference that was hit, if any.
func symbolAtPosition(doc *Document, line, col int) (*analysis.Symbol, *analysis.Reference) {
	if doc == nil || doc.analysis == nil {
		return nil, nil
	}
	// Convert LSP 0-based to ELPS 1-based.
	elpsLine := line + 1
	elpsCol := col + 1

	// Check references first — they point to specific usage sites.
	for _, ref := range doc.analysis.References {
		if ref.Source == nil || ref.Source.Line == 0 {
			continue
		}
		if ref.Source.Line == elpsLine && locContainsCol(ref.Source, ref.Symbol.Name, elpsCol) {
			return ref.Symbol, ref
		}
	}

	// Check definitions (symbols defined in this file).
	for _, sym := range doc.analysis.Symbols {
		if sym.Source == nil || sym.Source.Line == 0 {
			continue
		}
		if sym.Source.Line == elpsLine && locContainsCol(sym.Source, sym.Name, elpsCol) {
			return sym, nil
		}
	}
	return nil, nil
}

// locContainsCol checks whether a 1-based column falls within the token
// starting at loc. If loc has EndCol, that is used; otherwise the name
// length is used as a heuristic.
func locContainsCol(loc *token.Location, name string, col int) bool {
	start := loc.Col
	if start == 0 {
		return false
	}
	end := start + len(name)
	if loc.EndCol > 0 {
		end = loc.EndCol
	}
	return col >= start && col < end
}

// wordAtPosition extracts the symbol-like word at the given 0-based LSP
// position from the document content. The cursor can be inside or at the
// end of a word; in both cases the full word is returned.
func wordAtPosition(content string, line, col int) string {
	lines := strings.Split(content, "\n")
	if line < 0 || line >= len(lines) {
		return ""
	}
	ln := lines[line]
	if col < 0 || col > len(ln) {
		return ""
	}
	// Clamp col to the line length (cursor can be at end of line).
	if col >= len(ln) {
		col = len(ln)
	}
	// Scan backwards from cursor.
	start := col
	for start > 0 && isSymbolChar(ln[start-1]) {
		start--
	}
	// Scan forwards from cursor.
	end := col
	for end < len(ln) && isSymbolChar(ln[end]) {
		end++
	}
	return ln[start:end]
}

func isSymbolChar(c byte) bool {
	if c >= 'a' && c <= 'z' {
		return true
	}
	if c >= 'A' && c <= 'Z' {
		return true
	}
	if c >= '0' && c <= '9' {
		return true
	}
	switch c {
	case '-', '_', '!', '?', '+', '*', '/', '<', '>', '=', ':', '.', '#', '^':
		return true
	}
	return false
}

// scopeAtPosition returns the innermost scope that contains the given
// 1-based ELPS line and column. It walks the scope tree depth-first.
func scopeAtPosition(root *analysis.Scope, line, col int) *analysis.Scope {
	if root == nil {
		return nil
	}
	best := root
	for _, child := range root.Children {
		if s := scopeContaining(child, line, col); s != nil {
			best = s
		}
	}
	return best
}

// scopeContaining checks if a scope's node contains the position and
// recursively checks children for the most specific match.
func scopeContaining(scope *analysis.Scope, line, col int) *analysis.Scope {
	if scope.Node == nil || scope.Node.Source == nil {
		return nil
	}
	loc := scope.Node.Source
	if loc.Line == 0 {
		return nil
	}
	// A scope contains the position if its source range spans the line.
	// For scopes without EndLine, we accept any position after the start.
	startLine := loc.Line
	endLine := loc.EndLine
	if endLine == 0 {
		endLine = startLine + 1000 // heuristic: scope extends far
	}
	if line < startLine || line > endLine {
		return nil
	}
	if line == startLine && col < loc.Col {
		return nil
	}
	if line == endLine && loc.EndCol > 0 && col >= loc.EndCol {
		return nil
	}
	// This scope contains the position; check children for a tighter match.
	best := scope
	for _, child := range scope.Children {
		if s := scopeContaining(child, line, col); s != nil {
			best = s
		}
	}
	return best
}

// collectVisibleSymbols walks the scope chain outward from scope, collecting
// all symbols visible at that point.
func collectVisibleSymbols(scope *analysis.Scope) []*analysis.Symbol {
	seen := make(map[string]bool)
	var result []*analysis.Symbol
	for s := scope; s != nil; s = s.Parent {
		for name, sym := range s.Symbols {
			if !seen[name] {
				seen[name] = true
				result = append(result, sym)
			}
		}
	}
	return result
}

// mapSymbolKind converts an analysis.SymbolKind to an LSP SymbolKind.
func mapSymbolKind(kind analysis.SymbolKind) protocol.SymbolKind {
	switch kind {
	case analysis.SymFunction:
		return protocol.SymbolKindFunction
	case analysis.SymMacro:
		return protocol.SymbolKindFunction
	case analysis.SymVariable:
		return protocol.SymbolKindVariable
	case analysis.SymParameter:
		return protocol.SymbolKindVariable
	case analysis.SymSpecialOp:
		return protocol.SymbolKindOperator
	case analysis.SymBuiltin:
		return protocol.SymbolKindFunction
	case analysis.SymType:
		return protocol.SymbolKindClass
	default:
		return protocol.SymbolKindVariable
	}
}

// mapCompletionItemKind converts an analysis.SymbolKind to an LSP CompletionItemKind.
func mapCompletionItemKind(kind analysis.SymbolKind) protocol.CompletionItemKind {
	switch kind {
	case analysis.SymFunction, analysis.SymBuiltin:
		return protocol.CompletionItemKindFunction
	case analysis.SymMacro:
		return protocol.CompletionItemKindKeyword
	case analysis.SymVariable:
		return protocol.CompletionItemKindVariable
	case analysis.SymParameter:
		return protocol.CompletionItemKindVariable
	case analysis.SymSpecialOp:
		return protocol.CompletionItemKindKeyword
	case analysis.SymType:
		return protocol.CompletionItemKindClass
	default:
		return protocol.CompletionItemKindText
	}
}

// formatSignature builds a human-readable parameter string from a Signature.
func formatSignature(sig *analysis.Signature) string {
	if sig == nil || len(sig.Params) == 0 {
		return "()"
	}
	var parts []string
	for _, p := range sig.Params {
		s := p.Name
		switch p.Kind {
		case lisp.ParamRest:
			s = "&rest " + s
		case lisp.ParamOptional:
			s = "&optional " + s
		case lisp.ParamKey:
			s = "&key " + s
		}
		parts = append(parts, s)
	}
	return "(" + strings.Join(parts, " ") + ")"
}

// uriToPath converts a file:// URI to a filesystem path.
func uriToPath(uri string) string {
	if path, ok := strings.CutPrefix(uri, "file://"); ok {
		return path
	}
	return uri
}

// pathToURI converts a filesystem path to a file:// URI.
func pathToURI(path string) string {
	if strings.HasPrefix(path, "/") {
		return "file://" + path
	}
	return path
}

