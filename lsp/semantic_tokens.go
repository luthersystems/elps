// Copyright © 2024 The ELPS authors

package lsp

import (
	"sort"
	"strings"

	"github.com/luthersystems/elps/analysis"
	"github.com/luthersystems/elps/lisp"
	"github.com/tliron/glsp"
	protocol "github.com/tliron/glsp/protocol_3_16"
)

// Semantic token type indices — must match the order in semanticTokenLegend().
const (
	semTokenNamespace = iota
	semTokenType
	semTokenParameter
	semTokenVariable
	semTokenFunction
	semTokenMacro
	semTokenKeyword
	semTokenComment
	semTokenString
	semTokenNumber
	semTokenOperator
)

// Semantic token modifier bit flags — must match the order in semanticTokenLegend().
const (
	semModDefinition     = 1 << iota
	semModDefaultLibrary //nolint:unused // reserved for future use
)

// semanticTokenLegend returns the legend that the client uses to decode tokens.
func semanticTokenLegend() protocol.SemanticTokensLegend {
	return protocol.SemanticTokensLegend{
		TokenTypes: []string{
			"namespace",  // 0
			"type",       // 1
			"parameter",  // 2
			"variable",   // 3
			"function",   // 4
			"macro",      // 5
			"keyword",    // 6
			"comment",    // 7
			"string",     // 8
			"number",     // 9
			"operator",   // 10
		},
		TokenModifiers: []string{
			"definition",     // bit 0
			"defaultLibrary", // bit 1
		},
	}
}

// rawToken is an intermediate representation before delta encoding.
type rawToken struct {
	line      int // 0-based
	startChar int // 0-based
	length    int
	tokenType int
	modifiers int
}

// textDocumentSemanticTokensFull handles the textDocument/semanticTokens/full request.
func (s *Server) textDocumentSemanticTokensFull(_ *glsp.Context, params *protocol.SemanticTokensParams) (*protocol.SemanticTokens, error) {
	doc := s.docs.Get(params.TextDocument.URI)
	if doc == nil {
		return nil, nil
	}
	s.ensureAnalysis(doc)

	doc.mu.Lock()
	ast := doc.ast
	analysisResult := doc.analysis
	content := doc.Content
	doc.mu.Unlock()

	if ast == nil {
		return nil, nil
	}

	// Build lookup maps from the analysis result for fast symbol classification.
	symbolDefs := buildSymbolDefsMap(analysisResult)
	symbolRefs := buildSymbolRefsMap(analysisResult)

	var tokens []rawToken
	for _, expr := range ast {
		collectSemanticTokens(expr, symbolDefs, symbolRefs, content, &tokens)
	}

	// Sort by position (line, then character).
	sort.Slice(tokens, func(i, j int) bool {
		if tokens[i].line != tokens[j].line {
			return tokens[i].line < tokens[j].line
		}
		return tokens[i].startChar < tokens[j].startChar
	})

	// Delta-encode.
	data := deltaEncode(tokens)

	return &protocol.SemanticTokens{Data: data}, nil
}

// collectSemanticTokens recursively walks the AST and collects semantic tokens.
func collectSemanticTokens(
	v *lisp.LVal,
	defs map[symbolKey]*analysis.Symbol,
	refs map[symbolKey]*analysis.Symbol,
	content string,
	tokens *[]rawToken,
) {
	if v == nil || v.Source == nil || v.Source.Line == 0 {
		return
	}

	line := v.Source.Line - 1 // convert to 0-based
	col := max(v.Source.Col-1, 0)

	switch v.Type {
	case lisp.LInt, lisp.LFloat:
		length := tokenLength(v, content)
		*tokens = append(*tokens, rawToken{
			line: line, startChar: col, length: length,
			tokenType: semTokenNumber,
		})

	case lisp.LString:
		// String length includes quotes.
		length := len(v.Str) + 2
		// For multi-line strings, just highlight the first line.
		if strings.Contains(v.Str, "\n") {
			lines := strings.Split(content, "\n")
			if line < len(lines) {
				length = len(lines[line]) - col
			}
		}
		*tokens = append(*tokens, rawToken{
			line: line, startChar: col, length: length,
			tokenType: semTokenString,
		})

	case lisp.LSymbol:
		name := v.Str
		length := len(name)
		tokType, mods := classifySymbol(name, line, col, defs, refs)
		*tokens = append(*tokens, rawToken{
			line: line, startChar: col, length: length,
			tokenType: tokType, modifiers: mods,
		})

	case lisp.LSExpr:
		// For quoted lists like '(a b c), just recurse into children.
		for _, child := range v.Cells {
			collectSemanticTokens(child, defs, refs, content, tokens)
		}
		return
	}
}

// symbolKey uniquely identifies a symbol occurrence by position.
type symbolKey struct {
	line int // 1-based (ELPS convention)
	col  int // 1-based
}

// buildSymbolDefsMap creates a lookup from position to symbol definition.
func buildSymbolDefsMap(res *analysis.Result) map[symbolKey]*analysis.Symbol {
	m := make(map[symbolKey]*analysis.Symbol)
	if res == nil {
		return m
	}
	for _, sym := range res.Symbols {
		if sym.Source != nil && sym.Source.Line > 0 {
			m[symbolKey{sym.Source.Line, sym.Source.Col}] = sym
		}
	}
	return m
}

// buildSymbolRefsMap creates a lookup from position to the referenced symbol.
func buildSymbolRefsMap(res *analysis.Result) map[symbolKey]*analysis.Symbol {
	m := make(map[symbolKey]*analysis.Symbol)
	if res == nil {
		return m
	}
	for _, ref := range res.References {
		if ref.Source != nil && ref.Source.Line > 0 {
			m[symbolKey{ref.Source.Line, ref.Source.Col}] = ref.Symbol
		}
	}
	return m
}

// specialOps is the set of ELPS special operators and core forms that should
// be highlighted as keywords.
var specialOps = map[string]bool{
	"defun": true, "defmacro": true, "deftype": true, "defmethod": true,
	"lambda": true, "let": true, "let*": true, "flet": true, "labels": true,
	"if": true, "cond": true, "or": true, "and": true, "not": true,
	"set": true, "set!": true,
	"progn": true, "loop": true, "dotimes": true,
	"handler-bind": true, "ignore-errors": true, "rethrow": true,
	"in-package": true, "use-package": true, "export": true,
	"quote": true, "quasiquote": true, "unquote": true,
	"funcall": true, "apply": true,
	"debug-print": true,
	"assert-equal": true, "assert-nil": true, "assert-not-nil": true,
	"test": true, "test-let": true,
	"thread-first": true, "thread-last": true,
}

// classifySymbol determines the semantic token type for a symbol based on
// analysis results and built-in knowledge.
func classifySymbol(
	name string,
	line, col int,
	defs map[symbolKey]*analysis.Symbol,
	refs map[symbolKey]*analysis.Symbol,
) (tokenType int, modifiers int) {
	// Check if this is a keyword symbol (starts with :).
	if strings.HasPrefix(name, ":") {
		return semTokenVariable, 0
	}

	// Check if this is a package-qualified name (contains :).
	if i := strings.Index(name, ":"); i > 0 && !strings.HasPrefix(name, ":") {
		// The package prefix part is a namespace token — but we emit the
		// whole symbol as one token based on its resolved kind.
		// Fall through to analysis-based classification.
	}

	// Check analysis result — look up by 1-based position.
	key := symbolKey{line + 1, col + 1}

	// Check if this position is a definition.
	if sym, ok := defs[key]; ok {
		return symbolKindToTokenType(sym.Kind), semModDefinition
	}

	// Check if this position is a reference.
	if sym, ok := refs[key]; ok {
		return symbolKindToTokenType(sym.Kind), 0
	}

	// Fall back to name-based classification.
	if specialOps[name] {
		return semTokenKeyword, 0
	}
	if name == "true" || name == "false" {
		return semTokenKeyword, 0
	}

	return semTokenVariable, 0
}

// symbolKindToTokenType converts an analysis.SymbolKind to a semantic token type index.
func symbolKindToTokenType(kind analysis.SymbolKind) int {
	switch kind {
	case analysis.SymFunction, analysis.SymBuiltin:
		return semTokenFunction
	case analysis.SymMacro:
		return semTokenMacro
	case analysis.SymSpecialOp:
		return semTokenKeyword
	case analysis.SymParameter:
		return semTokenParameter
	case analysis.SymType:
		return semTokenType
	case analysis.SymVariable:
		return semTokenVariable
	default:
		return semTokenVariable
	}
}

// tokenLength computes the display length of a token. Falls back to
// heuristics when source end info is unavailable.
func tokenLength(v *lisp.LVal, content string) int {
	if v.Source.EndCol > 0 && v.Source.EndLine == v.Source.Line {
		return v.Source.EndCol - v.Source.Col
	}
	// Fall back: extract from source text.
	lines := strings.Split(content, "\n")
	line := v.Source.Line - 1
	col := v.Source.Col - 1
	if line >= 0 && line < len(lines) && col >= 0 && col < len(lines[line]) {
		// Scan forward to find end of number/atom.
		end := col
		for end < len(lines[line]) && !isDelimiter(lines[line][end]) {
			end++
		}
		if end > col {
			return end - col
		}
	}
	return 1
}

func isDelimiter(c byte) bool {
	switch c {
	case ' ', '\t', '\n', '\r', '(', ')', '[', ']', '"', ';':
		return true
	}
	return false
}

// deltaEncode converts sorted raw tokens into the LSP delta-encoded format.
// Each token is 5 integers: [deltaLine, deltaStartChar, length, tokenType, tokenModifiers].
func deltaEncode(tokens []rawToken) []protocol.UInteger {
	data := make([]protocol.UInteger, 0, len(tokens)*5)
	prevLine := 0
	prevChar := 0
	for _, tok := range tokens {
		deltaLine := tok.line - prevLine
		deltaChar := tok.startChar
		if deltaLine == 0 {
			deltaChar = tok.startChar - prevChar
		}
		data = append(data,
			protocol.UInteger(deltaLine),
			protocol.UInteger(deltaChar),
			protocol.UInteger(tok.length),
			protocol.UInteger(tok.tokenType),
			protocol.UInteger(tok.modifiers),
		)
		prevLine = tok.line
		prevChar = tok.startChar
	}
	return data
}
