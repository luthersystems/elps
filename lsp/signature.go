// Copyright © 2024 The ELPS authors

package lsp

import (
	"strings"

	"github.com/luthersystems/elps/analysis"
	"github.com/luthersystems/elps/lisp"
	"github.com/tliron/glsp"
	protocol "github.com/tliron/glsp/protocol_3_16"
)

// textDocumentSignatureHelp handles textDocument/signatureHelp requests.
// It finds the enclosing function call at the cursor position, looks up
// its signature, and returns parameter hints. Three strategies are tried
// in order:
//  1. AST-based: walk the parsed AST for the innermost call containing
//     the cursor (works for complete, well-formed code).
//  2. Text-based: scan raw text for parentheses (fallback for incomplete
//     code where the parser didn't include the unclosed expression).
//  3. Qualified lookup: if the name contains ":", look up the symbol in
//     the workspace package exports rather than the analysis scope.
func (s *Server) textDocumentSignatureHelp(_ *glsp.Context, params *protocol.SignatureHelpParams) (*protocol.SignatureHelp, error) {
	doc := s.docs.Get(params.TextDocument.URI)
	if doc == nil {
		return nil, nil
	}
	s.ensureAnalysis(doc)

	doc.mu.Lock()
	ast := doc.ast
	docAnalysis := doc.analysis
	content := doc.Content
	doc.mu.Unlock()

	line := int(params.Position.Line)
	col := int(params.Position.Character)

	// Strategy 1: AST-based lookup.
	var name string
	var argIdx int
	if len(ast) > 0 {
		elpsLine := line + 1
		elpsCol := col + 1
		name, argIdx = enclosingCall(ast, elpsLine, elpsCol)
	}

	// Strategy 2: text-based fallback for incomplete code.
	if name == "" {
		name, argIdx = enclosingCallText(content, line, col)
	}

	if name == "" {
		return nil, nil
	}

	// Strategy 3a: scope-based lookup.
	if docAnalysis != nil {
		sym := lookupCallable(docAnalysis, name)
		if sym != nil && sym.Signature != nil {
			return buildSignatureHelp(sym, argIdx), nil
		}
	}

	// Strategy 3b: qualified symbol lookup in package exports.
	if ext := s.lookupQualifiedCallable(name); ext != nil {
		sym := &analysis.Symbol{
			Name:      ext.Name,
			Kind:      ext.Kind,
			Source:    ext.Source,
			Signature: ext.Signature,
			DocString: ext.DocString,
			External:  true,
		}
		return buildSignatureHelp(sym, argIdx), nil
	}

	return nil, nil
}

// lookupQualifiedCallable looks up a qualified symbol (e.g. "string:join")
// in the workspace package exports and returns it if it has a signature.
func (s *Server) lookupQualifiedCallable(name string) *analysis.ExternalSymbol {
	pkgName, symName, ok := splitPackageQualified(name)
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

	for i := range exports {
		if exports[i].Name == symName && exports[i].Signature != nil {
			return &exports[i]
		}
	}
	return nil
}

// enclosingCallText is a text-based fallback for finding the enclosing
// function call when the AST doesn't contain the cursor position (e.g.,
// because the user is typing an incomplete expression with an unclosed
// paren). It scans backwards from the cursor position counting parentheses
// to find the innermost unclosed '(' and extracts the function name.
func enclosingCallText(content string, line, col int) (string, int) {
	lines := strings.Split(content, "\n")
	if line < 0 || line >= len(lines) {
		return "", 0
	}

	// Build the text up to the cursor position.
	var sb strings.Builder
	for i := 0; i < line; i++ {
		sb.WriteString(lines[i])
		sb.WriteByte('\n')
	}
	ln := lines[line]
	if col > len(ln) {
		col = len(ln)
	}
	sb.WriteString(ln[:col])
	text := sb.String()

	// Scan backwards to find the innermost unclosed '('.
	depth := 0
	inString := false
	openIdx := -1
	argCount := 0

	for i := len(text) - 1; i >= 0; i-- {
		ch := text[i]

		// Handle string literals (simplified: just track unescaped quotes).
		if ch == '"' && (i == 0 || text[i-1] != '\\') {
			inString = !inString
			continue
		}
		if inString {
			continue
		}

		if ch == ')' {
			depth++
		} else if ch == '(' {
			if depth > 0 {
				depth--
			} else {
				// Found the innermost unclosed '('.
				openIdx = i
				break
			}
		}
	}

	if openIdx < 0 {
		return "", 0
	}

	// Extract the function name after the '('.
	rest := text[openIdx+1:]
	name := extractLeadingSymbol(rest)
	if name == "" {
		return "", 0
	}

	// Count arguments: number of top-level whitespace-separated tokens
	// after the function name in the text between openIdx and cursor.
	afterName := rest[len(name):]
	argCount = countTopLevelArgs(afterName)

	return name, argCount
}

// extractLeadingSymbol extracts the first symbol-like token from text.
func extractLeadingSymbol(text string) string {
	// Skip leading whitespace.
	i := 0
	for i < len(text) && (text[i] == ' ' || text[i] == '\t' || text[i] == '\n' || text[i] == '\r') {
		i++
	}
	start := i
	for i < len(text) && isSymbolChar(text[i]) {
		i++
	}
	return text[start:i]
}

// countTopLevelArgs counts the number of top-level arguments in the text
// after a function name. It handles strings and nested parens.
func countTopLevelArgs(text string) int {
	count := 0
	depth := 0
	inString := false
	inArg := false

	for i := 0; i < len(text); i++ {
		ch := text[i]

		if ch == '"' && (i == 0 || text[i-1] != '\\') {
			inString = !inString
			if !inArg && depth == 0 {
				inArg = true
				count++
			}
			continue
		}
		if inString {
			continue
		}

		switch ch {
		case '(':
			if !inArg && depth == 0 {
				inArg = true
				count++
			}
			depth++
		case ')':
			depth--
		case ' ', '\t', '\n', '\r':
			if depth == 0 {
				inArg = false
			}
		default:
			if !inArg && depth == 0 {
				inArg = true
				count++
			}
		}
	}
	return count
}

// enclosingCall walks the AST to find the innermost s-expression
// containing the cursor position that looks like a function call.
// Returns the function name and the 0-based argument index, or ("", 0)
// if the cursor is not inside any call.
func enclosingCall(exprs []*lisp.LVal, line, col int) (string, int) {
	var bestName string
	var bestArgIdx int
	var bestDepth int

	walkForCall(exprs, line, col, 0, func(name string, argIdx, depth int) {
		if depth >= bestDepth {
			bestName = name
			bestArgIdx = argIdx
			bestDepth = depth
		}
	})

	return bestName, bestArgIdx
}

// walkForCall recursively walks the AST looking for s-expressions
// containing the given position. For each matching call, it invokes fn
// with the head symbol name, argument index, and nesting depth.
func walkForCall(exprs []*lisp.LVal, line, col, depth int, fn func(name string, argIdx, depth int)) {
	for _, expr := range exprs {
		walkNodeForCall(expr, line, col, depth, fn)
	}
}

func walkNodeForCall(node *lisp.LVal, line, col, depth int, fn func(name string, argIdx, depth int)) {
	if node == nil || node.Type != lisp.LSExpr || node.Quoted || len(node.Cells) == 0 {
		return
	}

	// Check if this s-expression contains the cursor.
	if !positionInside(node, line, col) {
		return
	}

	// This s-expression contains the cursor. If the head is a symbol,
	// it's a function call candidate.
	head := node.Cells[0]
	if head.Type == lisp.LSymbol {
		argIdx := computeArgIndex(node, line, col)
		fn(head.Str, argIdx, depth)
	}

	// Recurse into children for tighter matches.
	for _, child := range node.Cells {
		walkNodeForCall(child, line, col, depth+1, fn)
	}
}

// positionInside checks whether the 1-based line:col position falls
// within the source range of an s-expression node.
func positionInside(node *lisp.LVal, line, col int) bool {
	if node.Source == nil || node.Source.Line == 0 {
		return false
	}
	src := node.Source
	startLine := src.Line
	startCol := src.Col

	// Check start boundary.
	if line < startLine {
		return false
	}
	if line == startLine && col < startCol {
		return false
	}

	// Check end boundary if available.
	if src.EndLine > 0 {
		if line > src.EndLine {
			return false
		}
		if line == src.EndLine && src.EndCol > 0 && col > src.EndCol {
			return false
		}
	}

	return true
}

// computeArgIndex determines which argument position the cursor is at
// within an s-expression (0-based, not counting the head).
func computeArgIndex(node *lisp.LVal, line, col int) int {
	// Walk the children (excluding head) and find which argument
	// the cursor is at or after.
	argIdx := 0
	for i := 1; i < len(node.Cells); i++ {
		child := node.Cells[i]
		if child.Source == nil || child.Source.Line == 0 {
			continue
		}
		// If cursor is before this child's start, we're on the
		// previous argument (or before any arg).
		if line < child.Source.Line || (line == child.Source.Line && col < child.Source.Col) {
			break
		}
		argIdx = i - 1 // 0-based argument index
		// If cursor is within this child, check if it's an
		// s-expression we should recurse into (handled by caller).
		if i < len(node.Cells)-1 {
			next := node.Cells[i+1]
			if next.Source != nil && next.Source.Line > 0 {
				if line > next.Source.Line || (line == next.Source.Line && col >= next.Source.Col) {
					continue
				}
			}
		}
	}
	// If cursor is past all children, point to the last arg position + 1.
	if len(node.Cells) > 1 {
		last := node.Cells[len(node.Cells)-1]
		if last.Source != nil && last.Source.Line > 0 {
			pastLast := false
			if last.Source.EndLine > 0 && last.Source.EndCol > 0 {
				pastLast = line > last.Source.EndLine ||
					(line == last.Source.EndLine && col >= last.Source.EndCol)
			}
			if pastLast {
				argIdx = len(node.Cells) - 1 // next arg position (past all children)
			}
		}
	}
	return argIdx
}

// lookupCallable finds a callable symbol by name in the analysis result.
// It searches the scope chain from the root scope.
func lookupCallable(result *analysis.Result, name string) *analysis.Symbol {
	if result == nil || result.RootScope == nil {
		return nil
	}
	sym := result.RootScope.Lookup(name)
	if sym == nil {
		return nil
	}
	if sym.Signature == nil {
		return nil
	}
	return sym
}

// buildSignatureHelp constructs an LSP SignatureHelp from a symbol and
// active argument index.
func buildSignatureHelp(sym *analysis.Symbol, activeParam int) *protocol.SignatureHelp {
	sig := sym.Signature
	if sig == nil {
		return nil
	}

	// Build the label: "(name param1 &optional param2 &rest args)"
	label := "(" + sym.Name + " " + formatSignatureParams(sig) + ")"

	// Build parameter info with offset-based labels.
	var params []protocol.ParameterInformation
	// Track position within the label string for offset-based parameter labels.
	offset := len("(") + len(sym.Name) + len(" ") // past "(name "
	for i, p := range sig.Params {
		paramLabel := formatParam(p)
		start := offset
		end := offset + len(paramLabel)

		pi := protocol.ParameterInformation{
			Label: []protocol.UInteger{safeUint(start), safeUint(end)},
		}
		params = append(params, pi)

		offset = end
		if i < len(sig.Params)-1 {
			offset++ // space separator
		}
	}

	// Clamp active parameter to valid range.
	ap := activeParam
	if len(params) > 0 {
		maxIdx := len(params) - 1
		if ap > maxIdx {
			ap = maxIdx
		}
	}
	if ap < 0 {
		ap = 0
	}
	active := uint32(ap) // #nosec G115 -- clamped to [0, len(params)-1]

	sigInfo := protocol.SignatureInformation{
		Label:      label,
		Parameters: params,
	}

	// Add documentation if available.
	if sym.DocString != "" {
		doc := protocol.MarkupContent{
			Kind:  protocol.MarkupKindMarkdown,
			Value: sym.DocString,
		}
		sigInfo.Documentation = doc
	}

	return &protocol.SignatureHelp{
		Signatures:      []protocol.SignatureInformation{sigInfo},
		ActiveSignature: uintPtr(0),
		ActiveParameter: &active,
	}
}

// formatSignatureParams builds "param1 &optional param2 &rest args"
// without outer parens.
func formatSignatureParams(sig *analysis.Signature) string {
	if sig == nil || len(sig.Params) == 0 {
		return ""
	}
	var parts []string
	for _, p := range sig.Params {
		parts = append(parts, formatParam(p))
	}
	return strings.Join(parts, " ")
}

// formatParam formats a single parameter including its keyword prefix.
func formatParam(p lisp.ParamInfo) string {
	switch p.Kind {
	case lisp.ParamRest:
		return "&rest " + p.Name
	case lisp.ParamOptional:
		return "&optional " + p.Name
	case lisp.ParamKey:
		return "&key " + p.Name
	default:
		return p.Name
	}
}

func uintPtr(v uint32) *uint32 {
	return &v
}
