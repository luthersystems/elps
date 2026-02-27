// Copyright © 2024 The ELPS authors

package lsp

import (
	"github.com/luthersystems/elps/analysis"
	"github.com/tliron/glsp"
	protocol "github.com/tliron/glsp/protocol_3_16"
)

// callHierarchyData is stored in CallHierarchyItem.Data to carry context
// between prepare and incoming/outgoing calls requests.
type callHierarchyData struct {
	Name string `json:"name"`
	URI  string `json:"uri"`
	Line int    `json:"line"` // 0-based
	Col  int    `json:"col"`  // 0-based
}

// textDocumentPrepareCallHierarchy handles the textDocument/prepareCallHierarchy request.
func (s *Server) textDocumentPrepareCallHierarchy(_ *glsp.Context, params *protocol.CallHierarchyPrepareParams) ([]protocol.CallHierarchyItem, error) {
	doc := s.docs.Get(params.TextDocument.URI)
	if doc == nil {
		return nil, nil
	}
	s.ensureAnalysis(doc)

	sym, _ := symbolAtPosition(doc, int(params.Position.Line), int(params.Position.Character))
	if sym == nil {
		return nil, nil
	}
	// Only functions and macros have a call hierarchy.
	if !isCallable(sym.Kind) {
		return nil, nil
	}
	// Must have source location.
	if sym.Source == nil || sym.Source.Line == 0 {
		return nil, nil
	}

	item := symbolToCallHierarchyItem(sym, params.TextDocument.URI)
	return []protocol.CallHierarchyItem{item}, nil
}

// callHierarchyIncomingCalls handles the callHierarchy/incomingCalls request.
// It finds all functions that call the target function.
func (s *Server) callHierarchyIncomingCalls(_ *glsp.Context, params *protocol.CallHierarchyIncomingCallsParams) ([]protocol.CallHierarchyIncomingCall, error) {
	data := decodeCallHierarchyData(params.Item.Data)
	if data == nil {
		return nil, nil
	}

	doc := s.docs.Get(data.URI)
	if doc == nil {
		return nil, nil
	}
	s.ensureAnalysis(doc)

	doc.mu.Lock()
	res := doc.analysis
	doc.mu.Unlock()
	if res == nil {
		return nil, nil
	}

	// Find the target symbol.
	targetSym := findSymbolByName(res, data.Name)
	if targetSym == nil {
		return nil, nil
	}

	// Find all references to the target and group by enclosing function.
	type callerInfo struct {
		sym    *analysis.Symbol
		ranges []protocol.Range
	}
	callers := make(map[string]*callerInfo)

	for _, ref := range res.References {
		if ref.Symbol != targetSym {
			continue
		}
		if ref.Source == nil || ref.Source.Line == 0 {
			continue
		}
		// Find the enclosing function of this reference.
		enclosing := findEnclosingFunction(res.RootScope, ref.Source.Line, ref.Source.Col)
		if enclosing == nil {
			continue // reference is at top level, not inside a function
		}
		refRange := elpsToLSPRange(ref.Source, len(ref.Symbol.Name))
		key := enclosing.Name
		if c, ok := callers[key]; ok {
			c.ranges = append(c.ranges, refRange)
		} else {
			callers[key] = &callerInfo{sym: enclosing, ranges: []protocol.Range{refRange}}
		}
	}

	var result []protocol.CallHierarchyIncomingCall
	for _, c := range callers {
		if c.sym.Source == nil || c.sym.Source.Line == 0 {
			continue
		}
		item := symbolToCallHierarchyItem(c.sym, data.URI)
		result = append(result, protocol.CallHierarchyIncomingCall{
			From:       item,
			FromRanges: c.ranges,
		})
	}

	return result, nil
}

// callHierarchyOutgoingCalls handles the callHierarchy/outgoingCalls request.
// It finds all functions called from within the target function.
func (s *Server) callHierarchyOutgoingCalls(_ *glsp.Context, params *protocol.CallHierarchyOutgoingCallsParams) ([]protocol.CallHierarchyOutgoingCall, error) {
	data := decodeCallHierarchyData(params.Item.Data)
	if data == nil {
		return nil, nil
	}

	doc := s.docs.Get(data.URI)
	if doc == nil {
		return nil, nil
	}
	s.ensureAnalysis(doc)

	doc.mu.Lock()
	res := doc.analysis
	doc.mu.Unlock()
	if res == nil {
		return nil, nil
	}

	// Find the target function symbol.
	targetSym := findSymbolByName(res, data.Name)
	if targetSym == nil || targetSym.Source == nil {
		return nil, nil
	}

	// Find the scope of the target function.
	funcScope := findFunctionScope(res.RootScope, data.Name)
	if funcScope == nil || funcScope.Node == nil || funcScope.Node.Source == nil {
		return nil, nil
	}

	// Collect all callable references inside the function's source range.
	type calleeInfo struct {
		sym    *analysis.Symbol
		ranges []protocol.Range
	}
	callees := make(map[string]*calleeInfo)

	startLine := funcScope.Node.Source.Line
	endLine := funcScope.Node.Source.EndLine
	if endLine == 0 {
		endLine = startLine + 10000 // heuristic
	}

	for _, ref := range res.References {
		if ref.Source == nil || ref.Source.Line == 0 {
			continue
		}
		// Check if the reference is within the function's body.
		if ref.Source.Line < startLine || ref.Source.Line > endLine {
			continue
		}
		// Only include callable references.
		if !isCallable(ref.Symbol.Kind) {
			continue
		}
		// Skip self-references.
		if ref.Symbol == targetSym {
			continue
		}

		refRange := elpsToLSPRange(ref.Source, len(ref.Symbol.Name))
		key := ref.Symbol.Name
		if c, ok := callees[key]; ok {
			c.ranges = append(c.ranges, refRange)
		} else {
			callees[key] = &calleeInfo{sym: ref.Symbol, ranges: []protocol.Range{refRange}}
		}
	}

	var result []protocol.CallHierarchyOutgoingCall
	for _, c := range callees {
		item := symbolToCallHierarchyItem(c.sym, data.URI)
		result = append(result, protocol.CallHierarchyOutgoingCall{
			To:         item,
			FromRanges: c.ranges,
		})
	}

	return result, nil
}

// symbolToCallHierarchyItem creates a CallHierarchyItem from an analysis symbol.
func symbolToCallHierarchyItem(sym *analysis.Symbol, uri string) protocol.CallHierarchyItem {
	selRange := elpsToLSPRange(sym.Source, len(sym.Name))
	// For the enclosing range, use EndLine if available.
	encRange := selRange
	if sym.Source.EndLine > 0 {
		encRange = protocol.Range{
			Start: selRange.Start,
			End: protocol.Position{
				Line:      safeUint(sym.Source.EndLine - 1),
				Character: safeUint(sym.Source.EndCol - 1),
			},
		}
	}

	detail := symbolDetail(sym)
	return protocol.CallHierarchyItem{
		Name:           sym.Name,
		Kind:           mapSymbolKind(sym.Kind),
		Detail:         detail,
		URI:            uri,
		Range:          encRange,
		SelectionRange: selRange,
		Data: callHierarchyData{
			Name: sym.Name,
			URI:  uri,
			Line: int(selRange.Start.Line),
			Col:  int(selRange.Start.Character),
		},
	}
}

// isCallable returns true for symbol kinds that represent callable entities.
func isCallable(kind analysis.SymbolKind) bool {
	switch kind {
	case analysis.SymFunction, analysis.SymMacro, analysis.SymBuiltin, analysis.SymSpecialOp:
		return true
	}
	return false
}

// findSymbolByName finds a non-external symbol by name in the analysis result.
func findSymbolByName(res *analysis.Result, name string) *analysis.Symbol {
	for _, sym := range res.Symbols {
		if sym.Name == name && !sym.External {
			return sym
		}
	}
	// Fall back to external symbols.
	for _, sym := range res.Symbols {
		if sym.Name == name {
			return sym
		}
	}
	return nil
}

// findEnclosingFunction finds the function symbol that contains the given
// 1-based position by walking the scope tree.
func findEnclosingFunction(root *analysis.Scope, line, col int) *analysis.Symbol {
	scope := scopeAtPosition(root, line, col)
	// Walk up to find the nearest function scope.
	for scope != nil {
		if scope.Kind == analysis.ScopeFunction {
			// Find the symbol defined in the parent scope for this function.
			if scope.Node != nil && scope.Node.Source != nil {
				// The function name is typically the second child of the defun form.
				for _, sym := range scope.Parent.Symbols {
					if sym.Kind == analysis.SymFunction || sym.Kind == analysis.SymMacro {
						if sym.Source != nil && sym.Source.Line == scope.Node.Source.Line {
							return sym
						}
					}
				}
			}
		}
		scope = scope.Parent
	}
	return nil
}

// findFunctionScope finds the scope for a named function.
func findFunctionScope(root *analysis.Scope, name string) *analysis.Scope {
	// Look for a ScopeFunction child whose parent has the named symbol.
	for _, child := range root.Children {
		if child.Kind == analysis.ScopeFunction {
			if sym, ok := root.Symbols[name]; ok {
				if sym.Kind == analysis.SymFunction || sym.Kind == analysis.SymMacro {
					if child.Node != nil && sym.Source != nil &&
						child.Node.Source != nil && child.Node.Source.Line == sym.Source.Line {
						return child
					}
				}
			}
		}
		// Recurse for nested scopes.
		if found := findFunctionScope(child, name); found != nil {
			return found
		}
	}
	return nil
}

// decodeCallHierarchyData decodes the data field from a CallHierarchyItem.
// In tests the data arrives as a Go struct; over the wire it arrives as
// map[string]any from JSON deserialization.
func decodeCallHierarchyData(data any) *callHierarchyData {
	if data == nil {
		return nil
	}
	// Direct struct (in-process / test path).
	if d, ok := data.(callHierarchyData); ok {
		return &d
	}
	if d, ok := data.(*callHierarchyData); ok {
		return d
	}
	// JSON-deserialized path (over the wire).
	m, ok := data.(map[string]any)
	if !ok {
		return nil
	}
	name, _ := m["name"].(string)
	uri, _ := m["uri"].(string)
	if name == "" || uri == "" {
		return nil
	}
	line, _ := m["line"].(float64)
	col, _ := m["col"].(float64)
	return &callHierarchyData{
		Name: name,
		URI:  uri,
		Line: int(line),
		Col:  int(col),
	}
}
