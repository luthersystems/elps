// Copyright © 2024 The ELPS authors

package lsp

import (
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/token"
	"github.com/tliron/glsp"
	protocol "github.com/tliron/glsp/protocol_3_16"
)

// textDocumentSelectionRange handles textDocument/selectionRange.
// It returns progressively wider syntactic ranges for each requested
// position, enabling the "Expand Selection" editor command.
func (s *Server) textDocumentSelectionRange(_ *glsp.Context, params *protocol.SelectionRangeParams) ([]protocol.SelectionRange, error) {
	doc := s.docs.Get(params.TextDocument.URI)
	if doc == nil {
		return nil, nil
	}
	s.ensureAnalysis(doc)

	doc.mu.Lock()
	ast := doc.ast
	doc.mu.Unlock()

	if ast == nil {
		return nil, nil
	}

	result := make([]protocol.SelectionRange, len(params.Positions))
	for i, pos := range params.Positions {
		// Convert 0-based LSP position to 1-based ELPS coords.
		line := int(pos.Line) + 1
		col := int(pos.Character) + 1

		chain := nodesAtPosition(ast, line, col)
		if len(chain) == 0 {
			// No enclosing node — return a zero-width range at the cursor.
			result[i] = protocol.SelectionRange{
				Range: protocol.Range{
					Start: pos,
					End:   pos,
				},
			}
			continue
		}

		// chain is outermost→innermost; reverse to build the linked list
		// from innermost (no parent) to outermost.
		result[i] = buildSelectionRangeChain(chain)
	}

	return result, nil
}

// buildSelectionRangeChain constructs a linked SelectionRange from a
// chain of nodes ordered outermost→innermost.
func buildSelectionRangeChain(chain []*lisp.LVal) protocol.SelectionRange {
	// Start from the outermost node (no parent).
	var current *protocol.SelectionRange
	for _, node := range chain {
		r := nodeRange(node)
		sr := &protocol.SelectionRange{
			Range:  r,
			Parent: current,
		}
		current = sr
	}
	// current is now the innermost node.
	return *current
}

// nodeRange converts an LVal's Source location to an LSP range.
func nodeRange(node *lisp.LVal) protocol.Range {
	if node.Source == nil {
		return protocol.Range{}
	}
	loc := node.Source
	start := protocol.Position{
		Line:      safeUint(loc.Line - 1),
		Character: safeUint(loc.Col - 1),
	}
	var end protocol.Position
	if loc.EndLine > 0 && loc.EndCol > 0 {
		end = protocol.Position{
			Line:      safeUint(loc.EndLine - 1),
			Character: safeUint(loc.EndCol - 1),
		}
	} else {
		// Fallback: use the node's string representation length.
		nameLen := len(node.Str)
		if nameLen == 0 {
			nameLen = 1
		}
		end = protocol.Position{
			Line:      start.Line,
			Character: start.Character + safeUint(nameLen),
		}
	}
	return protocol.Range{Start: start, End: end}
}

// nodesAtPosition walks the AST recursively, collecting all nodes whose
// source range contains the given 1-based position. Returns nodes in
// outermost→innermost order.
func nodesAtPosition(exprs []*lisp.LVal, line, col int) []*lisp.LVal {
	for _, expr := range exprs {
		if chain := nodeChainAt(expr, line, col); chain != nil {
			return chain
		}
	}
	return nil
}

// nodeChainAt recursively builds a chain of enclosing nodes for the
// given position. Returns nil if the node does not contain the position.
func nodeChainAt(node *lisp.LVal, line, col int) []*lisp.LVal {
	if node == nil || node.Source == nil {
		return nil
	}
	if !locContainsPosition(node.Source, line, col) {
		return nil
	}

	// This node contains the position. Check children for a tighter match.
	if node.Type == lisp.LSExpr && len(node.Cells) > 0 {
		for _, child := range node.Cells {
			if childChain := nodeChainAt(child, line, col); childChain != nil {
				return append([]*lisp.LVal{node}, childChain...)
			}
		}
	}

	// No child contains the position — this is the innermost node.
	return []*lisp.LVal{node}
}

// locContainsPosition checks whether a 1-based (line, col) falls within
// the source range of the given location.
func locContainsPosition(loc *token.Location, line, col int) bool {
	if loc.Line == 0 || loc.Col == 0 {
		return false
	}

	startLine := loc.Line
	startCol := loc.Col
	endLine := loc.EndLine
	endCol := loc.EndCol

	// If no end info, use a heuristic: single-line, single-char range.
	if endLine == 0 {
		endLine = startLine
		endCol = startCol + 1
	}

	// Before start?
	if line < startLine || (line == startLine && col < startCol) {
		return false
	}
	// After end?
	if line > endLine || (line == endLine && col >= endCol) {
		return false
	}
	return true
}
