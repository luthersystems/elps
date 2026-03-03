// Copyright © 2024 The ELPS authors

package lsp

import (
	"strings"

	"github.com/luthersystems/elps/analysis"
	"github.com/luthersystems/elps/lisp"
	protocol "github.com/tliron/glsp/protocol_3_16"
)

// LSP 3.17 inlay hint types. The glsp library only supports 3.16,
// so we define the minimum set needed for the wire protocol.

// InlayHintParams is the parameter for textDocument/inlayHint.
type InlayHintParams struct {
	TextDocument protocol.TextDocumentIdentifier `json:"textDocument"`
	Range        protocol.Range                  `json:"range"`
}

// InlayHint is a single inlay hint returned to the client.
type InlayHint struct {
	Position     protocol.Position `json:"position"`
	Label        string            `json:"label"`
	Kind         int               `json:"kind"`                   // 1=Type, 2=Parameter
	PaddingLeft  bool              `json:"paddingLeft,omitempty"`
	PaddingRight bool              `json:"paddingRight,omitempty"`
}

const inlayHintKindParameter = 2

// minParamsForHint is the minimum number of required parameters a
// function must have before we emit inlay hints. Single-parameter
// functions are usually obvious from context.
const minParamsForHint = 2

// textDocumentInlayHint handles textDocument/inlayHint requests.
// It walks the AST looking for function/macro calls and emits
// parameter-name hints for non-obvious arguments.
func (s *Server) textDocumentInlayHint(params *InlayHintParams) ([]InlayHint, error) {
	doc := s.docs.Get(params.TextDocument.URI)
	if doc == nil {
		return nil, nil
	}
	s.ensureAnalysis(doc)

	doc.mu.Lock()
	ast := doc.ast
	docAnalysis := doc.analysis
	doc.mu.Unlock()

	if docAnalysis == nil || len(ast) == 0 {
		return nil, nil
	}

	// Convert the requested range from 0-based LSP to 1-based ELPS coords.
	startLine := int(params.Range.Start.Line) + 1
	startCol := int(params.Range.Start.Character) + 1
	endLine := int(params.Range.End.Line) + 1
	endCol := int(params.Range.End.Character) + 1

	ctx := &hintContext{
		analysis:  docAnalysis,
		server:    s,
		startLine: startLine,
		startCol:  startCol,
		endLine:   endLine,
		endCol:    endCol,
	}

	for _, node := range ast {
		walkForHints(ctx, node)
	}

	return ctx.hints, nil
}

// hintContext carries state through the AST walk.
type hintContext struct {
	analysis  *analysis.Result
	server    *Server
	startLine int
	startCol  int
	endLine   int
	endCol    int
	hints     []InlayHint
}

// walkForHints recursively walks AST nodes looking for function calls.
func walkForHints(ctx *hintContext, node *lisp.LVal) {
	if node == nil {
		return
	}

	// Only s-expressions can be function calls.
	if node.Type != lisp.LSExpr && node.Type != lisp.LArray {
		return
	}

	// Process this node if it's an unquoted s-expression with a symbol head.
	if node.Type == lisp.LSExpr && !node.Quoted && len(node.Cells) >= 2 {
		head := node.Cells[0]
		if head.Type == lisp.LSymbol {
			processCallNode(ctx, node, head.Str)
		}
	}

	// Recurse into children.
	for _, child := range node.Cells {
		walkForHints(ctx, child)
	}
}

// processCallNode checks if a call node should produce inlay hints.
func processCallNode(ctx *hintContext, node *lisp.LVal, name string) {
	// Check that the call overlaps the requested range.
	if !nodeOverlapsRange(node, ctx.startLine, ctx.startCol, ctx.endLine, ctx.endCol) {
		return
	}

	// Look up the callable's signature.
	sig := lookupSignature(ctx.analysis, ctx.server, name)
	if sig == nil {
		return
	}

	// Skip if too few required params (would be noisy).
	if countRequiredParams(sig) < minParamsForHint {
		return
	}

	ctx.hints = emitCallHints(ctx.hints, node, sig)
}

// lookupSignature finds a signature for a named callable by checking
// the analysis scope and falling back to qualified package lookup.
// Only returns signatures for functions and builtins — special operators
// and macros have non-standard argument semantics that would produce
// misleading hints (e.g., defun's "name" and "formals" params).
func lookupSignature(result *analysis.Result, s *Server, name string) *analysis.Signature {
	sym := lookupCallable(result, name)
	if sym != nil && hintableKind(sym.Kind) {
		return sym.Signature
	}
	// Fallback: qualified symbol in package exports.
	if ext := s.lookupQualifiedCallable(name); ext != nil {
		if hintableKind(ext.Kind) {
			return ext.Signature
		}
	}
	return nil
}

// hintableKind returns true if the symbol kind should produce inlay hints.
func hintableKind(kind analysis.SymbolKind) bool {
	switch kind {
	case analysis.SymFunction, analysis.SymBuiltin:
		return true
	default:
		return false
	}
}

// countRequiredParams returns the number of required (non-optional,
// non-rest, non-key) parameters in a signature.
func countRequiredParams(sig *analysis.Signature) int {
	n := 0
	for _, p := range sig.Params {
		if p.Kind == lisp.ParamRequired {
			n++
		}
	}
	return n
}

// nodeOverlapsRange checks if a node's source span overlaps the
// given 1-based line:col range.
func nodeOverlapsRange(node *lisp.LVal, startLine, startCol, endLine, endCol int) bool {
	src := node.Source
	if src == nil || src.Line == 0 {
		return false
	}
	// Node starts after range end → no overlap.
	if src.Line > endLine || (src.Line == endLine && src.Col > endCol) {
		return false
	}
	// Node ends before range start (use EndLine if available).
	if src.EndLine > 0 {
		if src.EndLine < startLine || (src.EndLine == startLine && src.EndCol > 0 && src.EndCol < startCol) {
			return false
		}
	}
	return true
}

// emitCallHints matches positional arguments to signature parameters
// and emits inlay hints for non-obvious ones.
func emitCallHints(hints []InlayHint, node *lisp.LVal, sig *analysis.Signature) []InlayHint {
	args := node.Cells[1:] // skip the head (function name)
	paramIdx := 0

	for _, arg := range args {
		if paramIdx >= len(sig.Params) {
			break
		}
		p := sig.Params[paramIdx]

		// Stop emitting hints at &rest or &key boundary.
		if p.Kind == lisp.ParamRest || p.Kind == lisp.ParamKey {
			break
		}

		paramIdx++

		// Skip if the argument is a keyword literal — self-documenting.
		if isKeywordLiteral(arg) {
			continue
		}

		// Skip if the argument text matches the parameter name.
		if argMatchesParam(arg, p.Name) {
			continue
		}

		// Need a source location to place the hint.
		if arg.Source == nil || arg.Source.Line == 0 {
			continue
		}

		hints = append(hints, InlayHint{
			Position: protocol.Position{
				Line:      safeUint(arg.Source.Line - 1),
				Character: safeUint(arg.Source.Col - 1),
			},
			Label:        p.Name + ":",
			Kind:         inlayHintKindParameter,
			PaddingRight: true,
		})
	}

	return hints
}

// isKeywordLiteral returns true if the node is a keyword symbol
// (starts with ':').
func isKeywordLiteral(node *lisp.LVal) bool {
	return node.Type == lisp.LSymbol && strings.HasPrefix(node.Str, ":")
}

// argMatchesParam returns true if the argument's text representation
// matches the parameter name (case-sensitive). This avoids emitting
// redundant hints like `x: x`.
func argMatchesParam(arg *lisp.LVal, paramName string) bool {
	if arg.Type == lisp.LSymbol {
		return arg.Str == paramName
	}
	return false
}
