// Copyright © 2024 The ELPS authors

package lsp

import (
	"strings"

	"github.com/luthersystems/elps/lisp"
	"github.com/tliron/glsp"
	protocol "github.com/tliron/glsp/protocol_3_16"
)

// textDocumentFoldingRange handles the textDocument/foldingRange request.
// It returns folding ranges for multi-line s-expressions and consecutive
// comment blocks.
func (s *Server) textDocumentFoldingRange(_ *glsp.Context, params *protocol.FoldingRangeParams) ([]protocol.FoldingRange, error) {
	doc := s.docs.Get(params.TextDocument.URI)
	if doc == nil {
		return nil, nil
	}

	doc.mu.Lock()
	ast := doc.ast
	content := doc.Content
	doc.mu.Unlock()

	var ranges []protocol.FoldingRange

	// Fold multi-line s-expressions from the AST.
	for _, expr := range ast {
		collectFoldingRanges(expr, &ranges)
	}

	// Fold consecutive comment blocks from source text.
	ranges = append(ranges, commentFoldingRanges(content)...)

	return ranges, nil
}

// collectFoldingRanges recursively walks the AST and emits a folding range
// for each s-expression that spans more than one line.
func collectFoldingRanges(v *lisp.LVal, ranges *[]protocol.FoldingRange) {
	if v == nil || v.Source == nil {
		return
	}

	// Only fold list expressions (s-expressions).
	if v.Type == lisp.LSExpr && v.Source.Line > 0 && v.Source.EndLine > 0 {
		startLine := v.Source.Line - 1 // convert to 0-based
		endLine := v.Source.EndLine - 1
		if endLine > startLine {
			kind := string(protocol.FoldingRangeKindRegion)
			*ranges = append(*ranges, protocol.FoldingRange{
				StartLine: safeUint(startLine),
				EndLine:   safeUint(endLine),
				Kind:      &kind,
			})
		}
	}

	// Recurse into children.
	for _, child := range v.Cells {
		collectFoldingRanges(child, ranges)
	}
}

// commentFoldingRanges detects consecutive lines starting with ";" and
// produces a folding range for each block of 2+ lines.
func commentFoldingRanges(content string) []protocol.FoldingRange {
	lines := strings.Split(content, "\n")
	var ranges []protocol.FoldingRange

	blockStart := -1
	for i, line := range lines {
		trimmed := strings.TrimSpace(line)
		isComment := strings.HasPrefix(trimmed, ";")
		if isComment {
			if blockStart < 0 {
				blockStart = i
			}
		} else {
			if blockStart >= 0 && i-1 > blockStart {
				kind := string(protocol.FoldingRangeKindComment)
				ranges = append(ranges, protocol.FoldingRange{
					StartLine: safeUint(blockStart),
					EndLine:   safeUint(i - 1),
					Kind:      &kind,
				})
			}
			blockStart = -1
		}
	}
	// Handle comment block at end of file.
	if blockStart >= 0 && len(lines)-1 > blockStart {
		kind := string(protocol.FoldingRangeKindComment)
		ranges = append(ranges, protocol.FoldingRange{
			StartLine: safeUint(blockStart),
			EndLine:   safeUint(len(lines) - 1),
			Kind:      &kind,
		})
	}

	return ranges
}
