// Copyright © 2024 The ELPS authors

package lsp

import (
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	protocol "github.com/tliron/glsp/protocol_3_16"
)

func TestFoldingRange(t *testing.T) {
	s := testServer()

	t.Run("single-line form is not folded", func(t *testing.T) {
		doc := openDoc(s, "file:///test/single.lisp", `(defun foo () 42)`)
		result, err := s.textDocumentFoldingRange(mockContext(), &protocol.FoldingRangeParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: doc.URI},
		})
		require.NoError(t, err)
		// Filter to region folds only (exclude comment folds).
		regions := filterFoldKind(result, protocol.FoldingRangeKindRegion)
		assert.Empty(t, regions)
	})

	t.Run("multi-line defun is folded", func(t *testing.T) {
		doc := openDoc(s, "file:///test/multi.lisp", "(defun foo (x)\n  (+ x 1))")
		result, err := s.textDocumentFoldingRange(mockContext(), &protocol.FoldingRangeParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: doc.URI},
		})
		require.NoError(t, err)
		regions := filterFoldKind(result, protocol.FoldingRangeKindRegion)
		require.NotEmpty(t, regions)
		// The outer defun spans lines 0-1 (0-based).
		assert.Equal(t, protocol.UInteger(0), regions[0].StartLine)
		assert.Equal(t, protocol.UInteger(1), regions[0].EndLine)
	})

	t.Run("nested multi-line forms produce separate ranges", func(t *testing.T) {
		src := "(defun outer ()\n  (let ((x 1))\n    (+ x\n       2)))"
		doc := openDoc(s, "file:///test/nested.lisp", src)
		result, err := s.textDocumentFoldingRange(mockContext(), &protocol.FoldingRangeParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: doc.URI},
		})
		require.NoError(t, err)
		regions := filterFoldKind(result, protocol.FoldingRangeKindRegion)
		// Should have at least 2: outer defun and inner let.
		assert.GreaterOrEqual(t, len(regions), 2)
	})

	t.Run("consecutive comments produce a comment fold", func(t *testing.T) {
		src := "; line 1\n; line 2\n; line 3\n(defun foo () 42)"
		doc := openDoc(s, "file:///test/comments.lisp", src)
		result, err := s.textDocumentFoldingRange(mockContext(), &protocol.FoldingRangeParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: doc.URI},
		})
		require.NoError(t, err)
		comments := filterFoldKind(result, protocol.FoldingRangeKindComment)
		require.Len(t, comments, 1)
		assert.Equal(t, protocol.UInteger(0), comments[0].StartLine)
		assert.Equal(t, protocol.UInteger(2), comments[0].EndLine)
	})

	t.Run("single comment line is not folded", func(t *testing.T) {
		src := "; just one comment\n(defun foo () 42)"
		doc := openDoc(s, "file:///test/onecomment.lisp", src)
		result, err := s.textDocumentFoldingRange(mockContext(), &protocol.FoldingRangeParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: doc.URI},
		})
		require.NoError(t, err)
		comments := filterFoldKind(result, protocol.FoldingRangeKindComment)
		assert.Empty(t, comments)
	})

	t.Run("nil doc returns nil", func(t *testing.T) {
		result, err := s.textDocumentFoldingRange(mockContext(), &protocol.FoldingRangeParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: "file:///missing.lisp"},
		})
		require.NoError(t, err)
		assert.Nil(t, result)
	})
}

func TestCommentFoldingRanges(t *testing.T) {
	t.Run("consecutive block", func(t *testing.T) {
		ranges := commentFoldingRanges("; a\n; b\n; c\n(code)")
		require.Len(t, ranges, 1)
		assert.Equal(t, protocol.UInteger(0), ranges[0].StartLine)
		assert.Equal(t, protocol.UInteger(2), ranges[0].EndLine)
	})

	t.Run("two separate blocks", func(t *testing.T) {
		ranges := commentFoldingRanges("; a\n; b\n\n; c\n; d")
		require.Len(t, ranges, 2)
	})

	t.Run("no comments", func(t *testing.T) {
		ranges := commentFoldingRanges("(defun foo () 42)")
		assert.Empty(t, ranges)
	})

	t.Run("comments at end of file", func(t *testing.T) {
		ranges := commentFoldingRanges("(code)\n; a\n; b")
		require.Len(t, ranges, 1)
		assert.Equal(t, protocol.UInteger(1), ranges[0].StartLine)
		assert.Equal(t, protocol.UInteger(2), ranges[0].EndLine)
	})
}

// filterFoldKind returns only folding ranges with the given kind.
func filterFoldKind(ranges []protocol.FoldingRange, kind protocol.FoldingRangeKind) []protocol.FoldingRange {
	var result []protocol.FoldingRange
	for _, r := range ranges {
		if r.Kind != nil && *r.Kind == string(kind) {
			result = append(result, r)
		}
	}
	return result
}
