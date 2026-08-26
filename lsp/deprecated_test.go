// Copyright © 2026 The ELPS authors

package lsp

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/analysis"
	"github.com/luthersystems/elps/lint"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	protocol "github.com/tliron/glsp/protocol_3_16"
)

// Deprecation is spelled the same way everywhere: a docstring paragraph that
// begins "Deprecated:" (lisp.DeprecationNotice is the detector). These tests
// cover the three surfaces the editor sees it through -- the hover banner, the
// completion item tag, and the diagnostic tag.
//
// Analysis reads a definition's docstring from the leading string literal of
// the form, so the marker has to live in that one string; the sources below
// use "\n\n" to open the second paragraph inside it.

const deprecatedDefun = `(defun old-fn (x)
  "Adds one to x.\n\nDeprecated: use new-fn instead."
  (+ x 1))`

func TestHoverOnDeprecatedDefun(t *testing.T) {
	s := testServer()
	openDoc(s, "file:///test.lisp", deprecatedDefun)

	hover, err := s.textDocumentHover(mockContext(), &protocol.HoverParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: "file:///test.lisp"},
			Position:     protocol.Position{Line: 0, Character: 7}, // on "old-fn"
		},
	})
	require.NoError(t, err)
	assertHoverContains(t, hover, "old-fn", "**Deprecated.** use new-fn instead.")

	mc := hover.Contents.(protocol.MarkupContent)
	assert.Contains(t, mc.Value, "Adds one to x.",
		"the banner must not replace the docstring")
	assert.Less(t, strings.Index(mc.Value, "**Deprecated.**"), strings.Index(mc.Value, "Adds one to x."),
		"the banner belongs above the docstring")
}

func TestHoverOnDeprecatedQualifiedSymbol(t *testing.T) {
	s := testServer()
	setTestAnalysisCfg(s, &analysis.Config{
		PackageExports: map[string][]analysis.ExternalSymbol{
			"substrate": {
				{
					Name:      "blend-paths",
					Kind:      analysis.SymFunction,
					Package:   "substrate",
					DocString: "Combines two paths.\n\nDeprecated: use join-paths instead.",
				},
			},
		},
	})
	openDoc(s, "file:///test.lisp", `(substrate:blend-paths "a" "b")`)

	hover, err := s.textDocumentHover(mockContext(), &protocol.HoverParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: "file:///test.lisp"},
			Position:     protocol.Position{Line: 0, Character: 2}, // on "substrate:blend-paths"
		},
	})
	require.NoError(t, err)
	assertHoverContains(t, hover, "blend-paths", "**Deprecated.** use join-paths instead.")
}

func TestHoverBannerWithoutNotice(t *testing.T) {
	// A marker paragraph with nothing after it still marks the symbol; the
	// banner then stands alone, with no trailing space.
	content := buildHoverContent(&analysis.Symbol{
		Name:      "old-fn",
		Kind:      analysis.SymFunction,
		DocString: "Adds one to x.\n\nDeprecated:",
	})
	assert.Contains(t, content, "\n\n**Deprecated.**\n\n")
	assert.NotContains(t, content, "**Deprecated.** \n")
}

func TestHoverNoBannerForPlainDocString(t *testing.T) {
	content := buildHoverContent(&analysis.Symbol{
		Name:      "new-fn",
		Kind:      analysis.SymFunction,
		DocString: "Adds one to x. Not deprecated: still supported.",
	})
	assert.NotContains(t, content, "**Deprecated.**")
}

func TestCompletionTagsDeprecatedPackageSymbol(t *testing.T) {
	s := testServer()
	setTestAnalysisCfg(s, &analysis.Config{
		PackageExports: map[string][]analysis.ExternalSymbol{
			"substrate": {
				{
					Name:      "blend-paths",
					Kind:      analysis.SymFunction,
					Package:   "substrate",
					DocString: "Combines two paths.\n\nDeprecated: use join-paths instead.",
				},
				{
					Name:      "join-paths",
					Kind:      analysis.SymFunction,
					Package:   "substrate",
					DocString: "Joins two paths.",
				},
			},
		},
	})
	openDoc(s, "file:///test.lisp", "(substrate:")

	result, err := s.textDocumentCompletion(mockContext(), &protocol.CompletionParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: "file:///test.lisp"},
			Position:     protocol.Position{Line: 0, Character: 11},
		},
	})
	require.NoError(t, err)
	items, ok := result.([]protocol.CompletionItem)
	require.True(t, ok)

	byLabel := make(map[string]protocol.CompletionItem, len(items))
	for _, item := range items {
		byLabel[item.Label] = item
	}
	deprecated, ok := byLabel["substrate:blend-paths"]
	require.True(t, ok, "deprecated export should still be offered")
	assert.Equal(t, []protocol.CompletionItemTag{protocol.CompletionItemTagDeprecated}, deprecated.Tags)
	assert.Nil(t, deprecated.Deprecated, "the legacy Deprecated field stays unset")

	live, ok := byLabel["substrate:join-paths"]
	require.True(t, ok)
	assert.Empty(t, live.Tags, "a live symbol carries no tags")
}

func TestCompletionTagsDeprecatedScopeSymbol(t *testing.T) {
	s := testServer()
	openDoc(s, "file:///test.lisp", deprecatedDefun+"\n(old-")

	result, err := s.textDocumentCompletion(mockContext(), &protocol.CompletionParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: "file:///test.lisp"},
			Position:     protocol.Position{Line: 3, Character: 5}, // after "(old-"
		},
	})
	require.NoError(t, err)
	items, ok := result.([]protocol.CompletionItem)
	require.True(t, ok)

	var found bool
	for _, item := range items {
		if item.Label != "old-fn" {
			continue
		}
		found = true
		assert.Equal(t, []protocol.CompletionItemTag{protocol.CompletionItemTagDeprecated}, item.Tags)
		assert.Nil(t, item.Deprecated, "the legacy Deprecated field stays unset")
	}
	assert.True(t, found, "the deprecated function should still be completed")
}

func TestConvertLintDiagnosticDeprecatedTag(t *testing.T) {
	d := convertLintDiagnostic(lint.Diagnostic{
		Pos:        lint.Position{File: "test.lisp", Line: 2, Col: 3},
		Message:    "use of deprecated function 'old-fn': use new-fn instead.",
		Analyzer:   "deprecated",
		Severity:   lint.SeverityWarning,
		Deprecated: true,
	})
	assert.Equal(t, []protocol.DiagnosticTag{protocol.DiagnosticTagDeprecated}, d.Tags)
}

func TestConvertLintDiagnosticBothTags(t *testing.T) {
	// Nothing produces both today, but the conversion must not drop one.
	d := convertLintDiagnostic(lint.Diagnostic{
		Pos:         lint.Position{File: "test.lisp", Line: 1, Col: 1},
		Message:     "dead call to a deprecated function",
		Analyzer:    "deprecated",
		Unnecessary: true,
		Deprecated:  true,
	})
	assert.Equal(t, []protocol.DiagnosticTag{
		protocol.DiagnosticTagUnnecessary,
		protocol.DiagnosticTagDeprecated,
	}, d.Tags)
}

func TestConvertLintDiagnosticNoTags(t *testing.T) {
	d := convertLintDiagnostic(lint.Diagnostic{
		Pos:      lint.Position{File: "test.lisp", Line: 1, Col: 1},
		Message:  "plain warning",
		Analyzer: "test-check",
	})
	assert.Empty(t, d.Tags)
}

// TestDiagnosticsPublishDeprecatedTag is the end-to-end path: opening a
// document runs the semantic `deprecated` analyzer and the published
// diagnostic carries the tag editors strike the call site out with.
func TestDiagnosticsPublishDeprecatedTag(t *testing.T) {
	s := testServer()
	ctx, captured := capturingContext()

	err := s.textDocumentDidOpen(ctx, &protocol.DidOpenTextDocumentParams{
		TextDocument: protocol.TextDocumentItem{
			URI:     "file:///test.lisp",
			Version: 1,
			Text:    "(defun old-fn (x) \"Deprecated: use new-fn.\" x)\n(old-fn 1)\n",
		},
	})
	require.NoError(t, err)
	require.Len(t, *captured, 1)

	var found bool
	for _, d := range (*captured)[0].Diagnostics {
		if d.Code == nil || d.Code.Value != "deprecated" {
			continue
		}
		found = true
		assert.Contains(t, d.Message, "use of deprecated function 'old-fn'")
		assert.Contains(t, d.Tags, protocol.DiagnosticTagDeprecated)
		assert.Equal(t, protocol.UInteger(1), d.Range.Start.Line, "the use, not the definition")
	}
	assert.True(t, found, "opening the document should publish a deprecated diagnostic")
}
