// Copyright © 2024 The ELPS authors

package lsp

import (
	"testing"

	"github.com/luthersystems/elps/analysis"
	"github.com/luthersystems/elps/parser/token"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	protocol "github.com/tliron/glsp/protocol_3_16"
)

func TestCodeActionSuppressLint(t *testing.T) {
	s := testServer()
	setTestAnalysisCfg(s, &analysis.Config{})

	src := "(set x 1)\n(set x 2)"
	doc := openDoc(s, "file:///test/suppress.lisp", src)

	// Simulate a lint diagnostic on line 1 (0-based) for "set-usage".
	sev := protocol.DiagnosticSeverityWarning
	diag := protocol.Diagnostic{
		Range: protocol.Range{
			Start: protocol.Position{Line: 1, Character: 1},
			End:   protocol.Position{Line: 1, Character: 4},
		},
		Severity: &sev,
		Source:   strPtr("elps-lint"),
		Code:     &protocol.IntegerOrString{Value: "set-usage"},
		Message:  "set reassigns symbol x; use set! to mutate",
	}

	result, err := s.textDocumentCodeAction(mockContext(), &protocol.CodeActionParams{
		TextDocument: protocol.TextDocumentIdentifier{URI: doc.URI},
		Range:        diag.Range,
		Context: protocol.CodeActionContext{
			Diagnostics: []protocol.Diagnostic{diag},
		},
	})
	require.NoError(t, err)
	require.NotNil(t, result)

	actions, ok := result.([]protocol.CodeAction)
	require.True(t, ok)
	require.NotEmpty(t, actions)

	// Should have a suppress action.
	var found bool
	for _, a := range actions {
		if a.Title == "Suppress with ; nolint:set-usage" {
			found = true
			require.NotNil(t, a.Edit)
			edits := a.Edit.Changes[doc.URI]
			require.Len(t, edits, 1)
			assert.Contains(t, edits[0].NewText, "; nolint:set-usage")
			break
		}
	}
	assert.True(t, found, "expected a suppress action for set-usage")
}

func TestCodeActionFixUndefinedSymbol(t *testing.T) {
	s := testServer()

	// Configure workspace with a package that exports "join".
	cfg := &analysis.Config{
		PackageExports: map[string][]analysis.ExternalSymbol{
			"string": {
				{
					Name:    "join",
					Kind:    analysis.SymFunction,
					Package: "string",
					Source:  &token.Location{File: "/lib/string.lisp", Line: 1, Col: 1},
				},
			},
		},
	}
	setTestAnalysisCfg(s, cfg)

	src := "(join items \",\")"
	doc := openDoc(s, "file:///test/undef.lisp", src)

	sev := protocol.DiagnosticSeverityError
	diag := protocol.Diagnostic{
		Range: protocol.Range{
			Start: protocol.Position{Line: 0, Character: 1},
			End:   protocol.Position{Line: 0, Character: 5},
		},
		Severity: &sev,
		Source:   strPtr("elps-lint"),
		Code:     &protocol.IntegerOrString{Value: "undefined-symbol"},
		Message:  "undefined symbol: join",
	}

	result, err := s.textDocumentCodeAction(mockContext(), &protocol.CodeActionParams{
		TextDocument: protocol.TextDocumentIdentifier{URI: doc.URI},
		Range:        diag.Range,
		Context: protocol.CodeActionContext{
			Diagnostics: []protocol.Diagnostic{diag},
		},
	})
	require.NoError(t, err)
	require.NotNil(t, result)

	actions, ok := result.([]protocol.CodeAction)
	require.True(t, ok)

	// Should have an "add use-package" action.
	var found bool
	for _, a := range actions {
		if a.Title == "Add (use-package 'string)" {
			found = true
			require.NotNil(t, a.Edit)
			edits := a.Edit.Changes[doc.URI]
			require.NotEmpty(t, edits)
			assert.Contains(t, edits[0].NewText, "(use-package 'string)")
			break
		}
	}
	assert.True(t, found, "expected an add use-package action for string")
}

func TestCodeActionNoDiagnostics(t *testing.T) {
	s := testServer()
	setTestAnalysisCfg(s, &analysis.Config{})

	doc := openDoc(s, "file:///test/clean.lisp", "(defun foo () 42)")
	result, err := s.textDocumentCodeAction(mockContext(), &protocol.CodeActionParams{
		TextDocument: protocol.TextDocumentIdentifier{URI: doc.URI},
		Range:        protocol.Range{},
		Context:      protocol.CodeActionContext{Diagnostics: []protocol.Diagnostic{}},
	})
	require.NoError(t, err)
	assert.Nil(t, result)
}

func TestCodeActionOnlyFilter(t *testing.T) {
	s := testServer()
	setTestAnalysisCfg(s, &analysis.Config{})

	doc := openDoc(s, "file:///test/filter.lisp", "(set x 1)")

	// Request only refactor actions — quickfix should be excluded.
	result, err := s.textDocumentCodeAction(mockContext(), &protocol.CodeActionParams{
		TextDocument: protocol.TextDocumentIdentifier{URI: doc.URI},
		Range:        protocol.Range{},
		Context: protocol.CodeActionContext{
			Diagnostics: []protocol.Diagnostic{},
			Only:        []protocol.CodeActionKind{protocol.CodeActionKindRefactor},
		},
	})
	require.NoError(t, err)
	assert.Nil(t, result)
}

func TestExtractSymbolFromMessage(t *testing.T) {
	tests := []struct {
		msg  string
		want string
	}{
		{"undefined symbol: foo", "foo"},
		{"undefined symbol: bar (did you mean baz?)", "bar"},
		{"some other error", ""},
		{"undefined symbol: my-func", "my-func"},
	}
	for _, tt := range tests {
		t.Run(tt.msg, func(t *testing.T) {
			assert.Equal(t, tt.want, extractSymbolFromMessage(tt.msg))
		})
	}
}

func TestUsePackageInsertPosition(t *testing.T) {
	t.Run("after in-package", func(t *testing.T) {
		pos := usePackageInsertPosition("(in-package 'myapp)\n\n(defun foo () 42)")
		assert.Equal(t, protocol.UInteger(1), pos.Line)
	})

	t.Run("after last use-package", func(t *testing.T) {
		pos := usePackageInsertPosition("(in-package 'myapp)\n(use-package 'string)\n\n(defun foo () 42)")
		assert.Equal(t, protocol.UInteger(2), pos.Line)
	})

	t.Run("at beginning when no package forms", func(t *testing.T) {
		pos := usePackageInsertPosition("(defun foo () 42)")
		assert.Equal(t, protocol.UInteger(0), pos.Line)
	})
}
