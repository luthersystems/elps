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

func TestPrepareCallHierarchy(t *testing.T) {
	s := testServer()
	setTestAnalysisCfg(s, &analysis.Config{})

	t.Run("function definition returns item", func(t *testing.T) {
		doc := openDoc(s, "file:///test/prepare.lisp", "(defun foo (x) x)")
		s.ensureAnalysis(doc)

		result, err := s.textDocumentPrepareCallHierarchy(mockContext(), &protocol.CallHierarchyPrepareParams{
			TextDocumentPositionParams: protocol.TextDocumentPositionParams{
				TextDocument: protocol.TextDocumentIdentifier{URI: doc.URI},
				Position:     protocol.Position{Line: 0, Character: 7}, // cursor on "foo"
			},
		})
		require.NoError(t, err)
		require.Len(t, result, 1)
		assert.Equal(t, "foo", result[0].Name)
		assert.Equal(t, protocol.SymbolKindFunction, result[0].Kind)
	})

	t.Run("variable returns nil", func(t *testing.T) {
		doc := openDoc(s, "file:///test/var.lisp", "(set my-var 42)")
		s.ensureAnalysis(doc)

		result, err := s.textDocumentPrepareCallHierarchy(mockContext(), &protocol.CallHierarchyPrepareParams{
			TextDocumentPositionParams: protocol.TextDocumentPositionParams{
				TextDocument: protocol.TextDocumentIdentifier{URI: doc.URI},
				Position:     protocol.Position{Line: 0, Character: 5}, // cursor on "my-var"
			},
		})
		require.NoError(t, err)
		assert.Nil(t, result)
	})

	t.Run("missing doc returns nil", func(t *testing.T) {
		result, err := s.textDocumentPrepareCallHierarchy(mockContext(), &protocol.CallHierarchyPrepareParams{
			TextDocumentPositionParams: protocol.TextDocumentPositionParams{
				TextDocument: protocol.TextDocumentIdentifier{URI: "file:///missing.lisp"},
				Position:     protocol.Position{Line: 0, Character: 0},
			},
		})
		require.NoError(t, err)
		assert.Nil(t, result)
	})
}

func TestCallHierarchyIncoming(t *testing.T) {
	s := testServer()
	setTestAnalysisCfg(s, &analysis.Config{})

	src := "(defun helper (x) x)\n(defun caller () (helper 42))"
	doc := openDoc(s, "file:///test/incoming.lisp", src)
	s.ensureAnalysis(doc)

	// Prepare on "helper" definition.
	items, err := s.textDocumentPrepareCallHierarchy(mockContext(), &protocol.CallHierarchyPrepareParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: doc.URI},
			Position:     protocol.Position{Line: 0, Character: 7},
		},
	})
	require.NoError(t, err)
	require.Len(t, items, 1)

	// Get incoming calls for "helper".
	result, err := s.callHierarchyIncomingCalls(mockContext(), &protocol.CallHierarchyIncomingCallsParams{
		Item: items[0],
	})
	require.NoError(t, err)
	// "caller" calls "helper", so we should get 1 incoming call.
	require.NotEmpty(t, result)
	assert.Equal(t, "caller", result[0].From.Name)
	require.NotEmpty(t, result[0].FromRanges)
	// FromRanges should point to line 1 where "helper" is called inside "caller".
	assert.Equal(t, protocol.UInteger(1), result[0].FromRanges[0].Start.Line)
}

func TestCallHierarchyOutgoing(t *testing.T) {
	s := testServer()
	setTestAnalysisCfg(s, &analysis.Config{})

	src := "(defun helper (x) x)\n(defun caller () (helper 42))"
	doc := openDoc(s, "file:///test/outgoing.lisp", src)
	s.ensureAnalysis(doc)

	// Prepare on "caller" definition.
	items, err := s.textDocumentPrepareCallHierarchy(mockContext(), &protocol.CallHierarchyPrepareParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: doc.URI},
			Position:     protocol.Position{Line: 1, Character: 7},
		},
	})
	require.NoError(t, err)
	require.Len(t, items, 1)

	// Get outgoing calls from "caller".
	result, err := s.callHierarchyOutgoingCalls(mockContext(), &protocol.CallHierarchyOutgoingCallsParams{
		Item: items[0],
	})
	require.NoError(t, err)
	// "caller" calls "helper", so we should get 1 outgoing call.
	require.NotEmpty(t, result)
	assert.Equal(t, "helper", result[0].To.Name)
	require.NotEmpty(t, result[0].FromRanges)
	// FromRanges should point to line 1 where "helper" is called.
	assert.Equal(t, protocol.UInteger(1), result[0].FromRanges[0].Start.Line)
}

func TestCrossFileIncomingCalls(t *testing.T) {
	s := testServer()
	setTestAnalysisCfg(s, &analysis.Config{})

	// File A defines "target".
	docA := openDoc(s, "file:///workspace/a.lisp", "(defun target (x) (+ x 1))")
	s.ensureAnalysis(docA)

	// Inject workspace refs: "target" is called from "remote-caller" in file B.
	targetKey := analysis.SymbolKey{Name: "target", Kind: analysis.SymFunction}.String()
	s.setTestWorkspaceRefs(map[string][]analysis.FileReference{
		targetKey: {
			{
				SymbolKey: analysis.SymbolKey{Name: "target", Kind: analysis.SymFunction},
				Source:    &token.Location{File: "/workspace/b.lisp", Line: 1, Col: 25, Pos: 24},
				File:      "/workspace/b.lisp",
				Enclosing: "remote-caller",
				EnclosingSource: &token.Location{
					File: "/workspace/b.lisp", Line: 1, Col: 8, Pos: 7,
				},
				EnclosingKind: analysis.SymFunction,
			},
		},
	})

	// Prepare on "target".
	items, err := s.textDocumentPrepareCallHierarchy(mockContext(), &protocol.CallHierarchyPrepareParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: docA.URI},
			Position:     protocol.Position{Line: 0, Character: 7}, // on "target"
		},
	})
	require.NoError(t, err)
	require.Len(t, items, 1)

	// Get incoming calls.
	result, err := s.callHierarchyIncomingCalls(mockContext(), &protocol.CallHierarchyIncomingCallsParams{
		Item: items[0],
	})
	require.NoError(t, err)
	require.Len(t, result, 1, "should have exactly 1 incoming call from cross-file")

	call := result[0]
	assert.Equal(t, "remote-caller", call.From.Name, "caller should be remote-caller")
	assert.Equal(t, "file:///workspace/b.lisp", call.From.URI, "caller should be from b.lisp")
	require.Len(t, call.FromRanges, 1, "should have exactly 1 call site range")
	// Injected at Line:1, Col:25 → LSP 0-based: line 0, char 24.
	assert.Equal(t, protocol.UInteger(0), call.FromRanges[0].Start.Line, "call site should be on line 0")
	assert.Equal(t, protocol.UInteger(24), call.FromRanges[0].Start.Character, "call site should start at char 24")
}

func TestCrossFileOutgoingCalls(t *testing.T) {
	s := testServer()

	// Set up analysis config with an external symbol "remote-fn" defined in file B.
	setTestAnalysisCfg(s, &analysis.Config{
		ExtraGlobals: []analysis.ExternalSymbol{
			{
				Name:   "remote-fn",
				Kind:   analysis.SymFunction,
				Source: &token.Location{File: "/workspace/b.lisp", Line: 1, Col: 8, Pos: 7},
			},
		},
	})

	// File A defines "caller" which calls "remote-fn".
	docA := openDoc(s, "file:///workspace/a.lisp", "(defun caller () (remote-fn 42))")
	s.ensureAnalysis(docA)

	// Prepare on "caller".
	items, err := s.textDocumentPrepareCallHierarchy(mockContext(), &protocol.CallHierarchyPrepareParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: docA.URI},
			Position:     protocol.Position{Line: 0, Character: 7}, // on "caller"
		},
	})
	require.NoError(t, err)
	require.Len(t, items, 1)

	// Get outgoing calls from "caller".
	result, err := s.callHierarchyOutgoingCalls(mockContext(), &protocol.CallHierarchyOutgoingCallsParams{
		Item: items[0],
	})
	require.NoError(t, err)
	require.Len(t, result, 1, "should have exactly 1 outgoing call")

	call := result[0]
	assert.Equal(t, "remote-fn", call.To.Name, "callee should be remote-fn")
	assert.Equal(t, "file:///workspace/b.lisp", call.To.URI, "callee should point to b.lisp")
	require.Len(t, call.FromRanges, 1, "should have exactly 1 call site range")
	// "(defun caller () (remote-fn 42))" — "remote-fn" starts at col 19 (1-based), LSP 0-based: 18.
	assert.Equal(t, protocol.UInteger(0), call.FromRanges[0].Start.Line, "call site should be on line 0")
	assert.Equal(t, protocol.UInteger(18), call.FromRanges[0].Start.Character, "call site should start at char 18")
}

func TestDecodeCallHierarchyData(t *testing.T) {
	t.Run("valid data", func(t *testing.T) {
		data := map[string]any{
			"name": "foo",
			"uri":  "file:///test.lisp",
			"line": float64(0),
			"col":  float64(5),
		}
		result := decodeCallHierarchyData(data)
		require.NotNil(t, result)
		assert.Equal(t, "foo", result.Name)
		assert.Equal(t, "file:///test.lisp", result.URI)
	})

	t.Run("nil data", func(t *testing.T) {
		assert.Nil(t, decodeCallHierarchyData(nil))
	})

	t.Run("missing name", func(t *testing.T) {
		data := map[string]any{"uri": "file:///test.lisp"}
		assert.Nil(t, decodeCallHierarchyData(data))
	})
}
