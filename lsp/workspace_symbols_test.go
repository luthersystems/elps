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

func TestWorkspaceSymbol(t *testing.T) {
	s := testServer()

	// Set up a workspace config with some external symbols.
	cfg := &analysis.Config{
		ExtraGlobals: []analysis.ExternalSymbol{
			{
				Name:    "my-handler",
				Kind:    analysis.SymFunction,
				Package: "user",
				Source:  &token.Location{File: "/workspace/handlers.lisp", Line: 5, Col: 1},
			},
			{
				Name:    "process-request",
				Kind:    analysis.SymFunction,
				Package: "user",
				Source:  &token.Location{File: "/workspace/handlers.lisp", Line: 15, Col: 1},
			},
			{
				Name:    "max-retries",
				Kind:    analysis.SymVariable,
				Package: "user",
				Source:  &token.Location{File: "/workspace/config.lisp", Line: 3, Col: 1},
			},
		},
		PackageExports: map[string][]analysis.ExternalSymbol{
			"router": {
				{
					Name:    "defendpoint",
					Kind:    analysis.SymMacro,
					Package: "router",
					Source:  &token.Location{File: "/workspace/router.lisp", Line: 10, Col: 1},
				},
			},
		},
	}
	setTestAnalysisCfg(s, cfg)

	t.Run("empty query returns all symbols", func(t *testing.T) {
		result, err := s.workspaceSymbol(mockContext(), &protocol.WorkspaceSymbolParams{
			Query: "",
		})
		require.NoError(t, err)
		// Should have globals + package exports.
		assert.GreaterOrEqual(t, len(result), 4)
		names := symbolNames(result)
		assert.Contains(t, names, "my-handler")
		assert.Contains(t, names, "process-request")
		assert.Contains(t, names, "max-retries")
		assert.Contains(t, names, "defendpoint")
	})

	t.Run("query filters by substring", func(t *testing.T) {
		result, err := s.workspaceSymbol(mockContext(), &protocol.WorkspaceSymbolParams{
			Query: "handler",
		})
		require.NoError(t, err)
		names := symbolNames(result)
		assert.Contains(t, names, "my-handler")
		assert.NotContains(t, names, "max-retries")
		assert.NotContains(t, names, "defendpoint")
	})

	t.Run("query is case-insensitive", func(t *testing.T) {
		result, err := s.workspaceSymbol(mockContext(), &protocol.WorkspaceSymbolParams{
			Query: "HANDLER",
		})
		require.NoError(t, err)
		names := symbolNames(result)
		assert.Contains(t, names, "my-handler")
	})

	t.Run("query matches package-qualified names", func(t *testing.T) {
		result, err := s.workspaceSymbol(mockContext(), &protocol.WorkspaceSymbolParams{
			Query: "router:def",
		})
		require.NoError(t, err)
		names := symbolNames(result)
		assert.Contains(t, names, "defendpoint")
	})

	t.Run("package exports have container name", func(t *testing.T) {
		result, err := s.workspaceSymbol(mockContext(), &protocol.WorkspaceSymbolParams{
			Query: "defendpoint",
		})
		require.NoError(t, err)
		require.Len(t, result, 1)
		require.NotNil(t, result[0].ContainerName)
		assert.Equal(t, "router", *result[0].ContainerName)
	})

	t.Run("symbol kind is mapped correctly", func(t *testing.T) {
		result, err := s.workspaceSymbol(mockContext(), &protocol.WorkspaceSymbolParams{
			Query: "max-retries",
		})
		require.NoError(t, err)
		require.Len(t, result, 1)
		assert.Equal(t, protocol.SymbolKindVariable, result[0].Kind)
	})

	t.Run("no results for non-matching query", func(t *testing.T) {
		result, err := s.workspaceSymbol(mockContext(), &protocol.WorkspaceSymbolParams{
			Query: "zzz-nonexistent",
		})
		require.NoError(t, err)
		assert.Empty(t, result)
	})

	t.Run("symbols without source are excluded", func(t *testing.T) {
		cfg2 := &analysis.Config{
			ExtraGlobals: []analysis.ExternalSymbol{
				{Name: "no-source", Kind: analysis.SymFunction},
				{Name: "has-source", Kind: analysis.SymFunction, Source: &token.Location{File: "/f.lisp", Line: 1, Col: 1}},
			},
		}
		setTestAnalysisCfg(s, cfg2)

		result, err := s.workspaceSymbol(mockContext(), &protocol.WorkspaceSymbolParams{
			Query: "",
		})
		require.NoError(t, err)
		names := symbolNames(result)
		assert.Contains(t, names, "has-source")
		assert.NotContains(t, names, "no-source")
	})

	t.Run("includes open document symbols", func(t *testing.T) {
		cfg3 := &analysis.Config{
			ExtraGlobals: []analysis.ExternalSymbol{},
		}
		setTestAnalysisCfg(s, cfg3)

		openDoc(s, "file:///workspace/active.lisp",
			`(defun active-fn (x) x)`)

		result, err := s.workspaceSymbol(mockContext(), &protocol.WorkspaceSymbolParams{
			Query: "active-fn",
		})
		require.NoError(t, err)
		names := symbolNames(result)
		assert.Contains(t, names, "active-fn")
	})
}

func TestMatchesQuery(t *testing.T) {
	assert.True(t, matchesQuery("defun", ""))
	assert.True(t, matchesQuery("my-handler", "handler"))
	assert.True(t, matchesQuery("MY-HANDLER", "handler"))
	// matchesQuery expects lowerQuery to already be lowered (caller does this).
	assert.False(t, matchesQuery("my-handler", "HANDLER"), "upper query not pre-lowered → no match")
	assert.False(t, matchesQuery("my-handler", "zzz"))
}

func symbolNames(syms []protocol.SymbolInformation) []string {
	names := make([]string, len(syms))
	for i, s := range syms {
		names[i] = s.Name
	}
	return names
}
