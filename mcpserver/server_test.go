package mcpserver

import (
	"context"
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/modelcontextprotocol/go-sdk/mcp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestDescribeServerAndListTools(t *testing.T) {
	tmp := t.TempDir()
	session, serverSession := connectTestServer(t, New(WithWorkspaceRoot(tmp)))
	defer session.Close()
	defer serverSession.Close()

	tools, err := session.ListTools(context.Background(), nil)
	require.NoError(t, err)
	assert.Len(t, tools.Tools, 10)

	res, err := session.CallTool(context.Background(), &mcp.CallToolParams{Name: "describe_server"})
	require.NoError(t, err)
	require.False(t, res.IsError)

	got := decodeStructured[DescribeServerResponse](t, res)
	assert.Equal(t, defaultImplementationName, got.Name)
	assert.Equal(t, tmp, got.DefaultWorkspaceRoot)
	assert.Len(t, got.Capabilities, 10)
}

func TestNavigationTools(t *testing.T) {
	tmp := t.TempDir()
	libPath := filepath.Join(tmp, "lib.lisp")
	mainPath := filepath.Join(tmp, "main.lisp")
	writeTestFile(t, libPath, "(in-package 'lib)\n(export 'add-one)\n(defun add-one (x) (+ x 1))")
	mainContent := "(defun run () (lib:add-one 1))"
	writeTestFile(t, mainPath, mainContent)

	session, serverSession := connectTestServer(t, New(WithWorkspaceRoot(tmp)))
	defer session.Close()
	defer serverSession.Close()

	pos := strings.Index(mainContent, "add-one")
	require.GreaterOrEqual(t, pos, 0)

	hoverRes, err := session.CallTool(context.Background(), &mcp.CallToolParams{
		Name: "hover",
		Arguments: map[string]any{
			"path":      mainPath,
			"line":      0,
			"character": pos,
		},
	})
	require.NoError(t, err)
	require.False(t, hoverRes.IsError)
	hover := decodeStructured[HoverResponse](t, hoverRes)
	assert.True(t, hover.Found)
	assert.Equal(t, "add-one", hover.SymbolName)

	defRes, err := session.CallTool(context.Background(), &mcp.CallToolParams{
		Name: "definition",
		Arguments: map[string]any{
			"path":      mainPath,
			"line":      0,
			"character": pos,
		},
	})
	require.NoError(t, err)
	require.False(t, defRes.IsError)
	definition := decodeStructured[DefinitionResponse](t, defRes)
	require.True(t, definition.Found)
	require.NotNil(t, definition.Location)
	assert.Equal(t, libPath, definition.Location.Path)

	refsRes, err := session.CallTool(context.Background(), &mcp.CallToolParams{
		Name: "references",
		Arguments: map[string]any{
			"path":                libPath,
			"line":                2,
			"character":           strings.Index("(defun add-one (x) (+ x 1))", "add-one"),
			"include_declaration": true,
		},
	})
	require.NoError(t, err)
	require.False(t, refsRes.IsError)
	refs := decodeStructured[ReferencesResponse](t, refsRes)
	assert.Equal(t, "add-one", refs.SymbolName)
	assert.Len(t, refs.References, 2)
}

func TestDiagnosticsAndPerfTools(t *testing.T) {
	tmp := t.TempDir()
	badPath := filepath.Join(tmp, "bad.lisp")
	writeTestFile(t, badPath, `(defun broken (`)
	perfPath := filepath.Join(tmp, "perf.lisp")
	perfContent := `(defun process (items) (map 'list (lambda (item) (db-put item)) items))`
	writeTestFile(t, perfPath, perfContent)

	session, serverSession := connectTestServer(t, New(WithWorkspaceRoot(tmp)))
	defer session.Close()
	defer serverSession.Close()

	diagRes, err := session.CallTool(context.Background(), &mcp.CallToolParams{
		Name: "diagnostics",
		Arguments: map[string]any{
			"path": badPath,
		},
	})
	require.NoError(t, err)
	require.False(t, diagRes.IsError)
	diagnostics := decodeStructured[DiagnosticsResponse](t, diagRes)
	require.Len(t, diagnostics.Files, 1)
	require.NotEmpty(t, diagnostics.Files[0].Diagnostics)
	assert.Equal(t, "error", diagnostics.Files[0].Diagnostics[0].Severity)

	perfRes, err := session.CallTool(context.Background(), &mcp.CallToolParams{
		Name: "perf_issues",
		Arguments: map[string]any{
			"paths": []string{perfPath},
			"top":   1,
		},
	})
	require.NoError(t, err)
	require.False(t, perfRes.IsError)
	perfIssues := decodeStructured[PerfIssuesResponse](t, perfRes)
	require.NotEmpty(t, perfIssues.Issues)
	assert.NotEmpty(t, perfIssues.Solved)

	graphRes, err := session.CallTool(context.Background(), &mcp.CallToolParams{
		Name: "call_graph",
		Arguments: map[string]any{
			"paths": []string{perfPath},
		},
	})
	require.NoError(t, err)
	require.False(t, graphRes.IsError)
	graph := decodeStructured[CallGraphResponse](t, graphRes)
	assert.NotEmpty(t, graph.Functions)
	assert.NotEmpty(t, graph.Edges)

	hotspotsRes, err := session.CallTool(context.Background(), &mcp.CallToolParams{
		Name: "hotspots",
		Arguments: map[string]any{
			"paths": []string{perfPath},
			"top":   1,
		},
	})
	require.NoError(t, err)
	require.False(t, hotspotsRes.IsError)
	hotspots := decodeStructured[HotspotsResponse](t, hotspotsRes)
	require.Len(t, hotspots.Functions, 1)
}

func TestWorkspaceSymbolsRefreshAcrossRequests(t *testing.T) {
	tmp := t.TempDir()
	libPath := filepath.Join(tmp, "lib.lisp")
	writeTestFile(t, libPath, "(in-package 'lib)\n(export 'alpha)\n(defun alpha () 1)")

	session, serverSession := connectTestServer(t, New(WithWorkspaceRoot(tmp)))
	defer session.Close()
	defer serverSession.Close()

	first := workspaceSymbols(t, session, tmp, "alpha")
	require.Len(t, first.Symbols, 1)
	assert.Equal(t, "alpha", first.Symbols[0].Name)

	writeTestFile(t, libPath, "(in-package 'lib)\n(export 'beta)\n(defun beta () 1)")

	second := workspaceSymbols(t, session, tmp, "beta")
	require.Len(t, second.Symbols, 1)
	assert.Equal(t, "beta", second.Symbols[0].Name)

	old := workspaceSymbols(t, session, tmp, "alpha")
	assert.Empty(t, old.Symbols)
}

func TestWorkspaceSymbolsDeduplicateExports(t *testing.T) {
	tmp := t.TempDir()
	libPath := filepath.Join(tmp, "lib.lisp")
	writeTestFile(t, libPath, "(in-package 'lib)\n(export 'alpha)\n(defun alpha () 1)")

	session, serverSession := connectTestServer(t, New(WithWorkspaceRoot(tmp)))
	defer session.Close()
	defer serverSession.Close()

	symbols := workspaceSymbols(t, session, tmp, "alpha")
	require.Len(t, symbols.Symbols, 1)
	assert.Equal(t, "alpha", symbols.Symbols[0].Name)
}

func TestDescribeServerIncludesRegistrarTools(t *testing.T) {
	srv := New(WithToolRegistrar(func(server *mcp.Server) error {
		mcp.AddTool(server, &mcp.Tool{Name: "custom_tool", Description: "custom tool"}, func(ctx context.Context, _ *mcp.CallToolRequest, _ struct{}) (*mcp.CallToolResult, map[string]any, error) {
			return nil, map[string]any{"ok": true}, nil
		})
		return nil
	}))

	session, serverSession := connectTestServer(t, srv)
	defer session.Close()
	defer serverSession.Close()

	res, err := session.CallTool(context.Background(), &mcp.CallToolParams{Name: "describe_server"})
	require.NoError(t, err)
	require.False(t, res.IsError)

	got := decodeStructured[DescribeServerResponse](t, res)
	var found bool
	for _, capability := range got.Capabilities {
		if capability.Name == "custom_tool" {
			found = true
			break
		}
	}
	assert.True(t, found)
}

func connectTestServer(t *testing.T, srv *Server) (*mcp.ClientSession, *mcp.ServerSession) {
	t.Helper()
	ctx := context.Background()
	serverTransport, clientTransport := mcp.NewInMemoryTransports()
	serverSession, err := srv.MCPServer().Connect(ctx, serverTransport, nil)
	require.NoError(t, err)
	client := mcp.NewClient(&mcp.Implementation{Name: "test-client", Version: "v1.0.0"}, nil)
	clientSession, err := client.Connect(ctx, clientTransport, nil)
	require.NoError(t, err)
	return clientSession, serverSession
}

func decodeStructured[T any](t *testing.T, res *mcp.CallToolResult) T {
	t.Helper()
	var out T
	data, err := json.Marshal(res.StructuredContent)
	require.NoError(t, err)
	require.NoError(t, json.Unmarshal(data, &out))
	return out
}

func writeTestFile(t *testing.T, path, content string) {
	t.Helper()
	require.NoError(t, os.WriteFile(path, []byte(content), 0o644))
}

func workspaceSymbols(t *testing.T, session *mcp.ClientSession, root string, query string) WorkspaceSymbolsResponse {
	t.Helper()
	res, err := session.CallTool(context.Background(), &mcp.CallToolParams{
		Name: "workspace_symbols",
		Arguments: map[string]any{
			"workspace_root": root,
			"query":          query,
		},
	})
	require.NoError(t, err)
	require.False(t, res.IsError)
	return decodeStructured[WorkspaceSymbolsResponse](t, res)
}
