package mcpserver

import (
	"context"
	"encoding/json"
	"os"
	"path/filepath"
	"runtime"
	"strings"
	"testing"
	"time"

	"github.com/luthersystems/elps/analysis/perf"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/modelcontextprotocol/go-sdk/mcp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestDescribeServerAndListTools(t *testing.T) {
	tmp := t.TempDir()
	session, serverSession := connectTestServer(t, New(WithWorkspaceRoot(tmp)))
	defer closeClientSession(t, session)
	defer closeServerSession(t, serverSession)

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

func TestNewProvidesStdlibQualifiedSymbolsByDefault(t *testing.T) {
	session, serverSession := connectTestServer(t, New())
	defer closeClientSession(t, session)
	defer closeServerSession(t, serverSession)

	content := `(string:join "," items)`
	res, err := session.CallTool(context.Background(), &mcp.CallToolParams{
		Name: "hover",
		Arguments: map[string]any{
			"path":      filepath.Join(t.TempDir(), "stdlib.lisp"),
			"line":      0,
			"character": strings.Index(content, "join"),
			"content":   content,
		},
	})
	require.NoError(t, err)
	require.False(t, res.IsError)

	hover := decodeStructured[HoverResponse](t, res)
	assert.True(t, hover.Found)
	assert.Equal(t, "join", hover.SymbolName)
}

func TestHoverPreservesBuiltinPackageInLocation(t *testing.T) {
	session, serverSession := connectTestServer(t, New())
	defer closeClientSession(t, session)
	defer closeServerSession(t, serverSession)

	content := `(string:join "," items)`
	res, err := session.CallTool(context.Background(), &mcp.CallToolParams{
		Name: "hover",
		Arguments: map[string]any{
			"path":      filepath.Join(t.TempDir(), "stdlib.lisp"),
			"line":      0,
			"character": strings.Index(content, "join"),
			"content":   content,
		},
	})
	require.NoError(t, err)
	require.False(t, res.IsError)

	hover := decodeStructured[HoverResponse](t, res)
	require.True(t, hover.Found)
	require.NotNil(t, hover.DefinedIn)
	assert.True(t, hover.DefinedIn.Virtual)
	assert.Equal(t, builtinURI("string", "join"), hover.DefinedIn.VirtualID)
}

func TestNavigationTools(t *testing.T) {
	tmp := t.TempDir()
	libPath := filepath.Join(tmp, "lib.lisp")
	mainPath := filepath.Join(tmp, "main.lisp")
	writeTestFile(t, libPath, "(in-package 'lib)\n(export 'add-one)\n(defun add-one (x) (+ x 1))")
	mainContent := "(defun run () (lib:add-one 1))"
	writeTestFile(t, mainPath, mainContent)

	session, serverSession := connectTestServer(t, New(WithWorkspaceRoot(tmp)))
	defer closeClientSession(t, session)
	defer closeServerSession(t, serverSession)

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
	defer closeClientSession(t, session)
	defer closeServerSession(t, serverSession)

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

	srv := New(WithWorkspaceRoot(tmp))
	srv.service.workspaceValidationInterval = 0
	session, serverSession := connectTestServer(t, srv)
	defer closeClientSession(t, session)
	defer closeServerSession(t, serverSession)

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

func TestWorkspaceStateReusedWhenFingerprintUnchanged(t *testing.T) {
	tmp := t.TempDir()
	libPath := filepath.Join(tmp, "lib.lisp")
	writeTestFile(t, libPath, "(in-package 'lib)\n(export 'alpha)\n(defun alpha () 1)")

	srv := New(WithWorkspaceRoot(tmp))
	builds := 0
	fingerprints := 0
	srv.service.buildWorkspaceStateHook = func(string) { builds++ }
	srv.service.workspaceFingerprintHook = func(string) { fingerprints++ }
	srv.service.workspaceValidationInterval = time.Hour

	session, serverSession := connectTestServer(t, srv)
	defer closeClientSession(t, session)
	defer closeServerSession(t, serverSession)

	first := workspaceSymbols(t, session, tmp, "alpha")
	require.Len(t, first.Symbols, 1)
	second := workspaceSymbols(t, session, tmp, "alpha")
	require.Len(t, second.Symbols, 1)
	assert.Equal(t, 1, builds)
	assert.Equal(t, 1, fingerprints)
}

func TestReferencesStayWithinPackage(t *testing.T) {
	tmp := t.TempDir()
	fooPath := filepath.Join(tmp, "foo.lisp")
	barPath := filepath.Join(tmp, "bar.lisp")
	fooUsePath := filepath.Join(tmp, "foo_use.lisp")
	barUsePath := filepath.Join(tmp, "bar_use.lisp")
	writeTestFile(t, fooPath, "(in-package 'foo)\n(export 'run)\n(defun run () 1)")
	writeTestFile(t, barPath, "(in-package 'bar)\n(export 'run)\n(defun run () 2)")
	writeTestFile(t, fooUsePath, "(defun use-foo () (foo:run))")
	writeTestFile(t, barUsePath, "(defun use-bar () (bar:run))")

	srv := New(WithWorkspaceRoot(tmp))
	srv.service.workspaceValidationInterval = 0
	session, serverSession := connectTestServer(t, srv)
	defer closeClientSession(t, session)
	defer closeServerSession(t, serverSession)

	fooRefsRes, err := session.CallTool(context.Background(), &mcp.CallToolParams{
		Name: "references",
		Arguments: map[string]any{
			"path":                fooPath,
			"line":                2,
			"character":           strings.Index("(defun run () 1)", "run"),
			"include_declaration": true,
		},
	})
	require.NoError(t, err)
	require.False(t, fooRefsRes.IsError)
	fooRefs := decodeStructured[ReferencesResponse](t, fooRefsRes)
	require.Len(t, fooRefs.References, 2)
	assert.Contains(t, []string{fooRefs.References[0].Path, fooRefs.References[1].Path}, fooPath)
	assert.Contains(t, []string{fooRefs.References[0].Path, fooRefs.References[1].Path}, fooUsePath)
	assert.NotContains(t, []string{fooRefs.References[0].Path, fooRefs.References[1].Path}, barUsePath)

	barRefsRes, err := session.CallTool(context.Background(), &mcp.CallToolParams{
		Name: "references",
		Arguments: map[string]any{
			"path":                barPath,
			"line":                2,
			"character":           strings.Index("(defun run () 2)", "run"),
			"include_declaration": true,
		},
	})
	require.NoError(t, err)
	require.False(t, barRefsRes.IsError)
	barRefs := decodeStructured[ReferencesResponse](t, barRefsRes)
	require.Len(t, barRefs.References, 2)
	assert.Contains(t, []string{barRefs.References[0].Path, barRefs.References[1].Path}, barPath)
	assert.Contains(t, []string{barRefs.References[0].Path, barRefs.References[1].Path}, barUsePath)
	assert.NotContains(t, []string{barRefs.References[0].Path, barRefs.References[1].Path}, fooUsePath)
}

func TestWorkspaceSymbolsDeduplicateExports(t *testing.T) {
	tmp := t.TempDir()
	libPath := filepath.Join(tmp, "lib.lisp")
	writeTestFile(t, libPath, "(in-package 'lib)\n(export 'alpha)\n(defun alpha () 1)")

	session, serverSession := connectTestServer(t, New(WithWorkspaceRoot(tmp)))
	defer closeClientSession(t, session)
	defer closeServerSession(t, serverSession)

	symbols := workspaceSymbols(t, session, tmp, "alpha")
	require.Len(t, symbols.Symbols, 1)
	assert.Equal(t, "alpha", symbols.Symbols[0].Name)
	assert.Equal(t, "lib", symbols.Symbols[0].Package)
}

func TestWorkspaceSymbolsIncludePrivateTopLevelDefinitions(t *testing.T) {
	tmp := t.TempDir()
	libPath := filepath.Join(tmp, "lib.lisp")
	writeTestFile(t, libPath, "(in-package 'lib)\n(defun helper () 1)\n(defun public () 2)\n(export 'public)")

	session, serverSession := connectTestServer(t, New(WithWorkspaceRoot(tmp)))
	defer closeClientSession(t, session)
	defer closeServerSession(t, serverSession)

	symbols := workspaceSymbols(t, session, tmp, "helper")
	require.Len(t, symbols.Symbols, 1)
	assert.Equal(t, "helper", symbols.Symbols[0].Name)
	assert.Equal(t, "lib", symbols.Symbols[0].Package)
	assert.Equal(t, libPath, symbols.Symbols[0].Path)
}

func TestWorkspaceSymbolsPreservePackageMetadata(t *testing.T) {
	tmp := t.TempDir()
	writeTestFile(t, filepath.Join(tmp, "foo.lisp"), "(in-package 'foo)\n(export 'run)\n(defun run () 1)")
	writeTestFile(t, filepath.Join(tmp, "bar.lisp"), "(in-package 'bar)\n(export 'run)\n(defun run () 2)")

	session, serverSession := connectTestServer(t, New(WithWorkspaceRoot(tmp)))
	defer closeClientSession(t, session)
	defer closeServerSession(t, serverSession)

	all := workspaceSymbols(t, session, tmp, "run")
	require.Len(t, all.Symbols, 2)
	packages := []string{all.Symbols[0].Package, all.Symbols[1].Package}
	assert.Contains(t, packages, "foo")
	assert.Contains(t, packages, "bar")

	fooOnly := workspaceSymbols(t, session, tmp, "foo:run")
	require.Len(t, fooOnly.Symbols, 1)
	assert.Equal(t, "foo", fooOnly.Symbols[0].Package)
	assert.Equal(t, "run", fooOnly.Symbols[0].Name)
}

func TestWorkspacePackageSymbolsOverrideStdlib(t *testing.T) {
	tmp := t.TempDir()
	stringPath := filepath.Join(tmp, "string.lisp")
	writeTestFile(t, stringPath, "(in-package 'string)\n(export 'join)\n(defun join (items sep) items)")

	env, err := lisplib.NewDocEnv()
	require.NoError(t, err)

	srv := New(WithWorkspaceRoot(tmp), WithRegistry(env.Runtime.Registry))
	srv.service.workspaceValidationInterval = 0
	session, serverSession := connectTestServer(t, srv)
	defer closeClientSession(t, session)
	defer closeServerSession(t, serverSession)

	symbols := workspaceSymbols(t, session, tmp, "string:join")
	require.Len(t, symbols.Symbols, 1)
	assert.Equal(t, "string", symbols.Symbols[0].Package)
	assert.Equal(t, stringPath, symbols.Symbols[0].Path)

	mainPath := filepath.Join(tmp, "main.lisp")
	content := "(defun run () (string:join items \",\"))"
	res, err := session.CallTool(context.Background(), &mcp.CallToolParams{
		Name: "definition",
		Arguments: map[string]any{
			"path":      mainPath,
			"line":      0,
			"character": strings.Index(content, "join"),
			"content":   content,
		},
	})
	require.NoError(t, err)
	require.False(t, res.IsError)

	definition := decodeStructured[DefinitionResponse](t, res)
	require.True(t, definition.Found)
	require.NotNil(t, definition.Location)
	assert.Equal(t, stringPath, definition.Location.Path)
}

func TestDescribeServerIncludesRegistrarTools(t *testing.T) {
	srv := New(WithToolRegistrar(func(server *mcp.Server) error {
		mcp.AddTool(server, &mcp.Tool{Name: "custom_tool", Description: "custom tool"}, func(ctx context.Context, _ *mcp.CallToolRequest, _ struct{}) (*mcp.CallToolResult, map[string]any, error) {
			return nil, map[string]any{"ok": true}, nil
		})
		return nil
	}))

	session, serverSession := connectTestServer(t, srv)
	defer closeClientSession(t, session)
	defer closeServerSession(t, serverSession)

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

func TestPerfSelectionHonorsServerConfigIncludeTestsAndExcludes(t *testing.T) {
	tmp := t.TempDir()
	writeTestFile(t, filepath.Join(tmp, "prod.lisp"), `(defun prod (items) (map 'list (lambda (item) (db-put item)) items))`)
	writeTestFile(t, filepath.Join(tmp, "prod_test.lisp"), `(defun prod-test (items) (map 'list (lambda (item) (db-put item)) items))`)
	writeTestFile(t, filepath.Join(tmp, "generated_case_test.lisp"), `(defun generated-case-test (items) (map 'list (lambda (item) (db-put item)) items))`)

	cfg := perf.DefaultConfig()
	cfg.IncludeTests = true
	cfg.ExcludeFiles = []string{"generated_*_test.lisp"}

	srv := New(WithWorkspaceRoot(tmp), WithPerfConfig(cfg))
	session, serverSession := connectTestServer(t, srv)
	defer closeClientSession(t, session)
	defer closeServerSession(t, serverSession)

	res, err := session.CallTool(context.Background(), &mcp.CallToolParams{
		Name: "call_graph",
		Arguments: map[string]any{
			"workspace_root": tmp,
		},
	})
	require.NoError(t, err)
	require.False(t, res.IsError)

	graph := decodeStructured[CallGraphResponse](t, res)
	var names []string
	for _, fn := range graph.Functions {
		names = append(names, fn.Name)
	}
	assert.Contains(t, names, "prod")
	assert.Contains(t, names, "prod-test")
	assert.NotContains(t, names, "generated-case-test")
}

func TestPerfSelectionHonorsRecursiveExcludeGlobs(t *testing.T) {
	tmp := t.TempDir()
	writeTestFile(t, filepath.Join(tmp, "prod.lisp"), `(defun prod (items) (map 'list (lambda (item) (db-put item)) items))`)
	buildDir := filepath.Join(tmp, "build", "nested")
	require.NoError(t, os.MkdirAll(buildDir, 0o750))
	writeTestFile(t, filepath.Join(buildDir, "bad.lisp"), `(defun bad (items) (map 'list (lambda (item) (db-put item)) items))`)

	cfg := perf.DefaultConfig()
	cfg.ExcludeFiles = []string{"build/**"}

	srv := New(WithWorkspaceRoot(tmp), WithPerfConfig(cfg))
	session, serverSession := connectTestServer(t, srv)
	defer closeClientSession(t, session)
	defer closeServerSession(t, serverSession)

	res, err := session.CallTool(context.Background(), &mcp.CallToolParams{
		Name: "call_graph",
		Arguments: map[string]any{
			"workspace_root": tmp,
		},
	})
	require.NoError(t, err)
	require.False(t, res.IsError)

	graph := decodeStructured[CallGraphResponse](t, res)
	var names []string
	for _, fn := range graph.Functions {
		names = append(names, fn.Name)
	}
	assert.Contains(t, names, "prod")
	assert.NotContains(t, names, "bad")
}

func TestWorkspaceTraversalErrorsSurface(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("permission-based traversal failure is not reliable on windows")
	}

	tmp := t.TempDir()
	writeTestFile(t, filepath.Join(tmp, "ok.lisp"), `(defun ok () 1)`)
	blockedDir := filepath.Join(tmp, "blocked")
	require.NoError(t, os.Mkdir(blockedDir, 0o750))
	writeTestFile(t, filepath.Join(blockedDir, "hidden.lisp"), `(defun hidden () 1)`)
	require.NoError(t, os.Chmod(blockedDir, 0))
	defer func() {
		require.NoError(t, os.Chmod(blockedDir, 0o750)) //nolint:gosec // restore temp test dir permissions for cleanup
	}()

	session, serverSession := connectTestServer(t, New(WithWorkspaceRoot(tmp)))
	defer closeClientSession(t, session)
	defer closeServerSession(t, serverSession)

	res, err := session.CallTool(context.Background(), &mcp.CallToolParams{
		Name: "workspace_symbols",
		Arguments: map[string]any{
			"workspace_root": tmp,
			"query":          "ok",
		},
	})
	require.NoError(t, err)
	require.True(t, res.IsError)
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
	require.NoError(t, os.WriteFile(path, []byte(content), 0o600))
}

func closeClientSession(t *testing.T, session *mcp.ClientSession) {
	t.Helper()
	require.NoError(t, session.Close())
}

func closeServerSession(t *testing.T, session *mcp.ServerSession) {
	t.Helper()
	require.NoError(t, session.Close())
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
