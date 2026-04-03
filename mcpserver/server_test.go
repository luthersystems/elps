package mcpserver

import (
	"bytes"
	"context"
	"encoding/json"
	"log/slog"
	"os"
	"path/filepath"
	"runtime"
	"strings"
	"testing"
	"time"

	"github.com/luthersystems/elps/analysis"
	"github.com/luthersystems/elps/analysis/perf"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser/token"
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
	assert.Len(t, tools.Tools, 16)

	res, err := session.CallTool(context.Background(), &mcp.CallToolParams{Name: "describe_server"})
	require.NoError(t, err)
	require.False(t, res.IsError)

	got := decodeStructured[DescribeServerResponse](t, res)
	assert.Equal(t, defaultImplementationName, got.Name)
	assert.Equal(t, tmp, got.DefaultWorkspaceRoot)
	assert.Len(t, got.Capabilities, 16)
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

func TestWorkspaceSymbolsExcludeStdlibExports(t *testing.T) {
	tmp := t.TempDir()
	srv := New(WithWorkspaceRoot(tmp))
	fingerprint, err := srv.service.fingerprintWorkspace(tmp)
	require.NoError(t, err)
	srv.service.workspaceValidationInterval = time.Hour
	srv.service.workspaces[tmp] = &workspaceState{
		cfg: &analysis.Config{
			PackageExports: map[string][]analysis.ExternalSymbol{
				"string": {
					{
						Name:    "join",
						Package: "string",
						Kind:    analysis.SymFunction,
						Source:  &token.Location{File: "/stdlib/string.lisp", Line: 1, Col: 1},
					},
				},
			},
		},
		symbols: []analysis.ExternalSymbol{
			{
				Name:    "helper",
				Package: "user",
				Kind:    analysis.SymFunction,
				Source:  &token.Location{File: filepath.Join(tmp, "local.lisp"), Line: 1, Col: 8},
			},
		},
		fingerprint: fingerprint,
	}
	srv.service.workspaces[tmp].validatedAt.Store(time.Now().UnixNano())

	_, join, err := srv.service.workspaceSymbolsTool(context.Background(), nil, WorkspaceSymbolsInput{
		WorkspaceRoot: &tmp,
		Query:         "join",
	})
	require.NoError(t, err)
	assert.Empty(t, join.Symbols, "workspace_symbols query should not return non-workspace package exports")
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

func TestWorkspaceTraversalErrorsAreLoggedAndSkipped(t *testing.T) {
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

	var logBuf bytes.Buffer
	logger := slog.New(slog.NewTextHandler(&logBuf, nil))
	session, serverSession := connectTestServer(t, New(WithWorkspaceRoot(tmp), WithLogger(logger)))
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
	require.False(t, res.IsError)
	symbols := decodeStructured[WorkspaceSymbolsResponse](t, res)
	require.Len(t, symbols.Symbols, 1)
	assert.Equal(t, "ok", symbols.Symbols[0].Name)
	assert.Contains(t, logBuf.String(), "skipping unreadable workspace path")
	assert.Contains(t, logBuf.String(), blockedDir)
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

func TestListPerfWorkspaceFiles_DoesNotMutateInput(t *testing.T) {
	tmp := t.TempDir()
	writeTestFile(t, filepath.Join(tmp, "a.lisp"), "(defun a () 1)")
	writeTestFile(t, filepath.Join(tmp, "b_test.lisp"), "(defun b () 2)")

	srv := New(WithWorkspaceRoot(tmp))
	paths, err := srv.service.listWorkspaceFiles(tmp, true)
	require.NoError(t, err)
	original := make([]string, len(paths))
	copy(original, paths)

	cfg := &perf.Config{ExcludeFiles: []string{"*_test.lisp"}}
	filtered, err := srv.service.listPerfWorkspaceFiles(tmp, cfg, false)
	require.NoError(t, err)
	require.NotEmpty(t, filtered)

	// The original paths slice must not have been mutated.
	assert.Equal(t, original, paths)
}

func TestWorkspaceEmptyRoot_NoFilesystemIO(t *testing.T) {
	srv := New()
	fingerprintCalled := false
	srv.service.workspaceFingerprintHook = func(string) { fingerprintCalled = true }

	state, err := srv.service.workspace("")
	require.NoError(t, err)
	assert.NotNil(t, state)
	assert.NotNil(t, state.cfg)
	assert.False(t, fingerprintCalled, "workspace fingerprinting should be skipped for empty root")
}

func TestWorkspaceValidationRaceSafety(t *testing.T) {
	tmp := t.TempDir()
	writeTestFile(t, filepath.Join(tmp, "lib.lisp"), "(defun lib () 1)")

	srv := New(WithWorkspaceRoot(tmp))
	srv.service.workspaceValidationInterval = 0

	// Prime the workspace cache.
	_, err := srv.service.workspace(tmp)
	require.NoError(t, err)

	// Concurrently read and mark validated — should not race with -race flag.
	done := make(chan struct{})
	for i := 0; i < 10; i++ {
		go func() {
			defer func() { done <- struct{}{} }()
			for j := 0; j < 50; j++ {
				_, _ = srv.service.workspace(tmp)
			}
		}()
		go func() {
			defer func() { done <- struct{}{} }()
			for j := 0; j < 50; j++ {
				srv.service.markWorkspaceValidated(tmp)
			}
		}()
	}
	for i := 0; i < 20; i++ {
		<-done
	}
}

func TestDiagnosticsWorkspaceWide(t *testing.T) {
	tmp := t.TempDir()
	writeTestFile(t, filepath.Join(tmp, "ok.lisp"), "(defun ok () 1)")
	writeTestFile(t, filepath.Join(tmp, "bad.lisp"), "(defun broken (")

	session, serverSession := connectTestServer(t, New(WithWorkspaceRoot(tmp)))
	defer closeClientSession(t, session)
	defer closeServerSession(t, serverSession)

	res, err := session.CallTool(context.Background(), &mcp.CallToolParams{
		Name: "diagnostics",
		Arguments: map[string]any{
			"workspace_root":    tmp,
			"include_workspace": true,
		},
	})
	require.NoError(t, err)
	require.False(t, res.IsError)

	diagnostics := decodeStructured[DiagnosticsResponse](t, res)
	require.Len(t, diagnostics.Files, 2, "should include both workspace files")
	var hasErrors bool
	for _, fd := range diagnostics.Files {
		if len(fd.Diagnostics) > 0 {
			hasErrors = true
		}
	}
	assert.True(t, hasErrors, "bad.lisp should produce diagnostics")
}

func TestDiagnostics_CrossFileSymbolResolution(t *testing.T) {
	// Regression test for #259: MCP diagnostics should resolve symbols
	// defined in other workspace files when workspace_root is set.
	tmp := t.TempDir()
	writeTestFile(t, filepath.Join(tmp, "a.lisp"), "(defun helper () 42)")
	writeTestFile(t, filepath.Join(tmp, "b.lisp"), "(defun caller () (helper))")

	session, serverSession := connectTestServer(t, New(WithWorkspaceRoot(tmp)))
	defer closeClientSession(t, session)
	defer closeServerSession(t, serverSession)

	res, err := session.CallTool(context.Background(), &mcp.CallToolParams{
		Name: "diagnostics",
		Arguments: map[string]any{
			"path":           filepath.Join(tmp, "b.lisp"),
			"workspace_root": tmp,
		},
	})
	require.NoError(t, err)
	require.False(t, res.IsError)

	diagnostics := decodeStructured[DiagnosticsResponse](t, res)
	require.NotEmpty(t, diagnostics.Files, "should have diagnostics for at least one file")
	// 'helper' should NOT be flagged as undefined — it's defined in a.lisp.
	for _, fd := range diagnostics.Files {
		for _, d := range fd.Diagnostics {
			assert.NotContains(t, d.Message, "helper",
				"cross-file symbol 'helper' should be resolved via workspace_root")
		}
	}
}

func TestDiagnostics_CrossFileSymbolResolution_WithoutWorkspace(t *testing.T) {
	// Negative test: without workspace_root, cross-file symbols are NOT resolved.
	// This proves the workspace_root fix is actually needed.
	tmp := t.TempDir()
	writeTestFile(t, filepath.Join(tmp, "a.lisp"), "(defun helper () 42)")
	writeTestFile(t, filepath.Join(tmp, "b.lisp"), "(defun caller () (helper))")

	// Create server WITHOUT workspace root — single-file analysis only.
	session, serverSession := connectTestServer(t, New())
	defer closeClientSession(t, session)
	defer closeServerSession(t, serverSession)

	// Analyze b.lisp alone, without workspace_root — single-file mode.
	res, err := session.CallTool(context.Background(), &mcp.CallToolParams{
		Name: "diagnostics",
		Arguments: map[string]any{
			"path": filepath.Join(tmp, "b.lisp"),
		},
	})
	require.NoError(t, err)
	require.False(t, res.IsError)

	diagnostics := decodeStructured[DiagnosticsResponse](t, res)
	// Without workspace context, 'helper' SHOULD be flagged as undefined.
	var helperFlagged bool
	for _, fd := range diagnostics.Files {
		for _, d := range fd.Diagnostics {
			if strings.Contains(d.Message, "helper") {
				helperFlagged = true
			}
		}
	}
	assert.True(t, helperFlagged,
		"without workspace_root, cross-file symbol 'helper' should be flagged as undefined")
}

func TestNullVsEmptySlice_References(t *testing.T) {
	tmp := t.TempDir()
	path := filepath.Join(tmp, "empty.lisp")
	writeTestFile(t, path, "(defun noop () 1)")

	srv := New(WithWorkspaceRoot(tmp))
	_, resp, err := srv.service.referencesTool(context.Background(), nil, ReferencesInput{
		Path:      path,
		Line:      0,
		Character: 100, // past end of line — no symbol
	})
	require.NoError(t, err)
	require.NotNil(t, resp.References, "references must be non-nil empty slice, not nil")
	assert.Empty(t, resp.References)
}

func TestNullVsEmptySlice_DocumentSymbols(t *testing.T) {
	tmp := t.TempDir()
	path := filepath.Join(tmp, "empty.lisp")
	writeTestFile(t, path, "; just a comment")

	srv := New(WithWorkspaceRoot(tmp))
	_, resp, err := srv.service.documentSymbolsTool(context.Background(), nil, DocumentQueryInput{Path: path})
	require.NoError(t, err)
	require.NotNil(t, resp.Symbols, "symbols must be non-nil empty slice, not nil")
	assert.Empty(t, resp.Symbols)
}

func TestNullVsEmptySlice_WorkspaceSymbols(t *testing.T) {
	tmp := t.TempDir()
	writeTestFile(t, filepath.Join(tmp, "lib.lisp"), "(defun alpha () 1)")

	srv := New(WithWorkspaceRoot(tmp))
	srv.service.workspaceValidationInterval = 0
	_, resp, err := srv.service.workspaceSymbolsTool(context.Background(), nil, WorkspaceSymbolsInput{
		WorkspaceRoot: &tmp,
		Query:         "nonexistent-symbol-xyz",
	})
	require.NoError(t, err)
	require.NotNil(t, resp.Symbols, "symbols must be non-nil empty slice, not nil")
	assert.Empty(t, resp.Symbols)
}

func TestReferences_Limit(t *testing.T) {
	tmp := t.TempDir()
	path := filepath.Join(tmp, "refs.lisp")
	writeTestFile(t, path, "(defun helper () 1)\n(defun a () (helper))\n(defun b () (helper))\n(defun c () (helper))")

	srv := New(WithWorkspaceRoot(tmp))
	_, resp, err := srv.service.referencesTool(context.Background(), nil, ReferencesInput{
		Path:               path,
		Line:               0,
		Character:          7, // "helper"
		IncludeDeclaration: true,
		Limit:              2,
	})
	require.NoError(t, err)
	assert.Len(t, resp.References, 2)
	assert.True(t, resp.Truncated)
	assert.GreaterOrEqual(t, resp.Total, 3)
}

func TestWorkspaceSymbols_Limit(t *testing.T) {
	tmp := t.TempDir()
	writeTestFile(t, filepath.Join(tmp, "many.lisp"), "(defun alpha () 1)\n(defun beta () 2)\n(defun gamma () 3)\n(defun delta () 4)")

	srv := New(WithWorkspaceRoot(tmp))
	srv.service.workspaceValidationInterval = 0
	_, resp, err := srv.service.workspaceSymbolsTool(context.Background(), nil, WorkspaceSymbolsInput{
		WorkspaceRoot: &tmp,
		Query:         "",
		Limit:         2,
	})
	require.NoError(t, err)
	assert.Len(t, resp.Symbols, 2)
	assert.True(t, resp.Truncated)
	assert.Equal(t, 4, resp.Total)
}

func TestDiagnostics_MaxFiles(t *testing.T) {
	tmp := t.TempDir()
	for _, name := range []string{"a.lisp", "b.lisp", "c.lisp", "d.lisp", "e.lisp"} {
		writeTestFile(t, filepath.Join(tmp, name), "(defun broken (")
	}

	srv := New(WithWorkspaceRoot(tmp))
	srv.service.workspaceValidationInterval = 0
	_, resp, err := srv.service.diagnosticsTool(context.Background(), nil, DiagnosticsInput{
		WorkspaceRoot:    &tmp,
		IncludeWorkspace: true,
		MaxFiles:         2,
	})
	require.NoError(t, err)
	assert.Len(t, resp.Files, 2)
	assert.True(t, resp.Truncated)
	assert.Equal(t, 5, resp.TotalFiles)
}

func TestDiagnostics_SeverityFilter(t *testing.T) {
	tmp := t.TempDir()
	// File with parse errors (severity: error) and lint warnings.
	// set x twice produces a set-usage warning.
	path := filepath.Join(tmp, "mixed.lisp")
	writeTestFile(t, path, "(set 'x 1)\n(set 'x 2)")

	srv := New(WithWorkspaceRoot(tmp))
	severity := "warning"
	_, resp, err := srv.service.diagnosticsTool(context.Background(), nil, DiagnosticsInput{
		Path:     &path,
		Severity: &severity,
	})
	require.NoError(t, err)
	require.Len(t, resp.Files, 1)
	for _, d := range resp.Files[0].Diagnostics {
		assert.Equal(t, "warning", d.Severity)
	}
}

func TestDiagnostics_InlineContentNoPath(t *testing.T) {
	srv := New()
	content := "(defun broken ("
	_, resp, err := srv.service.diagnosticsTool(context.Background(), nil, DiagnosticsInput{
		Content: &content,
	})
	require.NoError(t, err)
	require.Len(t, resp.Files, 1)
	assert.Equal(t, "<stdin>", resp.Files[0].Path)
	assert.NotEmpty(t, resp.Files[0].Diagnostics)
}

func TestDiagnostics_PathWithIncludeWorkspace(t *testing.T) {
	tmp := t.TempDir()
	writeTestFile(t, filepath.Join(tmp, "a.lisp"), "(defun broken (")
	writeTestFile(t, filepath.Join(tmp, "b.lisp"), "(defun also-broken (")
	targetPath := filepath.Join(tmp, "a.lisp")

	srv := New(WithWorkspaceRoot(tmp))
	srv.service.workspaceValidationInterval = 0
	_, resp, err := srv.service.diagnosticsTool(context.Background(), nil, DiagnosticsInput{
		Path:             &targetPath,
		WorkspaceRoot:    &tmp,
		IncludeWorkspace: true,
	})
	require.NoError(t, err)
	require.Len(t, resp.Files, 1, "should only return diagnostics for the specified path")
	assert.Equal(t, targetPath, resp.Files[0].Path)
}

func TestHover_FileNotFound(t *testing.T) {
	tmp := t.TempDir()
	srv := New(WithWorkspaceRoot(tmp))
	_, _, err := srv.service.hoverTool(context.Background(), nil, FileQueryInput{
		Path:      filepath.Join(tmp, "nonexistent.lisp"),
		Line:      0,
		Character: 0,
	})
	require.Error(t, err)
	assert.Contains(t, err.Error(), "file not found")
	var te *toolErr
	require.ErrorAs(t, err, &te, "loadDocument errors should be structured toolErr")
	assert.Equal(t, "file_not_found", te.Code)
}

func TestDefinition_FileNotFound(t *testing.T) {
	tmp := t.TempDir()
	srv := New(WithWorkspaceRoot(tmp))
	_, _, err := srv.service.definitionTool(context.Background(), nil, FileQueryInput{
		Path: filepath.Join(tmp, "nonexistent.lisp"), Line: 0, Character: 0,
	})
	require.Error(t, err)
	var te *toolErr
	require.ErrorAs(t, err, &te)
	assert.Equal(t, "file_not_found", te.Code)
}

func TestReferences_FileNotFound(t *testing.T) {
	tmp := t.TempDir()
	srv := New(WithWorkspaceRoot(tmp))
	_, _, err := srv.service.referencesTool(context.Background(), nil, ReferencesInput{
		Path: filepath.Join(tmp, "nonexistent.lisp"), Line: 0, Character: 0,
	})
	require.Error(t, err)
	var te *toolErr
	require.ErrorAs(t, err, &te)
	assert.Equal(t, "file_not_found", te.Code)
}

func TestDocumentSymbols_FileNotFound(t *testing.T) {
	tmp := t.TempDir()
	srv := New(WithWorkspaceRoot(tmp))
	_, _, err := srv.service.documentSymbolsTool(context.Background(), nil, DocumentQueryInput{
		Path: filepath.Join(tmp, "nonexistent.lisp"),
	})
	require.Error(t, err)
	var te *toolErr
	require.ErrorAs(t, err, &te)
	assert.Equal(t, "file_not_found", te.Code)
}

func TestEvalTool_EnvironmentIsolation(t *testing.T) {
	srv := New()
	// First call: define a function.
	_, resp1, err := srv.service.evalTool(context.Background(), nil, EvalInput{
		Expression: "(defun my-secret-fn () 42)\n(my-secret-fn)",
	})
	require.NoError(t, err)
	assert.Equal(t, "42", resp1.Value)

	// Second call: function should NOT exist — each call gets a fresh env.
	_, resp2, err := srv.service.evalTool(context.Background(), nil, EvalInput{
		Expression: "(my-secret-fn)",
	})
	require.NoError(t, err)
	assert.NotEmpty(t, resp2.Error, "function from previous eval call should not persist")
}

func TestLintTool_MixedValidInvalidChecks(t *testing.T) {
	srv := New()
	content := "(set 'x 1)\n(set 'x 2)\n(if true 1)"
	_, resp, err := srv.service.lintTool(context.Background(), nil, LintInput{
		Content: &content,
		Checks:  []string{"set-usage", "nonexistent-analyzer", "if-arity"},
	})
	require.NoError(t, err)
	// Only valid checks should produce results; invalid ones silently skipped.
	for _, d := range resp.Diagnostics {
		assert.Contains(t, []string{"set-usage", "if-arity"}, d.Code,
			"diagnostic code %q should be from a valid requested analyzer", d.Code)
	}
}

func TestMeta_FileCountAccuracy(t *testing.T) {
	tmp := t.TempDir()
	writeTestFile(t, filepath.Join(tmp, "a.lisp"), "(defun a () 1)")
	writeTestFile(t, filepath.Join(tmp, "b.lisp"), "(defun b () 2)")
	writeTestFile(t, filepath.Join(tmp, "c.lisp"), "(defun c () 3)")

	srv := New(WithWorkspaceRoot(tmp))
	srv.service.workspaceValidationInterval = 0
	_, resp, err := srv.service.diagnosticsTool(context.Background(), nil, DiagnosticsInput{
		WorkspaceRoot:    &tmp,
		IncludeWorkspace: true,
	})
	require.NoError(t, err)
	require.NotNil(t, resp.Meta)
	assert.Equal(t, 3, resp.Meta.FileCount, "meta file_count should match actual files scanned")
	assert.Greater(t, resp.Meta.ElapsedMs, int64(-1), "elapsed_ms should be non-negative")
}

func TestWorkspaceScanner_DotPrefixedRoot(t *testing.T) {
	// Create a workspace root with a dot prefix (e.g., .tmp).
	parent := t.TempDir()
	dotRoot := filepath.Join(parent, ".hidden-workspace")
	require.NoError(t, os.Mkdir(dotRoot, 0o750))
	writeTestFile(t, filepath.Join(dotRoot, "lib.lisp"), "(defun found-me () 1)")

	srv := New(WithWorkspaceRoot(dotRoot))
	srv.service.workspaceValidationInterval = 0
	_, resp, err := srv.service.workspaceSymbolsTool(context.Background(), nil, WorkspaceSymbolsInput{
		WorkspaceRoot: &dotRoot,
		Query:         "found-me",
	})
	require.NoError(t, err)
	require.Len(t, resp.Symbols, 1, "dot-prefixed workspace root should still be scanned")
	assert.Equal(t, "found-me", resp.Symbols[0].Name)
}

func TestDocumentSymbols_LimitOffset(t *testing.T) {
	tmp := t.TempDir()
	path := filepath.Join(tmp, "many.lisp")
	writeTestFile(t, path, "(defun a () 1)\n(defun b () 2)\n(defun c () 3)\n(defun d () 4)")

	srv := New(WithWorkspaceRoot(tmp))
	_, resp, err := srv.service.documentSymbolsTool(context.Background(), nil, DocumentQueryInput{
		Path:  path,
		Limit: 2,
	})
	require.NoError(t, err)
	assert.Len(t, resp.Symbols, 2)
	assert.True(t, resp.Truncated)
	assert.Equal(t, 4, resp.Total)

	// Second page
	_, resp2, err := srv.service.documentSymbolsTool(context.Background(), nil, DocumentQueryInput{
		Path:   path,
		Limit:  2,
		Offset: 2,
	})
	require.NoError(t, err)
	assert.Len(t, resp2.Symbols, 2)

	// No overlap
	for _, s1 := range resp.Symbols {
		for _, s2 := range resp2.Symbols {
			assert.NotEqual(t, s1.Name, s2.Name, "pages should not overlap")
		}
	}
}

func TestTestTool_AutoImportsTestingPackage(t *testing.T) {
	srv := New()
	// Content WITHOUT (use-package 'testing) — test macros should still work.
	content := "(test \"auto-import\" (assert-equal 4 (+ 2 2)))"
	_, resp, err := srv.service.testTool(context.Background(), nil, TestInput{
		Content: &content,
	})
	require.NoError(t, err)
	require.Equal(t, 1, resp.Total, "test should be found without explicit use-package")
	assert.Equal(t, 1, resp.Passed)
	assert.True(t, resp.Tests[0].Passed)
}

func TestTestTool_AutoImportsTestingInCustomPackage(t *testing.T) {
	srv := New()
	// File switches to a custom package — testing macros should still work.
	content := "(in-package 'my-test-pkg)\n(test \"custom-pkg\" (assert-equal 6 (* 2 3)))"
	_, resp, err := srv.service.testTool(context.Background(), nil, TestInput{
		Content: &content,
	})
	require.NoError(t, err)
	require.Equal(t, 1, resp.Total, "test should work in custom package without explicit use-package")
	assert.Equal(t, 1, resp.Passed)
	assert.True(t, resp.Tests[0].Passed, "test in custom package should pass")
}

func TestDiagnostics_SeverityFilterExcludesEmptyFiles(t *testing.T) {
	tmp := t.TempDir()
	writeTestFile(t, filepath.Join(tmp, "broken.lisp"), "(defun broken (")
	writeTestFile(t, filepath.Join(tmp, "ok.lisp"), "(defun ok () 1)")

	srv := New(WithWorkspaceRoot(tmp))
	srv.service.workspaceValidationInterval = 0
	severity := "error"
	_, resp, err := srv.service.diagnosticsTool(context.Background(), nil, DiagnosticsInput{
		WorkspaceRoot:    &tmp,
		IncludeWorkspace: true,
		Severity:         &severity,
	})
	require.NoError(t, err)
	for _, fd := range resp.Files {
		assert.NotEmpty(t, fd.Diagnostics, "files with no matching diagnostics should be excluded when severity filter is set, but %s was included", fd.Path)
	}
}

func TestCallGraph_TopLimitsOutput(t *testing.T) {
	tmp := t.TempDir()
	path := filepath.Join(tmp, "graph.lisp")
	writeTestFile(t, path, `
(defun a () (b) (c) (d))
(defun b () (c))
(defun c () (d))
(defun d () 1)
`)

	srv := New(WithWorkspaceRoot(tmp))
	_, resp, err := srv.service.callGraphTool(context.Background(), nil, PerfSelectionInput{
		Paths: []string{path},
		Top:   2,
	})
	require.NoError(t, err)
	assert.LessOrEqual(t, len(resp.Functions), 2)
	assert.True(t, resp.Truncated)
	assert.GreaterOrEqual(t, resp.TotalFunctions, 3)
}

func TestPerfIssues_TopLimitsIssues(t *testing.T) {
	tmp := t.TempDir()
	path := filepath.Join(tmp, "perf.lisp")
	writeTestFile(t, path, `
(defun a (items) (map 'list (lambda (item) (db-put item)) items))
(defun b (items) (map 'list (lambda (item) (db-get item)) items))
`)

	srv := New(WithWorkspaceRoot(tmp))
	_, resp, err := srv.service.perfIssuesTool(context.Background(), nil, PerfSelectionInput{
		Paths: []string{path},
		Top:   1,
	})
	require.NoError(t, err)
	if len(resp.Issues) > 0 {
		assert.LessOrEqual(t, len(resp.Issues), 1)
	}
}

func TestPerfIssues_SolvedFilteredByRules(t *testing.T) {
	tmp := t.TempDir()
	path := filepath.Join(tmp, "perf.lisp")
	writeTestFile(t, path, `
(defun a (items) (map 'list (lambda (item) (db-put item)) items))
(defun b () 1)
`)

	srv := New(WithWorkspaceRoot(tmp))
	_, fullResp, err := srv.service.perfIssuesTool(context.Background(), nil, PerfSelectionInput{
		Paths: []string{path},
		Top:   10,
	})
	require.NoError(t, err)

	if len(fullResp.Issues) > 0 {
		rule := fullResp.Issues[0].Rule
		_, filtered, err := srv.service.perfIssuesTool(context.Background(), nil, PerfSelectionInput{
			Paths: []string{path},
			Top:   10,
			Rules: []string{rule},
		})
		require.NoError(t, err)
		for _, s := range filtered.Solved {
			found := false
			for _, issue := range filtered.Issues {
				if issue.Function == s.Name {
					found = true
					break
				}
			}
			assert.True(t, found, "solved function %q should have a matching issue for rule %s", s.Name, rule)
		}
	}
}

func TestPerfFiles_IncludeTestsWithExplicitPaths(t *testing.T) {
	tmp := t.TempDir()
	prodPath := filepath.Join(tmp, "prod.lisp")
	testPath := filepath.Join(tmp, "prod_test.lisp")
	writeTestFile(t, prodPath, "(defun prod () 1)")
	writeTestFile(t, testPath, "(defun prod-test () 1)")

	srv := New(WithWorkspaceRoot(tmp))
	files, err := srv.service.selectPerfFiles(PerfSelectionInput{
		Paths:        []string{prodPath, testPath},
		IncludeTests: false,
	})
	require.NoError(t, err)
	assert.Contains(t, files, prodPath)
	assert.NotContains(t, files, testPath, "test file should be excluded even with explicit paths when include_tests=false")
}

func TestDocumentSymbols_MacroInContentOverride(t *testing.T) {
	tmp := t.TempDir()
	path := filepath.Join(tmp, "macros.lisp")
	content := "(defmacro my-when (cond &rest body) (list 'if cond (cons 'progn body)))\n(defun my-func () 1)"

	srv := New(WithWorkspaceRoot(tmp))
	_, resp, err := srv.service.documentSymbolsTool(context.Background(), nil, DocumentQueryInput{
		Path:    path,
		Content: &content,
	})
	require.NoError(t, err)
	var names []string
	for _, sym := range resp.Symbols {
		names = append(names, sym.Name)
	}
	assert.Contains(t, names, "my-func")
	assert.Contains(t, names, "my-when", "defmacro should appear in document_symbols with content override")
}

func TestHelpTool(t *testing.T) {
	srv := New()
	_, resp, err := srv.service.helpTool(context.Background(), nil, HelpInput{})
	require.NoError(t, err)
	assert.Contains(t, resp.Content, "Coordinate System")
	assert.Contains(t, resp.Content, "Path Resolution")
	assert.Contains(t, resp.Content, "Content Override")
	assert.Contains(t, resp.Content, "PERF001")
	assert.Contains(t, resp.Content, "set-usage")
}

func TestWorkspaceSymbols_Pagination(t *testing.T) {
	tmp := t.TempDir()
	writeTestFile(t, filepath.Join(tmp, "many.lisp"), "(defun alpha () 1)\n(defun beta () 2)\n(defun gamma () 3)\n(defun delta () 4)")

	srv := New(WithWorkspaceRoot(tmp))
	srv.service.workspaceValidationInterval = 0

	// First page
	_, page1, err := srv.service.workspaceSymbolsTool(context.Background(), nil, WorkspaceSymbolsInput{
		WorkspaceRoot: &tmp,
		Limit:         2,
		Offset:        0,
	})
	require.NoError(t, err)
	assert.Len(t, page1.Symbols, 2)
	assert.True(t, page1.Truncated)
	assert.Equal(t, 4, page1.Total)

	// Second page
	_, page2, err := srv.service.workspaceSymbolsTool(context.Background(), nil, WorkspaceSymbolsInput{
		WorkspaceRoot: &tmp,
		Limit:         2,
		Offset:        2,
	})
	require.NoError(t, err)
	assert.Len(t, page2.Symbols, 2)
	assert.True(t, page2.Truncated)

	// No overlap
	for _, s1 := range page1.Symbols {
		for _, s2 := range page2.Symbols {
			assert.NotEqual(t, s1.Name, s2.Name, "pages should not overlap")
		}
	}

	// Past end
	_, page3, err := srv.service.workspaceSymbolsTool(context.Background(), nil, WorkspaceSymbolsInput{
		WorkspaceRoot: &tmp,
		Limit:         2,
		Offset:        10,
	})
	require.NoError(t, err)
	assert.Empty(t, page3.Symbols)
	assert.True(t, page3.Truncated)
}

func TestDiagnostics_Pagination(t *testing.T) {
	tmp := t.TempDir()
	for _, name := range []string{"a.lisp", "b.lisp", "c.lisp"} {
		writeTestFile(t, filepath.Join(tmp, name), "(defun broken (")
	}

	srv := New(WithWorkspaceRoot(tmp))
	srv.service.workspaceValidationInterval = 0

	_, page1, err := srv.service.diagnosticsTool(context.Background(), nil, DiagnosticsInput{
		WorkspaceRoot:    &tmp,
		IncludeWorkspace: true,
		MaxFiles:         2,
		Offset:           0,
	})
	require.NoError(t, err)
	assert.Len(t, page1.Files, 2)
	assert.True(t, page1.Truncated)
	assert.Equal(t, 3, page1.TotalFiles)

	_, page2, err := srv.service.diagnosticsTool(context.Background(), nil, DiagnosticsInput{
		WorkspaceRoot:    &tmp,
		IncludeWorkspace: true,
		MaxFiles:         2,
		Offset:           2,
	})
	require.NoError(t, err)
	assert.Len(t, page2.Files, 1)
	assert.True(t, page2.Truncated)
}

func TestFormatTool_InlineContent(t *testing.T) {
	srv := New()
	content := "(defun foo (x y)\n(+ x y))"
	_, resp, err := srv.service.formatTool(context.Background(), nil, FormatInput{
		Content: &content,
	})
	require.NoError(t, err)
	assert.True(t, resp.Changed, "formatter should indent the body")
	assert.Contains(t, resp.Formatted, "  (+ x y)")
	assert.NotNil(t, resp.Meta)
	assert.GreaterOrEqual(t, resp.Meta.ElapsedMs, int64(0))
}

func TestFormatTool_NoChange(t *testing.T) {
	srv := New()
	content := "(defun foo (x y)\n  (+ x y))\n"
	_, resp, err := srv.service.formatTool(context.Background(), nil, FormatInput{
		Content: &content,
	})
	require.NoError(t, err)
	assert.False(t, resp.Changed)
	assert.Empty(t, resp.Formatted, "formatted content should be empty when unchanged")
}

func TestFormatTool_CheckOnly(t *testing.T) {
	srv := New()
	// Unformatted content — needs formatting.
	content := "(defun foo (x y)\n(+ x y))"
	_, resp, err := srv.service.formatTool(context.Background(), nil, FormatInput{
		Content:   &content,
		CheckOnly: true,
	})
	require.NoError(t, err)
	assert.True(t, resp.Changed)
	assert.Empty(t, resp.Formatted, "check_only should not return formatted content")
}

func TestFormatTool_CheckOnlyNoChange(t *testing.T) {
	srv := New()
	content := "(defun foo (x y)\n  (+ x y))\n"
	_, resp, err := srv.service.formatTool(context.Background(), nil, FormatInput{
		Content:   &content,
		CheckOnly: true,
	})
	require.NoError(t, err)
	assert.False(t, resp.Changed)
	assert.Empty(t, resp.Formatted)
}

func TestFormatTool_FileNotFound(t *testing.T) {
	tmp := t.TempDir()
	srv := New(WithWorkspaceRoot(tmp))
	_, _, err := srv.service.formatTool(context.Background(), nil, FormatInput{
		Path: filepath.Join(tmp, "nonexistent.lisp"),
	})
	require.Error(t, err)
	var te *toolErr
	require.ErrorAs(t, err, &te)
	assert.Equal(t, "file_not_found", te.Code)
}

func TestFormatTool_ParseError(t *testing.T) {
	srv := New()
	content := "(defun broken ("
	_, _, err := srv.service.formatTool(context.Background(), nil, FormatInput{
		Content: &content,
	})
	require.Error(t, err)
	var te *toolErr
	require.ErrorAs(t, err, &te)
	assert.Equal(t, "parse_error", te.Code)
}

func TestFormatTool_IndentSize(t *testing.T) {
	srv := New()
	content := "(defun foo (x)\n  (+ x 1))"
	_, resp, err := srv.service.formatTool(context.Background(), nil, FormatInput{
		Content:    &content,
		IndentSize: 4,
	})
	require.NoError(t, err)
	assert.Contains(t, resp.Formatted, "    (+ x 1)")
}

func TestLintTool_InlineContent(t *testing.T) {
	srv := New()
	content := "(set 'x 1)\n(set 'x 2)"
	_, resp, err := srv.service.lintTool(context.Background(), nil, LintInput{
		Content: &content,
	})
	require.NoError(t, err)
	assert.NotEmpty(t, resp.Diagnostics)
	assert.NotNil(t, resp.Meta)
}

func TestLintTool_ChecksFilter(t *testing.T) {
	srv := New()
	content := "(set 'x 1)\n(set 'x 2)\n(if true 1)"
	_, allResp, err := srv.service.lintTool(context.Background(), nil, LintInput{
		Content: &content,
	})
	require.NoError(t, err)

	_, filteredResp, err := srv.service.lintTool(context.Background(), nil, LintInput{
		Content: &content,
		Checks:  []string{"set-usage"},
	})
	require.NoError(t, err)
	assert.LessOrEqual(t, len(filteredResp.Diagnostics), len(allResp.Diagnostics))
	for _, d := range filteredResp.Diagnostics {
		assert.Equal(t, "set-usage", d.Code)
	}
}

func TestLintTool_SeverityFilter(t *testing.T) {
	srv := New()
	content := "(set 'x 1)\n(set 'x 2)"
	severity := "error"
	_, resp, err := srv.service.lintTool(context.Background(), nil, LintInput{
		Content:  &content,
		Severity: &severity,
	})
	require.NoError(t, err)
	for _, d := range resp.Diagnostics {
		assert.Equal(t, "error", d.Severity)
	}
}

func TestLintTool_Pagination(t *testing.T) {
	srv := New()
	// Multiple set-usage warnings
	content := "(set 'x 1)\n(set 'x 2)\n(set 'x 3)\n(set 'x 4)"
	_, resp, err := srv.service.lintTool(context.Background(), nil, LintInput{
		Content: &content,
		Limit:   1,
	})
	require.NoError(t, err)
	assert.LessOrEqual(t, len(resp.Diagnostics), 1)
	if len(resp.Diagnostics) > 0 {
		assert.True(t, resp.Truncated)
	}
}

func TestLintTool_FileNotFound(t *testing.T) {
	tmp := t.TempDir()
	srv := New(WithWorkspaceRoot(tmp))
	_, _, err := srv.service.lintTool(context.Background(), nil, LintInput{
		Path: filepath.Join(tmp, "nonexistent.lisp"),
	})
	require.Error(t, err)
	var te *toolErr
	require.ErrorAs(t, err, &te)
	assert.Equal(t, "file_not_found", te.Code)
}

func TestStructuredErrors_InvalidInput(t *testing.T) {
	srv := New()
	_, _, err := srv.service.formatTool(context.Background(), nil, FormatInput{})
	require.Error(t, err)
	var te *toolErr
	require.ErrorAs(t, err, &te)
	assert.Equal(t, "invalid_input", te.Code)
	assert.Contains(t, te.Error(), "invalid_input")
}

func TestMeta_WorkspaceSymbolsIncludesMeta(t *testing.T) {
	tmp := t.TempDir()
	writeTestFile(t, filepath.Join(tmp, "lib.lisp"), "(defun alpha () 1)")

	srv := New(WithWorkspaceRoot(tmp))
	srv.service.workspaceValidationInterval = 0
	_, resp, err := srv.service.workspaceSymbolsTool(context.Background(), nil, WorkspaceSymbolsInput{
		WorkspaceRoot: &tmp,
		Query:         "",
	})
	require.NoError(t, err)
	require.NotNil(t, resp.Meta)
	assert.Equal(t, tmp, resp.Meta.WorkspaceRoot)
	assert.GreaterOrEqual(t, resp.Meta.ElapsedMs, int64(0))
}

func TestMeta_DiagnosticsIncludesMeta(t *testing.T) {
	tmp := t.TempDir()
	path := filepath.Join(tmp, "ok.lisp")
	writeTestFile(t, path, "(defun ok () 1)")

	srv := New(WithWorkspaceRoot(tmp))
	_, resp, err := srv.service.diagnosticsTool(context.Background(), nil, DiagnosticsInput{
		Path: &path,
	})
	require.NoError(t, err)
	require.NotNil(t, resp.Meta)
	assert.Equal(t, 1, resp.Meta.FileCount)
}

func TestWorkspaceRootOverride(t *testing.T) {
	// Create two separate workspaces with different symbols.
	ws1 := t.TempDir()
	ws2 := t.TempDir()
	writeTestFile(t, filepath.Join(ws1, "lib1.lisp"), "(defun alpha () 1)")
	writeTestFile(t, filepath.Join(ws2, "lib2.lisp"), "(defun beta () 2)")

	// Server starts with ws1 as default, but we query ws2 via override.
	srv := New(WithWorkspaceRoot(ws1))
	srv.service.workspaceValidationInterval = 0
	session, serverSession := connectTestServer(t, srv)
	defer closeClientSession(t, session)
	defer closeServerSession(t, serverSession)

	// Query ws1 — should find alpha.
	ws1Symbols := workspaceSymbols(t, session, ws1, "alpha")
	require.Len(t, ws1Symbols.Symbols, 1)
	assert.Equal(t, "alpha", ws1Symbols.Symbols[0].Name)

	// Override workspace_root to ws2 — should find beta, not alpha.
	ws2Symbols := workspaceSymbols(t, session, ws2, "beta")
	require.Len(t, ws2Symbols.Symbols, 1)
	assert.Equal(t, "beta", ws2Symbols.Symbols[0].Name)

	// ws2 should NOT contain alpha.
	ws2Alpha := workspaceSymbols(t, session, ws2, "alpha")
	assert.Empty(t, ws2Alpha.Symbols)
}

func TestValidateCursor_StructuredError(t *testing.T) {
	err := validateCursor(-1, 0)
	require.Error(t, err)
	var te *toolErr
	require.ErrorAs(t, err, &te)
	assert.Equal(t, "invalid_position", te.Code)

	err = validateCursor(0, -1)
	require.Error(t, err)
	require.ErrorAs(t, err, &te)
	assert.Equal(t, "invalid_position", te.Code)
}

func TestLintTool_ReportsParseErrors(t *testing.T) {
	srv := New()
	content := "(defun broken ("
	_, resp, err := srv.service.lintTool(context.Background(), nil, LintInput{
		Content: &content,
	})
	require.NoError(t, err)
	require.NotEmpty(t, resp.Diagnostics, "lint tool must report parse errors for broken files")
	found := false
	for _, d := range resp.Diagnostics {
		if d.Severity == "error" {
			found = true
			break
		}
	}
	require.True(t, found, "at least one error-severity diagnostic must be present for broken code")
}

func TestPerfIssues_SolvedAlwaysArray(t *testing.T) {
	tmp := t.TempDir()
	path := filepath.Join(tmp, "clean.lisp")
	writeTestFile(t, path, "(defun clean () 1)")

	srv := New(WithWorkspaceRoot(tmp))
	// Use rules filter that matches nothing — solved should be [] not null.
	_, resp, err := srv.service.perfIssuesTool(context.Background(), nil, PerfSelectionInput{
		Paths: []string{path},
		Rules: []string{"PERF001"},
		Top:   5,
	})
	require.NoError(t, err)
	require.NotNil(t, resp.Solved, "solved must be initialized, not nil")
	assert.Empty(t, resp.Solved, "solved should be empty when rules filter matches nothing")

	// Without top, solved should still be [] not null.
	_, resp2, err := srv.service.perfIssuesTool(context.Background(), nil, PerfSelectionInput{
		Paths: []string{path},
	})
	require.NoError(t, err)
	require.NotNil(t, resp2.Solved, "solved must be initialized even without top")
	assert.Empty(t, resp2.Solved)
}

func TestPerfTools_SkipUnparseableFiles(t *testing.T) {
	tmp := t.TempDir()
	writeTestFile(t, filepath.Join(tmp, "good.lisp"), "(defun good (items) (map 'list (lambda (item) (db-put item)) items))")
	writeTestFile(t, filepath.Join(tmp, "broken.lisp"), "(defun broken (")

	srv := New(WithWorkspaceRoot(tmp))
	_, resp, err := srv.service.perfIssuesTool(context.Background(), nil, PerfSelectionInput{
		WorkspaceRoot: &tmp,
		Top:           5,
	})
	require.NoError(t, err, "perf tools should skip unparseable files, not error")
	require.NotNil(t, resp.Issues)
	require.NotNil(t, resp.Solved)
	assert.True(t, len(resp.Solved) > 0 || len(resp.Issues) > 0, "good.lisp should still produce analysis results")
}

func TestResolvePath_NoDoublePrefixing(t *testing.T) {
	tmp := t.TempDir()
	srv := New(WithWorkspaceRoot(tmp))
	// Create a file inside the workspace
	testFile := filepath.Join(tmp, "utils.lisp")
	writeTestFile(t, testFile, "(defun util () 1)")

	// When CWD is inside the workspace root, a relative path should not get double-prefixed.
	resolved, err := srv.service.resolvePath("utils.lisp", tmp)
	require.NoError(t, err)
	assert.Equal(t, filepath.Join(tmp, "utils.lisp"), resolved)
}

func TestResolvePath_AbsoluteInput(t *testing.T) {
	tmp := t.TempDir()
	srv := New(WithWorkspaceRoot(tmp))

	absPath := filepath.Join(tmp, "test.lisp")
	resolved, err := srv.service.resolvePath(absPath, tmp)
	require.NoError(t, err)
	assert.Equal(t, absPath, resolved)
}

func TestResolvePath_TraversalBlocked(t *testing.T) {
	tmp := t.TempDir()
	srv := New(WithWorkspaceRoot(tmp))

	_, err := srv.service.resolvePath("../../etc/passwd", tmp)
	require.Error(t, err)
	assert.Contains(t, err.Error(), "resolves outside workspace root")
}

func TestWordAtPosition(t *testing.T) {
	content := "(defun add (x y)\n  (+ x y))"
	tests := []struct {
		name      string
		line, col int
		want      string
	}{
		{"function name", 0, 7, "add"},
		{"param x", 0, 12, "x"},
		{"builtin +", 1, 3, "+"},
		{"past end of line", 0, 100, ""},
		{"negative line", -1, 0, ""},
		{"line past end", 99, 0, ""},
		{"on paren", 0, 0, ""},
		{"empty content", 0, 0, ""},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			src := content
			if tt.name == "empty content" {
				src = ""
			}
			got := wordAtPosition(src, tt.line, tt.col)
			assert.Equal(t, tt.want, got)
		})
	}
}

func TestPaginateSlice(t *testing.T) {
	items := []int{1, 2, 3, 4, 5}
	tests := []struct {
		name           string
		offset, limit  int
		wantLen        int
	}{
		{"no pagination", 0, 0, 5},
		{"limit only", 0, 3, 3},
		{"offset only", 2, 0, 3},
		{"offset+limit", 1, 2, 2},
		{"offset at end", 5, 2, 0},
		{"offset past end", 10, 2, 0},
		{"limit larger than items", 0, 100, 5},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := paginateSlice(items, tt.offset, tt.limit)
			assert.Len(t, got, tt.wantLen)
		})
	}
}

func TestHover_UndefinedSymbol(t *testing.T) {
	tmp := t.TempDir()
	path := filepath.Join(tmp, "test.lisp")
	writeTestFile(t, path, "(defun foo () (undefined-symbol-xyz 1 2))")

	srv := New(WithWorkspaceRoot(tmp))
	_, resp, err := srv.service.hoverTool(context.Background(), nil, FileQueryInput{
		Path:      path,
		Line:      0,
		Character: 15,
	})
	require.NoError(t, err)
	assert.False(t, resp.Found, "hover on undefined symbol should return found=false")
}

func TestHotspots_TopZeroErrors(t *testing.T) {
	tmp := t.TempDir()
	path := filepath.Join(tmp, "test.lisp")
	writeTestFile(t, path, "(defun foo () 1)")

	srv := New(WithWorkspaceRoot(tmp))
	_, _, err := srv.service.hotspotsTool(context.Background(), nil, PerfSelectionInput{
		Paths: []string{path},
		Top:   0,
	})
	require.Error(t, err, "hotspots with top=0 should error")
	assert.Contains(t, err.Error(), "top must be greater than zero")
}

func TestLintTool_InvalidCheckName(t *testing.T) {
	srv := New()
	content := "(defun foo () 1)"
	_, resp, err := srv.service.lintTool(context.Background(), nil, LintInput{
		Content: &content,
		Checks:  []string{"nonexistent-analyzer"},
	})
	require.NoError(t, err, "invalid check names should not error, just produce no results")
	assert.Empty(t, resp.Diagnostics)
}

func TestDiagnostics_SeverityFilterAllItems(t *testing.T) {
	tmp := t.TempDir()
	path := filepath.Join(tmp, "mixed.lisp")
	writeTestFile(t, path, "(set 'x 1)\n(set 'x 2)")

	srv := New(WithWorkspaceRoot(tmp))
	severity := "warning"
	_, resp, err := srv.service.diagnosticsTool(context.Background(), nil, DiagnosticsInput{
		Path:     &path,
		Severity: &severity,
	})
	require.NoError(t, err)
	require.Len(t, resp.Files, 1)
	for i, d := range resp.Files[0].Diagnostics {
		assert.Equal(t, "warning", d.Severity, "diagnostic %d should be warning severity", i)
	}
}

func TestDocTool_SymbolLookup(t *testing.T) {
	srv := New()
	_, resp, err := srv.service.docTool(context.Background(), nil, DocInput{Query: "map"})
	require.NoError(t, err)
	require.True(t, resp.Found)
	require.NotNil(t, resp.Symbol)
	assert.Equal(t, "map", resp.Symbol.Name)
	assert.NotEmpty(t, resp.Symbol.Doc)
	assert.NotNil(t, resp.Symbol.Formals)
}

func TestDocTool_QualifiedSymbol(t *testing.T) {
	srv := New()
	_, resp, err := srv.service.docTool(context.Background(), nil, DocInput{Query: "math:sin"})
	require.NoError(t, err)
	require.True(t, resp.Found)
	require.NotNil(t, resp.Symbol)
	assert.Equal(t, "sin", resp.Symbol.Name)
}

func TestDocTool_PackageLookup(t *testing.T) {
	srv := New()
	_, resp, err := srv.service.docTool(context.Background(), nil, DocInput{Query: "math", Package: true})
	require.NoError(t, err)
	require.True(t, resp.Found)
	require.NotNil(t, resp.Package)
	assert.Equal(t, "math", resp.Package.Name)
	assert.NotEmpty(t, resp.Package.Symbols)
}

func TestDocTool_NotFound(t *testing.T) {
	srv := New()
	_, resp, err := srv.service.docTool(context.Background(), nil, DocInput{Query: "nonexistent-xyz-123"})
	require.NoError(t, err)
	assert.False(t, resp.Found)
}

func TestTestTool_PassingTests(t *testing.T) {
	tmp := t.TempDir()
	path := filepath.Join(tmp, "pass_test.lisp")
	writeTestFile(t, path, "(use-package 'testing)\n(test \"math works\" (assert-equal 4 (+ 2 2)))")

	srv := New()
	_, resp, err := srv.service.testTool(context.Background(), nil, TestInput{Path: path})
	require.NoError(t, err)
	assert.Equal(t, path, resp.Path)
	require.Equal(t, 1, resp.Total)
	assert.Equal(t, 1, resp.Passed)
	assert.Equal(t, 0, resp.Failed)
	assert.True(t, resp.Tests[0].Passed)
	assert.Equal(t, "math works", resp.Tests[0].Name)
}

func TestTestTool_FailingTest(t *testing.T) {
	tmp := t.TempDir()
	path := filepath.Join(tmp, "fail_test.lisp")
	writeTestFile(t, path, "(use-package 'testing)\n(test \"bad math\" (assert-equal 5 (+ 2 2)))")

	srv := New()
	_, resp, err := srv.service.testTool(context.Background(), nil, TestInput{Path: path})
	require.NoError(t, err)
	assert.Equal(t, 1, resp.Total)
	assert.Equal(t, 0, resp.Passed)
	assert.Equal(t, 1, resp.Failed)
	assert.False(t, resp.Tests[0].Passed)
	assert.NotEmpty(t, resp.Tests[0].Error)
}

func TestTestTool_ParseError(t *testing.T) {
	tmp := t.TempDir()
	path := filepath.Join(tmp, "broken_test.lisp")
	writeTestFile(t, path, "(defun broken (")

	srv := New()
	_, resp, err := srv.service.testTool(context.Background(), nil, TestInput{Path: path})
	require.NoError(t, err)
	require.Equal(t, 1, resp.Failed)
	require.Len(t, resp.Tests, 1)
	assert.Equal(t, "<parse>", resp.Tests[0].Name)
	assert.False(t, resp.Tests[0].Passed)
	assert.NotEmpty(t, resp.Tests[0].Error)
}

func TestTestTool_NoTests(t *testing.T) {
	tmp := t.TempDir()
	path := filepath.Join(tmp, "empty_test.lisp")
	writeTestFile(t, path, "(defun helper () 1)")

	srv := New()
	_, resp, err := srv.service.testTool(context.Background(), nil, TestInput{Path: path})
	require.NoError(t, err)
	assert.Equal(t, 0, resp.Total)
	assert.Empty(t, resp.Tests)
}

func TestEvalTool_SimpleExpression(t *testing.T) {
	srv := New()
	_, resp, err := srv.service.evalTool(context.Background(), nil, EvalInput{Expression: "(+ 2 3)"})
	require.NoError(t, err)
	assert.Equal(t, "5", resp.Value)
	assert.Empty(t, resp.Error)
}

func TestEvalTool_MultipleExpressions(t *testing.T) {
	srv := New()
	_, resp, err := srv.service.evalTool(context.Background(), nil, EvalInput{
		Expression: "(+ 1 1)\n(* 3 4)",
	})
	require.NoError(t, err)
	assert.Equal(t, "12", resp.Value)
	require.Len(t, resp.Results, 2)
	assert.Equal(t, "2", resp.Results[0])
	assert.Equal(t, "12", resp.Results[1])
}

func TestEvalTool_Error(t *testing.T) {
	srv := New()
	_, resp, err := srv.service.evalTool(context.Background(), nil, EvalInput{Expression: "(error \"boom\")"})
	require.NoError(t, err)
	assert.NotEmpty(t, resp.Error)
	assert.Empty(t, resp.Value, "value should be empty when error occurs")
}

func TestEvalTool_MultiExprPartialFailure(t *testing.T) {
	srv := New()
	_, resp, err := srv.service.evalTool(context.Background(), nil, EvalInput{
		Expression: "(+ 1 1)\n(error \"boom\")\n(+ 3 3)",
	})
	require.NoError(t, err)
	assert.NotEmpty(t, resp.Error, "should stop at error")
	assert.Empty(t, resp.Value, "value should be empty on error")
}

func TestEvalTool_DefunAndCall(t *testing.T) {
	srv := New()
	_, resp, err := srv.service.evalTool(context.Background(), nil, EvalInput{
		Expression: "(defun double (x) (* x 2))\n(double 21)",
	})
	require.NoError(t, err)
	assert.Equal(t, "42", resp.Value)
	assert.Empty(t, resp.Error)
}

func TestEvalTool_ParseError(t *testing.T) {
	srv := New()
	_, resp, err := srv.service.evalTool(context.Background(), nil, EvalInput{Expression: "(defun broken ("})
	require.NoError(t, err)
	assert.NotEmpty(t, resp.Error)
	assert.Empty(t, resp.Value, "value should be empty on parse error")
}

func TestEvalTool_Batch(t *testing.T) {
	srv := New()
	_, resp, err := srv.service.evalTool(context.Background(), nil, EvalInput{
		Expressions: []string{
			"(+ 1 2)",
			"(* 3 4)",
			"(error \"boom\")",
			"(- 10 5)",
		},
	})
	require.NoError(t, err)
	require.Len(t, resp.Batch, 4)
	assert.Equal(t, "3", resp.Batch[0].Value)
	assert.Empty(t, resp.Batch[0].Error)
	assert.Equal(t, "12", resp.Batch[1].Value)
	assert.Empty(t, resp.Batch[1].Error)
	assert.NotEmpty(t, resp.Batch[2].Error, "error expression should produce error")
	assert.Empty(t, resp.Batch[2].Value)
	assert.Equal(t, "5", resp.Batch[3].Value, "batch items are independent — error in [2] doesn't affect [3]")
}

func TestDocTool_Batch(t *testing.T) {
	srv := New()
	_, resp, err := srv.service.docTool(context.Background(), nil, DocInput{
		Queries: []string{"map", "defun", "nonexistent-xyz"},
	})
	require.NoError(t, err)
	require.Len(t, resp.Batch, 3)
	assert.True(t, resp.Batch[0].Found)
	assert.Equal(t, "map", resp.Batch[0].Symbol.Name)
	assert.True(t, resp.Batch[1].Found)
	assert.Equal(t, "defun", resp.Batch[1].Symbol.Name)
	assert.False(t, resp.Batch[2].Found)
}

func TestTestTool_ContentOverride(t *testing.T) {
	srv := New()
	content := "(use-package 'testing)\n(test \"inline\" (assert-equal 6 (* 2 3)))"
	_, resp, err := srv.service.testTool(context.Background(), nil, TestInput{
		Content: &content,
	})
	require.NoError(t, err)
	assert.Equal(t, "<stdin>", resp.Path)
	require.Equal(t, 1, resp.Total)
	assert.Equal(t, 1, resp.Passed)
	assert.True(t, resp.Tests[0].Passed)
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
