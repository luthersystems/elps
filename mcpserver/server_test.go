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
	assert.Len(t, tools.Tools, 11)

	res, err := session.CallTool(context.Background(), &mcp.CallToolParams{Name: "describe_server"})
	require.NoError(t, err)
	require.False(t, res.IsError)

	got := decodeStructured[DescribeServerResponse](t, res)
	assert.Equal(t, defaultImplementationName, got.Name)
	assert.Equal(t, tmp, got.DefaultWorkspaceRoot)
	assert.Len(t, got.Capabilities, 11)
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
	data, _ := json.Marshal(resp)
	assert.Contains(t, string(data), `"references":[]`)
	assert.NotContains(t, string(data), `"references":null`)
}

func TestNullVsEmptySlice_DocumentSymbols(t *testing.T) {
	tmp := t.TempDir()
	path := filepath.Join(tmp, "empty.lisp")
	writeTestFile(t, path, "; just a comment")

	srv := New(WithWorkspaceRoot(tmp))
	_, resp, err := srv.service.documentSymbolsTool(context.Background(), nil, DocumentQueryInput{Path: path})
	require.NoError(t, err)
	data, _ := json.Marshal(resp)
	assert.Contains(t, string(data), `"symbols":[]`)
	assert.NotContains(t, string(data), `"symbols":null`)
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
	data, _ := json.Marshal(resp)
	assert.Contains(t, string(data), `"symbols":[]`)
	assert.NotContains(t, string(data), `"symbols":null`)
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
