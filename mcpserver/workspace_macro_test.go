package mcpserver

import (
	"context"
	"path/filepath"
	"strings"
	"sync"
	"testing"
	"time"

	"github.com/modelcontextprotocol/go-sdk/mcp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// TestWorkspaceIndexExpandsMacros_AddsReference covers the additive half of
// issue #403: a call site whose only reference to a function is produced by
// macro expansion.
//
// caller.lisp mentions `alpha` and `beta` only inside quoted data, which the
// analyzer skips as data when it walks a macro call opaquely. Expanding
// `run-all` splices those caller-side forms into code positions, so they
// become real references carrying caller.lisp's own source locations.
//
// Before the fix, buildWorkspaceState ran ScanWorkspaceRefs with a Config
// whose MacroExpander was still nil (it was assigned afterwards), so the
// workspace index never saw them and `references` reported the definition
// only.
func TestWorkspaceIndexExpandsMacros_AddsReference(t *testing.T) {
	tmp := t.TempDir()
	libPath := filepath.Join(tmp, "lib.lisp")
	callerPath := filepath.Join(tmp, "caller.lisp")
	libContent := "(defun alpha () 1)\n" +
		"(defun beta () 2)\n" +
		"(defmacro run-all (specs)\n" +
		"  (quasiquote (progn (unquote-splicing specs))))\n"
	writeTestFile(t, libPath, libContent)
	writeTestFile(t, callerPath, "(defun f () (run-all '((alpha) (beta))))\n")

	srv := New(WithWorkspaceRoot(tmp))
	session, serverSession := connectTestServer(t, srv)
	defer closeClientSession(t, session)
	defer closeServerSession(t, serverSession)

	res, err := session.CallTool(context.Background(), &mcp.CallToolParams{
		Name: "references",
		Arguments: map[string]any{
			"path":                libPath,
			"line":                0,
			"character":           strings.Index("(defun alpha () 1)", "alpha"),
			"include_declaration": true,
		},
	})
	require.NoError(t, err)
	require.False(t, res.IsError)

	refs := decodeStructured[ReferencesResponse](t, res)
	assert.Equal(t, "alpha", refs.SymbolName)
	paths := make([]string, 0, len(refs.References))
	for _, ref := range refs.References {
		paths = append(paths, ref.Path)
	}
	assert.Contains(t, paths, callerPath,
		"workspace index must be built with the macro expander, so the macro-expanded call in caller.lisp is a reference to alpha")
}

// TestWorkspaceIndexExpandsMacros_DropsShadowedReference covers the
// subtractive half of issue #403: index entries that exist only because the
// index was built without macro expansion.
//
// `with-binding` expands to a `let` that binds its first argument, so both
// occurrences of `total` in caller.lisp belong to that local binding, not to
// the global function of the same name. An index built without the expander
// walks the macro call opaquely, resolves each of them against the global
// scope, and records two cross-file references the language does not have.
//
// Two things go wrong downstream. `references` on the global `total` reports
// hits in caller.lisp that point at a local binding, and the `unused-function`
// lint check treats those phantoms as real external callers and suppresses the
// "unused function" diagnostic for a function nothing calls.
func TestWorkspaceIndexExpandsMacros_DropsShadowedReference(t *testing.T) {
	tmp := t.TempDir()
	libPath := filepath.Join(tmp, "lib.lisp")
	callerPath := filepath.Join(tmp, "caller.lisp")
	libContent := "(defun total () 42)\n" +
		"(defmacro with-binding (name value &rest body)\n" +
		"  (quasiquote (let ([(unquote name) (unquote value)]) (unquote-splicing body))))\n"
	writeTestFile(t, libPath, libContent)
	writeTestFile(t, callerPath, "(defun f () (with-binding total 1 (+ total 1)))\n")

	srv := New(WithWorkspaceRoot(tmp))
	session, serverSession := connectTestServer(t, srv)
	defer closeClientSession(t, session)
	defer closeServerSession(t, serverSession)

	res, err := session.CallTool(context.Background(), &mcp.CallToolParams{
		Name: "references",
		Arguments: map[string]any{
			"path":                libPath,
			"line":                0,
			"character":           strings.Index("(defun total () 42)", "total"),
			"include_declaration": true,
		},
	})
	require.NoError(t, err)
	require.False(t, res.IsError)

	refs := decodeStructured[ReferencesResponse](t, res)
	for _, ref := range refs.References {
		assert.NotEqual(t, callerPath, ref.Path,
			"total in caller.lisp is bound by the macro's let, so it must not be indexed as a reference to the global function")
	}

	diagRes, err := session.CallTool(context.Background(), &mcp.CallToolParams{
		Name:      "diagnostics",
		Arguments: map[string]any{"path": libPath},
	})
	require.NoError(t, err)
	require.False(t, diagRes.IsError)

	diags := decodeStructured[DiagnosticsResponse](t, diagRes)
	var messages []string
	for _, file := range diags.Files {
		for _, diag := range file.Diagnostics {
			messages = append(messages, diag.Message)
		}
	}
	assert.Contains(t, messages, "unused function: total",
		"the only apparent caller of total is a phantom index entry from unexpanded macro analysis")
}

// macroWorkspace writes a workspace whose files all call a macro, so both the
// index build and per-document analysis do real macro expansion against the
// server's env.
func macroWorkspace(t *testing.T) string {
	t.Helper()
	tmp := t.TempDir()
	writeTestFile(t, filepath.Join(tmp, "lib.lisp"),
		"(defun alpha () 1)\n"+
			"(defun beta () 2)\n"+
			"(defmacro run-all (specs)\n"+
			"  (quasiquote (progn (unquote-splicing specs))))\n")
	for i := range 6 {
		name := string(rune('a' + i))
		writeTestFile(t, filepath.Join(tmp, name+"caller.lisp"),
			"(defun f"+name+" () (run-all '((alpha) (beta))))\n")
	}
	return tmp
}

// TestWorkspaceIndexBuildsAreRaceFree is a -race regression test for the
// concurrency the #403 fix widens.
//
// Before the fix the expander ran only on the per-document path. Running it
// from ScanWorkspaceRefs as well puts macro expansion inside the index build,
// where NumCPU workers share it, and the workspace cache lets one root's build
// overlap with another root's document requests. Every one of those paths
// mutates the single lisp env behind the server (Runtime.Package, the call
// stack, the package registry).
//
// Both halves are exercised: duplicate concurrent builds of one root, and a
// build of a second root racing document analysis served from the first root's
// cached state. Without the service-wide expander and the build lock this
// reports data races inside analysis.LoadWorkspaceMacros and
// EnvMacroExpander.expand.
func TestWorkspaceIndexBuildsAreRaceFree(t *testing.T) {
	first := macroWorkspace(t)
	second := macroWorkspace(t)

	srv := New(WithWorkspaceRoot(first))
	srv.service.workspaceValidationInterval = time.Hour
	_, err := srv.service.workspace(first)
	require.NoError(t, err)

	cached := filepath.Join(first, "acaller.lisp")
	var wg sync.WaitGroup
	for i := range 8 {
		wg.Add(1)
		go func() {
			defer wg.Done()
			switch i % 3 {
			case 0:
				_, _, docErr := srv.service.loadDocument(cached, nil, &first)
				assert.NoError(t, docErr)
			case 1:
				_, buildErr := srv.service.buildWorkspaceState(first, "fingerprint", time.Now())
				assert.NoError(t, buildErr)
			default:
				_, buildErr := srv.service.buildWorkspaceState(second, "fingerprint", time.Now())
				assert.NoError(t, buildErr)
			}
		}()
	}
	wg.Wait()
}
