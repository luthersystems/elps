package mcpserver

import (
	"context"
	"path/filepath"
	"sync"
	"testing"
	"time"

	"github.com/modelcontextprotocol/go-sdk/mcp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// callLintMessages runs the lint tool over one path and returns the diagnostic
// messages.
func callLintMessages(t *testing.T, session *mcp.ClientSession, path string) []string {
	t.Helper()
	res, err := session.CallTool(context.Background(), &mcp.CallToolParams{
		Name:      "lint",
		Arguments: map[string]any{"path": path},
	})
	require.NoError(t, err)
	require.False(t, res.IsError)
	resp := decodeStructured[LintResponse](t, res)
	messages := make([]string, 0, len(resp.Diagnostics))
	for _, diag := range resp.Diagnostics {
		messages = append(messages, diag.Message)
	}
	return messages
}

// callDiagnosticsMessages runs the diagnostics tool over one path and returns
// the diagnostic messages.
func callDiagnosticsMessages(t *testing.T, session *mcp.ClientSession, path string) []string {
	t.Helper()
	res, err := session.CallTool(context.Background(), &mcp.CallToolParams{
		Name:      "diagnostics",
		Arguments: map[string]any{"path": path},
	})
	require.NoError(t, err)
	require.False(t, res.IsError)
	resp := decodeStructured[DiagnosticsResponse](t, res)
	messages := make([]string, 0)
	for _, file := range resp.Files {
		for _, diag := range file.Diagnostics {
			messages = append(messages, diag.Message)
		}
	}
	return messages
}

// TestLintAndDiagnosticsAgree_MacroShadowedCaller is the issue #424 regression
// test. It is a CATCH: it fails on main.
//
// `with-binding` expands to a `let` binding its first argument, so both
// occurrences of `total` in caller.lisp belong to that local binding, not to
// the global function. Analysis WITH the expander sees no cross-file reference
// to the global `total` and reports "unused function: total"; analysis WITHOUT
// it records two phantom references and suppresses the diagnostic.
//
// diagnostics serves from s.workspace(root), whose config carries the
// service-wide expander since #422. lint built a throwaway
// lint.BuildAnalysisConfig with no Env, no Registry and no MacroExpander, so
// the two tools returned contradictory answers for the same file.
func TestLintAndDiagnosticsAgree_MacroShadowedCaller(t *testing.T) {
	tmp := t.TempDir()
	libPath := filepath.Join(tmp, "lib.lisp")
	writeTestFile(t, libPath, "(defun total () 42)\n"+
		"(defmacro with-binding (name value &rest body)\n"+
		"  (quasiquote (let ([(unquote name) (unquote value)]) (unquote-splicing body))))\n")
	writeTestFile(t, filepath.Join(tmp, "caller.lisp"),
		"(defun f () (with-binding total 1 (+ total 1)))\n")

	srv := New(WithWorkspaceRoot(tmp))
	session, serverSession := connectTestServer(t, srv)
	defer closeClientSession(t, session)
	defer closeServerSession(t, serverSession)

	diagMessages := callDiagnosticsMessages(t, session, libPath)
	lintMessages := callLintMessages(t, session, libPath)

	require.Contains(t, diagMessages, "unused function: total",
		"precondition: diagnostics expands macros, so the caller-side `total` is a local binding")
	assert.Contains(t, lintMessages, "unused function: total",
		"lint must analyse with the same macro expander as diagnostics; without it the macro's local binding looks like an external caller")
	assert.Equal(t, diagMessages, lintMessages,
		"lint and diagnostics run the same analyzers over the same file in the same workspace and must not contradict each other")
}

// TestLintAndDiagnosticsAgree_MacroExpandedCall is the additive half of the
// same divergence, and is also a CATCH: it fails on main.
//
// caller.lisp mentions `alpha` only inside quoted data. Expanding `run-all`
// splices that form into code position, making it a real reference. Analysis
// with the expander therefore does NOT report `alpha` unused; analysis without
// it does.
func TestLintAndDiagnosticsAgree_MacroExpandedCall(t *testing.T) {
	tmp := t.TempDir()
	libPath := filepath.Join(tmp, "lib.lisp")
	writeTestFile(t, libPath, "(defun alpha () 1)\n"+
		"(defmacro run-all (specs)\n"+
		"  (quasiquote (progn (unquote-splicing specs))))\n")
	writeTestFile(t, filepath.Join(tmp, "caller.lisp"),
		"(defun f () (run-all '((alpha))))\n")

	srv := New(WithWorkspaceRoot(tmp))
	session, serverSession := connectTestServer(t, srv)
	defer closeClientSession(t, session)
	defer closeServerSession(t, serverSession)

	diagMessages := callDiagnosticsMessages(t, session, libPath)
	lintMessages := callLintMessages(t, session, libPath)

	require.NotContains(t, diagMessages, "unused function: alpha",
		"precondition: diagnostics expands run-all, so caller.lisp really does call alpha")
	assert.NotContains(t, lintMessages, "unused function: alpha",
		"lint must see the macro-expanded call site too")
	assert.Equal(t, diagMessages, lintMessages,
		"lint and diagnostics must not contradict each other")
}

// TestLintRunsSemanticAnalyzersWithoutWorkspaceRoot is also a CATCH: it fails
// on main.
//
// With no workspace root configured and none in the request, lint skipped
// analysis entirely (`if root != "" && ...`), so every semantic analyzer was a
// no-op and the tool answered "no diagnostics" for a file diagnostics flags
// twice over. loadDocument has no such guard — it calls s.workspace("") and
// gets a config with stdlib exports and the expander — which is why the two
// tools diverged hardest in exactly the configuration where a caller has the
// least other context to check against.
func TestLintRunsSemanticAnalyzersWithoutWorkspaceRoot(t *testing.T) {
	srv := New()
	session, serverSession := connectTestServer(t, srv)
	defer closeClientSession(t, session)
	defer closeServerSession(t, serverSession)

	content := "(defun orphan () 1)\n(undefined-thing)\n"
	callWithContent := func(name string) []string {
		res, err := session.CallTool(context.Background(), &mcp.CallToolParams{
			Name:      name,
			Arguments: map[string]any{"content": content},
		})
		require.NoError(t, err)
		require.False(t, res.IsError)
		return sortedMessages(t, name, res)
	}

	diagMessages := callWithContent("diagnostics")
	lintMessages := callWithContent("lint")

	require.Contains(t, diagMessages, "undefined symbol: undefined-thing",
		"precondition: diagnostics analyses even without a workspace root")
	assert.Equal(t, diagMessages, lintMessages,
		"lint must run the same semantic analyzers as diagnostics when there is no workspace root")
}

// sortedMessages extracts diagnostic messages from either tool's response.
func sortedMessages(t *testing.T, tool string, res *mcp.CallToolResult) []string {
	t.Helper()
	messages := make([]string, 0)
	if tool == "lint" {
		for _, diag := range decodeStructured[LintResponse](t, res).Diagnostics {
			messages = append(messages, diag.Message)
		}
		return messages
	}
	for _, file := range decodeStructured[DiagnosticsResponse](t, res).Files {
		for _, diag := range file.Diagnostics {
			messages = append(messages, diag.Message)
		}
	}
	return messages
}

// TestLintToolIsRaceFreeWithWorkspaceBuilds is a GUARD, not a catch: it passes
// on main, where the lint tool analysed with no expander at all and so could
// not touch the shared env.
//
// It exists to fail the tempting version of the #424 fix — adding
// `Env: s.env` to the LintConfig. That routes every lint request through the
// package-level analysis.LoadWorkspaceMacros and a fresh EnvMacroExpander,
// both against the one env the server indexes every root with, which is
// precisely the race #422 closed (issue #403). Serving lint from
// s.workspace(root) instead shares the cached state, the single service-wide
// expander and buildMu, so this stays clean under -race.
//
// That is not a hypothetical: adding `Env: s.env` to the old LintConfig and
// running this test under -race reports analysis.evalPreambleForm called from
// lintTool's analysis.LoadWorkspaceMacros racing the same function called from
// buildWorkspaceState's (*EnvMacroExpander).LoadWorkspaceMacros.
func TestLintToolIsRaceFreeWithWorkspaceBuilds(t *testing.T) {
	first := macroWorkspace(t)
	second := macroWorkspace(t)

	srv := New(WithWorkspaceRoot(first))
	srv.service.workspaceValidationInterval = time.Hour
	_, err := srv.service.workspace(first)
	require.NoError(t, err)

	cached := filepath.Join(first, "acaller.lisp")
	other := filepath.Join(second, "acaller.lisp")

	var wg sync.WaitGroup
	for i := range 12 {
		wg.Add(1)
		go func() {
			defer wg.Done()
			ctx := context.Background()
			switch i % 4 {
			case 0:
				_, _, lintErr := srv.service.lintTool(ctx, nil, LintInput{Path: cached})
				assert.NoError(t, lintErr)
			case 1:
				root := second
				_, _, lintErr := srv.service.lintTool(ctx, nil, LintInput{Path: other, WorkspaceRoot: &root})
				assert.NoError(t, lintErr)
			case 2:
				_, _, docErr := srv.service.loadDocument(cached, nil, &first)
				assert.NoError(t, docErr)
			default:
				_, buildErr := srv.service.buildWorkspaceState(second, "fingerprint", time.Now())
				assert.NoError(t, buildErr)
			}
		}()
	}
	wg.Wait()
}
