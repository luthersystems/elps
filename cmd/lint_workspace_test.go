// Copyright © 2026 The ELPS authors

package cmd

import (
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// TestLintCommand_WorkspaceExpandsMacros checks that "elps lint
// --workspace" expands a workspace macro: the function the macro defines
// is not an undefined symbol. Before, the command never gave the linter an
// env, so no workspace macro was expanded (the lint twin of elps#952).
func TestLintCommand_WorkspaceExpandsMacros(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test in short mode")
	}
	bin := buildTestBinary(t)
	root := t.TempDir()
	require.NoError(t, os.WriteFile(filepath.Join(root, "macros.lisp"),
		[]byte("(defmacro defhelper (name) (quasiquote (defun (unquote name) () 1)))\n"), 0o600))
	require.NoError(t, os.WriteFile(filepath.Join(root, "app.lisp"),
		[]byte("(defhelper helper-one)\n(helper-one)\n(not-defined-anywhere)\n"), 0o600))

	cmd := exec.CommandContext(t.Context(), bin, "lint", "--workspace=.", "app.lisp") //nolint:gosec // test executes freshly built local binary
	cmd.Dir = root
	out, _ := cmd.CombinedOutput()
	assert.Contains(t, string(out), "undefined symbol: not-defined-anywhere")
	assert.NotContains(t, string(out), "helper-one", "the function the workspace macro defines resolves")
}
