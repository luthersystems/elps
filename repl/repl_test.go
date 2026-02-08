package repl

import (
	"bytes"
	"io"
	"os"
	"path/filepath"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func runReplWithString(t *testing.T, input string) (string, error) {
	t.Helper()
	inR, inW := io.Pipe()
	outR, outW := io.Pipe()

	go func() {
		defer inW.Close() //nolint:errcheck // test cleanup
		_, _ = io.WriteString(inW, input)
	}()

	go func() {
		RunRepl("elps> ", WithStdin(inR), WithStderr(outW))
		inR.Close()  //nolint:errcheck,gosec // test cleanup
		outW.Close() //nolint:errcheck,gosec // test cleanup
	}()

	var output bytes.Buffer
	_, _ = io.Copy(&output, outR)
	outR.Close() //nolint:errcheck,gosec // test cleanup

	return output.String(), nil
}

func TestEnsureHistoryFilePermissions_CreatesWithRestrictedMode(t *testing.T) {
	dir := t.TempDir()
	histFile := filepath.Join(dir, ".elps_history")

	// File does not exist yet.
	ensureHistoryFilePermissions(histFile)

	info, err := os.Stat(histFile)
	require.NoError(t, err, "history file should be created")
	assert.Equal(t, os.FileMode(0600), info.Mode().Perm(), "new history file should have mode 0600")
}

func TestEnsureHistoryFilePermissions_RestrictsExistingFile(t *testing.T) {
	dir := t.TempDir()
	histFile := filepath.Join(dir, ".elps_history")

	// Create the file with overly permissive mode.
	err := os.WriteFile(histFile, []byte("some history"), 0644)
	require.NoError(t, err)

	ensureHistoryFilePermissions(histFile)

	info, err := os.Stat(histFile)
	require.NoError(t, err)
	assert.Equal(t, os.FileMode(0600), info.Mode().Perm(), "existing history file should be restricted to 0600")

	// Verify contents are preserved.
	data, err := os.ReadFile(histFile)
	require.NoError(t, err)
	assert.Equal(t, "some history", string(data))
}

func TestEnsureHistoryFilePermissions_EmptyPathNoOp(t *testing.T) {
	// Should not panic or error with empty path.
	ensureHistoryFilePermissions("")
}

func TestRunRepl(t *testing.T) {
	testCases := []struct {
		name     string
		input    string
		expected string
	}{
		{
			name:     "Simple Addition",
			input:    `(+ 1 1)`,
			expected: "2\n",
		},
		{
			name:     "Error",
			input:    `fnord`,
			expected: "unbound symbol",
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			got, err := runReplWithString(t, tc.input)
			require.NoError(t, err)
			require.Contains(t, got, tc.expected)
		})
	}
}
