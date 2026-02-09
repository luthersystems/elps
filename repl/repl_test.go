package repl

import (
	"bytes"
	"encoding/json"
	"io"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser"
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

// newTestEnv creates a fully initialized environment for testing, matching
// the setup in RunRepl but without os.Exit calls.
func newTestEnv(t *testing.T) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	rc := lisp.InitializeUserEnv(env,
		lisp.WithReader(parser.NewReader()),
		lisp.WithLibrary(&lisp.RelativeFileSystemLibrary{}),
	)
	require.True(t, rc.IsNil(), "InitializeUserEnv: %v", rc)
	rc = lisplib.LoadLibrary(env)
	require.True(t, rc.IsNil(), "LoadLibrary: %v", rc)
	rc = env.InPackage(lisp.String(lisp.DefaultUserPackage))
	require.True(t, rc.IsNil(), "InPackage: %v", rc)
	return env
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
	err := os.WriteFile(histFile, []byte("some history"), 0600)
	require.NoError(t, err)

	ensureHistoryFilePermissions(histFile)

	info, err := os.Stat(histFile)
	require.NoError(t, err)
	assert.Equal(t, os.FileMode(0600), info.Mode().Perm(), "existing history file should be restricted to 0600")

	// Verify contents are preserved.
	data, err := os.ReadFile(histFile) //nolint:gosec // test file path from t.TempDir(), not user input
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

func TestEvalSimpleExpression(t *testing.T) {
	env := newTestEnv(t)
	cfg := &config{eval: "(+ 1 2)"}
	var stdout, stderr bytes.Buffer
	code := runEval(env, cfg, &stdout, &stderr)
	assert.Equal(t, 0, code, "exit code should be 0")
	assert.Equal(t, "3\n", stdout.String())
	assert.Empty(t, stderr.String())
}

func TestEvalMultipleExpressions(t *testing.T) {
	env := newTestEnv(t)
	cfg := &config{eval: "(set 'x 10) (* x x)"}
	var stdout, stderr bytes.Buffer
	code := runEval(env, cfg, &stdout, &stderr)
	assert.Equal(t, 0, code)
	// Only the last result is printed.
	assert.Equal(t, "100\n", stdout.String())
}

func TestEvalError(t *testing.T) {
	env := newTestEnv(t)
	cfg := &config{eval: "fnord"}
	var stdout, stderr bytes.Buffer
	code := runEval(env, cfg, &stdout, &stderr)
	assert.Equal(t, 1, code, "exit code should be 1 for errors")
	assert.Empty(t, stdout.String())
	assert.Contains(t, stderr.String(), "unbound symbol")
}

func TestEvalParseError(t *testing.T) {
	env := newTestEnv(t)
	cfg := &config{eval: "(+ 1"}
	var stdout, stderr bytes.Buffer
	code := runEval(env, cfg, &stdout, &stderr)
	assert.Equal(t, 1, code)
	assert.Empty(t, stdout.String())
	assert.NotEmpty(t, stderr.String())
}

func TestEvalJSON(t *testing.T) {
	env := newTestEnv(t)
	cfg := &config{eval: "(+ 1 2)", json: true}
	var stdout, stderr bytes.Buffer
	code := runEval(env, cfg, &stdout, &stderr)
	assert.Equal(t, 0, code)

	var result jsonResult
	err := json.Unmarshal([]byte(strings.TrimSpace(stdout.String())), &result)
	require.NoError(t, err)
	assert.Equal(t, "result", result.Type)
	assert.Equal(t, "int", result.ValueType)
	assert.Equal(t, "3", result.Value)
}

func TestEvalJSONError(t *testing.T) {
	env := newTestEnv(t)
	cfg := &config{eval: "fnord", json: true}
	var stdout, stderr bytes.Buffer
	code := runEval(env, cfg, &stdout, &stderr)
	assert.Equal(t, 1, code)

	var result jsonError
	err := json.Unmarshal([]byte(strings.TrimSpace(stdout.String())), &result)
	require.NoError(t, err)
	assert.Equal(t, "error", result.Type)
	assert.Contains(t, result.Message, "unbound symbol")
}

func TestEvalJSONParseError(t *testing.T) {
	env := newTestEnv(t)
	cfg := &config{eval: "(+ 1", json: true}
	var stdout, stderr bytes.Buffer
	code := runEval(env, cfg, &stdout, &stderr)
	assert.Equal(t, 1, code)

	var result jsonParseError
	err := json.Unmarshal([]byte(strings.TrimSpace(stdout.String())), &result)
	require.NoError(t, err)
	assert.Equal(t, "parse_error", result.Type)
	assert.NotEmpty(t, result.Message)
}

func TestBatchJSON(t *testing.T) {
	env := newTestEnv(t)
	input := "(+ 1 2)\n(* 3 4)\n"
	cfg := &config{
		json:  true,
		batch: true,
		stdin: io.NopCloser(strings.NewReader(input)),
	}
	var stdout, stderr bytes.Buffer
	runBatch(env, cfg, &stdout, &stderr)

	lines := strings.Split(strings.TrimSpace(stdout.String()), "\n")
	require.Len(t, lines, 2)

	var r1 jsonResult
	require.NoError(t, json.Unmarshal([]byte(lines[0]), &r1))
	assert.Equal(t, "result", r1.Type)
	assert.Equal(t, "3", r1.Value)

	var r2 jsonResult
	require.NoError(t, json.Unmarshal([]byte(lines[1]), &r2))
	assert.Equal(t, "result", r2.Type)
	assert.Equal(t, "12", r2.Value)
}

func TestBatchMultilineExpression(t *testing.T) {
	env := newTestEnv(t)
	// Multi-line expression split across lines.
	input := "(+\n  1\n  2)\n"
	cfg := &config{
		json:  true,
		batch: true,
		stdin: io.NopCloser(strings.NewReader(input)),
	}
	var stdout, stderr bytes.Buffer
	runBatch(env, cfg, &stdout, &stderr)

	lines := strings.Split(strings.TrimSpace(stdout.String()), "\n")
	require.Len(t, lines, 1)

	var r jsonResult
	require.NoError(t, json.Unmarshal([]byte(lines[0]), &r))
	assert.Equal(t, "3", r.Value)
}

func TestBatchPlainText(t *testing.T) {
	env := newTestEnv(t)
	input := "(+ 1 2)\n"
	cfg := &config{
		batch: true,
		stdin: io.NopCloser(strings.NewReader(input)),
	}
	var stdout, stderr bytes.Buffer
	runBatch(env, cfg, &stdout, &stderr)
	assert.Equal(t, "3\n", stdout.String())
}

func TestBatchError(t *testing.T) {
	env := newTestEnv(t)
	input := "fnord\n(+ 1 2)\n"
	cfg := &config{
		json:  true,
		batch: true,
		stdin: io.NopCloser(strings.NewReader(input)),
	}
	var stdout, stderr bytes.Buffer
	runBatch(env, cfg, &stdout, &stderr)

	lines := strings.Split(strings.TrimSpace(stdout.String()), "\n")
	require.Len(t, lines, 2)

	var e jsonError
	require.NoError(t, json.Unmarshal([]byte(lines[0]), &e))
	assert.Equal(t, "error", e.Type)
	assert.Contains(t, e.Message, "unbound symbol")

	var r jsonResult
	require.NoError(t, json.Unmarshal([]byte(lines[1]), &r))
	assert.Equal(t, "3", r.Value)
}

func TestEmitResult(t *testing.T) {
	tests := []struct {
		name     string
		val      *lisp.LVal
		expected map[string]string
	}{
		{
			name: "integer",
			val:  lisp.Int(42),
			expected: map[string]string{
				"type":       "result",
				"value_type": "int",
				"value":      "42",
			},
		},
		{
			name: "string",
			val:  lisp.String("hello"),
			expected: map[string]string{
				"type":       "result",
				"value_type": "string",
				"value":      `"hello"`,
			},
		},
		{
			name: "nil list",
			val:  lisp.Nil(),
			expected: map[string]string{
				"type":       "result",
				"value_type": "list",
				"value":      "()",
			},
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			var buf bytes.Buffer
			emitResult(&buf, tc.val)
			var m map[string]string
			err := json.Unmarshal([]byte(strings.TrimSpace(buf.String())), &m)
			require.NoError(t, err)
			assert.Equal(t, tc.expected, m)
		})
	}
}

func TestEmitParseError(t *testing.T) {
	var buf bytes.Buffer
	emitParseError(&buf, io.ErrUnexpectedEOF)
	var m map[string]string
	err := json.Unmarshal([]byte(strings.TrimSpace(buf.String())), &m)
	require.NoError(t, err)
	assert.Equal(t, "parse_error", m["type"])
	assert.Equal(t, "unexpected EOF", m["message"])
}
