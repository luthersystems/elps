// Copyright © 2026 The ELPS authors

package cmd

import (
	"bytes"
	"os"
	"path/filepath"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func resetRunFlags(t *testing.T) {
	t.Helper()
	prevExpr, prevPrint, prevRoot := runExpression, runPrint, runRootDir
	runExpression, runPrint, runRootDir = false, false, ""
	t.Cleanup(func() {
		runExpression, runPrint, runRootDir = prevExpr, prevPrint, prevRoot
	})
}

// TestRunExpressionFlag pins that -e evaluates its arguments as expressions.
// The flag (and -p) were declared, registered, and documented in the command's
// help text with worked examples, but never read: `elps run -e '(+ 1 2)'`
// treated the expression as a filename and failed with
// "library error: open (+ 1 2): no such file or directory".
func TestRunExpressionFlag(t *testing.T) {
	resetRunFlags(t)
	runExpression = true
	runPrint = true

	var out bytes.Buffer
	require.NoError(t, runElps([]string{`(+ 1 2)`}, &out))
	assert.Equal(t, "3\n", out.String())
}

// TestRunExpressionsShareOneEnvironment pins that multiple -e arguments are
// evaluated in order in a single environment, as the help text implies for
// file arguments.
func TestRunExpressionsShareOneEnvironment(t *testing.T) {
	resetRunFlags(t)
	runExpression = true
	runPrint = true

	var out bytes.Buffer
	require.NoError(t, runElps([]string{`(set 'a 5)`, `(* a 2)`}, &out))
	assert.Equal(t, "5\n10\n", out.String())
}

// TestRunExpressionError pins that a failing expression is reported as an
// error rather than silently succeeding.
func TestRunExpressionError(t *testing.T) {
	resetRunFlags(t)
	runExpression = true

	var out bytes.Buffer
	err := runElps([]string{`(undefined-function-xyz)`}, &out)
	require.Error(t, err)
	assert.Empty(t, out.String())
}

// TestRunPrintFlagWithFile pins that -p prints the value of a loaded file,
// and that it stays silent when unset.
func TestRunPrintFlagWithFile(t *testing.T) {
	dir := t.TempDir()
	path := filepath.Join(dir, "demo.lisp")
	require.NoError(t, os.WriteFile(path, []byte("(+ 20 22)\n"), 0o600))

	t.Run("print", func(t *testing.T) {
		resetRunFlags(t)
		runPrint = true
		runRootDir = dir
		var out bytes.Buffer
		require.NoError(t, runElps([]string{path}, &out))
		assert.Equal(t, "42\n", out.String())
	})

	t.Run("no print", func(t *testing.T) {
		resetRunFlags(t)
		runRootDir = dir
		var out bytes.Buffer
		require.NoError(t, runElps([]string{path}, &out))
		assert.Empty(t, out.String())
	})
}

// TestRunCommandFlagsRegistered guards against the flags being dropped from
// the command while the help text keeps advertising them.
func TestRunCommandFlagsRegistered(t *testing.T) {
	for _, name := range []string{"expression", "print", "root-dir"} {
		assert.NotNil(t, runCmd.Flags().Lookup(name), "missing flag: %s", name)
	}
}
