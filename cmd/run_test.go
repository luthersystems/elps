// Copyright © 2026 The ELPS authors

package cmd

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"testing"

	"github.com/luthersystems/elps/internal/rootlibrary"
	"github.com/luthersystems/elps/lisp"
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

func TestRunRootDirReplacedAfterConstruction(t *testing.T) {
	parent := t.TempDir()
	root := filepath.Join(parent, "root")
	require.NoError(t, os.Mkdir(root, 0o700))
	require.NoError(t, os.WriteFile(filepath.Join(root, "value.lisp"), []byte("42"), 0o600))
	require.NoError(t, os.WriteFile(filepath.Join(parent, "value.lisp"), []byte("99"), 0o600))
	lib, err := rootlibrary.Open(root)
	require.NoError(t, err)
	t.Cleanup(func() { require.NoError(t, lib.Close()) })
	// A root handle must stay attached to the original directory, even if
	// its pathname is replaced by a symlink after loader construction.
	require.NoError(t, os.Rename(root, filepath.Join(parent, "moved")))
	require.NoError(t, os.Symlink(".", root))
	_, _, data, err := lib.LoadSource(lisp.NewSourceContext("", ""), "value.lisp")
	require.NoError(t, err)
	assert.Equal(t, "42", string(data), "must not read the replacement root")
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

func TestRunRootDirSymlinks(t *testing.T) {
	parent := t.TempDir()
	root := filepath.Join(parent, "jail")
	require.NoError(t, os.Mkdir(root, 0o700))
	require.NoError(t, os.WriteFile(filepath.Join(parent, "outside.lisp"), []byte("42"), 0o600))
	require.NoError(t, os.WriteFile(filepath.Join(root, "inside.lisp"), []byte("42"), 0o600))
	for name, target := range map[string]string{
		"file.lisp":  "../outside.lisp",
		"dir":        "..",
		"chain.lisp": "file.lisp",
		"ok.lisp":    "inside.lisp",
	} {
		require.NoError(t, os.Symlink(target, filepath.Join(root, name)))
	}

	for _, tc := range []struct {
		name    string
		path    string
		allowed bool
	}{
		{"file", "file.lisp", false},
		{"directory", "dir/outside.lisp", false},
		{"chain", "chain.lisp", false},
		{"inside", "ok.lisp", true},
		{"parent", "../outside.lisp", false},
		{"absolute", filepath.Join(parent, "outside.lisp"), false},
	} {
		for _, mode := range []string{"argument", "load-file", "expression"} {
			t.Run(tc.name+"/"+mode, func(t *testing.T) {
				resetRunFlags(t)
				runRootDir = root // Deliberately different from the working directory.
				runPrint = true
				arg := tc.path
				if mode != "argument" {
					source := fmt.Sprintf("(load-file %q)", filepath.ToSlash(tc.path))
					if mode == "expression" {
						runExpression = true
						arg = source
					} else {
						arg = "esc.lisp"
						require.NoError(t, os.WriteFile(filepath.Join(root, arg), []byte(source), 0o600))
					}
				}
				var out bytes.Buffer
				err := runElps([]string{arg}, &out)
				if tc.allowed {
					require.NoError(t, err)
					assert.Equal(t, "42\n", out.String())
				} else {
					require.Error(t, err, "escaping root must return an ordinary error")
					assert.Empty(t, out.String())
				}
			})
		}
	}
}

func TestRunRootDirRelativeLoads(t *testing.T) {
	parent := t.TempDir()
	root := filepath.Join(parent, "jail")
	require.NoError(t, os.MkdirAll(filepath.Join(root, "sub"), 0o700))
	require.NoError(t, os.WriteFile(filepath.Join(root, "sub", "main.lisp"), []byte(`(load-file "value.lisp")`), 0o600))
	require.NoError(t, os.WriteFile(filepath.Join(root, "sub", "value.lisp"), []byte("42"), 0o600))
	require.NoError(t, os.Symlink("sub/main.lisp", filepath.Join(root, "main.lisp")))
	require.NoError(t, os.Symlink("jail", filepath.Join(parent, "alias")))

	for _, tc := range []struct {
		name string
		cwd  string
		root string
	}{
		{"explicit", parent, root},
		{"relative", parent, "jail"},
		{"symlink-root", parent, "alias"},
		{"default", root, ""},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Chdir(tc.cwd)
			resetRunFlags(t)
			runRootDir = tc.root
			runPrint = true
			var out bytes.Buffer
			require.NoError(t, runElps([]string{"main.lisp"}, &out))
			assert.Equal(t, "42\n", out.String())
		})
	}
}

func TestRunRootDirFilesystemRoot(t *testing.T) {
	resetRunFlags(t)
	path := filepath.Join(t.TempDir(), "main.lisp")
	require.NoError(t, os.WriteFile(path, []byte("42"), 0o600))
	runRootDir = filepath.VolumeName(path) + string(filepath.Separator)
	runPrint = true
	var out bytes.Buffer
	require.NoError(t, runElps([]string{path}, &out))
	assert.Equal(t, "42\n", out.String())
}
