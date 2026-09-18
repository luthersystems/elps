// Copyright © 2026 The ELPS authors

package repl

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestDiagnosticSymlinkReplacement(t *testing.T) {
	root := t.TempDir()
	file := filepath.Join(root, "main.lisp")
	outside := filepath.Join(t.TempDir(), "private.lisp")
	require.NoError(t, os.WriteFile(file, []byte("missing-symbol"), 0o600))
	require.NoError(t, os.WriteFile(outside, []byte("OUTSIDE-SECRET"), 0o600))
	output, err := runReplWithString(t, `(load-file "main.lisp")`, WithRootDir(root),
		WithEvalFunc(func(env *lisp.LEnv, expr *lisp.LVal) *lisp.LVal {
			result := env.Eval(expr)
			// This callback runs on the REPL goroutine; report errors without FailNow.
			if err := os.Remove(file); err != nil {
				t.Error(err)
				return result
			}
			if err := os.Symlink(outside, file); err != nil {
				t.Error(err)
			}
			return result
		}))
	require.NoError(t, err)
	assert.Contains(t, output, "unbound symbol")
	assert.NotContains(t, output, "OUTSIDE-SECRET")
}

func TestDiagnosticForgedLabels(t *testing.T) {
	root := t.TempDir()
	outside := filepath.Join(t.TempDir(), "private.lisp")
	require.NoError(t, os.WriteFile(outside, []byte("OUTSIDE-FIRST\nOUTSIDE-SECOND\nOUTSIDE-THIRD\n"), 0o600))
	for _, loader := range []string{"load-string", "load-bytes"} {
		for _, line := range []int{1, 3} {
			t.Run(fmt.Sprintf("%s/line%d", loader, line), func(t *testing.T) {
				source := fmt.Sprintf("%q", strings.Repeat("\n", line-1)+"missing-symbol")
				if loader == "load-bytes" {
					source = "(to-bytes " + source + ")"
				}
				expr := fmt.Sprintf("(%s %s :name %q)", loader, source, filepath.ToSlash(outside))
				output, err := runReplWithString(t, expr, WithRootDir(root))
				require.NoError(t, err)
				assert.Contains(t, output, "unbound symbol")
				assert.NotContains(t, output, "OUTSIDE-")
				assert.Contains(t, output, fmt.Sprintf("%d |  missing-symbol", line))
			})
		}
	}
}
