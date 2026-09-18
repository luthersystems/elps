// Copyright © 2026 The ELPS authors

package cmd

import (
	"bytes"
	"context"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/luthersystems/elps/internal/rootlibrary"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/x/debugger"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestDiagnosticForgedLabels(t *testing.T) {
	bin := buildTestBinary(t)
	root := t.TempDir()
	outside := filepath.Join(t.TempDir(), "private.lisp")
	require.NoError(t, os.WriteFile(outside, []byte("OUTSIDE-FIRST\nOUTSIDE-SECOND\nOUTSIDE-THIRD\n"), 0o600))
	for _, mode := range []string{"run", "debug", "repl-eval", "repl-batch"} {
		for _, loader := range []string{"load-string", "load-bytes"} {
			for _, line := range []int{1, 3} {
				t.Run(fmt.Sprintf("%s/%s/line%d", mode, loader, line), func(t *testing.T) {
					source := fmt.Sprintf("%q", strings.Repeat("\n", line-1)+"missing-symbol")
					if loader == "load-bytes" {
						source = "(to-bytes " + source + ")"
					}
					expr := fmt.Sprintf("(%s %s :name %q)", loader, source, filepath.ToSlash(outside))
					var args []string
					switch mode {
					case "run":
						args = []string{"run", "--root-dir", root, "-e", expr}
					case "debug":
						require.NoError(t, os.WriteFile(filepath.Join(root, "main.lisp"), []byte(expr), 0o600))
						args = []string{"debug", "--stdio", "--root-dir", root, "main.lisp"}
					case "repl-eval":
						args = []string{"repl", "--root-dir", root, "--eval", expr}
					case "repl-batch":
						args = []string{"repl", "--root-dir", root, "--batch", "--json=false"}
					}
					ctx, cancel := context.WithTimeout(t.Context(), 15*time.Second)
					defer cancel()
					command := exec.CommandContext(ctx, bin, args...) //nolint:gosec // freshly built test binary
					if mode == "repl-batch" {
						command.Stdin = strings.NewReader(expr + "\n")
					}
					output, err := command.CombinedOutput()
					require.NoError(t, ctx.Err(), "diagnostic command timed out")
					if mode == "repl-batch" {
						require.NoError(t, err)
					} else {
						require.Error(t, err)
					}
					assert.Contains(t, string(output), "unbound symbol")
					assert.NotContains(t, string(output), "OUTSIDE-", "diagnostic leaked a file outside the root")
					assert.Contains(t, string(output), fmt.Sprintf("%d |  missing-symbol", line), "show the evaluated source")
				})
			}
		}
	}
}

func TestDiagnosticSymlinkReplacement(t *testing.T) {
	for _, mode := range []string{"run", "debug"} {
		t.Run(mode, func(t *testing.T) {
			root := t.TempDir()
			file := filepath.Join(root, "main.lisp")
			outside := filepath.Join(t.TempDir(), "private.lisp")
			require.NoError(t, os.WriteFile(file, []byte("missing-symbol"), 0o600))
			require.NoError(t, os.WriteFile(outside, []byte("OUTSIDE-SECRET"), 0o600))
			lib, err := rootlibrary.Open(root)
			require.NoError(t, err)
			t.Cleanup(func() { require.NoError(t, lib.Close()) })
			env := lisp.NewEnv(nil)
			env.Runtime.Reader = parser.NewReader()
			env.Runtime.Library = lib
			require.True(t, lisp.InitializeUserEnv(env).IsNil())
			if mode == "debug" {
				dbg := debugger.New()
				dbg.Enable()
				env.Runtime.Debugger = dbg
			}
			result := env.LoadFile("main.lisp")
			require.Equal(t, lisp.LError, result.Type)
			require.NoError(t, os.Remove(file))
			require.NoError(t, os.Symlink(outside, file))
			stderr, err := os.CreateTemp(t.TempDir(), "stderr")
			require.NoError(t, err)
			t.Cleanup(func() { require.NoError(t, stderr.Close()) })
			previous := os.Stderr
			os.Stderr = stderr
			t.Cleanup(func() { os.Stderr = previous })
			renderLispErrorContext(t.Context(), env.Runtime, result, file)
			var output bytes.Buffer
			_, err = stderr.Seek(0, 0)
			require.NoError(t, err)
			_, err = output.ReadFrom(stderr)
			require.NoError(t, err)
			assert.Contains(t, output.String(), "unbound symbol")
			assert.NotContains(t, output.String(), "OUTSIDE-SECRET")
		})
	}
}
