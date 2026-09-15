// Copyright © 2026 The ELPS authors

package cmd

import (
	"bytes"
	"context"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// Each inner call returns, so this loop cannot hit the tail-iteration limit.
const runEndlessProgram = `(defun inner (k) (if (= k 0) 0 (inner (- k 1))))
(dotimes (i 2147483647) (inner 10))`

func TestRunReportCancellation(t *testing.T) {
	for _, timeout := range []bool{false, true} {
		t.Run(map[bool]string{false: "parent-cancelled", true: "timeout"}[timeout], func(t *testing.T) {
			resetRunFlags(t)
			runExpression = true
			parent, cancel := context.WithCancel(t.Context())
			defer cancel()
			want := "context-cancelled: context canceled\n"
			if timeout {
				runTimeout = 20 * time.Millisecond
				want = "context-cancelled: context deadline exceeded\n"
			} else {
				cancel()
			}
			var stdout, stderr bytes.Buffer
			var env *lisp.LEnv
			err := runElpsReport(parent, []string{runEndlessProgram}, &stdout, &stderr,
				lisp.WithMaxAlloc(256), func(e *lisp.LEnv) *lisp.LVal {
					env = e
					return lisp.Nil()
				})
			require.ErrorIs(t, err, errRendered)
			require.Equal(t, want, stderr.String())
			require.Empty(t, stdout.String())
			require.Equal(t, 256, env.Runtime.MaxAllocBytes())
			if timeout {
				_, ok := env.Context().Deadline()
				require.True(t, ok, "evaluation must use the derived timeout context")
				require.NoError(t, parent.Err())
			}
		})
	}
}

func TestRunLimits(t *testing.T) {
	bin := buildTestBinary(t)
	root := t.TempDir()
	require.NoError(t, os.WriteFile(filepath.Join(root, "loop.lisp"), []byte(runEndlessProgram), 0o600))
	for _, tc := range []struct {
		name string
		args []string
		want string
		fail bool
	}{
		{"timeout", []string{"--timeout", "1s", "-e", runEndlessProgram}, "context-cancelled: context deadline exceeded", true},
		{"steps", []string{"--max-steps", "100", "-e", runEndlessProgram}, "step-limit-exceeded", true},
		{"file-steps", []string{"--max-steps", "100", "--root-dir", root, "loop.lisp"}, "step-limit-exceeded", true},
		{"normal-defaults", []string{"-e", "-p", "(+ 20 22)"}, "42\n", false},
		{"normal-limited", []string{"--timeout", "1s", "--max-steps", "100", "-e", "-p", "(+ 20 22)"}, "42\n", false},
		{"budget-per-argument", []string{"--max-steps", "5", "-e", "-p", "(+ 20 22)", "(+ 20 22)"}, "42\n42\n", false},
		{"negative-timeout", []string{"--timeout=-1s", "-e", "42"}, "timeout must be non-negative", true},
		{"negative-steps", []string{"--max-steps=-1", "-e", "42"}, "max-steps must be non-negative", true},
	} {
		t.Run(tc.name, func(t *testing.T) {
			ctx, cancel := context.WithTimeout(t.Context(), 3*time.Second)
			defer cancel()
			cmd := exec.CommandContext(ctx, bin, append([]string{"run"}, tc.args...)...) //nolint:gosec // locally built test binary
			start := time.Now()
			out, err := cmd.CombinedOutput()
			elapsed := time.Since(start)
			require.NoError(t, ctx.Err(), "runner failed to stop: %s", out)
			if tc.fail {
				require.Error(t, err)
			} else {
				require.NoError(t, err, "%s", out)
			}
			assert.Contains(t, string(out), tc.want)
			if tc.name == "timeout" {
				assert.GreaterOrEqual(t, elapsed, time.Second)
				assert.Less(t, elapsed, 2500*time.Millisecond)
				assert.Equal(t, 1, strings.Count(string(out), "\n"), "cancellation must be one line")
			}
		})
	}
}

func TestRunLimitHelp(t *testing.T) {
	for _, name := range []string{"timeout", "max-steps"} {
		flag := runCmd.Flags().Lookup(name)
		require.NotNil(t, flag, "missing flag: %s", name)
		assert.Contains(t, flag.Usage, "unlimited")
	}
}
