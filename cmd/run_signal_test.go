// Copyright © 2026 The ELPS authors

//go:build unix

package cmd

import (
	"bufio"
	"context"
	"io"
	"os"
	"os/exec"
	"strings"
	"syscall"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestEvaluationSignals(t *testing.T) {
	bin := buildTestBinary(t)
	for _, mode := range []string{"run", "repl-eval", "repl-batch"} {
		for _, sig := range []os.Signal{os.Interrupt, syscall.SIGTERM} {
			t.Run(mode+"/"+sig.String(), func(t *testing.T) {
				ctx, cancel := context.WithTimeout(t.Context(), 5*time.Second)
				defer cancel()
				source := `(debug-print "ready") ` + runEndlessProgram
				args := []string{"run", "-e", source}
				if mode == "repl-eval" {
					args = []string{"repl", "-e", source}
				} else if mode == "repl-batch" {
					args = []string{"repl", "--batch", "--json=false"}
				}
				cmd := exec.CommandContext(ctx, bin, args...) //nolint:gosec // locally built test binary
				cmd.Stdin = strings.NewReader(source + "\n")
				stderr, err := cmd.StderrPipe()
				require.NoError(t, err)
				require.NoError(t, cmd.Start())
				reader := bufio.NewReader(stderr)
				ready, err := reader.ReadString('\n')
				require.NoError(t, err)
				require.Equal(t, "\"ready\"\n", ready, "output before cancellation must be delivered")
				require.NoError(t, cmd.Process.Signal(sig))
				out, err := io.ReadAll(reader)
				require.NoError(t, err)
				err = cmd.Wait()
				require.NoError(t, ctx.Err(), "signal did not stop evaluation")
				require.Error(t, err)
				assert.Equal(t, 1, cmd.ProcessState.ExitCode(), "must exit normally with failure, not die from a signal")
				assert.Equal(t, "context-cancelled: context canceled\n", string(out))
			})
		}
	}
}

// This child stands in for a native call that has observed cancellation but
// cannot return. Keeping it isolated also avoids changing the test runner's
// own signal disposition.
func TestEvaluationSignalHelper(t *testing.T) {
	if os.Getenv("ELPS_TEST_SIGNAL_HELPER") != "1" {
		return
	}
	ctx, stop := evaluationContext(context.Background(), 0)
	defer stop()
	_, _ = io.WriteString(os.Stdout, "ready\n")
	<-ctx.Done()
	_, _ = io.WriteString(os.Stdout, "cancelled\n")
	select {}
}

func TestEvaluationSecondInterrupt(t *testing.T) {
	ctx, cancel := context.WithTimeout(t.Context(), 5*time.Second)
	defer cancel()
	bin, err := os.Executable()
	require.NoError(t, err)
	cmd := exec.CommandContext(ctx, bin, "-test.run=^TestEvaluationSignalHelper$") //nolint:gosec // current test executable
	cmd.Env = append(os.Environ(), "ELPS_TEST_SIGNAL_HELPER=1")
	stdout, err := cmd.StdoutPipe()
	require.NoError(t, err)
	require.NoError(t, cmd.Start())
	reader := bufio.NewReader(stdout)
	line, err := reader.ReadString('\n')
	require.NoError(t, err)
	require.Equal(t, "ready\n", line)
	require.NoError(t, cmd.Process.Signal(os.Interrupt))
	line, err = reader.ReadString('\n')
	require.NoError(t, err)
	require.Equal(t, "cancelled\n", line)
	require.NoError(t, cmd.Process.Signal(os.Interrupt))
	require.Error(t, cmd.Wait())
	require.NoError(t, ctx.Err())
	assert.Equal(t, 130, cmd.ProcessState.ExitCode())
}
