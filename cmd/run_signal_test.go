// Copyright © 2026 The ELPS authors

//go:build unix

package cmd

import (
	"bufio"
	"bytes"
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

func TestRunSignalDuringFinalOutput(t *testing.T) {
	bin := buildTestBinary(t)
	// Far larger than the pipe buffer, so the final write cannot finish
	// until the parent drains stdout.
	const valueSize = (4 << 20) + 2 // string contents and quotes
	for _, mode := range []string{"run", "repl"} {
		t.Run(mode, func(t *testing.T) {
			ctx, cancel := context.WithTimeout(t.Context(), 5*time.Second)
			defer cancel()
			args := []string{mode, "-e", `(string:repeat "x" 4194304)`}
			if mode == "run" {
				args = append(args, "-p")
			}
			cmd := exec.CommandContext(ctx, bin, args...) //nolint:gosec // locally built test binary
			var stderr bytes.Buffer
			cmd.Stderr = &stderr
			stdout, err := cmd.StdoutPipe()
			require.NoError(t, err)
			require.NoError(t, cmd.Start())
			defer func() { _ = cmd.Process.Kill() }()
			// Read just one byte to establish that printing has started, leaving
			// the rest blocked in the pipe when SIGINT arrives.
			first := make([]byte, 1)
			_, err = io.ReadFull(stdout, first)
			require.NoError(t, err)
			require.NoError(t, cmd.Process.Signal(os.Interrupt))
			// Allow the signal handler to run while stdout remains blocked.
			time.Sleep(100 * time.Millisecond)
			n, err := io.Copy(io.Discard, stdout)
			require.NoError(t, err)
			err = cmd.Wait()
			require.NoError(t, ctx.Err(), "subprocess exceeded deadline")
			assert.Equal(t, int64(valueSize), n, "complete result including newline must drain")
			assert.Error(t, err, "cancelled final output must fail")
			assert.Equal(t, 1, cmd.ProcessState.ExitCode())
			assert.Equal(t, "context-cancelled: context canceled\n", stderr.String())
		})
	}
}
