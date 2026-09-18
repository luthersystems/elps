// Copyright © 2026 The ELPS authors

package repl

import (
	"bytes"
	"context"
	"io"
	"strconv"
	"strings"
	"testing"
	"time"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestEvalContextCancellation(t *testing.T) {
	for _, json := range []bool{false, true} {
		t.Run(map[bool]string{false: "text", true: "json"}[json], func(t *testing.T) {
			env := newTestEnv(t)
			ctx, cancel := context.WithTimeout(t.Context(), 20*time.Millisecond)
			defer cancel()
			cfg := newConfig(WithContext(ctx), WithJSON(json), WithEval(`(dotimes (i 2147483647))`))
			var stdout, stderr bytes.Buffer
			require.Equal(t, 1, runEval(env, cfg, &stdout, &stderr))
			if json {
				assert.Equal(t, "{\"type\":\"error\",\"message\":\"context-cancelled: context deadline exceeded\"}\n", stdout.String()) //nolint:testifylint // exact JSON line framing is part of the contract
				assert.Empty(t, stderr.String())
			} else {
				assert.Equal(t, "context-cancelled: context deadline exceeded\n", stderr.String())
				assert.Empty(t, stdout.String())
			}
		})
	}
}

type cancellingWriter struct {
	output bytes.Buffer
	cancel context.CancelFunc
}

func (w *cancellingWriter) Write(p []byte) (int, error) {
	w.cancel()
	return w.output.Write(p)
}

func TestEvalContextCancellationDuringFinalOutput(t *testing.T) {
	for _, json := range []bool{false, true} {
		t.Run(map[bool]string{false: "text", true: "json"}[json], func(t *testing.T) {
			env := newTestEnv(t)
			ctx, cancel := context.WithCancel(t.Context())
			defer cancel()
			cfg := newConfig(WithContext(ctx), WithJSON(json), WithEval(`(+ 1 2)`))
			stdout := &cancellingWriter{cancel: cancel}
			var stderr bytes.Buffer
			assert.Equal(t, 1, runEval(env, cfg, stdout, &stderr))
			if json {
				assert.Equal(t, "{\"type\":\"result\",\"value_type\":\"int\",\"value\":\"3\"}\n"+ //nolint:testifylint // preserve exact framing of both JSON lines
					"{\"type\":\"error\",\"message\":\"context-cancelled: context canceled\"}\n", stdout.output.String())
				assert.Empty(t, stderr.String())
			} else {
				assert.Equal(t, "3\n", stdout.output.String())
				assert.Equal(t, "context-cancelled: context canceled\n", stderr.String())
			}
		})
	}
}

func TestBatchContextCancelsInputWait(t *testing.T) {
	env := newTestEnv(t)
	ctx, cancel := context.WithCancel(t.Context())
	defer cancel()
	input, writer := io.Pipe()
	t.Cleanup(func() {
		_ = input.Close()
		_ = writer.Close()
	})
	cfg := newConfig(WithContext(ctx), WithStdin(input))
	var stdout, stderr bytes.Buffer
	done := make(chan struct{})
	go func() {
		defer close(done)
		runBatch(env, cfg, &stdout, &stderr)
	}()
	// A completed pipe write proves the scanner started reading input.
	_, err := io.WriteString(writer, "\n")
	require.NoError(t, err)
	cancel()
	select {
	case <-done:
	case <-time.After(time.Second):
		t.Fatal("cancelled batch REPL still waiting for input")
	}
	assert.Empty(t, stdout.String())
	assert.Equal(t, "context-cancelled: context canceled\n", stderr.String())
}

func TestInteractiveContextCancellation(t *testing.T) {
	ctx, cancel := context.WithCancel(t.Context())
	defer cancel()
	output, err := runReplWithString(t, "42\n", WithContext(ctx),
		WithEvalFunc(func(env *lisp.LEnv, expr *lisp.LVal) *lisp.LVal {
			assert.Same(t, ctx, env.Context())
			cancel()
			return env.Eval(expr)
		}))
	require.NoError(t, err)
	assert.Equal(t, 1, strings.Count(output, "context-cancelled: context canceled\n"))
}

// observedInput exposes the lifetime of readline's underlying pending read.
// No bytes are supplied, so Read must return an error and end its ioloop.
type observedInput struct {
	*io.PipeReader
	started chan struct{}
	done    chan struct{}
}

func (r *observedInput) Read(p []byte) (int, error) {
	close(r.started)
	defer close(r.done)
	return r.PipeReader.Read(p)
}

func TestInteractiveCancellationReleasesInput(t *testing.T) {
	for i := range 5 {
		t.Run(strconv.Itoa(i), func(t *testing.T) {
			env := newTestEnv(t)
			ctx, cancel := context.WithCancel(t.Context())
			defer cancel()
			reader, writer := io.Pipe()
			defer func() { _ = reader.Close() }()
			defer func() { _ = writer.Close() }()
			input := &observedInput{reader, make(chan struct{}), make(chan struct{})}
			finished := make(chan struct{})
			go func() {
				defer close(finished)
				RunEnv(env, "", "", WithContext(ctx), WithStdin(input))
			}()
			deadline := time.NewTimer(time.Second)
			defer deadline.Stop()
			select {
			case <-input.started:
			case <-deadline.C:
				t.Fatal("interactive input read did not start")
			}
			cancel()
			select {
			case <-finished:
			case <-deadline.C:
				t.Fatal("cancelled interactive REPL did not return")
			}
			select {
			case <-input.done:
			case <-deadline.C:
				t.Fatal("cancelled interactive REPL left underlying input read blocked")
			}
		})
	}
}
