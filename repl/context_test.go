// Copyright © 2026 The ELPS authors

package repl

import (
	"bytes"
	"context"
	"io"
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
				assert.Equal(t, "{\"type\":\"error\",\"message\":\"context-cancelled: context deadline exceeded\"}\n", stdout.String())
				assert.Empty(t, stderr.String())
			} else {
				assert.Equal(t, "context-cancelled: context deadline exceeded\n", stderr.String())
				assert.Empty(t, stdout.String())
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
