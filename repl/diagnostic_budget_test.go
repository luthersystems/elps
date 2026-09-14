// Copyright © 2026 The ELPS authors

package repl

import (
	"bytes"
	"context"
	"io"
	"strconv"
	"strings"
	"testing"

	"github.com/stretchr/testify/require"
)

type cancelDiagnosticWriter struct {
	bytes.Buffer
	cancel context.CancelFunc
	calls  int
}

func (w *cancelDiagnosticWriter) WriteString(s string) (int, error) { return w.Write([]byte(s)) }

func (w *cancelDiagnosticWriter) Write(p []byte) (int, error) {
	w.calls++
	w.cancel()
	return w.Buffer.Write(p)
}

func TestEvalDiagnosticBudget(t *testing.T) {
	for _, limit := range []int{5, 256} {
		t.Run(strconv.Itoa(limit), func(t *testing.T) {
			env := newTestEnv(t)
			env.Runtime.MaxAlloc = limit
			name := strings.Repeat("x", 2048)
			cfg := newConfig(WithEval("(defun " + name + " (n) (if (= n 0) (error 'boom) (+ 1 (" + name + " (- n 1))))) (" + name + " 20)"))
			var out bytes.Buffer
			require.Equal(t, 1, runEval(env, cfg, io.Discard, &out))
			t.Logf("limit=%d stderr bytes=%d", limit, out.Len())
			require.LessOrEqual(t, out.Len(), limit)
			require.Contains(t, out.String(), "#<truncated>"[:min(limit, len("#<truncated>"))])
		})
	}
}

func TestEvalDiagnosticCancelWriting(t *testing.T) {
	env := newTestEnv(t)
	env.Runtime.MaxAlloc = 256
	ctx, cancel := context.WithCancel(t.Context())
	defer cancel()
	name := strings.Repeat("x", 50000)
	cfg := newConfig(WithContext(ctx), WithEval("(defun "+name+" (n) (if (= n 0) (error 'boom) (+ 1 ("+name+" (- n 1))))) ("+name+" 20)"))
	out := &cancelDiagnosticWriter{cancel: cancel}
	require.Equal(t, 1, runEval(env, cfg, io.Discard, out))
	t.Logf("writes=%d stderr bytes=%d", out.calls, out.Len())
	require.LessOrEqual(t, out.Len(), env.Runtime.MaxAlloc)
	require.LessOrEqual(t, out.calls, 2)
	require.Contains(t, out.String(), "#<truncated>")
}

func TestEvalDiagnosticConditionBudget(t *testing.T) {
	env := newTestEnv(t)
	env.Runtime.MaxAlloc = 256
	cfg := newConfig(WithEval("(error '" + strings.Repeat("condition", 10000) + ")"))
	var out bytes.Buffer
	require.Equal(t, 1, runEval(env, cfg, io.Discard, &out))
	require.LessOrEqual(t, out.Len(), env.Runtime.MaxAlloc)
	require.True(t, strings.HasSuffix(out.String(), "#<truncated>"))
}
