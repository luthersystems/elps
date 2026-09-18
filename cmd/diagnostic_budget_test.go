// Copyright © 2026 The ELPS authors

package cmd

import (
	"bytes"
	"context"
	"io"
	"os"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/require"
)

const diagnosticReproducer = `(set 'n (string:repeat "x" 50000)) (load-string (format-string "(defun {0} (n) (if (= n 0) (error 'boom) (+ 1 ({0} (- n 1))))) ({0} 300)" n))`

func TestRunDiagnosticByteCap(t *testing.T) {
	resetRunFlags(t)
	runExpression = true
	f, err := os.CreateTemp(t.TempDir(), "stderr")
	require.NoError(t, err)
	t.Cleanup(func() { require.NoError(t, f.Close()) })
	previous := os.Stderr
	os.Stderr = f
	t.Cleanup(func() { os.Stderr = previous })
	require.ErrorIs(t, runElps([]string{diagnosticReproducer}, io.Discard), errRendered)
	info, err := f.Stat()
	require.NoError(t, err)
	t.Logf("stderr bytes: %d", info.Size())
	require.LessOrEqual(t, info.Size(), int64(lisp.DefaultMaxAlloc))
	data, err := os.ReadFile(f.Name())
	require.NoError(t, err)
	require.Contains(t, string(data), "#<truncated>")
}

type cancelDiagnosticWriter struct {
	bytes.Buffer
	cancel            context.CancelFunc
	after             int
	writesAfterCancel int
}

func (w *cancelDiagnosticWriter) Write(p []byte) (int, error) {
	if w.Len() >= w.after {
		w.writesAfterCancel++
	}
	n, err := w.Buffer.Write(p)
	if w.Len() >= w.after {
		w.cancel()
	}
	return n, err
}

func (w *cancelDiagnosticWriter) WriteString(s string) (int, error) { return w.Write([]byte(s)) }

// MaxAlloc caps the diagnostic once it is above the floor the runtime applies
// to text describing a failure; the fixture overruns both limits below.
func TestRunDiagnosticSmallBudget(t *testing.T) {
	resetRunFlags(t)
	runExpression = true
	name := strings.Repeat("x", 20000)
	expr := "(defun " + name + " (n) (if (= n 0) (error 'boom) (+ 1 (" + name + " (- n 1))))) (" + name + " 20)"
	for _, limit := range []int{128 << 10, 256 << 10} {
		var out bytes.Buffer
		require.ErrorIs(t, runElpsReport(t.Context(), []string{expr}, io.Discard, &out, lisp.WithMaxAlloc(limit)), errRendered)
		require.LessOrEqual(t, out.Len(), limit)
		require.Contains(t, out.String(), "#<truncated>")
	}
}

// Below the floor MaxAlloc caps program data only: an embedder who set it to
// bound collection sizes still gets a diagnostic it can read.
func TestRunDiagnosticFloorsTinyBudget(t *testing.T) {
	resetRunFlags(t)
	runExpression = true
	name := strings.Repeat("x", 2048)
	expr := "(defun " + name + " (n) (if (= n 0) (error 'boom) (+ 1 (" + name + " (- n 1))))) (" + name + " 20)"
	var out bytes.Buffer
	require.ErrorIs(t, runElpsReport(t.Context(), []string{expr}, io.Discard, &out, lisp.WithMaxAlloc(5)), errRendered)
	require.Contains(t, out.String(), "boom")
	require.NotContains(t, out.String(), "#<truncated>")
}

func TestRunDiagnosticCancelWriting(t *testing.T) {
	resetRunFlags(t)
	runExpression = true
	for _, after := range []int{0, 20000} {
		ctx, cancel := context.WithCancel(t.Context())
		out := &cancelDiagnosticWriter{cancel: cancel, after: after}
		name := strings.Repeat("x", 50000)
		expr := "(defun " + name + " (n) (if (= n 0) (error 'boom) (+ 1 (" + name + " (- n 1))))) (" + name + " 20)"
		err := runElpsReport(ctx, []string{expr}, io.Discard, out, lisp.WithMaxAlloc(32768))
		cancel()
		require.ErrorIs(t, err, errRendered)
		require.LessOrEqual(t, out.Len(), after+4096+len("#<truncated>"))
		require.LessOrEqual(t, out.writesAfterCancel, 2)
		require.Contains(t, out.String(), "#<truncated>")
	}
}

func TestRunDiagnosticConditionBudget(t *testing.T) {
	resetRunFlags(t)
	runExpression = true
	var out bytes.Buffer
	const limit = 128 << 10
	expr := "(error '" + strings.Repeat("condition", 12000) + ")"
	require.ErrorIs(t, runElpsReport(t.Context(), []string{expr}, io.Discard, &out, lisp.WithMaxAlloc(limit)), errRendered)
	require.LessOrEqual(t, out.Len(), limit)
	require.True(t, strings.HasSuffix(out.String(), "#<truncated>"))
}
