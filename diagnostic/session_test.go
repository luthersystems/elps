// Copyright © 2026 The ELPS authors

package diagnostic

import (
	"bytes"
	"context"
	"io"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/stretchr/testify/require"
)

type sessionCancelWriter struct {
	bytes.Buffer
	cancel context.CancelFunc
}

func (w *sessionCancelWriter) Write(p []byte) (int, error)       { w.cancel(); return w.Buffer.Write(p) }
func (w *sessionCancelWriter) WriteString(s string) (int, error) { return w.Write([]byte(s)) }

func TestSessionSourceBudget(t *testing.T) {
	for _, source := range []string{strings.Repeat("x", 1<<20), strings.Repeat("\t", 120), strings.Repeat("\n", 1<<20)} {
		path := filepath.Join(t.TempDir(), "source.lisp")
		require.NoError(t, os.WriteFile(path, []byte(source), 0600))
		var out bytes.Buffer
		s := (&Renderer{Color: ColorAlways}).NewSession(t.Context(), &out, 256)
		s.Header(func() { s.Text("boom") })
		s.Span(Span{File: path, Line: 1, Col: 100})
		s.Note(func() { s.Text(strings.Repeat("f", 512)) })
		n, err := s.Finish()
		require.NoError(t, err)
		require.Equal(t, out.Len(), n)
		require.LessOrEqual(t, n, 256)
		require.True(t, strings.HasSuffix(out.String(), truncationMarker))
	}
}

func TestSessionSourceWorkBudget(t *testing.T) {
	path := filepath.Join(t.TempDir(), "source.lisp")
	require.NoError(t, os.WriteFile(path, []byte(strings.Repeat("\n", 10000)), 0600))
	var out bytes.Buffer
	s := (&Renderer{}).NewSession(t.Context(), &out, 256)
	s.work = 100
	s.Span(Span{File: path, Line: 9000})
	_, err := s.Finish()
	require.NoError(t, err)
	require.LessOrEqual(t, out.Len(), 256)
	require.Contains(t, out.String(), truncationMarker)
	require.LessOrEqual(t, s.work, 0)
}

func TestSessionCancelBeforeSourceRead(t *testing.T) {
	ctx, cancel := context.WithCancel(t.Context())
	defer cancel()
	out := &sessionCancelWriter{cancel: cancel}
	r := &Renderer{SourceReader: func(string) ([]byte, error) { t.Fatal("source read after cancellation"); return nil, nil }}
	s := r.NewSession(ctx, out, 256)
	s.Span(Span{File: "source.lisp", Line: 1})
	_, err := s.Finish()
	require.NoError(t, err)
	require.LessOrEqual(t, out.Len(), 256)
	require.Contains(t, out.String(), truncationMarker)
}

type shortDiagnosticWriter struct{}

func (shortDiagnosticWriter) Write(p []byte) (int, error) { return len(p) - 1, nil }

func TestSessionShortWrite(t *testing.T) {
	s := (&Renderer{}).NewSession(t.Context(), shortDiagnosticWriter{}, 256)
	s.Text("message")
	n, err := s.Finish()
	require.Equal(t, 6, n)
	require.ErrorIs(t, err, io.ErrShortWrite)
}
