package token

import (
	"errors"
	"fmt"
	"io"
	"strings"
	"testing"

	"github.com/stretchr/testify/require"
)

func TestScanLineAcrossWindows(t *testing.T) {
	for _, size := range []int{8, 17, DefaultBufSize} {
		for _, ending := range []string{"", "\n"} {
			t.Run(fmt.Sprintf("%d/%q", size, ending), func(t *testing.T) {
				line := ";" + strings.Repeat("日🙂a", 40000) + "(hidden)"
				s := newScannerBuf("line.lisp", strings.NewReader("x "+line+ending), make([]byte, size))
				require.True(t, s.AcceptRune('x'))
				s.Ignore()
				require.True(t, s.AcceptSpace())
				s.Ignore()
				require.True(t, s.AcceptRune(';'))
				got, err := s.ScanLine()
				require.NoError(t, err)
				require.True(t, line == got, "got %d bytes, want %d", len(got), len(line))
				require.Equal(t, size, cap(s.buf), "scanner window must stay bounded")
				if ending != "" {
					require.True(t, s.AcceptRune('\n'), "ScanLine must leave the newline unconsumed")
					s.Ignore()
					require.Equal(t, 2, s.LocStart().Line)
					require.Equal(t, 1, s.LocStart().Col)
				}
				require.ErrorIs(t, s.ScanRune(), io.EOF)
				require.NoError(t, s.Err())
			})
		}
	}
}

func TestScannerAcceptSeqReportsErrors(t *testing.T) {
	s := newScannerBuf("large.lisp", strings.NewReader(strings.Repeat("x", 30)), make([]byte, 10))
	require.Equal(t, 10, s.AcceptSeq(func(rune) bool { return true }))
	require.ErrorContains(t, s.Err(), "token exceeds maximum allowable size")
	s.Ignore()
	require.False(t, s.AcceptRune('x'), "ignoring a partial token must not clear its scan error")
	require.ErrorContains(t, s.ScanRune(), "token exceeds maximum allowable size")
}

func TestScannerStringAndShortReadEOF(t *testing.T) {
	for name, s := range map[string]*Scanner{
		"string": NewScannerString("eof.lisp", "abc"),
		"reader": NewScanner("eof.lisp", strings.NewReader("abc")),
	} {
		t.Run(name, func(t *testing.T) {
			require.Equal(t, 3, s.AcceptSeq(func(rune) bool { return true }))
			require.True(t, s.EOF())
			require.NoError(t, s.Err())
			require.Equal(t, "abc", s.Text())
			require.ErrorIs(t, s.ScanRune(), io.EOF)
		})
	}
}

type unexpectedEOFReader struct {
	data          string
	errorWithData bool
}

func (r *unexpectedEOFReader) Read(p []byte) (int, error) {
	n := copy(p, r.data)
	r.data = r.data[n:]
	if n == 0 || (r.errorWithData && r.data == "") {
		return n, io.ErrUnexpectedEOF
	}
	return n, nil
}

func TestScannerPreservesUnexpectedEOF(t *testing.T) {
	for _, tt := range []struct {
		name          string
		bufferSize    int
		errorWithData bool
	}{
		{name: "after data", bufferSize: 8},
		{name: "with data", bufferSize: 8, errorWithData: true},
		{name: "with full window", bufferSize: 3, errorWithData: true},
	} {
		t.Run(tt.name, func(t *testing.T) {
			r := &unexpectedEOFReader{data: "abc", errorWithData: tt.errorWithData}
			s := newScannerBuf("truncated.lisp", r, make([]byte, tt.bufferSize))
			require.NoError(t, s.Err(), "valid buffered data precedes the read error")
			require.Equal(t, 3, s.AcceptSeq(func(rune) bool { return true }))
			require.Equal(t, "abc", s.Text())
			require.ErrorIs(t, s.Err(), io.ErrUnexpectedEOF)
			require.ErrorIs(t, s.ScanRune(), io.ErrUnexpectedEOF)
			require.False(t, s.EOF())
		})
	}
}

func TestScannerInvalidUTF8IsNotEOF(t *testing.T) {
	for name, s := range map[string]*Scanner{
		"string": NewScannerString("invalid.lisp", "\xff"),
		"reader": NewScanner("invalid.lisp", strings.NewReader("\xff")),
	} {
		t.Run(name, func(t *testing.T) {
			err := s.ScanRune()
			require.ErrorContains(t, err, "invalid utf-8")
			require.False(t, errors.Is(err, io.EOF))
			require.ErrorContains(t, s.Err(), "invalid utf-8")
		})
	}
}

type dataErrorReader struct {
	reads int
}

func (r *dataErrorReader) Read(p []byte) (int, error) {
	r.reads++
	if r.reads == 1 {
		return copy(p, ";abc"), errors.New("source read failed")
	}
	return copy(p, "\n(hidden)"), nil
}

func TestScanLinePreservesReadErrorWithData(t *testing.T) {
	r := &dataErrorReader{}
	s := NewScanner("read-error.lisp", r)
	_, err := s.ScanLine()
	require.ErrorContains(t, err, "source read failed")
	require.ErrorContains(t, s.Err(), "source read failed")
	require.Equal(t, 1, r.reads, "a terminal read error must not be overwritten by a retry")
}
