// Copyright © 2026 The ELPS authors

package token

import (
	"bytes"
	"errors"
	"fmt"
	"io"
	"strings"
	"testing"

	"github.com/luthersystems/elps/internal/fuzzseed"
)

// scanWhole scans src as one token, rune by rune, and returns the token text
// and the error that ended the scan.
func scanWhole(s *Scanner) (string, error) {
	for {
		if err := s.ScanRune(); err != nil {
			return s.Text(), err
		}
	}
}

// TestNewScannerKnownLengthWindow pins that sizing the window to an in-memory
// reader's remaining bytes changes nothing observable: every size, including
// a single token spanning the whole source at and around the edges of the
// sized window and of DefaultBufSize, scans to the same text and the same
// terminating error as the DefaultBufSize window an opaque reader gets.
func TestNewScannerKnownLengthWindow(t *testing.T) {
	sizes := []int{0, 1, 3, minKnownLenBufSize - 2, minKnownLenBufSize - 1,
		minKnownLenBufSize, minKnownLenBufSize + 1, 200,
		DefaultBufSize - 2, DefaultBufSize - 1, DefaultBufSize, DefaultBufSize + 1}
	for _, n := range sizes {
		// A multi-byte rune at the end exercises the utf8.UTFMax slack.
		src := strings.Repeat("a", n)
		if n >= 2 {
			src = src[:n-2] + "é"
		}
		if len(src) != n {
			t.Fatalf("source length %d, want %d", len(src), n)
		}
		wantText, wantErr := scanWhole(NewScanner("t", struct{ io.Reader }{strings.NewReader(src)}))
		for name, r := range map[string]io.Reader{
			"strings": strings.NewReader(src),
			"bytes":   bytes.NewReader([]byte(src)),
		} {
			s := NewScanner("t", r)
			if n < DefaultBufSize {
				if want := max(n+1, minKnownLenBufSize); cap(s.buf) != want {
					t.Errorf("%s/%d: window %d bytes, want %d", name, n, cap(s.buf), want)
				}
			} else if cap(s.buf) != DefaultBufSize {
				t.Errorf("%s/%d: window %d bytes, want DefaultBufSize", name, n, cap(s.buf))
			}
			text, err := scanWhole(s)
			if text != wantText {
				t.Errorf("%s/%d: scanned %d bytes, want %d", name, n, len(text), len(wantText))
			}
			if (err == nil) != (wantErr == nil) || (err != nil && err.Error() != wantErr.Error()) {
				t.Errorf("%s/%d: error %v, want %v", name, n, err, wantErr)
			}
			if n < DefaultBufSize && !errors.Is(err, io.EOF) {
				t.Errorf("%s/%d: error %v, want io.EOF", name, n, err)
			}
		}
	}
}

// TestNewScannerKnownLengthPartiallyRead sizes the window from the unread
// remainder, not the reader's total size.
func TestNewScannerKnownLengthPartiallyRead(t *testing.T) {
	r := strings.NewReader("skipped (a b)")
	if _, err := r.Seek(int64(len("skipped ")), io.SeekStart); err != nil {
		t.Fatal(err)
	}
	text, err := scanWhole(NewScanner("t", r))
	if text != "(a b)" || !errors.Is(err, io.EOF) {
		t.Fatalf("scanned %q, %v; want %q, EOF", text, err, "(a b)")
	}
}

// scanTrace drives s rune by rune, emitting a token at every space or newline
// and a ScanLine at every ';', and records every token, location and error.
func scanTrace(s *Scanner) []string {
	var out []string
	for {
		c, ok := s.Peek()
		if !ok {
			err := s.ScanRune()
			out = append(out, fmt.Sprintf("end %q %v eof=%t", s.Text(), err, s.EOF()))
			return out
		}
		switch c {
		case ';':
			text, err := s.ScanLine()
			out = append(out, fmt.Sprintf("line %q %v %+v", text, err, *s.LocStart()))
			s.Ignore()
		case ' ', '\n':
			if err := s.ScanRune(); err != nil {
				return append(out, fmt.Sprintf("err %v", err))
			}
			tok := s.EmitToken(SYMBOL)
			out = append(out, fmt.Sprintf("tok %q %+v", tok.Text, *tok.Source))
		default:
			if err := s.ScanRune(); err != nil {
				return append(out, fmt.Sprintf("err %q %v %+v", s.Text(), err, *s.Loc()))
			}
		}
	}
}

// TestNewScannerKnownLengthMatchesDefaultWindow is the differential form of
// the invariant NewScanner's sizing rests on: fill reslices the window to the
// bytes read, so right after construction an in-memory reader's scanner holds
// the same bytes and the same read error as a DefaultBufSize window's, and
// only the window's capacity differs.  Every fuzz seed must then scan to the
// same trace through both.
func TestNewScannerKnownLengthMatchesDefaultWindow(t *testing.T) {
	for i, src := range fuzzseed.All() {
		opaque := NewScanner("t", struct{ io.Reader }{bytes.NewReader(src)})
		for name, r := range map[string]io.Reader{
			"strings": strings.NewReader(string(src)),
			"bytes":   bytes.NewReader(src),
		} {
			s := NewScanner("t", r)
			if !bytes.Equal(s.buf, opaque.buf) || !errors.Is(s.readErr, opaque.readErr) {
				t.Fatalf("seed %d/%s: window holds %d bytes (err %v), default window %d (err %v)",
					i, name, len(s.buf), s.readErr, len(opaque.buf), opaque.readErr)
			}
		}
		want := scanTrace(NewScanner("t", struct{ io.Reader }{bytes.NewReader(src)}))
		for name, s := range map[string]*Scanner{
			"strings": NewScanner("t", strings.NewReader(string(src))),
			"bytes":   NewScanner("t", bytes.NewReader(src)),
		} {
			got := scanTrace(s)
			if len(got) != len(want) {
				t.Fatalf("seed %d/%s: %d trace entries, want %d", i, name, len(got), len(want))
			}
			for j := range got {
				if got[j] != want[j] {
					t.Fatalf("seed %d/%s: entry %d\n got %s\nwant %s", i, name, j, got[j], want[j])
				}
			}
		}
	}
}
