// Copyright © 2026 The ELPS authors

package diagnostic

import (
	"bytes"
	"fmt"
	"math/rand"
	"strings"
	"testing"
)

// TestSourceCacheMatchesScanner holds the line index to the bufio.Scanner
// result it replaced, for text with carriage returns, empty lines, a
// missing or present final newline and lines too long for the scanner
// (luthersystems/elps#981).
func TestSourceCacheMatchesScanner(t *testing.T) {
	r := rand.New(rand.NewSource(1)) //nolint:gosec // a repeatable test sequence
	pieces := []string{"a", "bc", "\n", "\r", "\r\n", "\t", "é", "\n\n"}
	long := strings.Repeat("x", maxIndexedLine+10)
	huge := strings.Repeat("y", 70000)
	for trial := range 3000 {
		var b strings.Builder
		for n := r.Intn(30); n > 0; n-- {
			b.WriteString(pieces[r.Intn(len(pieces))])
		}
		switch r.Intn(20) {
		case 0:
			b.WriteString(long)
		case 1:
			b.WriteString("\n" + huge + "\nafter\n")
		}
		data := []byte(b.String())
		rd := &Renderer{SourceReader: func(string) ([]byte, error) { return data, nil }}
		cache := sourceCache{}
		for line := 0; line <= bytes.Count(data, []byte{'\n'})+2; line++ {
			want := scanSourceLine(data, line)
			if line <= 0 {
				want = ""
			}
			if got := cache.line(rd, "f.lisp", line); got != want {
				t.Fatalf("trial %d line %d of %q: cache %q, scanner %q", trial, line, data, got, want)
			}
		}
	}
}

// TestRenderAllReadsEachFileOnce: RenderAll prints what Render prints for
// each diagnostic and reads a file once for all its spans.
func TestRenderAllReadsEachFileOnce(t *testing.T) {
	src := []byte("(defun f ()\n  (g 1))\n(f)\n")
	reads := 0
	rd := &Renderer{Color: ColorNever, SourceReader: func(string) ([]byte, error) {
		reads++
		return src, nil
	}}
	var diags []Diagnostic
	for i := 1; i <= 3; i++ {
		diags = append(diags, Diagnostic{
			Severity: SeverityError,
			Message:  fmt.Sprintf("problem %d", i),
			Spans:    []Span{{File: "a.lisp", Line: i, Col: 1}},
		})
	}
	var want bytes.Buffer
	for i, d := range diags {
		if i > 0 {
			want.WriteString("\n")
		}
		if err := rd.Render(&want, d); err != nil {
			t.Fatal(err)
		}
	}
	reads = 0
	var got bytes.Buffer
	if err := rd.RenderAll(&got, diags); err != nil {
		t.Fatal(err)
	}
	if got.String() != want.String() {
		t.Fatalf("RenderAll wrote\n%s\nRender wrote\n%s", got.String(), want.String())
	}
	if reads != 1 {
		t.Errorf("RenderAll read the source %d times, want 1", reads)
	}
}
