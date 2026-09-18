// Copyright © 2026 The ELPS authors

package diagnostic

import (
	"bufio"
	"bytes"
	"io"
	"os"
	"strings"
)

func (ew *errWriter) step() bool {
	return ew.err == nil && (ew.session == nil || ew.session.Step())
}

func (ew *errWriter) repeat(s string, count int) {
	for i := 0; i < count && ew.step(); i++ {
		ew.print(s)
	}
}

func (ew *errWriter) displayWidth(s string) int {
	width := 0
	for _, ch := range s {
		if !ew.step() {
			return width
		}
		if ch == '\t' {
			width += tabWidth
		} else {
			width += runeCellWidth(ch)
		}
	}
	return width
}

// Source scanning shares the output session's work allowance. In particular,
// a late line in a huge file cannot cause an unbounded read or allocation.
func (r *Renderer) readSourceLineFor(ew *errWriter, file string, line int) string {
	if ew.session == nil {
		return r.readSourceLine(file, line)
	}
	s := ew.session
	if line <= 0 || file == "" || file == "<native code>" || !s.Step() {
		return ""
	}
	var input io.Reader
	if r.SourceReader != nil {
		// Custom callbacks own their allocation/cancellation policy. Only the
		// returned data's bounded scan is controlled by this session.
		data, err := r.SourceReader(file)
		if err != nil {
			return ""
		}
		input = bytes.NewReader(data)
	} else {
		f, err := os.Open(file) //nolint:gosec // user-specified diagnostic source
		if err != nil {
			return ""
		}
		defer f.Close() //nolint:errcheck // read-only diagnostic source
		input = f
	}
	// Scanner growth is capped before allocation, even for a huge source line.
	capacity := min(64*1024, s.Remaining())
	if capacity <= 0 {
		s.Stop()
		return ""
	}
	scanner := bufio.NewScanner(&sourceBudgetReader{s: s, r: input})
	scanner.Buffer(make([]byte, min(4096, capacity)), capacity)
	for i := 1; s.Step() && scanner.Scan(); i++ {
		if i == line {
			return scanner.Text()
		}
	}
	if scanner.Err() != nil {
		s.Stop()
	}
	return ""
}

type sourceBudgetReader struct {
	s *Session
	r io.Reader
}

func (r *sourceBudgetReader) Read(p []byte) (int, error) {
	if !r.s.Step() {
		return 0, io.EOF
	}
	n := min(len(p), 4096, r.s.work)
	if n <= 0 {
		r.s.Stop()
		return 0, io.EOF
	}
	read, err := r.r.Read(p[:n])
	r.s.work -= read
	return read, err
}

// Preserve source bytes while expanding tabs, with bounded intermediate writes.
func (ew *errWriter) sourceText(source string) {
	for len(source) > 0 && ew.step() {
		n := min(len(source), 4096)
		if i := strings.IndexByte(source[:n], '\t'); i >= 0 {
			ew.print(source[:i])
			ew.print(tabExpansion)
			source = source[i+1:]
		} else {
			ew.print(source[:n])
			source = source[n:]
		}
	}
}
