// Copyright © 2024 The ELPS authors

package diagnostic

import (
	"bufio"
	"bytes"
	"fmt"
	"io"
	"os"
	"strings"
	"unicode/utf8"
)

// Renderer formats diagnostics as Rust-style annotated source snippets.
type Renderer struct {
	// Color controls ANSI color output. Default is ColorAuto.
	Color ColorMode

	// SourceReader reads source file contents. If nil, os.ReadFile is used.
	SourceReader func(string) ([]byte, error)
}

// Render writes a single diagnostic to w.
func (r *Renderer) Render(w io.Writer, d Diagnostic) error {
	p := choosePalette(r.Color, fileFromWriter(w))
	bw := bufio.NewWriter(w)
	ew := &errWriter{w: bw}

	// Header: "error: message" or "warning: message"
	r.writeHeader(ew, d, p)

	// Source spans
	for _, span := range d.Spans {
		r.writeSpan(ew, span, p)
	}

	// Notes
	for _, note := range d.Notes {
		ew.printf("   %s=%s note: %s\n", p.boldCyan, p.reset, note)
	}

	if ew.err != nil {
		return ew.err
	}
	return bw.Flush()
}

// RenderAll writes all diagnostics to w separated by blank lines.
func (r *Renderer) RenderAll(w io.Writer, diags []Diagnostic) error {
	for i, d := range diags {
		if i > 0 {
			if _, err := io.WriteString(w, "\n"); err != nil {
				return err
			}
		}
		if err := r.Render(w, d); err != nil {
			return err
		}
	}
	return nil
}

// errWriter wraps a writer and captures the first error, short-circuiting
// subsequent writes. This avoids checking every fmt.Fprintf return value.
type errWriter struct {
	w   io.Writer
	err error
}

func (ew *errWriter) printf(format string, a ...interface{}) {
	if ew.err != nil {
		return
	}
	_, ew.err = fmt.Fprintf(ew.w, format, a...)
}

func (ew *errWriter) print(s string) {
	if ew.err != nil {
		return
	}
	_, ew.err = io.WriteString(ew.w, s)
}

func (r *Renderer) writeHeader(ew *errWriter, d Diagnostic, p palette) {
	var sevColor, sevText string
	switch d.Severity {
	case SeverityError:
		sevColor = p.boldRed
		sevText = "error"
	case SeverityWarning:
		sevColor = p.yellow
		sevText = "warning"
	case SeverityNote:
		sevColor = p.boldCyan
		sevText = "note"
	}
	ew.printf("%s%s%s%s:%s %s%s%s\n",
		sevColor, p.bold, sevText, p.reset,
		p.reset,
		p.bold, d.Message, p.reset)
}

func (r *Renderer) writeSpan(ew *errWriter, span Span, p palette) {
	// Location line: "  --> file:line:col"
	loc := span.File
	if span.Line > 0 {
		loc = fmt.Sprintf("%s:%d", span.File, span.Line)
		if span.Col > 0 {
			loc = fmt.Sprintf("%s:%d:%d", span.File, span.Line, span.Col)
		}
	}
	ew.printf("  %s-->%s %s\n", p.boldBlue, p.reset, loc)

	// Try to read and display the source line
	source := r.readSourceLine(span.File, span.Line)
	if source == "" {
		// No source available — just show the location line with a gutter
		ew.printf("   %s|%s\n", p.boldBlue, p.reset)
		return
	}

	lineStr := fmt.Sprintf("%d", span.Line)
	pad := strings.Repeat(" ", len(lineStr))

	// Empty gutter line
	ew.printf(" %s%s |%s\n", p.boldBlue, pad, p.reset)

	// Source line with line number
	// Replace tabs with spaces for consistent alignment
	displaySource := strings.ReplaceAll(source, "\t", "    ")
	ew.printf(" %s%s |%s  %s\n", p.boldBlue, lineStr, p.reset, displaySource)

	// Underline
	col := span.Col
	endCol := span.EndCol
	if col <= 0 {
		col = 1
	}
	if endCol <= 0 {
		endCol = r.detectEndCol(source, col)
	}
	if endCol < col {
		endCol = col
	}
	underLen := endCol - col + 1

	// Account for tab expansion in positioning
	prefix := ""
	if col > 1 && col-1 <= len(source) {
		prefix = source[:col-1]
	}
	displayCol := displayWidth(prefix)

	underPad := strings.Repeat(" ", displayCol)
	underline := strings.Repeat("^", underLen)

	ew.printf(" %s%s |%s  %s%s%s%s", p.boldBlue, pad, p.reset, underPad, p.boldRed, underline, p.reset)
	if span.Label != "" {
		ew.printf(" %s%s%s", p.boldRed, span.Label, p.reset)
	}
	ew.print("\n")

	// Trailing gutter
	ew.printf(" %s%s |%s\n", p.boldBlue, pad, p.reset)
}

func (r *Renderer) readSourceLine(file string, line int) string {
	if line <= 0 || file == "" || file == "<native code>" {
		return ""
	}
	reader := r.SourceReader
	if reader == nil {
		reader = func(name string) ([]byte, error) {
			return os.ReadFile(name) //nolint:gosec // reads user-specified source files for display
		}
	}
	data, err := reader(file)
	if err != nil {
		return ""
	}
	scanner := bufio.NewScanner(bytes.NewReader(data))
	for i := 1; scanner.Scan(); i++ {
		if i == line {
			return scanner.Text()
		}
	}
	return ""
}

// detectEndCol scans from col to find the end of the current token.
func (r *Renderer) detectEndCol(source string, col int) int {
	if col <= 0 || col > len(source) {
		return col
	}
	end := col - 1 // 0-based
	for end < len(source) {
		ch, size := utf8.DecodeRuneInString(source[end:])
		if ch == ' ' || ch == '\t' || ch == ')' || ch == ']' || ch == '(' || ch == '[' {
			break
		}
		end += size
	}
	if end == col-1 {
		return col // single character
	}
	return end // convert back to 1-based end column
}

// displayWidth returns the display width of a string, expanding tabs to 4 spaces.
func displayWidth(s string) int {
	w := 0
	for _, ch := range s {
		if ch == '\t' {
			w += 4
		} else {
			w++
		}
	}
	return w
}

// fileFromWriter attempts to extract an *os.File from a writer for terminal
// detection. Returns nil if the writer is not backed by a file.
func fileFromWriter(w io.Writer) *os.File {
	if f, ok := w.(*os.File); ok {
		return f
	}
	return nil
}
