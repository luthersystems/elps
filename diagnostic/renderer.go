// Copyright © 2024 The ELPS authors

package diagnostic

import (
	"bufio"
	"bytes"
	"fmt"
	"io"
	"os"
	"strconv"
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

	lineStr := strconv.Itoa(span.Line)
	pad := strings.Repeat(" ", len(lineStr))

	// Empty gutter line
	ew.printf(" %s%s |%s\n", p.boldBlue, pad, p.reset)

	// Source line with line number
	// Replace tabs with spaces for consistent alignment
	displaySource := strings.ReplaceAll(source, "\t", tabExpansion)
	ew.printf(" %s%s |%s  %s\n", p.boldBlue, lineStr, p.reset, displaySource)

	// Underline.
	//
	// Both halves of it -- how far in the carets start, and how many there
	// are -- have to be measured in the SAME unit, and that unit has to be
	// TERMINAL CELLS, because a terminal is what lays the two lines out on
	// top of each other.  Issue #469 was that they were not: the indent was
	// computed with displayWidth (cells) and the length with `endCol - col +
	// 1` (bytes), so any span containing a multi-byte rune got more carets
	// than the text it pointed at and they ran on over what followed.  On
	// "(f 加算 1)" the two-rune token got six carets.
	//
	// Bytes are not the unit and neither are runes.  A rune count is right
	// for the common case only because most runes happen to occupy one cell;
	// an East Asian wide character occupies two and a combining mark
	// occupies none, so 加算 needs FOUR carets rather than two or six.  Both
	// measurements below therefore go through displayWidth, which is the one
	// place that decides what a cell is.
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

	// Byte indices into source: start inclusive, end exclusive.  Span.EndCol
	// is an INCLUSIVE 1-based byte column (see its doc comment), so the
	// exclusive end index is endCol itself.  Both are clamped because Col and
	// EndCol arrive from a caller and nothing upstream promises they are
	// inside this line.
	start, end := col-1, endCol
	if start > len(source) {
		start = len(source)
	}
	if end > len(source) {
		end = len(source)
	}
	if end < start {
		end = start
	}

	displayCol := displayWidth(source[:start])
	underLen := displayWidth(source[start:end])
	if underLen < 1 {
		// A zero-width span still has to point somewhere: a caret under the
		// start column is more use than a blank line.  Reachable when the
		// span names a position past the end of the line, and when the
		// spanned text is nothing but zero-width marks.
		underLen = 1
	}

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

const (
	// tabWidth is how many terminal cells a tab is rendered as.  It is a
	// choice, not a fact -- a real terminal advances to the next tab stop --
	// but the renderer rewrites tabs into spaces before printing the source
	// line, so within this package it becomes a fact.
	tabWidth = 4

	// tabExpansion is what writeSpan substitutes for a tab in the printed
	// source line.  It MUST be tabWidth spaces: the source line and the caret
	// line are laid out by two different pieces of code and only agree
	// because these two constants do.
	tabExpansion = "    "
)

// displayWidth returns how many terminal cells s occupies, with tabs counted
// as tabWidth.
//
// This is the single definition of "how wide is this text" for the renderer,
// and both halves of the caret underline go through it -- that is the whole
// of the fix for issue #469, which sized the underline in bytes while
// indenting it in cells.
//
// It counts CELLS and not runes.  The two agree for the Latin text that most
// diagnostics are about, which is why counting runes survived here as long as
// it did, but they part company in both directions: an East Asian wide
// character or an emoji occupies two cells, and a combining mark occupies
// none.  Under a rune count "加算" indents following text by two columns when
// the terminal has moved four, and "é" written as e + U+0301 indents by two
// when the terminal has moved one.
//
// Per-rune width comes from runeCellWidth in width.go, this package's
// replacement for the go-runewidth dependency (issue #516).  One consequence
// is deliberate and pinned by the golden tests: codepoints in the Unicode
// East Asian "Ambiguous" width class (→, ①, Greek and Cyrillic letters, box
// drawing) count as ONE cell.  go-runewidth's package-level default decided
// that from the environment -- its init reads LC_ALL/LC_CTYPE/LANG, and under
// an East Asian locale ambiguous codepoints become two cells -- so the same
// diagnostic would have been underlined differently depending on the locale
// of the shell that ran it.  Ambiguous-as-narrow is the choice essentially
// every modern terminal makes.
func displayWidth(s string) int {
	w := 0
	for _, ch := range s {
		if ch == '\t' {
			w += tabWidth
			continue
		}
		w += runeCellWidth(ch)
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
