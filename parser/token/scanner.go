// Copyright © 2018 The ELPS authors

package token

import (
	"fmt"
	"io"
	"strings"
	"unicode"
	"unicode/utf8"
)

// Scanner facilitates construction of tokens from a byte stream (io.Reader).
type Scanner struct {
	file         string
	path         string
	totalPos     int
	linePos      int // totalPos at the first byte of the line
	line         int // line number at linePos
	startLinePos int // totalPos at the starting byte of the token
	startLine    int // line nuber at startLinePos

	r       io.Reader
	readErr error

	buf   []byte
	start int // start of the current token
	pos   int // index of ch, a utf-8 rune in input
	next  int // index of the rune following pos
	c     Rune
	peek  []Rune
}

func newScannerBuf(file string, r io.Reader, buf []byte) *Scanner {
	s := &Scanner{
		file: file,
		r:    r,
		buf:  buf,
		line: 1,
	}
	s.fill(0)

	return s
}

// NewScanner initializes and returns a new Scanner.
func NewScanner(file string, r io.Reader) *Scanner {
	buf := make([]byte, 128<<10)
	return newScannerBuf(file, r, buf)
}

// SetPath associates a physical location (e.g. filesystem path) with s to aid
// in debugging projects which scan many ungrouped files.
func (s *Scanner) SetPath(path string) {
	s.path = path
}

// EmitToken returns a token containing the text scanned since the last call to
// either EmitToken or Ignore.
func (s *Scanner) EmitToken(typ Type) *Token {
	tok := &Token{
		Type:   typ,
		Text:   s.Text(),
		Source: s.LocStart(),
	}
	s.Ignore()
	return tok
}

// Ignore causes the scanner to skip all text scanned since the last call to
// either EmitToken or Ignore.
func (s *Scanner) Ignore() {
	s.start = s.next
	s.startLine = s.line
	s.startLinePos = s.linePos
	if s.c.C == '\n' {
		s.startLine++
		s.startLinePos = s.totalPos + 1
	}
}

// Text returns a string containing text scanned since the last call to either
// EmitToken or Ignore.
func (s *Scanner) Text() string {
	return string(s.buf[s.start:s.next])
}

// Rune returns the current unicode rune that is being scanned.  The rune
// returned by Rune is the last rune in a token returned by EmitToken.
func (s *Scanner) Rune() rune {
	return s.c.C
}

// Peek returns the next rune to be scanned, if there are any.  If an invalid
// utf-8 sequence or EOF prevents futher runes from being scanned Peek returns
// a false second value.  If Peek returns a false value the next call to
// s.ScanRune will return an error that reflects of the cause.
func (s *Scanner) Peek() (rune, bool) {
	if len(s.peek) > 0 {
		return s.peek[0].C, true
	}
	err := s.checkExtend()
	if err != nil {
		return 0, false
	}
	c, n := utf8.DecodeRune(s.buf[s.next:])
	peek := Rune{c, n}
	if peek.IsRuneError() {
		return utf8.RuneError, false
	}
	s.peek = append(s.peek, peek)
	return c, true
}

// ScanRune attempts to scan a utf-8 rune from the input for inclusion in the
// current token.  If an error prevents a valid unicode rune from being scanned
// then an error will be returned.
func (s *Scanner) ScanRune() error {
	err := s.checkRuneError()
	if err != nil {
		return err
	}
	if len(s.peek) > 0 {
		s.scan(s.peek[0])
		s.peek = s.peek[1:]
		return s.checkRuneError()
	}
	err = s.checkExtend()
	if err != nil {
		return err
	}
	c, n := utf8.DecodeRune(s.buf[s.next:])
	s.scan(Rune{c, n})
	err = s.checkRuneError()
	if err != nil {
		// The UTF-8 sequence may be invalid due to a read error so we have to
		// check first.
		if s.readErr != nil {
			return s.readErr
		}
		return err
	}
	return nil
}

func (s *Scanner) scan(r Rune) {
	old := s.c
	s.c = r
	s.totalPos += old.N
	s.pos += old.N
	s.next += r.N
	if old.C == '\n' {
		s.line++
		s.linePos = s.totalPos
	}
}

// Err returns an error encountered during the last read on the input stream.
// Err will always return false while there are still buffered runes that need
// to be accepted.
func (s *Scanner) Err() error {
	if s.readErr == nil {
		return nil
	}
	if s.readErr == io.EOF {
		return nil
	}
	if len(s.buf) == s.next {
		// No buffer space remaining to be accepted
		return s.readErr
	}
	if len(s.buf)-s.next < utf8.UTFMax {
		c, n := utf8.DecodeRune(s.buf[s.next:])
		if c == utf8.RuneError && n == 1 {
			// Not possible to scan another valid rune -- possibly truncated
			// utf-8 sequence.
			return s.readErr
		}
	}
	// There are still runes to consume before the error needs to be reported.
	return nil
}

func (s *Scanner) EOF() bool {
	if len(s.buf) == 0 {
		return true
	}
	if s.readErr == nil {
		return false
	}
	if s.readErr != io.EOF {
		return false
	}
	return s.next >= len(s.buf)
}

func (s *Scanner) Accept(fn func(rune) bool) bool {
	peek, ok := s.Peek()
	if !ok {
		return false
	}
	if fn(peek) {
		err := s.ScanRune()
		if err != nil {
			return false
		}
		return true
	}
	return false
}

func (s *Scanner) AcceptRune(c rune) bool {
	peek, ok := s.Peek()
	if !ok {
		return false
	}
	if peek == c {
		err := s.ScanRune()
		if err != nil {
			return false
		}
		return true
	}
	return false
}

func (s *Scanner) AcceptDigit() bool {
	peek, ok := s.Peek()
	if !ok {
		return false
	}
	if '0' <= peek && peek <= '9' {
		err := s.ScanRune()
		if err != nil {
			return false
		}
		return true
	}
	return false
}

func (s *Scanner) AcceptSpace() bool {
	peek, ok := s.Peek()
	if !ok {
		return false
	}
	if unicode.IsSpace(peek) {
		err := s.ScanRune()
		if err != nil {
			return false
		}
		return true
	}
	return false
}

func (s *Scanner) AcceptAny(charset string) bool {
	if len(charset) == 1 {
		return s.AcceptRune(rune(charset[0]))
	}
	peek, ok := s.Peek()
	if !ok {
		return false
	}
	if strings.ContainsRune(charset, peek) {
		err := s.ScanRune()
		if err != nil {
			return false
		}
		return true
	}
	return false
}

func (s *Scanner) AcceptSeq(fn func(rune) bool) int {
	var n int
	for s.Accept(fn) {
		n++
	}
	return n
}

func (s *Scanner) AcceptSeqRune(c rune) int {
	var n int
	for s.AcceptRune(c) {
		n++
	}
	return n
}

func (s *Scanner) AcceptSeqAny(charset string) int {
	var n int
	for s.AcceptAny(charset) {
		n++
	}
	return n
}

func (s *Scanner) AcceptSeqDigit() int {
	var n int
	for s.AcceptDigit() {
		n++
	}
	return n
}

func (s *Scanner) AcceptSeqSpace() int {
	var n int
	for s.AcceptSpace() {
		n++
	}
	return n
}

func (s *Scanner) AcceptString(literal string) (int, bool) {
	var n int
	for _, c := range literal {
		if !s.AcceptRune(c) {
			return n, false
		}
		n++
	}
	return n, true
}

func (s *Scanner) checkRuneError() error {
	if s.c.IsRuneError() {
		return fmt.Errorf("invalid utf-8 sequence in source text starting with byte %q", s.buf[s.pos])
	}
	return nil
}

// LocStart returns a Location referencing the beginning of the current token,
// just beyond the end of the previous token.
func (s *Scanner) LocStart() *Location {
	startPos := s.totalPos - (s.pos - s.start)
	if s.start > s.pos {
		startPos = s.totalPos + s.c.N
	}
	return &Location{
		File: s.file,
		Path: s.path,
		Line: s.line,
		Pos:  startPos,
	}
}

// Loc returns a Location referencing the current scanner position, the last
// position of the current token.
func (s *Scanner) Loc() *Location {
	return &Location{
		File: s.file,
		Path: s.path,
		Line: s.line,
		Pos:  s.totalPos,
	}
}

func (s *Scanner) checkExtend() error {
	rem := len(s.buf) - s.next
	if rem < utf8.UTFMax {
		s.extend()
	}
	if len(s.buf) == 0 {
		return io.EOF
	}
	if s.next == len(s.buf) {
		// If this is happening then we haven't seen EOF and the extension
		// routine was unable to do anything to extend the buffer.
		return fmt.Errorf("token exceeds maximum allowable size")
	}
	return nil
}

func (s *Scanner) extend() bool {
	if s.start == 0 {
		return false
	}

	end := copy(s.buf, s.buf[s.start:])
	s.pos -= s.start
	s.next -= s.start
	s.start = 0

	s.fill(end)

	return true
}

func (s *Scanner) fill(end int) {
	if s.readErr == io.EOF {
		s.buf = s.buf[:end]
	}
	n, err := io.ReadFull(s.r, s.buf[end:])
	s.buf = s.buf[:end+n]
	if err == io.ErrUnexpectedEOF {
		return
	}
	s.readErr = err
}

// Rune contains a rune that read by Scanner during peeking operations.
type Rune struct {
	C rune
	N int
}

// IsRuneError returns true if Rune represents an invalid utf-8 sequence read
// by utf8.DecodeRune.
func (r Rune) IsRuneError() bool {
	return r.C == utf8.RuneError && r.N == 1
}
