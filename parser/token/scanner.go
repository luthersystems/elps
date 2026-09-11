// Copyright © 2018 The ELPS authors

package token

import (
	"errors"
	"fmt"
	"io"
	"strings"
	"unicode"
	"unicode/utf8"
)

// Scanner facilitates construction of tokens from a byte stream (io.Reader).
type Scanner struct {
	r       io.Reader
	readErr error
	scanErr error
	path    string
	file    string
	peek    []Rune
	buf     []byte
	c       Rune

	linePos      int // totalPos at the first byte of the line
	startLine    int // line number at startLinePos
	startLinePos int // totalPos at the starting byte of the token
	start        int // start of the current token
	pos          int // index of ch, a utf-8 rune in input
	next         int // index of the rune following pos
	line         int // line number at linePos

	totalPos int
}

func newScannerBuf(file string, r io.Reader, buf []byte) *Scanner {
	s := &Scanner{
		file:      file,
		r:         r,
		buf:       buf,
		line:      1,
		startLine: 1,
	}
	s.fill(0)

	return s
}

// DefaultBufSize is the size of the sliding window NewScanner allocates.  It
// limits text retained between EmitToken or Ignore calls: the window never
// grows. ScanLine releases chunks as it reads, so comments can exceed this size.
const DefaultBufSize = 128 << 10

// NewScanner initializes and returns a new Scanner reading through a
// DefaultBufSize sliding window.
func NewScanner(file string, r io.Reader) *Scanner {
	buf := make([]byte, DefaultBufSize)
	return newScannerBuf(file, r, buf)
}

// NewScannerString initializes and returns a new Scanner reading src, sizing
// the sliding window to src rather than allocating the DefaultBufSize window
// NewScanner uses. The window can hold the complete source, so no token can
// overrun it.
//
// This exists because the fixed window is charged per SCANNER, not per byte
// scanned, and the parser re-reads short strings: readsBackAsSymbol scans each
// #' operand and each half of every package-qualified symbol to check that it
// reads back as a symbol (issue #319).  Through NewScanner that is 128KiB per
// check -- 2.6GB and 1.7s to parse 10k qualified symbols, which is both a
// pointless cost on ordinary source and an allocation amplification an
// attacker controls, in a parser whose job is to survive untrusted phylum
// source.
func NewScannerString(file, src string) *Scanner {
	s := newScannerBuf(file, strings.NewReader(src), make([]byte, len(src)))
	// The complete source is buffered even when the final read filled it exactly.
	s.readErr = io.EOF
	return s
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
// utf-8 sequence or EOF prevents further runes from being scanned Peek returns
// a false second value.  If Peek returns a false value the next call to
// s.ScanRune will return an error that reflects the cause.
func (s *Scanner) Peek() (rune, bool) {
	if s.scanErr != nil {
		return 0, false
	}
	if len(s.peek) > 0 {
		return s.peek[0].C, true
	}
	err := s.checkExtend()
	if err != nil {
		if !errors.Is(err, io.EOF) {
			s.scanErr = err
		}
		return 0, false
	}
	c, n := utf8.DecodeRune(s.buf[s.next:])
	peek := Rune{c, n}
	if peek.IsRuneError() {
		s.scanErr = fmt.Errorf("invalid utf-8 sequence in source text starting with byte %q", s.buf[s.next])
		return utf8.RuneError, false
	}
	s.peek = append(s.peek, peek)
	return c, true
}

// ScanRune attempts to scan a utf-8 rune from the input for inclusion in the
// current token.  If an error prevents a valid unicode rune from being scanned
// then an error will be returned.
func (s *Scanner) ScanRune() (scanErr error) {
	if s.scanErr != nil {
		return s.scanErr
	}
	defer func() {
		if scanErr != nil && !errors.Is(scanErr, io.EOF) {
			s.scanErr = scanErr
		}
	}()
	err := s.checkRuneError()
	if err != nil {
		return err
	}
	if len(s.peek) > 0 {
		s.scan(s.peek[0])
		// Drop the consumed rune by shifting the tail down one slot and
		// shortening the slice by one.  This keeps the backing array (and
		// its capacity), so the next Peek's append does not allocate.
		// Reslicing with s.peek[1:] instead would leave a one-element
		// buffer with both length AND capacity zero, and every rune of
		// source used to allocate a fresh backing array that way.  Unlike
		// a bare s.peek[:0] this stays correct if the buffer ever holds
		// more than one rune.
		kept := copy(s.peek, s.peek[1:])
		s.peek = s.peek[:kept]
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
		if s.readErr != nil && !errors.Is(s.readErr, io.EOF) {
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

// Err returns a scanning error, including errors encountered by Peek or the
// Accept helpers. Scanning errors are terminal. Input read errors are reported
// once the valid buffered runes preceding them have been consumed; EOF is nil.
func (s *Scanner) Err() error {
	if s.scanErr != nil {
		return s.scanErr
	}
	if s.readErr == nil {
		return nil
	}
	if errors.Is(s.readErr, io.EOF) {
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
	if !errors.Is(s.readErr, io.EOF) {
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
		return err == nil
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
		return err == nil
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
		return err == nil
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
		return err == nil
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
		return err == nil
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

// ScanLine consumes the rest of the current line, excluding the newline, and
// returns all text since the last EmitToken or Ignore. It releases scanned
// chunks so the line need not fit in the sliding window. The returned text
// uses memory proportional to the line length, for format-preserving callers.
// Callers must save LocStart before calling: consumed text is ignored.
func (s *Scanner) ScanLine() (string, error) {
	var text strings.Builder
	flush := func() {
		text.Write(s.buf[s.start:s.next])
		s.Ignore()
	}
	for {
		// Leave room to decode a complete UTF-8 rune across a window edge.
		if s.next-s.start >= len(s.buf)-utf8.UTFMax {
			flush()
		}
		c, ok := s.Peek()
		if !ok {
			if err := s.Err(); err != nil {
				return "", err
			}
			if !s.EOF() {
				return "", s.ScanRune()
			}
			break
		}
		if c == '\n' {
			break
		}
		if err := s.ScanRune(); err != nil {
			return "", err
		}
	}
	flush()
	return text.String(), nil
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
	if !s.c.IsRuneError() {
		return nil
	}
	// s.pos indexes the current rune within buf, but buf is a sliding window.
	// Ignore() moves start past the current rune, and the next extend() then
	// discards everything before start -- which leaves pos pointing before the
	// start of the buffer.  The offending byte is simply no longer buffered,
	// so report the error without quoting it rather than indexing out of
	// range.
	//
	// Found by FuzzParseProgramFaultTolerant on the two-byte input
	// "\xe4\xb8" (a truncated three-byte UTF-8 sequence): the second
	// ReadToken panicked with "index out of range [-1]".
	if s.pos < 0 || s.pos >= len(s.buf) {
		return errors.New("invalid utf-8 sequence in source text")
	}
	return fmt.Errorf("invalid utf-8 sequence in source text starting with byte %q", s.buf[s.pos])
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
		Line: s.startLine,
		Pos:  startPos,
		Col:  startPos - s.startLinePos + 1,
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
		Col:  s.totalPos - s.linePos + 1,
	}
}

func (s *Scanner) checkExtend() error {
	rem := len(s.buf) - s.next
	if rem < utf8.UTFMax {
		s.extend()
	}
	if s.next == len(s.buf) {
		if s.readErr != nil {
			return s.readErr
		}
		if len(s.buf) == 0 {
			return io.EOF
		}
		// If this is happening then we haven't seen EOF and the extension
		// routine was unable to do anything to extend the buffer.
		return errors.New("token exceeds maximum allowable size")
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
	if s.readErr != nil {
		s.buf = s.buf[:end]
		return
	}
	// Read directly so a reader's ErrUnexpectedEOF remains distinguishable
	// from a short final window ending in EOF. ReadFull synthesizes the former
	// from the latter and discards errors returned with a full buffer.
	for end < len(s.buf) {
		n, err := s.r.Read(s.buf[end:])
		end += n
		if err != nil {
			s.readErr = err
			break
		}
	}
	s.buf = s.buf[:end]
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
