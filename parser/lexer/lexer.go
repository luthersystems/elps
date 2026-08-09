// Copyright © 2018 The ELPS authors

package lexer

import (
	"errors"
	"fmt"
	"io"
	"strings"
	"unicode"

	"github.com/luthersystems/elps/parser/token"
)

type LexFn func(*Lexer) []*token.Token

const (
	miscWordRunes   = "0123456789" + miscWordSymbols
	miscWordSymbols = "._+-*/=<>!&~%?$"
)

type Lexer struct {
	scanner           *token.Scanner
	lex               LexFn
	precedingNewlines int
	precedingSpaces   int
}

func New(s *token.Scanner) *Lexer {
	lex := &Lexer{
		scanner: s,
		lex:     (*Lexer).readToken,
	}
	return lex
}

func (lex *Lexer) ReadToken() []*token.Token {
	return lex.lex(lex)
}

func (lex *Lexer) readToken() []*token.Token {
	lex.skipWhitespace()
	if !lex.scanner.Accept(func(c rune) bool { return true }) {
		if lex.scanner.EOF() {
			return lex.emit(token.EOF, "")
		}
		if err := lex.scanner.Err(); err != nil {
			return lex.emitError(err, false)
		}
		// The scanner could not produce a rune, is not at EOF, and reports no
		// read error.  Either the input holds a byte sequence that is not
		// valid UTF-8, or the current token has outgrown the scanner buffer.
		// ScanRune reports which, and for an invalid sequence it also consumes
		// the offending byte.
		//
		// Returning an ERROR token here is load-bearing.  Falling through to
		// the dispatch below re-examines the PREVIOUS rune without having
		// consumed any input, so the lexer re-emits the same zero-width token
		// forever.  Found by FuzzParseProgram: the four bytes "abc\xff" made
		// rdparser.ParseProgram append empty symbols until the process was
		// killed -- a remotely triggerable hang for any embedder that parses
		// untrusted source.  A wedged scanner keeps reporting ERROR on every
		// subsequent call, which is exactly what TokenStream requires.
		err := lex.scanner.ScanRune()
		if err == nil {
			err = errors.New("unable to scan rune in source text")
		}
		return lex.emitError(err, false)
	}
	switch lex.scanner.Rune() {
	case '(':
		return lex.charToken(token.PAREN_L)
	case ')':
		return lex.charToken(token.PAREN_R)
	case '[':
		return lex.charToken(token.BRACE_L)
	case ']':
		return lex.charToken(token.BRACE_R)
	case '\'':
		return lex.charToken(token.QUOTE)
	case ':':
		return lex.readSymbol()
	case ';':
		lex.scanner.AcceptSeq(func(c rune) bool { return c != '\n' })
		return lex.emitText(token.COMMENT)
	case '#':
		_ = lex.readChar()
		err := lex.scanner.Err()
		if err != nil {
			return lex.emitError(err, false)
		}
		switch lex.scanner.Rune() {
		case '!':
			tok := lex.emitText(token.HASH_BANG)
			lex.lex = (*Lexer).readHashBang
			// Deliberately NOT routed through emitMacroChar.  That guard
			// exists for #' and #^, which must be followed immediately by the
			// symbol or expression they apply to.  A hash-bang has no such
			// operand: readHashBang consumes the rest of the LINE, so an
			// empty shebang body ("#!\n", or "#! /usr/bin/env elps") is
			// well formed and the guard only rejected it spuriously.
			//
			// Found by FuzzFormat: the file "#!" (no trailing newline) parses,
			// but `elps fmt` normalises it to "#!\n", which the guard then
			// rejected -- the formatter turned a valid source file into one it
			// could not read back.
			return tok
		case '\'':
			tok := lex.emitText(token.FUN_REF)
			lex.lex = (*Lexer).readFunRef
			return lex.emitMacroChar(tok)
		case '^':
			tok := lex.emitText(token.UNBOUND)
			return lex.emitMacroChar(tok)
		case 'o', 'O':
			tok := lex.emitText(token.INT_OCTAL_MACRO)
			lex.lex = (*Lexer).readOctalLiteral
			return lex.emitMacroChar(tok)
		case 'x', 'X':
			tok := lex.emitText(token.INT_HEX_MACRO)
			lex.lex = (*Lexer).readHexLiteral
			return lex.emitMacroChar(tok)
		default:
			lex.scanner.Ignore()
			return lex.errorf("invalid dispatch macro character %q", lex.scanner.Rune())
		}
	case '-':
		// '-' is the subtraction/negation symbol on its own, and the sign of
		// a numeric or symbolic literal when something is glued to it.  It is
		// a plain SYMBOL whenever nothing CAN be glued to it: at whitespace,
		// at a closing bracket, and at end of input.
		//
		// Closing brackets and end of input were missing from that set, and
		// the omission was observable.  "(-- )" lexed as NEGATIVE + SYMBOL,
		// which ParseNegative merges into the single symbol "--"; the same
		// run written "(--)" lexed as NEGATIVE + NEGATIVE and parsed as TWO
		// symbols.  `elps fmt` does not write a space before ')', so it
		// silently rewrote the one-symbol form into the two-symbol form --
		// the formatter changing the program it was asked to tidy.  Found by
		// FuzzFormatCompact on "(------ )".
		if c, ok := lex.scanner.Peek(); !ok || unicode.IsSpace(c) || c == ')' || c == ']' {
			return lex.emitText(token.SYMBOL)
		}
		return lex.emitText(token.NEGATIVE)
	case '"':
		n := 0
		for lex.scanner.AcceptSeq(func(c rune) bool { return c != '"' && c != '\n' }) != 0 {
			n++
			if lex.scanner.Accept(func(c rune) bool { return c == '\n' }) {
				return lex.errorf("unterminated string literal")
			}
			// The run just accepted stopped at a '"' (or at EOF).  Whether
			// that quote CLOSES the string depends on how many backslashes
			// immediately precede it: an odd number means the last one
			// escapes the quote, an even number means they escape each other
			// and the quote is the terminator.
			//
			// The parity has to be counted, not read off the last rune.
			// Testing `Rune() == '\\'` treats every run ending in a backslash
			// as an escape, so "a\\" -- a string whose value is a single
			// backslash -- consumed its own closing quote and then ran off the
			// end of the input as an unterminated literal.  NO elps string
			// could end in a backslash.  Found by FuzzGeneratedPipeline.
			if trailingBackslashes(lex.scanner.Text())%2 == 1 {
				// Wait until parsing to check the escaped character
				if !lex.scanner.Accept(func(c rune) bool { return true }) {
					return lex.errorf("unterminated string literal %q", lex.peekRune())
				}
			}
		}
		if !lex.scanner.AcceptRune('"') {
			if lex.scanner.EOF() {
				return lex.errorf("unexpected EOF")
			}
			err := lex.scanner.Err()
			if err != nil {
				return lex.errorf("scan failure: %v", err)
			}
			return lex.errorf("unexpected rune %q", lex.peekRune())
		}
		if n > 0 {
			// This was a normal string
			return lex.emitText(token.STRING)
		}
		if !lex.scanner.AcceptRune('"') {
			// This is just an empty string -- not raw.
			return lex.emitText(token.STRING)
		}
		// This is a raw string
		for {
			_, ok := lex.scanner.AcceptString(`"""`)
			if ok {
				return lex.emitText(token.STRING_RAW)
			}
			if !lex.scanner.Accept(func(c rune) bool { return true }) {
				return lex.errorf("unterminated raw-string literal %q", lex.peekRune())
			}
		}
	default:
		if isDigit(lex.scanner.Rune()) {
			return lex.readNumber()
		}
		if isWordStart(lex.scanner.Rune()) {
			return lex.readSymbol()
		}
		err := fmt.Errorf("unexpected text starting with %q", lex.scanner.Rune())
		return lex.emit(token.INVALID, err.Error())
	}
}

func (lex *Lexer) resetState() {
	lex.lex = (*Lexer).readToken
}

func (lex *Lexer) emitMacroChar(tok []*token.Token) []*token.Token {
	if unicode.IsSpace(lex.peekRune()) {
		lex.resetState()
		return lex.errorf("whitespace following %s", tok[0].Text)
	}
	return tok
}

func (lex *Lexer) emit(typ token.Type, text string) []*token.Token {
	tok := []*token.Token{{
		Type:              typ,
		Text:              text,
		Source:            lex.scanner.LocStart(),
		PrecedingNewlines: lex.precedingNewlines,
		PrecedingSpaces:   lex.precedingSpaces,
	}}
	lex.scanner.Ignore()
	return tok
}

func (lex *Lexer) emitText(typ token.Type) []*token.Token {
	tok := lex.scanner.EmitToken(typ)
	tok.PrecedingNewlines = lex.precedingNewlines
	tok.PrecedingSpaces = lex.precedingSpaces
	return []*token.Token{tok}
}

func (lex *Lexer) emitError(err error, expectEOF bool) []*token.Token {
	if errors.Is(err, io.EOF) {
		if expectEOF {
			return lex.emit(token.EOF, "")
		}
		return lex.emit(token.ERROR, "unexpected EOF")
	}
	return lex.emit(token.ERROR, err.Error())
}

func (lex *Lexer) errorf(format string, v ...interface{}) []*token.Token {
	return lex.emitError(fmt.Errorf(format, v...), false)
}

func (lex *Lexer) charToken(typ token.Type) []*token.Token {
	tok := lex.scanner.EmitToken(typ)
	tok.PrecedingNewlines = lex.precedingNewlines
	tok.PrecedingSpaces = lex.precedingSpaces
	return []*token.Token{tok}
}

func (lex *Lexer) readHashBang() []*token.Token {
	lex.resetState()
	lex.scanner.AcceptSeq(func(c rune) bool { return c != '\n' })
	return lex.emitText(token.COMMENT)
}

func (lex *Lexer) readFunRef() []*token.Token {
	lex.resetState()
	// The operand of #' is a symbol, so its first rune has to be a symbol
	// START rune.  isWord is the CONTINUATION class and admits digits;
	// readSymbol gets away with using it because it is only ever entered
	// after isWordStart or ':' has already matched.  Reusing it for the first
	// rune here let "#'0" through as a symbol literally named "0" -- a name
	// nothing else in the language can write, and one that turns back into an
	// INTEGER the moment the form is printed in its longhand
	// "(lisp:function 0)".  Found by FuzzMinifySource on the input "0'0#'0".
	if !lex.scanner.Accept(isWordStart) {
		if lex.scanner.AcceptRune(':') {
			// A leading ':' yields a symbol the parser rejects with position
			// information; let it do that rather than reporting here.
			return lex.readSymbol()
		}
		// #' must name a function.  Without this guard the lexer emitted a
		// ZERO-LENGTH symbol and the parser built (lisp:function ||) -- a
		// reference to a symbol with no name.  emitMacroChar already rejected
		// "#' " (whitespace), so the hole was "#'" at end of input, and "#'"
		// followed by punctuation such as "#'(a b)", which parsed as an empty
		// function reference followed by an unrelated expression.
		// readOctalLiteral and readHexLiteral carry the same guard; this one
		// was simply missing.
		//
		// Found by FuzzFormat: `elps fmt` accepted "#'" and emitted "#'\n",
		// which the whitespace guard then rejected -- the formatter turned a
		// file it could read into one it could not.
		return lex.errorf("expected a symbol following #'")
	}
	lex.scanner.AcceptSeq(isWord)
	if lex.scanner.AcceptRune(':') {
		// This may produce an invalid symbol that should be detected during
		// parsing.
		return lex.readSymbol()
	}
	return lex.emitText(token.SYMBOL)
}

func (lex *Lexer) readSymbol() []*token.Token {
	lex.scanner.AcceptSeq(isWord)
	if lex.scanner.AcceptRune(':') {
		// This may produce an invalid symbol that should be detected during
		// parsing.
		return lex.readSymbol()
	}
	return lex.emitText(token.SYMBOL)
}

func (lex *Lexer) readOctalLiteral() []*token.Token {
	lex.resetState()
	n := lex.scanner.AcceptSeq(func(c rune) bool {
		return '0' <= c && c <= '7'
	})
	if n == 0 {
		return lex.errorf("invalid octal literal character: %q", lex.peekRune())
	}
	if unicode.IsDigit(lex.peekRune()) || isWord(lex.peekRune()) {
		return lex.errorf("invalid octal literal character: %q", lex.peekRune())
	}
	return lex.emitText(token.INT_OCTAL)
}

func (lex *Lexer) readHexLiteral() []*token.Token {
	lex.resetState()
	n := lex.scanner.AcceptSeq(func(c rune) bool {
		return isDigit(c) || ('a' <= c && c <= 'f') || ('A' <= c && c <= 'F')
	})
	if n == 0 {
		return lex.errorf("invalid hexidecimal literal character: %q", lex.peekRune())
	}
	if unicode.IsDigit(lex.peekRune()) || isWord(lex.peekRune()) {
		return lex.errorf("invalid hexidecimal literal character: %q", lex.peekRune())
	}
	return lex.emitText(token.INT_HEX)
}

func (lex *Lexer) readNumber() []*token.Token {
	// TODO: support octal and hex integer literals
	lex.scanner.AcceptSeqDigit() // the first digit already scanned
	switch {
	case lex.scanner.AcceptRune('.'):
		return lex.readFloatFraction()
	case lex.scanner.AcceptAny("eE"):
		return lex.readFloatExponent()
	default:
		return lex.emitText(token.INT)
	}
	// the returned string may not actually be a usable number (overflow), but
	// we can find that out at parse time -- not scan time.
}

func (lex *Lexer) readFloatFraction() []*token.Token {
	if lex.scanner.AcceptSeqDigit() == 0 {
		return lex.errorf("invalid floating point literal starting: %v", lex.scanner.Text())
	}
	switch {
	case lex.scanner.AcceptAny("eE"):
		return lex.readFloatExponent()
	default:
		return lex.emitText(token.FLOAT)
	}
}

// readFloatExponent scans the exponent part of a float literal, with the 'e'
// or 'E' already consumed by the caller.  The grammar is [+-]?digit+.
//
// Both callers used to consume one UNCONDITIONAL rune before getting here,
// which made the exponent require TWO characters: "1e5" was rejected as an
// invalid float while "1e55" was accepted, and so were "1e+5" and "1.5e-2"
// (the sign supplying the extra rune).  Rejecting a literal every other lisp
// accepts is a source-compatibility hole, not just a formatter bug -- elps
// could not read back a float it printed itself.  Found by
// FuzzGeneratedPipeline.
func (lex *Lexer) readFloatExponent() []*token.Token {
	lex.scanner.AcceptAny("+-") // optional sign
	if lex.scanner.AcceptSeqDigit() == 0 {
		return lex.errorf("invalid floating point literal starting: %v", lex.scanner.Text())
	}
	return lex.emitText(token.FLOAT)
}

// trailingBackslashes counts the backslashes at the end of s.
func trailingBackslashes(s string) int {
	n := 0
	for i := len(s) - 1; i >= 0 && s[i] == '\\'; i-- {
		n++
	}
	return n
}

func (lex *Lexer) skipWhitespace() {
	if lex.scanner.AcceptSeqSpace() > 0 {
		text := lex.scanner.Text()
		lex.precedingNewlines = strings.Count(text, "\n")
		if lex.precedingNewlines == 0 {
			lex.precedingSpaces = len(text)
		} else {
			lex.precedingSpaces = 0
		}
		lex.scanner.Ignore()
	} else {
		lex.precedingNewlines = 0
		lex.precedingSpaces = 0
	}
}

func (lex *Lexer) peekRune() rune {
	r, _ := lex.scanner.Peek()
	return r
}

func (lex *Lexer) readChar() error {
	_ = lex.scanner.ScanRune()
	return nil
}

func isWordStart(c rune) bool {
	return unicode.IsLetter(c) || strings.ContainsRune(miscWordSymbols, c)
}

func isWord(c rune) bool {
	return unicode.IsLetter(c) || strings.ContainsRune(miscWordRunes, c)
}

func isDigit(c rune) bool {
	return '0' <= c && c <= '9'
}
