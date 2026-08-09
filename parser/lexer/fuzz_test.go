// Copyright © 2026 The ELPS authors

package lexer_test

import (
	"bytes"
	"testing"
	"time"

	"github.com/luthersystems/elps/internal/fuzzseed"
	"github.com/luthersystems/elps/parser/lexer"
	"github.com/luthersystems/elps/parser/token"
)

// The lexer sits directly on the byte stream, so it is where undecodable input
// does its damage.  The invariant that matters is PROGRESS: rdparser.
// TokenSource pulls tokens in an unbounded loop and stops only on EOF or on a
// parse error raised from an ERROR/INVALID token, so a lexer that keeps
// returning some other token without consuming input never terminates.  That
// is exactly the bug FuzzParseProgram found on "abc\xff".
//
// TokenStream (parser/rdparser/source.go) states the contract this checks:
//
//	ReadToken returns a set of token from an input source.  When no more
//	tokens can be generated ReadToken returns a token with type token.EOF.
//	ReadToken never returns an empty slice.  In the presence of io errors a
//	TokenStream must return a token with type token.ERROR whenever called.

// maxTokenSlack bounds how many tokens the lexer may emit relative to the size
// of its input before the run is judged non-terminating.  Every token consumes
// at least one byte of input except for the single terminal EOF/ERROR, so
// len(src)+2 is already generous; the slack is there so a legitimate
// multi-token emission (emitMacroChar) can never trip it.
const maxTokenSlack = 8

// lexAll drains the lexer and returns the tokens read.  It fails the test if
// the lexer emits more tokens than the input can justify, which is how a
// non-consuming loop shows up.
func lexAll(t *testing.T, src []byte) []*token.Token {
	t.Helper()
	lx := lexer.New(token.NewScanner("fuzz", bytes.NewReader(src)))
	limit := len(src) + maxTokenSlack
	var out []*token.Token
	for {
		toks := lx.ReadToken()
		if len(toks) == 0 {
			t.Fatal("ReadToken returned an empty slice, violating the TokenStream contract")
		}
		for _, tok := range toks {
			if tok == nil {
				t.Fatal("ReadToken returned a nil token")
			}
			out = append(out, tok)
			if tok.Type == token.EOF || tok.Type == token.ERROR || tok.Type == token.INVALID {
				return out
			}
		}
		if len(out) > limit {
			t.Fatalf("lexer emitted %d tokens for %d bytes of input without terminating "+
				"(last token %v %q): it is not consuming input",
				len(out), len(src), out[len(out)-1].Type, out[len(out)-1].Text)
		}
	}
}

// FuzzLexer asserts the lexer terminates on every input and that its tokens
// are well formed.
func FuzzLexer(f *testing.F) {
	for _, src := range fuzzseed.All() {
		f.Add(src)
	}
	f.Fuzz(func(t *testing.T, src []byte) {
		toks := lexAll(t, src)
		for i, tok := range toks {
			if tok.Source == nil {
				t.Fatalf("token %d (%v %q) has no source location", i, tok.Type, tok.Text)
			}
			if tok.Source.Line < 1 {
				t.Fatalf("token %d (%v %q) has a non-positive line number %d",
					i, tok.Type, tok.Text, tok.Source.Line)
			}
		}
	})
}

// TestLexerTerminatesOnUndecodableInput pins the specific regression: bytes
// that are not valid UTF-8 must produce a terminal ERROR token rather than an
// endless run of zero-width tokens.  "abc\xff" is the minimised input for the
// hang; the rest are the neighbouring shapes (prefix position, inside a
// quote, after a sign, and a truncated multi-byte sequence).
func TestLexerTerminatesOnUndecodableInput(t *testing.T) {
	for _, src := range []string{
		"abc\xff",
		"\xff",
		"(\xff",
		"'\xff",
		"-\xff",
		"\xe4\xb8",
		"\xed\xa0\x80",
		"(a\xffb)",
	} {
		t.Run(src, func(t *testing.T) {
			done := make(chan []*token.Token, 1)
			go func() {
				done <- lexAll(t, []byte(src))
			}()
			select {
			case toks := <-done:
				last := toks[len(toks)-1]
				switch last.Type {
				case token.EOF, token.ERROR, token.INVALID:
				default:
					t.Fatalf("lexer stopped on %v, want EOF/ERROR/INVALID", last.Type)
				}
			case <-time.After(30 * time.Second):
				t.Fatal("lexer did not terminate")
			}
		})
	}
}

// TestLexerReportsErrorRepeatedly checks the other half of the TokenStream
// contract: once the scanner is wedged the lexer must keep returning ERROR.
// The recovering parser relies on this -- it re-reads after every error and
// would loop forever against a lexer that went quiet.
func TestLexerReportsErrorRepeatedly(t *testing.T) {
	lx := lexer.New(token.NewScanner("fuzz", bytes.NewReader([]byte("\xe4\xb8"))))
	for i := range 5 {
		toks := lx.ReadToken()
		if len(toks) == 0 {
			t.Fatalf("read %d: empty token slice", i)
		}
		switch toks[0].Type {
		case token.ERROR, token.INVALID, token.EOF:
		default:
			t.Fatalf("read %d: got %v %q, want a terminal token type",
				i, toks[0].Type, toks[0].Text)
		}
	}
}
