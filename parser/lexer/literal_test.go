// Copyright © 2026 The ELPS authors

package lexer

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/parser/token"
)

// lexAll returns the token types and texts produced for src, stopping at EOF
// or the first ERROR/INVALID token.
func lexAll(t *testing.T, src string) ([]*token.Token, bool) {
	t.Helper()
	lex := New(token.NewScanner("test", strings.NewReader(src)))
	var out []*token.Token
	for range 64 {
		for _, tok := range lex.ReadToken() {
			switch tok.Type {
			case token.EOF:
				return out, true
			case token.ERROR, token.INVALID:
				return out, false
			}
			out = append(out, tok)
		}
	}
	t.Fatalf("lexer did not terminate on %q", src)
	return nil, false
}

// TestFloatExponentSingleDigit pins the exponent grammar, [+-]?digit+ after
// e/E.  readNumber and readFloatFraction each consumed one UNCONDITIONAL rune
// before handing over to readFloatExponent, which then demanded a sign-or-digit
// of its own, so an exponent needed TWO characters: "1e5" was a scan error
// while "1e55" and "1e+5" both lexed.  Every other lisp accepts "1e5", and elps
// itself prints floats in a form it could not read back.
//
// The rejections are pinned alongside the acceptances because the fix WIDENS
// the accepted language, and a fix that widens it too far is just as wrong.
func TestFloatExponentSingleDigit(t *testing.T) {
	accept := []struct {
		src  string
		text string
	}{
		{"1e5", "1e5"},
		{"2e3", "2e3"},
		{"1E5", "1E5"},
		{"1.5e2", "1.5e2"},
		{"1e+5", "1e+5"},
		{"1e-5", "1e-5"},
		{"1.5e-2", "1.5e-2"},
		{"1e55", "1e55"},
		{"0.0e0", "0.0e0"},
		{"1e308", "1e308"},
	}
	for _, tt := range accept {
		t.Run("accept/"+tt.src, func(t *testing.T) {
			toks, ok := lexAll(t, tt.src)
			if !ok {
				t.Fatalf("%q was rejected", tt.src)
			}
			if len(toks) != 1 || toks[0].Type != token.FLOAT || toks[0].Text != tt.text {
				t.Fatalf("%q lexed as %v, want a single FLOAT %q", tt.src, toks, tt.text)
			}
		})
	}

	reject := []string{"1e", "1e+", "1e-", "1.", "1.e5", "1.5e", "1.5e+"}
	for _, src := range reject {
		t.Run("reject/"+src, func(t *testing.T) {
			if _, ok := lexAll(t, src); ok {
				t.Fatalf("%q was accepted; it is not a valid float literal", src)
			}
		})
	}

	// Shapes that must keep SPLITTING rather than becoming one token.  This is
	// how the lexer has always treated a number glued to a word ("1abc" is INT
	// then SYMBOL), so "1e5e5" following suit is the consistent outcome.
	split := map[string][]string{
		"1e5e5":   {"1e5", "e5"},
		"1abc":    {"1", "abc"},
		"1.2.3e4": {"1.2", ".3e4"},
	}
	for src, want := range split {
		t.Run("split/"+src, func(t *testing.T) {
			toks, ok := lexAll(t, src)
			if !ok {
				t.Fatalf("%q was rejected, expected it to split into %v", src, want)
			}
			if len(toks) != len(want) {
				t.Fatalf("%q lexed into %d tokens, want %d (%v)", src, len(toks), len(want), toks)
			}
			for i := range want {
				if toks[i].Text != want[i] {
					t.Fatalf("%q token %d = %q, want %q", src, i, toks[i].Text, want[i])
				}
			}
		})
	}
}

// TestStringTrailingBackslash pins the escape-parity rule.  The lexer inferred
// "the next quote is escaped" from the LAST RUNE of a maximal non-quote run,
// so any run ending in a backslash consumed the following quote -- and no elps
// string could end in a backslash at all.  "a\\", whose value is a single
// backslash, ran off the end of the input as an unterminated literal.
//
// Parity has to be counted: an even number of trailing backslashes escape each
// other and leave the quote as the terminator.
func TestStringTrailingBackslash(t *testing.T) {
	accept := []string{
		`"a\\"`,        // value: a\
		`"\\"`,         // value: \
		`"\\\\"`,       // value: \\
		`"a\"b"`,       // an escaped quote still works
		`"a\\b"`,       //
		`"a\\\"b"`,     // backslash then escaped quote
		`"\\\\\\\\"`,   // four backslashes
		`"x\\" "y\\"`,  // two of them in a row
		`""`,           //
		`"\n"`,         //
		`"c:\\path\\"`, //
	}
	for _, src := range accept {
		t.Run("accept/"+src, func(t *testing.T) {
			if _, ok := lexAll(t, src); !ok {
				t.Fatalf("%q was rejected", src)
			}
		})
	}

	// Genuinely unterminated literals must STILL be rejected -- an odd number
	// of trailing backslashes really does escape the quote.
	reject := []string{
		`"`,
		`"abc`,
		`"a\`,
		`"a\"`,
		`"\\\"`,
		"\"a\nb\"",
	}
	for _, src := range reject {
		t.Run("reject/"+src, func(t *testing.T) {
			if _, ok := lexAll(t, src); ok {
				t.Fatalf("%q was accepted; it is not terminated", src)
			}
		})
	}

	// Token boundaries, not just acceptance: the whole point is that the
	// closing quote is recognised as the terminator rather than swallowed.
	toks, ok := lexAll(t, `"a\\" b`)
	if !ok {
		t.Fatal(`"a\\" b was rejected`)
	}
	if len(toks) != 2 || toks[0].Type != token.STRING || toks[0].Text != `"a\\"` || toks[1].Text != "b" {
		t.Fatalf(`"a\\" b lexed as %v, want STRING("a\\\\") then SYMBOL(b)`, toks)
	}
}
