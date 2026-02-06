// Copyright © 2018 The ELPS authors

package lexer

import (
	"reflect"
	"strings"
	"testing"

	"github.com/luthersystems/elps/parser/token"
)

func TestLexer(t *testing.T) {
	tests := []struct {
		input  string
		tokens []*token.Token
	}{
		{``, []*token.Token{
			testToken(token.EOF, ""),
		}},
		{`abc`, []*token.Token{
			testToken(token.SYMBOL, "abc"),
			testToken(token.EOF, ""),
		}},
		{`=+()[]`, []*token.Token{
			testToken(token.SYMBOL, "=+"),
			testToken(token.PAREN_L, "("),
			testToken(token.PAREN_R, ")"),
			testToken(token.BRACE_L, "["),
			testToken(token.BRACE_R, "]"),
			testToken(token.EOF, ""),
		}},
		{`(#^-abc ''pkg:xyz')`, []*token.Token{
			testToken(token.PAREN_L, "("),
			testToken(token.UNBOUND, "#^"),
			testToken(token.NEGATIVE, "-"),
			testToken(token.SYMBOL, "abc"),
			testToken(token.QUOTE, "'"),
			testToken(token.QUOTE, "'"),
			testToken(token.SYMBOL, "pkg:xyz"),
			testToken(token.QUOTE, "'"),
			testToken(token.PAREN_R, ")"),
			testToken(token.EOF, ""),
		}},
		{`10 -5 0.1 0 12e12 12e-12 12.02E+5`, []*token.Token{
			testToken(token.INT, "10"),
			testToken(token.NEGATIVE, "-"),
			testToken(token.INT, "5"),
			testToken(token.FLOAT, "0.1"),
			testToken(token.INT, "0"),
			testToken(token.FLOAT, "12e12"),
			testToken(token.FLOAT, "12e-12"),
			testToken(token.FLOAT, "12.02E+5"),
			testToken(token.EOF, ""),
		}},
		{`"abc" "" """"""`, []*token.Token{
			testToken(token.STRING, `"abc"`),
			testToken(token.STRING, `""`),
			testToken(token.STRING_RAW, `""""""`),
			testToken(token.EOF, ""),
		}},
		{`"abc\n" "\x0a" """
"""`, []*token.Token{
			testToken(token.STRING, `"abc\n"`),
			testToken(token.STRING, `"\x0a"`),
			testToken(token.STRING_RAW, "\"\"\"\n\"\"\""),
			testToken(token.EOF, ""),
		}},
	}
testloop:
	for i, test := range tests {
		lex := New(token.NewScanner("", strings.NewReader(test.input)))
		var tokens []*token.Token
		numToken := 0
		for {
			toks := lex.ReadToken()
			if len(toks) != 1 {
				t.Fatalf("test %d: lexer returned %d tokens", i, len(toks))
			}
			tok := toks[0]
			tok.Source = nil
			tok.PrecedingSpaces = 0
			tokens = append(tokens, tok)
			if tok.Type == token.EOF || tok.Type == token.ERROR {
				break
			}
			numToken++
			if numToken > 100000 {
				t.Errorf("test %d: apparent infinite scanning loop", i)
				for _, tok := range tokens[len(tokens)-10:] {
					t.Log(tok)
				}
				continue testloop
			}
		}
		if !reflect.DeepEqual(tokens, test.tokens) {
			t.Errorf("test %d: unexpected tokens for input", i)
			t.Logf("source:\n\t%s", test.input)
			t.Logf("tokens:")
			for _, tok := range tokens {
				t.Logf("\t%v", tok)
			}
		}
	}
}

func testToken(typ token.Type, text string) *token.Token {
	return &token.Token{
		Type: typ,
		Text: text,
	}
}
