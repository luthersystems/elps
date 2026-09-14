// Copyright © 2026 The ELPS authors

package lexer

import (
	"fmt"
	"strings"
	"testing"

	"github.com/luthersystems/elps/parser/token"
	"github.com/stretchr/testify/require"
)

func TestLiteralSizeDiagnostics(t *testing.T) {
	for _, kind := range []string{"string", "raw", "symbol"} {
		for _, n := range []int{131071, 131072, 140000, 200000} {
			t.Run(fmt.Sprintf("%s/%d", kind, n), func(t *testing.T) {
				literal := strings.Repeat("a", n)
				typ := token.SYMBOL
				if kind == "string" {
					literal = `"` + literal + `"`
					typ = token.STRING
				}
				if kind == "raw" {
					literal = `"""` + literal + `"""`
					typ = token.STRING_RAW
				}
				lex := New(token.NewScanner("size.lisp", strings.NewReader(" \n  "+literal+" ")))
				tok := lex.ReadToken()[0]
				require.Equal(t, 2, tok.Source.Line)
				require.Equal(t, 3, tok.Source.Col)
				require.Equal(t, 4, tok.Source.Pos)
				if len(literal) < token.DefaultBufSize {
					require.Equal(t, typ, tok.Type)
					require.Equal(t, literal, tok.Text)
					require.Equal(t, token.EOF, lex.ReadToken()[0].Type)
				} else {
					require.Equal(t, token.ERROR, tok.Type)
					require.Equal(t, "token exceeds maximum allowable size (131072 bytes)", tok.Text)
				}
			})
		}
	}
	// Closing delimiters count toward the window, including all six raw quotes.
	for _, quote := range []string{`"`, `"""`} {
		literal := quote + strings.Repeat("a", token.DefaultBufSize-1-2*len(quote)) + quote
		lex := New(token.NewScanner("size.lisp", strings.NewReader(literal)))
		require.NotEqual(t, token.ERROR, lex.ReadToken()[0].Type)
		require.Equal(t, token.EOF, lex.ReadToken()[0].Type)
	}
}

func TestInvalidNumericLiteralDiagnostics(t *testing.T) {
	for _, literal := range []string{"0x10", "1_000", "1.2.3", "1e5x", "12abc", "1:foo", "1+2", "1日"} {
		t.Run(literal, func(t *testing.T) {
			lex := New(token.NewScanner("number.lisp", strings.NewReader("  "+literal+")")))
			tok := lex.ReadToken()[0]
			require.Equal(t, token.ERROR, tok.Type)
			require.Contains(t, tok.Text, fmt.Sprintf("invalid numeric literal %q", literal))
			if literal == "0x10" {
				require.Contains(t, tok.Text, "hex is spelled #x10")
			}
			require.Equal(t, 3, tok.Source.Col)
			require.Equal(t, token.PAREN_R, lex.ReadToken()[0].Type, "consume the entire bad literal")
		})
	}
}

func TestUnsupportedDispatchListsPrefixes(t *testing.T) {
	lex := New(token.NewScanner("dispatch.lisp", strings.NewReader("#b101")))
	tok := lex.ReadToken()[0]
	require.Equal(t, token.ERROR, tok.Type)
	require.Contains(t, tok.Text, "invalid dispatch macro character 'b'")
	for _, prefix := range []string{"#!", "#'", "#^", "#o", "#O", "#x", "#X"} {
		require.Contains(t, tok.Text, prefix)
	}
	require.Equal(t, 1, tok.Source.Col)
}

func TestBOMLocations(t *testing.T) {
	lex := New(token.NewScanner("bom.lisp", strings.NewReader("\ufeffx")))
	tok := lex.ReadToken()[0]
	require.Equal(t, token.SYMBOL, tok.Type)
	require.Equal(t, "x", tok.Text)
	require.Equal(t, 3, tok.Source.Pos)
	require.Equal(t, 4, tok.Source.Col)
	for _, src := range []string{" \ufeff", "\ufeff\ufeff", "x\ufeff", "\n\ufeff", "\"\ufeff\"", ";\ufeff"} {
		t.Run(fmt.Sprintf("%q", src), func(t *testing.T) {
			lex := New(token.NewScanner("bom.lisp", strings.NewReader(src)))
			tok := lex.ReadToken()[0]
			require.Equal(t, token.ERROR, tok.Type)
			require.Contains(t, tok.Text, "unexpected byte-order mark")
		})
	}
}

func TestLiteralSizeAtUTF8Boundary(t *testing.T) {
	for _, prefix := range []string{"", `"`, `"""`} {
		src := prefix + strings.Repeat("a", token.DefaultBufSize-len(prefix)-1) + "日" + prefix
		lex := New(token.NewScanner("utf8.lisp", strings.NewReader(src)))
		tok := lex.ReadToken()[0]
		require.Equal(t, token.ERROR, tok.Type)
		require.Equal(t, "token exceeds maximum allowable size (131072 bytes)", tok.Text)
		require.Equal(t, 1, tok.Source.Col)
	}
}

func TestRadixNumericSuffix(t *testing.T) {
	for _, literal := range []string{"#x10:foo", "#o17:bar", "#x10x", "#o178", "#X10:foo", "#O17:bar"} {
		t.Run(literal, func(t *testing.T) {
			lex := New(token.NewScanner("number.lisp", strings.NewReader(literal+")")))
			lex.ReadToken() // Dispatch prefix is a separate token.
			tok := lex.ReadToken()[0]
			require.Equal(t, token.ERROR, tok.Type)
			require.Contains(t, tok.Text, fmt.Sprintf("invalid numeric literal %q", literal))
			require.Equal(t, token.PAREN_R, lex.ReadToken()[0].Type)
		})
	}
	for _, src := range []string{"#x10 :foo", "#o17 :bar", "#x10 x", "#o17 8", "#x10", "#o17", "#xFF"} {
		t.Run(src, func(t *testing.T) {
			lex := New(token.NewScanner("number.lisp", strings.NewReader(src)))
			for tok := lex.ReadToken()[0]; tok.Type != token.EOF; tok = lex.ReadToken()[0] {
				require.NotEqual(t, token.ERROR, tok.Type)
			}
		})
	}
	lex := New(token.NewScanner("number.lisp", strings.NewReader("#x-1")))
	lex.ReadToken()
	require.Equal(t, token.ERROR, lex.ReadToken()[0].Type)
}
