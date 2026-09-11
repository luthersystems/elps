package lexer

import (
	"errors"
	"io"
	"strings"
	"testing"

	"github.com/luthersystems/elps/parser/token"
	"github.com/stretchr/testify/require"
)

func TestScannerErrorsAreNotDropped(t *testing.T) {
	for name, src := range map[string]string{
		"symbol":      strings.Repeat("a", token.DefaultBufSize+10),
		"function":    "#'" + strings.Repeat("a", token.DefaultBufSize+10),
		"integer":     strings.Repeat("1", token.DefaultBufSize+10),
		"fraction":    "1." + strings.Repeat("1", token.DefaultBufSize+10),
		"exponent":    "1e" + strings.Repeat("1", token.DefaultBufSize+10),
		"octal":       "#o" + strings.Repeat("1", token.DefaultBufSize+10),
		"hexadecimal": "#x" + strings.Repeat("a", token.DefaultBufSize+10),
		"string":      `"` + strings.Repeat("a", token.DefaultBufSize+10) + `"`,
		"raw-string":  `"""` + strings.Repeat("a", token.DefaultBufSize+10) + `"""`,
	} {
		t.Run(name, func(t *testing.T) {
			lex := New(token.NewScanner("oversized.lisp", strings.NewReader(src)))
			tok := lex.ReadToken()[0]
			if strings.HasPrefix(src, "#") {
				tok = lex.ReadToken()[0]
			}
			require.Equal(t, token.ERROR, tok.Type)
			require.Contains(t, tok.Text, "token exceeds maximum allowable size")
			require.Equal(t, token.ERROR, lex.ReadToken()[0].Type, "scanner failures must remain terminal")
		})
	}
	for _, src := range []string{"#", "#\xff", ";\xff", "#!\xff", "abc\xff"} {
		t.Run(src, func(t *testing.T) {
			lex := New(token.NewScanner("invalid.lisp", strings.NewReader(src)))
			tok := lex.ReadToken()[0]
			if tok.Type == token.HASH_BANG {
				tok = lex.ReadToken()[0]
			}
			require.Equal(t, token.ERROR, tok.Type)
		})
	}
}

type failingReader struct{}

func (failingReader) Read([]byte) (int, error) { return 0, errors.New("source read failed") }

func TestCommentReadErrorsAreNotDropped(t *testing.T) {
	for _, prefix := range []string{";", "#!"} {
		t.Run(prefix, func(t *testing.T) {
			r := io.MultiReader(strings.NewReader(prefix+strings.Repeat(" ", 300000)), failingReader{})
			lex := New(token.NewScanner("broken.lisp", r))
			tok := lex.ReadToken()[0]
			if tok.Type == token.HASH_BANG {
				tok = lex.ReadToken()[0]
			}
			require.Equal(t, token.ERROR, tok.Type)
			require.Contains(t, tok.Text, "source read failed")
			require.Equal(t, 1, tok.Source.Line)
		})
	}
}
