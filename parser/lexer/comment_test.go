package lexer_test

import (
	"fmt"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
	"github.com/luthersystems/elps/parser/lexer"
	"github.com/luthersystems/elps/parser/rdparser"
	"github.com/luthersystems/elps/parser/token"
	"github.com/stretchr/testify/require"
)

// TestLongCommentsStayComments covers issue #657 / U1 through the lexer and
// each parser mode. A fixed expected AST catches code escaping the comment even
// when two parser modes would agree on the same corrupted input.
func TestLongCommentsStayComments(t *testing.T) {
	for _, prefix := range []string{";", "#!"} {
		for _, size := range []int{131071, 131072, 131073, 300000} {
			for _, ending := range []string{"", "\n", "\r\n"} {
				t.Run(fmt.Sprintf("%s/%d/%q", prefix, size, ending), func(t *testing.T) {
					const hidden = `(debug-print "EXECUTED-FROM-COMMENT" 1)`
					comment := prefix + strings.Repeat(" ", size-len(prefix)-len(hidden)) + hidden
					src := comment + ending
					if ending != "" {
						src += `(debug-print "normal" 2)` + "\n"
					}
					t.Run("lexer", func(t *testing.T) {
						lex := lexer.New(token.NewScanner("comment.lisp", strings.NewReader(src)))
						want := comment
						column := 1
						if prefix == "#!" {
							require.Equal(t, token.HASH_BANG, lex.ReadToken()[0].Type)
							want = want[2:]
							column = 3
						}
						if ending == "\r\n" {
							want += "\r"
						}
						tok := lex.ReadToken()[0]
						require.Equal(t, token.COMMENT, tok.Type)
						require.True(t, tok.Text == want, "comment text: got %d bytes, want %d", len(tok.Text), len(want))
						require.Equal(t, 1, tok.Source.Line)
						require.Equal(t, column, tok.Source.Col)
						next := lex.ReadToken()[0]
						if ending == "" {
							require.Equal(t, token.EOF, next.Type)
						} else {
							require.Equal(t, token.PAREN_L, next.Type)
							require.Equal(t, 2, next.Source.Line)
							require.Equal(t, 1, next.Source.Col)
							require.Equal(t, len(comment)+len(ending), next.Source.Pos)
						}
					})
					for _, mode := range []string{"strict", "fault-tolerant", "format-preserving"} {
						t.Run(mode, func(t *testing.T) {
							var exprs []*lisp.LVal
							switch mode {
							case "fault-tolerant":
								p := rdparser.New(token.NewScanner("comment.lisp", strings.NewReader(src)))
								result := p.ParseProgramFaultTolerant()
								require.Empty(t, result.Errors)
								exprs = result.Exprs
							default:
								reader := parser.NewReader()
								if mode == "format-preserving" {
									reader = parser.NewReader(parser.WithFormatPreserving())
								}
								var err error
								exprs, err = reader.Read("comment.lisp", strings.NewReader(src))
								require.NoError(t, err)
							}
							if ending == "" {
								require.Empty(t, exprs)
							} else {
								require.Len(t, exprs, 1)
								require.Equal(t, `(debug-print "normal" 2)`, exprs[0].String())
							}
						})
					}
				})
			}
		}
	}
}
