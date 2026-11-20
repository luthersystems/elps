// Copyright © 2026 The ELPS authors

package lsp

import (
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	protocol "github.com/tliron/glsp/protocol_3_16"
)

// TestSemanticTokensPositionEncoding checks that semantic token starts and
// lengths use the negotiated position encoding. Under UTF-16 a non-ASCII
// string must not move the tokens after it (luthersystems/elps#991).
func TestSemanticTokensPositionEncoding(t *testing.T) {
	type tok struct{ start, length int }
	cases := []struct {
		name string
		src  string
		enc  positionEncoding
		want []tok
	}{
		{"utf-16 two-byte rune", `(set s "café") (set x 1)`, encodingUTF16,
			[]tok{{1, 3}, {5, 1}, {7, 6}, {16, 3}, {20, 1}, {22, 1}}},
		{"utf-16 surrogate pair", `(set s "😀") (set x 1)`, encodingUTF16,
			[]tok{{1, 3}, {5, 1}, {7, 4}, {14, 3}, {18, 1}, {20, 1}}},
		{"utf-16 non-ASCII symbol", `(set ñame 1) (set x 2)`, encodingUTF16,
			[]tok{{1, 3}, {5, 4}, {10, 1}, {14, 3}, {18, 1}, {20, 1}}},
		{"utf-16 multi-line literal", "(f \"\"\"é\nb\"\"\" 1)", encodingUTF16,
			[]tok{{1, 1}, {3, 4}}},
		{"utf-16 quoted symbol", `(list 'é 'x)`, encodingUTF16,
			[]tok{{1, 4}, {7, 1}, {10, 1}}},
		{"utf-8 keeps byte columns", `(set s "café") (set x 1)`, encodingUTF8,
			[]tok{{1, 3}, {5, 1}, {7, 7}, {17, 3}, {21, 1}, {23, 1}}},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			s := testServer()
			s.posEncoding.Store(int32(c.enc))
			doc := openDoc(s, "file:///test/encoding.lisp", c.src)
			result, err := s.textDocumentSemanticTokensFull(mockContext(), &protocol.SemanticTokensParams{
				TextDocument: protocol.TextDocumentIdentifier{URI: doc.URI},
			})
			require.NoError(t, err)
			require.NotNil(t, result)
			var got []tok
			for _, rt := range decodeTokens(result.Data) {
				if rt.line == 0 {
					got = append(got, tok{rt.startChar, rt.length})
				}
			}
			assert.Equal(t, c.want, got)
		})
	}
}
