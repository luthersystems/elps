// Copyright © 2024 The ELPS authors

package lsp

import (
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	protocol "github.com/tliron/glsp/protocol_3_16"
)

func TestSemanticTokensFull(t *testing.T) {
	s := testServer()

	t.Run("number tokens", func(t *testing.T) {
		doc := openDoc(s, "file:///test/numbers.lisp", "42")
		result, err := s.textDocumentSemanticTokensFull(mockContext(), &protocol.SemanticTokensParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: doc.URI},
		})
		require.NoError(t, err)
		require.NotNil(t, result)
		// Single token: deltaLine=0, deltaChar=0, length=2, type=number(9), mods=0
		require.Len(t, result.Data, 5)
		assert.Equal(t, protocol.UInteger(semTokenNumber), result.Data[3])
	})

	t.Run("string tokens", func(t *testing.T) {
		doc := openDoc(s, "file:///test/strings.lisp", `"hello"`)
		result, err := s.textDocumentSemanticTokensFull(mockContext(), &protocol.SemanticTokensParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: doc.URI},
		})
		require.NoError(t, err)
		require.NotNil(t, result)
		require.Len(t, result.Data, 5)
		assert.Equal(t, protocol.UInteger(semTokenString), result.Data[3])
	})

	t.Run("keyword special ops", func(t *testing.T) {
		doc := openDoc(s, "file:///test/defun.lisp", "(defun foo (x) x)")
		result, err := s.textDocumentSemanticTokensFull(mockContext(), &protocol.SemanticTokensParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: doc.URI},
		})
		require.NoError(t, err)
		require.NotNil(t, result)
		// Should have tokens for: defun, foo, x (param), x (ref).
		// At minimum 4 tokens = 20 integers.
		assert.GreaterOrEqual(t, len(result.Data), 15)
	})

	t.Run("function definition has definition modifier", func(t *testing.T) {
		doc := openDoc(s, "file:///test/funcdef.lisp", "(defun my-func () 1)")
		s.ensureAnalysis(doc)
		result, err := s.textDocumentSemanticTokensFull(mockContext(), &protocol.SemanticTokensParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: doc.URI},
		})
		require.NoError(t, err)
		require.NotNil(t, result)
		// Find the token for "my-func" — it should have the definition modifier.
		tokens := decodeTokens(result.Data)
		var found bool
		for _, tok := range tokens {
			if tok.tokenType == semTokenFunction && tok.modifiers&semModDefinition != 0 {
				found = true
				break
			}
		}
		assert.True(t, found, "expected a function token with definition modifier")
	})

	t.Run("nil doc returns nil", func(t *testing.T) {
		result, err := s.textDocumentSemanticTokensFull(mockContext(), &protocol.SemanticTokensParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: "file:///missing.lisp"},
		})
		require.NoError(t, err)
		assert.Nil(t, result)
	})

	t.Run("multiple lines delta encoding", func(t *testing.T) {
		src := "42\n\"hello\""
		doc := openDoc(s, "file:///test/multiline.lisp", src)
		result, err := s.textDocumentSemanticTokensFull(mockContext(), &protocol.SemanticTokensParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: doc.URI},
		})
		require.NoError(t, err)
		require.NotNil(t, result)
		require.Len(t, result.Data, 10) // 2 tokens
		// First token: line 0
		assert.Equal(t, protocol.UInteger(0), result.Data[0]) // deltaLine
		// Second token: line 1 (deltaLine = 1)
		assert.Equal(t, protocol.UInteger(1), result.Data[5]) // deltaLine
		assert.Equal(t, protocol.UInteger(0), result.Data[6]) // deltaChar (reset on new line)
	})
}

func TestDeltaEncode(t *testing.T) {
	tokens := []rawToken{
		{line: 0, startChar: 0, length: 3, tokenType: semTokenKeyword, modifiers: 0},
		{line: 0, startChar: 5, length: 4, tokenType: semTokenFunction, modifiers: semModDefinition},
		{line: 1, startChar: 2, length: 1, tokenType: semTokenVariable, modifiers: 0},
	}
	data := deltaEncode(tokens)
	require.Len(t, data, 15) // 3 tokens * 5

	// Token 1: deltaLine=0, deltaChar=0, len=3, keyword(6), mods=0
	assert.Equal(t, protocol.UInteger(0), data[0])
	assert.Equal(t, protocol.UInteger(0), data[1])
	assert.Equal(t, protocol.UInteger(3), data[2])
	assert.Equal(t, protocol.UInteger(semTokenKeyword), data[3])

	// Token 2: same line, deltaChar=5, len=4, function(4), mods=definition(1)
	assert.Equal(t, protocol.UInteger(0), data[5])
	assert.Equal(t, protocol.UInteger(5), data[6])
	assert.Equal(t, protocol.UInteger(4), data[7])
	assert.Equal(t, protocol.UInteger(semTokenFunction), data[8])
	assert.Equal(t, protocol.UInteger(semModDefinition), data[9])

	// Token 3: new line, deltaLine=1, deltaChar=2, len=1, variable(3), mods=0
	assert.Equal(t, protocol.UInteger(1), data[10])
	assert.Equal(t, protocol.UInteger(2), data[11])
	assert.Equal(t, protocol.UInteger(1), data[12])
	assert.Equal(t, protocol.UInteger(semTokenVariable), data[13])
}

func TestSemanticTokenLegend(t *testing.T) {
	legend := semanticTokenLegend()
	// Verify legend indices match our constants.
	assert.Equal(t, "namespace", legend.TokenTypes[semTokenNamespace])
	assert.Equal(t, "function", legend.TokenTypes[semTokenFunction])
	assert.Equal(t, "macro", legend.TokenTypes[semTokenMacro])
	assert.Equal(t, "keyword", legend.TokenTypes[semTokenKeyword])
	assert.Equal(t, "string", legend.TokenTypes[semTokenString])
	assert.Equal(t, "number", legend.TokenTypes[semTokenNumber])
	assert.Equal(t, "parameter", legend.TokenTypes[semTokenParameter])
	assert.Equal(t, "variable", legend.TokenTypes[semTokenVariable])
	assert.Equal(t, "comment", legend.TokenTypes[semTokenComment])
	assert.Equal(t, "type", legend.TokenTypes[semTokenType])
	assert.Equal(t, "operator", legend.TokenTypes[semTokenOperator])
}

// decodeTokens converts delta-encoded data back to raw tokens for testing.
func decodeTokens(data []protocol.UInteger) []rawToken {
	var tokens []rawToken
	prevLine := 0
	prevChar := 0
	for i := 0; i+4 < len(data); i += 5 {
		line := prevLine + int(data[i])
		char := int(data[i+1])
		if data[i] == 0 {
			char = prevChar + int(data[i+1])
		}
		tokens = append(tokens, rawToken{
			line:      line,
			startChar: char,
			length:    int(data[i+2]),
			tokenType: int(data[i+3]),
			modifiers: int(data[i+4]),
		})
		prevLine = line
		prevChar = char
	}
	return tokens
}
