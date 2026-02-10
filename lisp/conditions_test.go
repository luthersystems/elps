// Copyright © 2024 The ELPS authors

package lisp

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestErrorVal_Condition(t *testing.T) {
	lerr := Errorf("test error %d", 42)
	ev := (*ErrorVal)(lerr)
	assert.Equal(t, "error", ev.Condition())
}

func TestErrorVal_Condition_Custom(t *testing.T) {
	lerr := ErrorConditionf("my-condition", "something went wrong")
	ev := (*ErrorVal)(lerr)
	assert.Equal(t, "my-condition", ev.Condition())
}

func TestErrorVal_Condition_ParseError(t *testing.T) {
	lerr := ErrorConditionf(CondParseError, "unexpected token")
	ev := (*ErrorVal)(lerr)
	assert.Equal(t, CondParseError, ev.Condition())
}

func TestErrorVal_Condition_UnmatchedSyntax(t *testing.T) {
	lerr := ErrorConditionf(CondUnmatchedSyntax, "unclosed bracket")
	ev := (*ErrorVal)(lerr)
	assert.Equal(t, CondUnmatchedSyntax, ev.Condition())
}

// Verify condition constants match expected values.
func TestConditionConstants(t *testing.T) {
	assert.Equal(t, "parse-error", CondParseError)
	assert.Equal(t, "scan-error", CondScanError)
	assert.Equal(t, "unmatched-syntax", CondUnmatchedSyntax)
	assert.Equal(t, "mismatched-syntax", CondMismatchedSyntax)
	assert.Equal(t, "invalid-symbol", CondInvalidSymbol)
	assert.Equal(t, "invalid-octal-literal", CondInvalidOctalLiteral)
	assert.Equal(t, "invalid-hex-literal", CondInvalidHexLiteral)
	assert.Equal(t, "integer-overflow-error", CondOverflow)
}
