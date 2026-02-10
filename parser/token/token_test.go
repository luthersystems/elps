// Copyright © 2018 The ELPS authors

package token

import (
	"errors"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestTypeString(t *testing.T) {
	used := make(map[string]bool)
	for tok := Type(0); tok < numTokenTypes; tok++ {
		str := tok.String()
		t.Log(str)
		if str == "" {
			t.Errorf("token type %x has empty string value", tok)
			continue
		}
		if used[str] {
			t.Errorf("token type string used twice: %v", tok)
		}
		used[str] = true
	}
}

func TestLocationError_Unwrap(t *testing.T) {
	inner := errors.New("inner error")
	lerr := &LocationError{
		Err:    inner,
		Source: &Location{File: "test.lisp", Line: 1, Col: 1},
	}
	assert.Equal(t, inner, lerr.Unwrap())
	assert.True(t, errors.Is(lerr, inner))
}

func TestLocationError_Code(t *testing.T) {
	lerr := &LocationError{
		Err:    errors.New("bad syntax"),
		Source: &Location{File: "test.lisp", Line: 1, Col: 1},
		Code:   "parse-error",
	}
	assert.Equal(t, "parse-error", lerr.Code)
	assert.Contains(t, lerr.Error(), "bad syntax")
}

func TestLocationError_Error(t *testing.T) {
	lerr := &LocationError{
		Err:    errors.New("something"),
		Source: &Location{File: "test.lisp", Line: 5, Col: 3},
	}
	assert.Equal(t, "test.lisp:5:3: something", lerr.Error())
}

func TestLocation_EndFields_ZeroDefault(t *testing.T) {
	loc := &Location{File: "test.lisp", Line: 1, Col: 1}
	assert.Equal(t, 0, loc.EndPos)
	assert.Equal(t, 0, loc.EndLine)
	assert.Equal(t, 0, loc.EndCol)
	// String() is unchanged — only shows start position.
	assert.Equal(t, "test.lisp:1:1", loc.String())
}
