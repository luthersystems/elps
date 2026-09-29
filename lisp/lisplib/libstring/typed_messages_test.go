// Copyright © 2026 The ELPS authors

package libstring_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// split and repeat decode their arguments with lisp.Func2; their messages,
// results and check order are pinned exactly as they were written by hand.
func TestSplitRepeatTypedDecoders(t *testing.T) {
	env := newAffixEnv(t)
	for src, want := range map[string]string{
		`(string:split 1 2)`:       "first argument is not a string: int",
		`(string:split "a,b" 2)`:   "second argument is not a string: int",
		`(string:split 'a ",")`:    "first argument is not a string: symbol",
		`(string:split "a,b" ",")`: `'("a" "b")`,
		`(string:split "ab" "")`:   `'("a" "b")`,
		`(string:repeat 1 2)`:      "first argument is not a string: int",
		`(string:repeat 1 2.0)`:    "first argument is not a string: int",
		`(string:repeat "a" 2.0)`:  "second argument is not an int: float",
		`(string:repeat "a" -1)`:   "count is negative: -1",
		`(string:repeat "ab" 3)`:   `"ababab"`,
		`(string:repeat "ab" 1)`:   `"ab"`,
	} {
		v := env.LoadString("test", src)
		if v.Type == lisp.LError {
			assert.Equal(t, want, (*lisp.ErrorVal)(v).ErrorMessage(), src)
			continue
		}
		require.NotEqual(t, lisp.LError, v.Type)
		assert.Equal(t, want, v.String(), src)
	}
}
