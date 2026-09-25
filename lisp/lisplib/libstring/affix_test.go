// Copyright © 2026 The ELPS authors

package libstring_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libstring"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func newAffixEnv(t *testing.T) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	require.NoError(t, lisp.GoError(lisp.InitializeUserEnv(env, lisp.WithReader(parser.NewReader()))))
	require.NoError(t, lisp.GoError(libstring.LoadPackage(env)))
	return env
}

var affixFunctions = []string{"has-prefix?", "has-suffix?", "contains?", "trim-prefix", "trim-suffix"}

// The affix functions accept only strings, exactly like split and trim: bytes,
// symbols, numbers and nil are rejected with the same messages.
func TestStringAffixArgumentErrors(t *testing.T) {
	for _, fn := range affixFunctions {
		for _, tc := range []struct{ args, want string }{
			{`1 "a"`, "first argument is not a string: int"},
			{`"a" 1`, "second argument is not a string: int"},
			{`(to-bytes "a") "a"`, "first argument is not a string: bytes"},
			{`"a" (to-bytes "a")`, "second argument is not a string: bytes"},
			{`'a "a"`, "first argument is not a string: symbol"},
			{`"a" ()`, "second argument is not a string: list"},
			{`1 2`, "first argument is not a string: int"},
			{`"a"`, "invalid number of arguments: 1"},
			{`"a" "b" "c"`, "invalid number of arguments: 3"},
		} {
			t.Run(fn+" "+tc.args, func(t *testing.T) {
				env := newAffixEnv(t)
				got := env.LoadString("affix.lisp", "(string:"+fn+" "+tc.args+")")
				require.Equal(t, lisp.LError, got.Type, "%v", got)
				assert.Contains(t, env.Render(got), "string:"+fn+": "+tc.want)
			})
		}
	}
}

func TestStringAffixResults(t *testing.T) {
	env := newAffixEnv(t)
	for _, tc := range []struct{ expr, want string }{
		{`(string:has-prefix? "abc" "ab")`, "true"},
		{`(string:has-prefix? "abc" "abcd")`, "false"},
		{`(string:has-suffix? "abc" "bc")`, "true"},
		{`(string:contains? "abc" "")`, "true"},
		{`(string:trim-prefix "aaa" "a")`, `"aa"`},
		{`(string:trim-suffix "aaa" "a")`, `"aa"`},
		// Byte semantics: a prefix that ends inside a multibyte rune still
		// matches, as in Go's strings package. "é" is c3 a9.
		{`(string:has-prefix? "é" "\xc3")`, "true"},
		{`(string:contains? "é" "\xa9")`, "true"},
		{`(string:trim-prefix "é" "\xc3")`, `"\xa9"`},
		// Precomposed é (U+00E9) and e + combining acute (U+0301) differ:
		// no normalization is applied.
		{`(string:contains? "José" "é")`, "false"},
	} {
		got := env.LoadString("affix.lisp", tc.expr)
		assert.Equal(t, tc.want, got.String(), tc.expr)
	}
}

// The predicates return the canonical true/false symbols.
func TestStringAffixPredicatesReturnBooleans(t *testing.T) {
	env := newAffixEnv(t)
	for _, fn := range affixFunctions[:3] {
		got := env.LoadString("affix.lisp", "(string:"+fn+` "abc" "")`)
		require.Equal(t, lisp.LSymbol, got.Type, "%v", got)
		assert.Equal(t, lisp.TrueSymbol, got.Str, fn)
		got = env.LoadString("affix.lisp", "(string:"+fn+` "" "abc")`)
		require.Equal(t, lisp.LSymbol, got.Type, "%v", got)
		assert.Equal(t, lisp.FalseSymbol, got.Str, fn)
	}
}
