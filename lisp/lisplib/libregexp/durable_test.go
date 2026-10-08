// Copyright © 2026 The ELPS authors

package libregexp_test

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
	"github.com/luthersystems/elps/lisp/lisplib/libregexp"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/require"
)

func durableTestEnv(t *testing.T) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	require.NoError(t, lisp.GoError(lisp.InitializeUserEnv(env)))
	require.NoError(t, lisp.GoError(lisplib.LoadLibrary(env)))
	require.NoError(t, lisp.GoError(env.InPackage(lisp.String(lisp.DefaultUserPackage))))
	return env
}

func matches(t *testing.T, env *lisp.LEnv, re *lisp.LVal, text string) bool {
	t.Helper()
	v := libregexp.BuiltinIsMatch(env, lisp.QExpr([]*lisp.LVal{re, lisp.String(text)}))
	require.NoError(t, lisp.GoError(v))
	return lisp.True(v)
}

// TestDurableRegexpCodec dumps and loads compiled regexps and checks that
// each loaded regexp has the same pattern and matches the same texts.
func TestDurableRegexpCodec(t *testing.T) {
	t.Parallel()
	env := durableTestEnv(t)
	reg, err := libjson.NewFrozenDurableRegistry(libregexp.DurableRegexpCodec)
	require.NoError(t, err)
	cases := []struct {
		pattern string
		texts   []string
	}{
		{`(?i)^ab+c$`, []string{"ABBC", "abc", "ac", "xabc"}},
		{`(?s)a.b`, []string{"a\nb", "ab", "axb"}},
		{`^[\p{Greek}]+é$`, []string{"αβγé", "abcé", "αβγe", ""}},
		{``, []string{"", "anything"}},
	}
	for _, c := range cases {
		re := libregexp.BuiltinCompile(env, lisp.QExpr([]*lisp.LVal{lisp.String(c.pattern)}))
		require.NoError(t, lisp.GoError(re))
		b, err := libjson.DumpDurable(env, re, reg)
		require.NoError(t, err)
		require.Contains(t, string(b), libregexp.DurableRegexpName)
		got, err := libjson.LoadDurable(env, b, reg)
		require.NoError(t, err)
		pattern := libregexp.BuiltinPattern(env, lisp.QExpr([]*lisp.LVal{got}))
		require.NoError(t, lisp.GoError(pattern))
		require.Equal(t, c.pattern, pattern.Str)
		for _, text := range c.texts {
			require.Equal(t, matches(t, env, re, text), matches(t, env, got, text), "pattern %q, text %q", c.pattern, text)
		}
	}
}

// TestDurableRegexpCodecRefusesBadPayload pins the errors of a payload that
// is not a string and of a pattern that does not compile.
func TestDurableRegexpCodecRefusesBadPayload(t *testing.T) {
	t.Parallel()
	env := durableTestEnv(t)
	_, err := libregexp.DurableRegexpCodec.Load(env, 1, lisp.Int(3))
	require.EqualError(t, err, "elps:regexp: payload is not a string")
	_, err = libregexp.DurableRegexpCodec.Load(env, 1, lisp.String("a("))
	require.ErrorContains(t, err, "elps:regexp: invalid pattern: error parsing regexp: missing closing )")
	_, err = libregexp.DurableRegexpCodec.Save(env, lisp.String("a"))
	require.EqualError(t, err, "elps:regexp: not a compiled regexp")
}

// TestDurableRegexpCodecChargesCompile pins that a load charges the
// compile as regexp-compile does: one step per complete KiB of pattern.
func TestDurableRegexpCodecChargesCompile(t *testing.T) {
	t.Parallel()
	env := durableTestEnv(t)
	pattern := lisp.String(strings.Repeat("a", 3*1024+5))
	used := func(f func()) int64 {
		env.Runtime.SetStepBudget(1000)
		f()
		_, n := env.Runtime.StepBudget()
		return n
	}
	load := used(func() {
		_, err := libregexp.DurableRegexpCodec.Load(env, 1, pattern)
		require.NoError(t, err)
	})
	compile := used(func() {
		require.NoError(t, lisp.GoError(libregexp.BuiltinCompile(env, lisp.QExpr([]*lisp.LVal{pattern}))))
	})
	require.Equal(t, int64(3), load)
	require.Equal(t, compile, load)
}
