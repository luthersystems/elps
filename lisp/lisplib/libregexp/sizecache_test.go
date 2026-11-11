// Copyright © 2026 The ELPS authors

package libregexp_test

import (
	"fmt"
	"regexp"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libregexp"
)

// sizeCachePatterns are patterns of each kind the charge treats apart: a
// large program, an ordinary pattern with counted repetition, a long
// pattern without it, and a short one that never needs a size.
var sizeCachePatterns = []string{
	"(((a?){10}){10}){10}(((a){10}){10}){10}$",
	`^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$`,
	"^(" + strings.Repeat("ab|", 60) + "a)+$",
	`^a+$`,
}

// The size cache changes no charge: the first match of a pattern on a VM,
// which fills the cache, and every later match, which reads it, charge the
// steps of a compiled value of the pattern.  The charges are consensus
// facts for a host that meters steps.
func TestSizeCacheKeepsTheCharge(t *testing.T) {
	text := strings.Repeat("a", 3000)
	for _, patt := range sizeCachePatterns {
		env := chargeEnv(t)
		compiled := libregexp.BuiltinCompile(env, lisp.SExpr([]*lisp.LVal{lisp.String(patt)}))
		require.NotEqual(t, lisp.LError, compiled.Type, "%v", compiled)
		want := matchSteps(t, env, compiled, text)
		host := lisp.Native(regexp.MustCompile(patt))
		for i := range 3 {
			assert.Equal(t, want, matchSteps(t, env, lisp.String(patt), text), "%s, string, match %d", patt, i)
			assert.Equal(t, want, matchSteps(t, env, host, text), "%s, host, match %d", patt, i)
		}
		insts := libregexp.CachedSizes(env.Runtime)
		if libregexp.ProgramSize(patt) > 0 || len(patt) >= 128 || strings.Contains(patt, "{") {
			assert.Contains(t, insts, patt, "the size of %s was not cached", patt)
		} else {
			assert.NotContains(t, insts, patt, "a pattern that needs no size was cached")
		}
		patterns, _ := libregexp.CachedPatterns(env.Runtime)
		if libregexp.ProgramBytes(patt) <= libregexp.MaxEntryProgBytes {
			assert.True(t, patterns[patt], "the matcher of %s was not cached", patt)
		} else {
			assert.False(t, patterns[patt], "the matcher of %s is over the entry limit and was cached", patt)
		}
	}
}

// An invalid pattern is not cached, and its error is the same each time.
func TestSizeCacheKeepsTheError(t *testing.T) {
	env := chargeEnv(t)
	const bad = "a{2"
	call := lisp.SExpr([]*lisp.LVal{lisp.String("(" + bad), lisp.String("aa")})
	first := libregexp.BuiltinIsMatch(env, call)
	require.Equal(t, lisp.LError, first.Type)
	second := libregexp.BuiltinIsMatch(env, call)
	assert.Equal(t, first.String(), second.String())
	patterns, _ := libregexp.CachedPatterns(env.Runtime)
	assert.NotContains(t, patterns, "("+bad)
}

// With the pattern cached, a match of a host regexp or of a pattern string
// compiles nothing.
func TestSizeCacheSavesTheCompile(t *testing.T) {
	patt := sizeCachePatterns[1]
	env := chargeEnv(t)
	text := lisp.String("01234567-89ab-cdef-0123-456789abcdef")
	match := func(re *lisp.LVal) float64 {
		call := lisp.SExpr([]*lisp.LVal{re, text})
		require.Equal(t, "true", libregexp.BuiltinIsMatch(env, call).String())
		return testing.AllocsPerRun(50, func() { libregexp.BuiltinIsMatch(env, call) })
	}
	compile := testing.AllocsPerRun(50, func() { regexp.MustCompile(patt) })
	require.Greater(t, compile, 20.0, "the pattern compiles in too few allocations to tell")

	assert.Less(t, match(lisp.Native(regexp.MustCompile(patt))), 5.0, "a host regexp match compiled")
	assert.Less(t, match(lisp.String(patt)), 5.0, "a pattern string match compiled")
}

// The cache keeps at most SizeCacheBytes of patterns.  It is emptied when a
// new pattern would pass the bound, and a pattern longer than the bound is
// not kept.
func TestSizeCacheIsBounded(t *testing.T) {
	env := chargeEnv(t)
	pad := strings.Repeat("b", 1000)
	for i := range 2 * libregexp.SizeCacheBytes / 1000 {
		patt := fmt.Sprintf("a{%d}%s", i, pad)
		_ = matchSteps(t, env, lisp.String(patt), "a")
		patterns, n := libregexp.CachedPatterns(env.Runtime)
		require.LessOrEqual(t, n, libregexp.SizeCacheBytes)
		require.LessOrEqual(t, len(patterns), libregexp.SizeCacheEntries)
		require.Contains(t, patterns, patt)
		sum := 0
		for k := range patterns {
			sum += len(k)
		}
		require.Equal(t, sum, n, "the byte count does not match the keys")
	}
	long := "a{1}" + strings.Repeat("b", libregexp.SizeCacheBytes)
	_ = matchSteps(t, env, lisp.String(long), "a")
	patterns, _ := libregexp.CachedPatterns(env.Runtime)
	assert.NotContains(t, patterns, long)
}

// The cache keeps at most SizeCacheEntries patterns, however short.
func TestSizeCacheBoundsTheEntries(t *testing.T) {
	env := chargeEnv(t)
	for i := range 3 * libregexp.SizeCacheEntries {
		patt := fmt.Sprintf("^x%d$", i)
		_ = matchSteps(t, env, lisp.String(patt), "x")
		patterns, _ := libregexp.CachedPatterns(env.Runtime)
		require.LessOrEqual(t, len(patterns), libregexp.SizeCacheEntries)
		require.True(t, patterns[patt], "the newest pattern must be cached")
	}
}

// largeProgram returns a short pattern whose program is large: Unicode
// classes under a counted repeat.  i makes the patterns distinct.
func largeProgram(i, repeat int) string {
	return fmt.Sprintf(`x%d(\pL\pN){%d}`, i, repeat)
}

// A pattern whose program is over the entry limit is never kept, however
// short its text: the cache would keep memory the VM's allocation limit
// does not see.  Its charge and its result stay those of a compiled value.
func TestSizeCacheSkipsALargeProgram(t *testing.T) {
	patt := largeProgram(0, 1000)
	require.Less(t, len(patt), 24, "the pattern must be short")
	require.Greater(t, libregexp.ProgramBytes(patt), libregexp.MaxEntryProgBytes,
		"the pattern must compile to a program over the entry limit")
	env := chargeEnv(t)
	compiled := libregexp.BuiltinCompile(env, lisp.SExpr([]*lisp.LVal{lisp.String(patt)}))
	require.NotEqual(t, lisp.LError, compiled.Type, "%v", compiled)
	text := "x0" + strings.Repeat("a", 300)
	want := matchSteps(t, env, compiled, text)
	for i := range 3 {
		assert.Equal(t, want, matchSteps(t, env, lisp.String(patt), text), "match %d", i)
	}
	patterns, _ := libregexp.CachedPatterns(env.Runtime)
	assert.False(t, patterns[patt], "a program over the entry limit was cached")
	assert.Zero(t, libregexp.CachedProgBytes(env.Runtime))
	assert.Contains(t, libregexp.CachedSizes(env.Runtime), patt, "its size is still cached")
}

// Many distinct short patterns with large programs keep at most
// MaxCacheProgBytes of programs in all.
func TestSizeCacheBoundsTheProgramBytes(t *testing.T) {
	env := chargeEnv(t)
	repeat := 100
	size := libregexp.ProgramBytes(largeProgram(0, repeat))
	require.LessOrEqual(t, size, libregexp.MaxEntryProgBytes, "each program must be cached")
	require.Greater(t, size*libregexp.SizeCacheEntries, libregexp.MaxCacheProgBytes,
		"the entry bound alone must not bound these programs")
	for i := range libregexp.SizeCacheEntries {
		patt := largeProgram(i, repeat)
		_ = matchSteps(t, env, lisp.String(patt), "x")
		require.LessOrEqual(t, libregexp.CachedProgBytes(env.Runtime), libregexp.MaxCacheProgBytes)
		patterns, _ := libregexp.CachedPatterns(env.Runtime)
		require.True(t, patterns[patt], "the newest pattern must be cached")
		sum := 0
		for k, hasRe := range patterns {
			if hasRe {
				sum += libregexp.ProgramBytes(k)
			}
		}
		require.Equal(t, sum, libregexp.CachedProgBytes(env.Runtime), "the program bytes do not match the entries")
	}
}
