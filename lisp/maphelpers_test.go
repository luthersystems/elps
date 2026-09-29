// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// keyLVal is the key as MapKeys would list it.
func keyLVal(k lisp.MapKey) *lisp.LVal {
	switch k.Type {
	case lisp.LInt:
		return lisp.Int(k.Int)
	case lisp.LSymbol:
		return lisp.Quote(lisp.Symbol(k.Str))
	default:
		return lisp.String(k.Str)
	}
}

// rangeRender renders MapRange's walk the way MapEntries renders.
func rangeRender(env *lisp.LEnv, m *lisp.LVal) string {
	var cells []*lisp.LVal
	if lerr := env.MapRange(m, func(k lisp.MapKey, v *lisp.LVal) bool {
		cells = append(cells, lisp.QExpr([]*lisp.LVal{keyLVal(k), v}))
		return true
	}); lerr.Type == lisp.LError {
		return lerr.String()
	}
	return lisp.QExpr(cells).String()
}

func TestMapRangeMatchesMapEntries(t *testing.T) {
	env := newLimitTestEnv(t)
	for _, src := range []string{
		`(sorted-map)`,
		`(sorted-map "b" 2 "a" 1 'c 3)`,
		`(sorted-map 3 "three" "x" 1 -1 "neg" 'sym 2 10 "ten")`,
		`(json:load-string "{\"z\": 1, \"a\": [1, 2], \"m\": {\"k\": null}}")`,
	} {
		m := env.LoadString("test", src)
		require.Equal(t, lisp.LSortMap, m.Type, src)
		assert.Equal(t, m.MapEntries().String(), rangeRender(env, m), src)
	}
}

// A custom backing is read through its Entries method.
func TestMapRangeCustomBacking(t *testing.T) {
	env := newLimitTestEnv(t)
	src := env.LoadString("test", `(sorted-map "b" 2 1 "one" 'a 1)`)
	custom := lisp.SortedMapFromData(lisp.NewMapData(wrappedMap{src.Map()}))
	assert.Equal(t, src.MapEntries().String(), rangeRender(env, custom))
}

type wrappedMap struct{ lisp.Map }

func TestMapRangeStopsAndNoAlloc(t *testing.T) {
	env := newLimitTestEnv(t)
	m := env.LoadString("test", `(sorted-map "a" 1 "b" 2 "c" 3 4 4)`)
	var seen []string
	env.MapRange(m, func(k lisp.MapKey, _ *lisp.LVal) bool {
		seen = append(seen, keyLVal(k).String())
		return len(seen) < 2
	})
	assert.Equal(t, []string{"4", `"a"`}, seen)

	sum := 0
	fn := func(_ lisp.MapKey, v *lisp.LVal) bool { sum += v.Int; return true }
	env.MapRange(m, fn) // warm the pool
	allocs := testing.AllocsPerRun(100, func() { env.MapRange(m, fn) })
	if !raceEnabled { // the race detector makes sync.Pool drop buffers
		assert.Zero(t, allocs)
	}
	// MapEntries, for comparison, allocates per call.
	assert.NotZero(t, testing.AllocsPerRun(100, func() { _ = m.MapEntries() }))
}

func TestMapRangeReentrant(t *testing.T) {
	env := newLimitTestEnv(t)
	m := env.LoadString("test", `(sorted-map "a" (sorted-map "x" 1) "b" (sorted-map "y" 2))`)
	var out []string
	env.MapRange(m, func(k lisp.MapKey, v *lisp.LVal) bool {
		env.MapRange(v, func(k2 lisp.MapKey, _ *lisp.LVal) bool {
			out = append(out, k.Str+k2.Str)
			return true
		})
		return true
	})
	assert.Equal(t, []string{"ax", "by"}, out)
}

// MapRange on a value keys would refuse raises keys' own error -- not a map,
// or a map larger than MaxAlloc -- instead of panicking, and calls fn for
// nothing.
func TestMapRangeErrorsMatchKeys(t *testing.T) {
	env := newLimitTestEnv(t)
	require.NotEqual(t, lisp.LError, env.LoadString("test", `(set 'big (sorted-map 1 1 2 2 3 3))`).Type)
	require.True(t, lisp.WithMaxAlloc(2)(env).IsNil())
	for _, src := range []string{`3`, `()`, `"s"`, `big`} {
		v := env.LoadString("test", src)
		require.NotEqual(t, lisp.LError, v.Type, src)
		want := env.LoadString("test", "(keys "+src+")")
		require.Equal(t, lisp.LError, want.Type, src)
		called := false
		got := env.MapRange(v, func(lisp.MapKey, *lisp.LVal) bool { called = true; return true })
		require.Equal(t, lisp.LError, got.Type, src)
		assert.Equal(t, (*lisp.ErrorVal)(want).ErrorMessage(), (*lisp.ErrorVal)(got).ErrorMessage(), src)
		assert.False(t, called, src)
	}
}
