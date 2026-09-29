// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// rangeRender renders MapRange's walk the way MapEntries renders.
func rangeRender(m *lisp.LVal) string {
	var cells []*lisp.LVal
	m.MapRange(func(k lisp.MapKey, v *lisp.LVal) bool {
		cells = append(cells, lisp.QExpr([]*lisp.LVal{k.LVal(), v}))
		return true
	})
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
		assert.Equal(t, m.MapEntries().String(), rangeRender(m), src)
	}
}

// A custom backing is read through its Entries method.
func TestMapRangeCustomBacking(t *testing.T) {
	env := newLimitTestEnv(t)
	src := env.LoadString("test", `(sorted-map "b" 2 1 "one" 'a 1)`)
	custom := lisp.SortedMapFromData(lisp.NewMapData(wrappedMap{src.Map()}))
	assert.Equal(t, src.MapEntries().String(), rangeRender(custom))
}

type wrappedMap struct{ lisp.Map }

func TestMapRangeStopsAndNoAlloc(t *testing.T) {
	env := newLimitTestEnv(t)
	m := env.LoadString("test", `(sorted-map "a" 1 "b" 2 "c" 3 4 4)`)
	var seen []string
	m.MapRange(func(k lisp.MapKey, _ *lisp.LVal) bool {
		seen = append(seen, k.LVal().String())
		return len(seen) < 2
	})
	assert.Equal(t, []string{"4", `"a"`}, seen)

	sum := 0
	fn := func(_ lisp.MapKey, v *lisp.LVal) bool { sum += v.Int; return true }
	m.MapRange(fn) // warm the pool
	allocs := testing.AllocsPerRun(100, func() { m.MapRange(fn) })
	assert.Zero(t, allocs)
	// MapEntries, for comparison, allocates per call.
	assert.NotZero(t, testing.AllocsPerRun(100, func() { _ = m.MapEntries() }))
}

func TestMapRangeReentrant(t *testing.T) {
	env := newLimitTestEnv(t)
	m := env.LoadString("test", `(sorted-map "a" (sorted-map "x" 1) "b" (sorted-map "y" 2))`)
	var out []string
	m.MapRange(func(k lisp.MapKey, v *lisp.LVal) bool {
		v.MapRange(func(k2 lisp.MapKey, _ *lisp.LVal) bool {
			out = append(out, k.Str+k2.Str)
			return true
		})
		return true
	})
	assert.Equal(t, []string{"ax", "by"}, out)
}
