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

// SortedMapFromPairs and MapIncr raise exactly what their Lisp spelling
// raises; compare them through builtins against the Lisp forms.
func TestSortedMapFromPairsAndMapIncrMatchLisp(t *testing.T) {
	env := newLimitTestEnv(t)
	env.AddBuiltins(false,
		&testBuiltinDef{name: "go-sorted-map", formals: lisp.Formals(lisp.VarArgSymbol, "kv"),
			fn: func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal { return lisp.SortedMapFromPairs(env, args.Cells...) }},
		&testBuiltinDef{name: "go-incr", formals: lisp.Formals("m", "k", "n"),
			fn: func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
				return env.MapIncr(args.Cells[0], args.Cells[1], args.Cells[2])
			}})
	require.NotEqual(t, lisp.LError, env.LoadString("test", `
(defun lisp-incr (m k n)
  (assoc! m k (+ (let ([x (get m k)]) (if (nil? x) 0 x)) n)))`).Type)
	pairs := [][2]string{
		{`(sorted-map "a" 1 'b 2 3 4)`, `(go-sorted-map "a" 1 'b 2 3 4)`},
		{`(sorted-map "a")`, `(go-sorted-map "a")`},
		{`(sorted-map 1.5 1)`, `(go-sorted-map 1.5 1)`},
		{`(sorted-map)`, `(go-sorted-map)`},
	}
	for _, setup := range []string{`(sorted-map "a" 1)`, `(sorted-map)`, `()`, `3`, `(sorted-map "a" "x")`, `(sorted-map "a" ())`} {
		for _, args := range []string{`"a" 2`, `"b" 2.5`, `"a" "y"`, `1.5 1`, `'a 1`} {
			pairs = append(pairs, [2]string{
				`(let ([m ` + setup + `]) (list (lisp-incr m ` + args + `) m))`,
				`(let ([m ` + setup + `]) (list (go-incr m ` + args + `) m))`,
			})
		}
	}
	for _, p := range pairs {
		want := env.LoadString("test", p[0])
		got := env.LoadString("test", p[1])
		require.Equal(t, want.Type, got.Type, "%s: %v vs %v", p[1], want, got)
		if want.Type == lisp.LError {
			assert.Equal(t, want.Str, got.Str, p[1])
			assert.Equal(t, (*lisp.ErrorVal)(want).ErrorMessage(), (*lisp.ErrorVal)(got).ErrorMessage(), p[1])
		} else {
			assert.Equal(t, want.String(), got.String(), p[1])
		}
	}
}
