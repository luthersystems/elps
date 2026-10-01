// Copyright © 2026 The ELPS authors

package libjson_test

import (
	"math"
	"math/rand/v2"
	"strings"
	"testing"

	"github.com/luthersystems/elps/internal/fuzzval"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
	"github.com/stretchr/testify/require"
)

func TestTagComposition(t *testing.T) {
	env := newJSONEnv(t)
	rng := rand.New(rand.NewPCG(751, 2)) //nolint:gosec // G404: deterministic property inputs
	seeds := fuzzval.Seeds()
	for range 500 {
		b := make([]byte, 128)
		for i := range b {
			b[i] = byte(rng.Uint64() & 0xff)
		}
		seeds = append(seeds, b)
	}
	for _, seed := range seeds {
		v := fuzzval.New(seed, env).Value()
		tagged, err := libjson.Tag(v)
		fused, fusedErr := libjson.DumpTyped(v)
		if err != nil {
			require.Error(t, fusedErr)
			continue
		}
		require.NoError(t, fusedErr)
		plain, err := libjson.Dump(tagged, false)
		require.NoError(t, err)
		require.Equal(t, plain, fused)
		loaded := libjson.LoadWith(plain, libjson.LoadOpts{ExactIntegers: true, Strict: true})
		require.NotEqual(t, lisp.LError, loaded.Type, "%s: %v", plain, loaded)
		unfused, err := libjson.Untag(loaded)
		require.NoError(t, err)
		decoded, err := libjson.LoadTyped(fused)
		require.NoError(t, err)
		assertTagExact(t, v, unfused)
		assertTagExact(t, unfused, decoded)
		if c, err := libjson.Canonize(v); err == nil {
			tagged, err := libjson.Tag(c)
			require.NoError(t, err)
			assertTagExact(t, c, tagged)
		}
	}
}

func TestTagWholeFloatText(t *testing.T) {
	for _, tc := range []struct {
		f    float64
		text string
	}{
		{1, "~d1"}, {0, "~d0"}, {math.Copysign(0, -1), "~d-0"},
		{1e20, "~d100000000000000000000"}, {1e21, "~d1e+21"},
		{math.MaxFloat64, "~d1.7976931348623157e+308"},
	} {
		v, err := libjson.Tag(lisp.Float(tc.f))
		require.NoError(t, err)
		require.Equal(t, lisp.LString, v.Type)
		require.Equal(t, tc.text, v.Str)
		back, err := libjson.Untag(v)
		require.NoError(t, err)
		require.Equal(t, math.Float64bits(tc.f), math.Float64bits(back.Float))
	}
	for _, text := range []string{"~d", "~d1.0", "~d01", "~d1E+21", "~d1e21", "~dNaN", "~d0.5", "~unknown"} {
		_, err := libjson.Untag(lisp.String(text))
		require.Error(t, err, text)
	}
}

func TestStrictPlainLoad(t *testing.T) {
	for _, text := range []string{
		` {"a":1}`, `{"a":1} `, `{"a": 1}`, `{"b":1,"a":2}`, `{"a":1,"a":2}`,
		`"<"`, `"\u003C"`, `"\u0061"`, `"\/"`, `"\u000a"`, `"\ud800"`,
		`1.0`, `1.50`, `1E-7`, `1e-07`, `1e21`, `1e+20`,
	} {
		require.NotEqual(t, lisp.LError, libjson.Load([]byte(text), false).Type, text)
		require.Equal(t, lisp.LError, libjson.LoadWith([]byte(text), libjson.LoadOpts{Strict: true}).Type, text)
	}
	for _, text := range []string{`{"a":1,"b":2}`, `"\u003c\u0026\u2028"`, `"\n"`, `"é"`, `-0`, `1e+21`, `1e-7`, `9007199254740993`} {
		require.NotEqual(t, lisp.LError, libjson.LoadWith([]byte(text), libjson.LoadOpts{Strict: true}).Type, text)
	}
	env := newTypedTestEnv(t)
	for _, family := range []string{"bytes", "string", "message"} {
		src := `(json:load-` + family + ` (json:dump-` + family + ` 1) :strict true)`
		require.Equal(t, lisp.LFloat, env.LoadString("test", src).Type, src)
	}
}

func assertTagExact(t *testing.T, want, got *lisp.LVal) {
	t.Helper()
	require.Equal(t, want.Type, got.Type)
	switch want.Type {
	case lisp.LInt:
		require.Equal(t, want.Int, got.Int)
	case lisp.LFloat:
		if math.IsNaN(want.Float) {
			require.True(t, math.IsNaN(got.Float))
		} else {
			require.Equal(t, math.Float64bits(want.Float), math.Float64bits(got.Float))
		}
	case lisp.LString, lisp.LSymbol:
		require.Equal(t, want.Str, got.Str)
	case lisp.LBytes:
		require.Equal(t, string(want.Bytes()), string(got.Bytes()))
	case lisp.LSortMap:
		a, b := want.MapEntries(), got.MapEntries()
		require.Len(t, b.Cells, len(a.Cells))
		for i := range a.Cells {
			assertTagExact(t, a.Cells[i], b.Cells[i])
		}
	case lisp.LSExpr, lisp.LArray, lisp.LTaggedVal:
		require.Equal(t, want.Str, got.Str)
		require.Len(t, got.Cells, len(want.Cells))
		for i := range want.Cells {
			assertTagExact(t, want.Cells[i], got.Cells[i])
		}
	default:
		t.Fatalf("unexpected type %v", want.Type)
	}
}

func TestTagBuiltins(t *testing.T) {
	env := newTypedTestEnv(t)
	for _, src := range []string{
		`(json:tag '(1.0 :k sym "~s"))`,
		`(json:untag (json:tag '(1.0 :k sym "~s")))`,
		`(json:untag (json:load-string (json:dump-string (json:tag '(1.0 :k sym))) :strict true :exact-integers true))`,
	} {
		require.NotEqual(t, lisp.LError, env.LoadString("test", src).Type, src)
	}
	for _, src := range []string{
		`(json:untag "~d1.0")`, `(json:untag (vector "~#list" (vector)))`,
		`(json:load-string "1.0" :strict true)`, `(json:load-string "1.0" :typed true :strict false)`,
	} {
		require.Equal(t, lisp.LError, env.LoadString("test", src).Type, src)
	}
}

func TestUntagLimitsAndMalformedValues(t *testing.T) {
	_, err := libjson.Untag(lisp.String("abc"), libjson.WithTypedMaxBytes(4))
	require.ErrorIs(t, err, libjson.ErrTypedLimit)
	_, err = libjson.Untag(lisp.Vector([]*lisp.LVal{nil}))
	require.Error(t, err)
	_, err = libjson.Untag(lisp.Vector([]*lisp.LVal{lisp.String("~#array"), lisp.Vector([]*lisp.LVal{
		lisp.Vector([]*lisp.LVal{lisp.Int(1000000), lisp.Int(1000000)}), lisp.Vector(nil),
	})}))
	require.Error(t, err)
	env := newTypedTestEnv(t, lisp.WithMaxSteps(20))
	require.NotEqual(t, lisp.LError, env.Put(lisp.Symbol("input"), lisp.String(strings.Repeat("x", 40<<10))).Type)
	v := env.LoadString("test", `(json:tag input)`)
	require.Equal(t, lisp.LError, v.Type)
}
