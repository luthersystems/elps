// Copyright © 2026 The ELPS authors

package libjson_test

import (
	"context"
	"errors"
	"math"
	"math/rand/v2"
	"strconv"
	"strings"
	"testing"

	"github.com/luthersystems/elps/internal/fuzzval"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
	"github.com/stretchr/testify/require"
)

func canonMap(t *testing.T, kv ...*lisp.LVal) *lisp.LVal {
	t.Helper()
	m := lisp.SortedMap()
	for i := 0; i+1 < len(kv); i += 2 {
		require.NotEqual(t, lisp.LError, m.MapSetLVal(kv[i], kv[i+1]).Type)
	}
	return m
}

// The typed encoding distinguishes numeric types, sequence shapes and key
// types. Compare it as well as the root type, rather than Lisp equal?.
func canonExact(t *testing.T, want, got *lisp.LVal) {
	t.Helper()
	require.Equal(t, want.Type, got.Type)
	a, err := libjson.DumpTyped(want)
	require.NoError(t, err)
	b, err := libjson.DumpTyped(got)
	require.NoError(t, err)
	require.Equal(t, a, b)
}

func checkCanonizeInvariant(t *testing.T, v *lisp.LVal) bool {
	t.Helper()
	original, originalErr := libjson.Dump(v, false)
	c, err := libjson.Canonize(v)
	if err != nil {
		require.Nil(t, c)
		return false
	}
	b, err := libjson.DumpWith(v, libjson.DumpOpts{Canonize: true})
	require.NoError(t, err)
	require.NoError(t, originalErr)
	after, err := libjson.Dump(v, false)
	require.NoError(t, err)
	require.Equal(t, original, after, "canonize mutated its input")
	require.Equal(t, original, b, "adopting canonize changed dump bytes")
	typed, err := libjson.DumpWith(v, libjson.DumpOpts{Canonize: true, Typed: true})
	require.NoError(t, err)
	require.Equal(t, b, typed, "plain and typed canonical bytes differ")
	back := libjson.LoadWith(b, libjson.LoadOpts{ExactIntegers: true})
	canonExact(t, c, back)
	canonExact(t, c, libjson.LoadWith(original, libjson.LoadOpts{ExactIntegers: true}))
	back = libjson.LoadWith(b, libjson.LoadOpts{Typed: true})
	canonExact(t, c, back)
	again, err := libjson.Canonize(c)
	require.NoError(t, err)
	canonExact(t, c, again)
	for _, sn := range []bool{false, true} {
		a, err := libjson.Dump(v, sn)
		require.NoError(t, err)
		b, err := libjson.Dump(c, sn)
		require.NoError(t, err)
		require.Equal(t, a, b, "string-numbers byte guarantee")
		canonical, err := libjson.DumpWith(v, libjson.DumpOpts{Canonize: true, StringNumbers: sn})
		require.NoError(t, err)
		require.Equal(t, a, canonical, "canonical option string-numbers byte guarantee")
	}
	return true
}

func TestCanonizeMappings(t *testing.T) {
	env := newTypedTestEnv(t)
	for _, tc := range []struct{ src, want string }{
		{`'sym`, `"sym"`}, {`:kind`, `":kind"`}, {`'nil`, `"nil"`},
		{`'user:foo`, `"user:foo"`}, {`'true`, `true`}, {`'false`, `false`},
		{`()`, `()`}, {`json:null`, `()`}, {`(vector)`, `(vector)`},
		{`1.0`, `1`}, {`0.0`, `0`}, {`0.25`, `0.25`},
		{`'(a :b 1.0 ())`, `(vector "a" ":b" 1 ())`},
		{`(sorted-map 'b 1 :a (vector 2.0 "é😀"))`, `(sorted-map ":a" (vector 2 "é😀") "b" 1)`},
		{`(to-bytes "hi")`, `"aGk="`},
	} {
		t.Run(tc.src, func(t *testing.T) {
			v := env.LoadString("test", tc.src)
			require.NotEqual(t, lisp.LError, v.Type)
			c, err := libjson.Canonize(v)
			require.NoError(t, err)
			canonExact(t, env.LoadString("test", tc.want), c)
			require.True(t, checkCanonizeInvariant(t, v))
		})
	}
	for _, v := range []*lisp.LVal{
		lisp.Bytes(nil), lisp.Bytes([]byte{}),
		env.TaggedValue(lisp.Symbol("user:box"), lisp.QExpr([]*lisp.LVal{lisp.Int(1)})),
		lisp.Array(lisp.QExpr(nil), []*lisp.LVal{lisp.Int(7)}),
		lisp.Quote(lisp.Quote(lisp.Symbol("x"))),
		lisp.Native(map[string]any{"a": []any{"hello", true}}), //elpsvet:allow-native test input owns its map
	} {
		require.True(t, checkCanonizeInvariant(t, v))
	}
}

func TestCanonizeErrorsNameValueAndPath(t *testing.T) {
	for _, tc := range []struct {
		name                 string
		v                    *lisp.LVal
		message, value, path string
	}{
		{"leading tilde value", lisp.String("~text"), "leading ~", "~text", `$["box"][0]`},
		{"leading tilde symbol", lisp.Symbol("~sym"), "leading ~", "~sym", `$["box"][0]`},
		{"NaN", lisp.Float(math.NaN()), "nonfinite", "NaN", `$["box"][0]`},
		{"positive infinity", lisp.Float(math.Inf(1)), "nonfinite", "+Inf", `$["box"][0]`},
		{"negative infinity", lisp.Float(math.Inf(-1)), "nonfinite", "-Inf", `$["box"][0]`},
		{"negative zero", lisp.Float(math.Copysign(0, -1)), "negative zero", "-0", `$["box"][0]`},
		{"large integral float", lisp.Float(1<<53 + 2), "whole-number float", "9007199254740994", `$["box"][0]`},
		{"invalid UTF-8", lisp.String("\xff"), "UTF-8", `\xff`, `$["box"][0]`},
		{"surrogate UTF-8", lisp.String("\xed\xa0\x80"), "UTF-8", `\xed`, `$["box"][0]`},
		{"integer key", canonMap(t, lisp.Int(9), lisp.Int(1), lisp.Int(10), lisp.Int(2)), "int map key", "9", `$["box"][0]`},
		{"leading tilde key", canonMap(t, lisp.String("~key"), lisp.Int(1)), "leading ~", "~key", `$["box"][0]`},
		{"invalid UTF-8 key", canonMap(t, lisp.String("\xff"), lisp.Int(1)), "UTF-8", `\xff`, `$["box"][0]`},
		{"function", lisp.Fun("f", lisp.Formals(), nil), "unsupported", "function", `$["box"][0]`},
	} {
		t.Run(tc.name, func(t *testing.T) {
			v := canonMap(t, lisp.String("box"), lisp.Vector([]*lisp.LVal{tc.v}))
			c, err := libjson.Canonize(v)
			require.Nil(t, c)
			require.ErrorContains(t, err, tc.message)
			require.ErrorContains(t, err, tc.value)
			require.ErrorContains(t, err, tc.path)
		})
	}
	for _, n := range []int64{1<<53 + 1, -(1<<53 + 1), math.MaxInt64, math.MinInt64} {
		if n < math.MinInt || n > math.MaxInt {
			continue
		}
		_, err := libjson.Canonize(lisp.Int(int(n)))
		require.ErrorContains(t, err, "int magnitude")
		require.ErrorContains(t, err, strconv.FormatInt(n, 10))
		require.ErrorContains(t, err, "at $")
	}
}

func TestCanonizeStableConditionAndCaseData(t *testing.T) {
	cycle := lisp.Vector([]*lisp.LVal{nil})
	cycle.Cells[1].Cells[0] = cycle
	deep := lisp.Vector(nil)
	for range libjson.DefaultTypedMaxDepth {
		deep = lisp.Vector([]*lisp.LVal{deep})
	}
	cases := []struct {
		name, code, value string
		v                 *lisp.LVal
	}{
		{"leading tilde value", "leading-tilde", "~text", lisp.String("~text")},
		{"leading tilde key", "leading-tilde", "~key", canonMap(t, lisp.String("~key"), lisp.Int(1))},
		{"float range", "float-range", "100000000000000000000", lisp.Float(1e20)},
		{"negative zero", "negative-zero", "-0", lisp.Float(math.Copysign(0, -1))},
		{"NaN", "non-finite", "NaN", lisp.Float(math.NaN())},
		{"positive infinity", "non-finite", "+Inf", lisp.Float(math.Inf(1))},
		{"negative infinity", "non-finite", "-Inf", lisp.Float(math.Inf(-1))},
		{"invalid UTF-8", "invalid-utf8", `\xff`, lisp.String("\xff")},
		{"invalid UTF-8 key", "invalid-utf8", `\xff`, canonMap(t, lisp.String("\xff"), lisp.Int(1))},
		{"key collision", "key-collision", "a", canonHostMap(lisp.String("a"), lisp.Symbol("a"))},
		{"key order", "key-order", "a", canonHostMap(lisp.Symbol("z"), lisp.String("a"))},
		{"int key", "key-type", "9", canonMap(t, lisp.Int(9), lisp.Int(1))},
		{"unsupported key", "key-type", "float", canonHostMap(lisp.Float(1.5))},
		{"depth", "depth", "array", deep},
		{"cycle", "cycle", "array", cycle},
		{"unsupported", "unsupported", "function", lisp.Fun("f", lisp.Formals(), nil)},
		{"native number", "unsupported", "native number 1", lisp.Native(1)}, //elpsvet:allow-native test owns this native scalar
		{"limit", "limit", "bytes", lisp.String(strings.Repeat("x", 2000))},
	}
	if strconv.IntSize == 64 {
		n := int64(1<<53 + 1)
		cases = append(cases, struct {
			name, code, value string
			v                 *lisp.LVal
		}{"int range", "int-range", strconv.FormatInt(n, 10), lisp.Int(int(n))})
	} else {
		cases = append(cases, struct {
			name, code, value string
			v                 *lisp.LVal
		}{"platform float range", "float-range", "2147483648", lisp.Float(1 << 31)})
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			env := newTypedTestEnv(t)
			if tc.code == "limit" {
				require.NoError(t, lisp.GoError(lisp.WithMaxAlloc(100)(env)))
			}
			v := canonMap(t, lisp.String("box"), lisp.Vector([]*lisp.LVal{tc.v}))
			r := libjson.CanonizeBuiltin(env, lisp.SExpr([]*lisp.LVal{v}))
			require.Equal(t, lisp.LError, r.Type)
			require.False(t, lisp.IsInternalPanic(r))
			require.Equal(t, "json:canonize-error", r.Str)
			require.Len(t, r.Cells, 3, "handler arguments must be message, case, path")
			require.Equal(t, lisp.LString, r.Cells[0].Type)
			require.Contains(t, r.Cells[0].Str, tc.value)
			require.Equal(t, lisp.LSymbol, r.Cells[1].Type)
			require.Equal(t, ":"+tc.code, r.Cells[1].Str)
			require.Equal(t, lisp.LString, r.Cells[2].Type)
			require.Contains(t, r.Cells[2].Str, `$["box"][0]`)
			require.Contains(t, r.Cells[0].Str, r.Cells[2].Str)
		})
	}
	// Limits still wrap the sentinel for Go callers.
	_, err := libjson.Canonize(lisp.String("text"), libjson.WithTypedMaxBytes(2))
	require.ErrorIs(t, err, libjson.ErrTypedLimit)
}

func TestCanonizeIntegerEdgesAndFloatText(t *testing.T) {
	for _, n := range []int64{-(1 << 53), -(1 << 53) + 1, 1<<53 - 1, 1 << 53} {
		if n < math.MinInt || n > math.MaxInt {
			continue
		}
		require.True(t, checkCanonizeInvariant(t, lisp.Int(int(n))))
		require.True(t, checkCanonizeInvariant(t, lisp.Float(float64(n))))
	}
	for _, f := range []float64{1, -1, 0, 0.1, 1e-6, math.Nextafter(1e-6, 0), 1e-7, math.SmallestNonzeroFloat64, math.Nextafter(1, 0)} {
		require.True(t, checkCanonizeInvariant(t, lisp.Float(f)))
	}
	if strconv.IntSize == 32 {
		_, err := libjson.Canonize(lisp.Float(1 << 31))
		require.ErrorContains(t, err, "platform int")
	}
}

func TestCanonizeAdoptionASCIIStringKeys(t *testing.T) {
	v := canonMap(t, lisp.String("text"), lisp.String("<>&"), lisp.String("int"), lisp.Int(42),
		lisp.String("float"), lisp.Float(1.0), lisp.String("nested"),
		lisp.Vector([]*lisp.LVal{canonMap(t, lisp.String("x"), lisp.Float(0.1))}))
	require.True(t, checkCanonizeInvariant(t, v))
}

func TestCanonizeMixedKeysAndUTF8Order(t *testing.T) {
	v := canonMap(t, lisp.Symbol("z"), lisp.Int(1), lisp.Symbol(":a"), lisp.Int(2),
		lisp.String("a"), lisp.Int(3), lisp.Symbol("true"), lisp.Int(4),
		lisp.String("\ue000"), lisp.Int(5), lisp.String("𐀀"), lisp.Int(6))
	require.True(t, checkCanonizeInvariant(t, v))
	// assoc! may replace a key's type; conversion must use its actual text.
	env := newTypedTestEnv(t)
	v = env.LoadString("test", `(let ((m (sorted-map "a" 1))) (assoc! m 'a 2) (assoc! m :b 3) m)`)
	require.True(t, checkCanonizeInvariant(t, v))
}

// A host map can retain colliding key types or supply entries out of order,
// unlike the stock map, whose string/symbol namespace merges equal text.
type canonEntryMap struct{ pairs []*lisp.LVal }

func (m canonEntryMap) Len() int                              { return len(m.pairs) }
func (m canonEntryMap) Get(*lisp.LVal) (*lisp.LVal, bool)     { return lisp.Nil(), false }
func (m canonEntryMap) Set(*lisp.LVal, *lisp.LVal) *lisp.LVal { return lisp.Errorf("read-only") }
func (m canonEntryMap) Del(*lisp.LVal) *lisp.LVal             { return lisp.Errorf("read-only") }
func (m canonEntryMap) Keys() *lisp.LVal                      { return lisp.QExpr(nil) }
func (m canonEntryMap) Entries(buf []*lisp.LVal) *lisp.LVal {
	copy(buf, m.pairs)
	return lisp.Int(len(m.pairs))
}

func canonHostMap(keys ...*lisp.LVal) *lisp.LVal {
	pairs := make([]*lisp.LVal, len(keys))
	for i, k := range keys {
		pairs[i] = lisp.QExpr([]*lisp.LVal{k, lisp.Int(i)})
	}
	return lisp.SortedMapFromData(lisp.NewMapData(canonEntryMap{pairs: pairs}))
}

func TestCanonizeHostKeyCollisionAndOrder(t *testing.T) {
	for _, tc := range []struct {
		v             *lisp.LVal
		reason, value string
	}{
		{canonHostMap(lisp.String("a"), lisp.Symbol("a")), "collision", "a"},
		{canonHostMap(lisp.Symbol("z"), lisp.String("a")), "order", "a"},
		{canonHostMap(lisp.Int(10)), "int map key", "10"},
		{canonHostMap(lisp.Float(1.5)), "unsupported map key", "float"},
	} {
		_, err := libjson.Canonize(canonMap(t, lisp.String("box"), tc.v))
		require.ErrorContains(t, err, tc.reason)
		require.ErrorContains(t, err, tc.value)
		require.ErrorContains(t, err, `$["box"]`)
	}
	require.True(t, checkCanonizeInvariant(t, canonHostMap(lisp.Symbol(":a"), lisp.String("b"))))
}

func TestCanonizeNativeByteGuarantees(t *testing.T) {
	for _, v := range []any{int(1), float64(1), map[string]int{"a": 1}} {
		_, err := libjson.Canonize(lisp.Native(v)) //elpsvet:allow-native test owns each native input
		require.ErrorContains(t, err, "native number")
		require.ErrorContains(t, err, "string-numbers")
		require.ErrorContains(t, err, "at $")
	}
	for _, v := range []any{nil, true, "hello", []byte{0, 255}, []string{"a", "b"}, map[string]string{"a": "b"}} {
		require.True(t, checkCanonizeInvariant(t, lisp.Native(v))) //elpsvet:allow-native test owns each native input
	}
	_, err := libjson.Canonize(lisp.Native(map[string]any{"a": nil, "b": nil}), libjson.WithTypedMaxValues(4)) //elpsvet:allow-native test owns this nil-valued map
	require.Error(t, err, "native nulls must count as values")
	_, err = libjson.Canonize(lisp.Native(map[int]string{9: "nine"})) //elpsvet:allow-native test owns this int-keyed native map
	require.ErrorContains(t, err, "int map key 9")
	require.ErrorContains(t, err, "$[key 9]")
	for _, v := range []any{map[float64]string(nil), map[bool]string(nil)} {
		native := lisp.Native(v) //elpsvet:allow-native test owns these unsupported nil maps
		_, err := libjson.Dump(native, false)
		require.Error(t, err)
		_, err = libjson.Canonize(native)
		require.Error(t, err, "nil maps with unsupported key types must not bypass plain dump errors")
	}
}

type canonBytesHook []byte

func (canonBytesHook) MarshalJSON() ([]byte, error) { return []byte(`"custom"`), nil }

type canonByteHook byte

func (*canonByteHook) MarshalJSON() ([]byte, error) { return []byte(`"custom"`), nil }

type canonStringHook string

func (*canonStringHook) MarshalJSON() ([]byte, error) { return []byte(`"custom"`), nil }

func TestCanonizeRejectsNativeMarshalHooksInContainers(t *testing.T) {
	for _, v := range []any{canonBytesHook(nil), []canonByteHook{1}, []canonStringHook{"ordinary"}} {
		_, err := libjson.Canonize(lisp.Native(v)) //elpsvet:allow-native test owns each host marshal-hook value
		require.ErrorContains(t, err, "opaque native")
		require.ErrorContains(t, err, "at $")
	}
}

func TestCanonizeGoOptions(t *testing.T) {
	v := lisp.QExpr([]*lisp.LVal{lisp.Symbol("a"), lisp.Float(1)})
	want, err := libjson.Dump(v, false)
	require.NoError(t, err)
	for _, opts := range []libjson.DumpOpts{{Canonize: true}, {Canonize: true, Typed: true}} {
		canon, canonErr := libjson.DumpWith(v, opts)
		require.NoError(t, canonErr)
		require.Equal(t, want, canon)
	}
	got, err := libjson.DumpWith(v, libjson.DumpOpts{Typed: true})
	require.NoError(t, err)
	back := libjson.LoadWith(got, libjson.LoadOpts{Typed: true, ExactIntegers: true})
	canonExact(t, v, back)
	_, err = libjson.DumpWith(v, libjson.DumpOpts{Typed: true, StringNumbers: true})
	require.ErrorContains(t, err, "string-numbers")
	require.Equal(t, lisp.LError, libjson.LoadWith(got, libjson.LoadOpts{Typed: true, StringNumbers: true}).Type)
}

func TestCanonizeFreshLimitsCyclesAndSteps(t *testing.T) {
	leaf := lisp.Vector([]*lisp.LVal{lisp.Int(1)})
	v := lisp.Vector([]*lisp.LVal{leaf, leaf})
	c, err := libjson.Canonize(v)
	require.NoError(t, err)
	require.NotSame(t, v, c)
	require.NotSame(t, leaf, c.Cells[1].Cells[0])
	require.NotSame(t, c.Cells[1].Cells[0], c.Cells[1].Cells[1])
	v = lisp.SExpr([]*lisp.LVal{nil})
	v.Cells[0] = v
	_, err = libjson.Canonize(v)
	require.ErrorContains(t, err, "cycle")
	v = lisp.Vector(nil)
	for range libjson.DefaultTypedMaxDepth {
		v = lisp.Vector([]*lisp.LVal{v})
	}
	_, err = libjson.Canonize(v)
	require.ErrorContains(t, err, "depth")
	_, err = libjson.Canonize(v, libjson.WithTypedMaxDepth(2*libjson.DefaultTypedMaxDepth))
	require.ErrorContains(t, err, "depth", "canonize cannot admit a result the default typed encoder refuses")
	_, err = libjson.Canonize(lisp.String(strings.Repeat("x", 100)), libjson.WithTypedMaxBytes(50))
	require.Error(t, err)
	env := newTypedTestEnv(t)
	require.NoError(t, lisp.GoError(env.PutGlobal(lisp.Symbol("big"), lisp.String(strings.Repeat("x", 2000)))))
	base := typedSteps(t, env, `(identity big)`)
	require.Equal(t, int64(2), typedSteps(t, env, `(json:canonize big)`)-base)
	require.NoError(t, lisp.GoError(lisp.WithMaxAlloc(100)(env)))
	require.Equal(t, lisp.LError, env.LoadString("test", `(json:canonize big)`).Type)
	env = newTypedTestEnv(t)
	require.NoError(t, lisp.GoError(env.PutGlobal(lisp.Symbol("many"), lisp.Vector(makeCanonLeaves(10000)))))
	env.Runtime.SetStepBudget(2)
	r := env.LoadStringContext(context.Background(), "test", `(json:canonize many)`)
	require.Equal(t, lisp.LError, r.Type)
	require.Contains(t, r.String(), "step")
}

func TestCanonizeChargesWhileValidatingStrings(t *testing.T) {
	stop := errors.New("test charge stopped")
	_, err := libjson.Canonize(lisp.String(strings.Repeat("x", 5000)+"\xff"),
		libjson.WithTypedCharge(func(int) error { return stop }))
	require.ErrorIs(t, err, stop, "charge must stop the scan before its final invalid byte")
}

func makeCanonLeaves(n int) []*lisp.LVal {
	out := make([]*lisp.LVal, n)
	for i := range out {
		out[i] = lisp.Int(i)
	}
	return out
}

func TestCanonizeDumpOptions(t *testing.T) {
	env := newTypedTestEnv(t)
	for _, src := range []string{
		`(equal? (json:dump-bytes '(a 1.0) :canonize true) (json:dump-bytes (json:canonize '(a 1.0)) :typed true))`,
		`(equal? (json:dump-string '(a 1.0) :canonize true) "[\"a\",1]")`,
		`(equal? (json:dump-bytes '(a 1.0) :typed true :canonize true) (json:dump-bytes '(a 1.0) :canonize true))`,
		`(float? (json:load-string (json:dump-string 1.0 :typed true) :typed true))`,
		`(int? (json:load-bytes (json:dump-bytes 1 :typed true) :typed true :exact-integers true))`,
		`(progn (json:use-string-numbers true) true)`,
		`(equal? (json:dump-string 1 :canonize true) "1")`,
		`(equal? (json:dump-string 1 :typed true) "1")`,
		`(equal? (json:dump-string 1 :canonize true :string-numbers true) "\"1\"")`,
	} {
		r := env.LoadString("test", src)
		require.NotEqual(t, lisp.LError, r.Type, "%s: %v", src, r)
		require.True(t, lisp.True(r), "%s", src)
	}
	for _, src := range []string{
		`(json:dump-string 1 :typed true :string-numbers true)`,
		`(json:dump-bytes 1 :typed true :string-numbers false)`,
		`(json:load-string "1" :typed true :string-numbers true)`,
		`(json:load-bytes (to-bytes "1") :typed true :string-numbers false)`,
	} {
		r := env.LoadString("test", src)
		require.Equal(t, lisp.LError, r.Type, "%s", src)
		require.Contains(t, r.String(), "string-numbers")
	}
}

func TestCanonizeGeneratedRoundTripInvariant(t *testing.T) {
	env := newTypedTestEnv(t)
	accepted := 0
	for _, seed := range fuzzval.Seeds() {
		if checkCanonizeInvariant(t, fuzzval.New(seed, env).Value()) {
			accepted++
		}
	}
	rng := rand.New(rand.NewPCG(751, 2)) //nolint:gosec // deterministic test values
	for range 2000 {
		seed := make([]byte, 128)
		for i := range seed {
			seed[i] = byte(rng.Uint32() & 0xff)
		}
		if checkCanonizeInvariant(t, fuzzval.New(seed, env).Value()) {
			accepted++
		}
		checkCanonizeInvariant(t, lisp.Float(math.Float64frombits(rng.Uint64())))
	}
	require.Greater(t, accepted, 100)
}
