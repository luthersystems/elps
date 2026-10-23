// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"errors"
	"math"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

type codecInner struct {
	Name string `elps:"name"`
	N    int    `elps:"n,omitempty"`
}

type codecOuter struct {
	Prefix    string            `elps:"prefix"`
	HDK       int               `elps:"hdk"`
	Ratio     float64           `elps:"ratio"`
	Autoflush bool              `elps:"autoflush"`
	Compare   *lisp.LVal        `elps:"compare"`
	Raw       []byte            `elps:"raw"`
	Items     []codecInner      `elps:"items"`
	Pair      [2]uint8          `elps:"pair"`
	Tags      map[string]int    `elps:"tags"`
	Inner     *codecInner       `elps:"inner,omitempty"`
	Any       any               `elps:"any,omitempty"`
	Skip      string            `elps:"-"`
	ByID      map[int]string    `elps:"by_id,omitempty"`
	Nested    map[string][]bool `elps:"nested,omitempty"`
}

func TestEncodeDecodeRoundTrip(t *testing.T) {
	in := codecOuter{
		Prefix:    "p",
		HDK:       50,
		Ratio:     0.5,
		Autoflush: true,
		Compare:   lisp.Nil(),
		Raw:       []byte{1, 2},
		Items:     []codecInner{{Name: "a", N: 1}, {Name: "b"}},
		Pair:      [2]uint8{3, 4},
		Tags:      map[string]int{"x": 1},
		Inner:     &codecInner{Name: "i"},
		ByID:      map[int]string{7: "seven"},
		Nested:    map[string][]bool{"k": {true, false}},
		Skip:      "not encoded",
	}
	v, err := lisp.Encode(in)
	require.NoError(t, err)
	assert.Equal(t, lisp.LSortMap, v.Type)
	_, hasSkip := lisp.Lookup[*lisp.LVal](mustMap(t, v), "Skip")
	assert.False(t, hasSkip)
	_, hasAny := lisp.Lookup[*lisp.LVal](mustMap(t, v), "any")
	assert.False(t, hasAny, "omitempty leaves a nil any out")

	out, err := lisp.Decode[codecOuter](v)
	require.NoError(t, err)
	in.Skip = ""
	assert.Equal(t, in, out)
}

func mustMap(t *testing.T, v *lisp.LVal) lisp.MapView {
	t.Helper()
	m, ok := lisp.AsMap(v)
	require.True(t, ok)
	return m
}

func TestEncodeShapes(t *testing.T) {
	v, err := lisp.Encode(codecInner{Name: "x"})
	require.NoError(t, err)
	assert.Equal(t, `(sorted-map "name" "x")`, v.String())

	v, err = lisp.Encode([]int{1, 2})
	require.NoError(t, err)
	assert.Equal(t, lisp.LArray, v.Type)

	for _, x := range []any{nil, []int(nil), map[string]int(nil), (*codecInner)(nil), (*lisp.LVal)(nil)} {
		v, err = lisp.Encode(x)
		require.NoError(t, err)
		assert.True(t, v.IsNil(), "%T", x)
	}

	v, err = lisp.Encode(true)
	require.NoError(t, err)
	assert.True(t, v.IsSymbol(lisp.TrueSymbol))

	_, err = lisp.Encode(uint64(math.MaxUint64))
	require.ErrorContains(t, err, "as an int")
	_, err = lisp.Encode(map[string]func(){"f": nil})
	require.ErrorContains(t, err, "f: cannot encode a func()")
	_, err = lisp.Encode(map[float64]int{1: 1})
	require.ErrorContains(t, err, "as a map key")
}

func TestDecodeKeys(t *testing.T) {
	type s struct {
		Prefix string `elps:"prefix"`
		Count  int
	}
	m := lisp.SortedMap()
	m.MapSetLVal(lisp.Symbol(":prefix"), lisp.String("kw"))
	m.MapSetLVal(lisp.Symbol("COUNT"), lisp.Int(3))
	m.MapSetString("extra", lisp.Int(9))
	got, err := lisp.Decode[s](m)
	require.NoError(t, err)
	assert.Equal(t, s{Prefix: "kw", Count: 3}, got)

	// An exact spelling wins over a case-folded one, whatever the order.
	m = lisp.SortedMap()
	m.MapSetString("PREFIX", lisp.String("folded"))
	m.MapSetString("prefix", lisp.String("exact"))
	m.MapSetString("Prefix", lisp.String("folded too"))
	got, err = lisp.Decode[s](m)
	require.NoError(t, err)
	assert.Equal(t, "exact", got.Prefix)

	// Without an exact key the first in sorted order wins.
	m = lisp.SortedMap()
	m.MapSetString("Prefix", lisp.String("second"))
	m.MapSetString("PREFIX", lisp.String("first"))
	got, err = lisp.Decode[s](m)
	require.NoError(t, err)
	assert.Equal(t, "first", got.Prefix)
}

func TestDecodeIntoKeepsDefaults(t *testing.T) {
	o := codecOuter{HDK: 50, Prefix: "default"}
	require.NoError(t, lisp.DecodeInto(lisp.MapOf("prefix", "p"), &o))
	assert.Equal(t, 50, o.HDK)
	assert.Equal(t, "p", o.Prefix)
}

func TestDecodeScalars(t *testing.T) {
	n, err := lisp.Decode[int](lisp.Float(3))
	require.NoError(t, err)
	assert.Equal(t, 3, n)
	_, err = lisp.Decode[int](lisp.Float(3.5))
	require.Error(t, err)
	_, err = lisp.Decode[int8](lisp.Int(300))
	require.Error(t, err)
	_, err = lisp.Decode[uint](lisp.Int(-1))
	require.Error(t, err)
	f, err := lisp.Decode[float32](lisp.Int(2))
	require.NoError(t, err)
	assert.InDelta(t, 2, f, 0)

	s, err := lisp.Decode[string](lisp.Symbol(":pending"))
	require.NoError(t, err)
	assert.Equal(t, "pending", s)
	s, err = lisp.Decode[string](lisp.Symbol("sym"))
	require.NoError(t, err)
	assert.Equal(t, "sym", s)
	_, err = lisp.Decode[string](lisp.Bool(true))
	require.Error(t, err)

	b, err := lisp.Decode[bool](lisp.Nil())
	require.NoError(t, err)
	assert.False(t, b)
	_, err = lisp.Decode[bool](lisp.Int(1))
	require.Error(t, err)

	bs, err := lisp.Decode[[]byte](lisp.QExpr([]*lisp.LVal{lisp.Int(1), lisp.Int(255)}))
	require.NoError(t, err)
	assert.Equal(t, []byte{1, 255}, bs)

	src := []byte{9}
	bs, err = lisp.Decode[[]byte](lisp.Bytes(src))
	require.NoError(t, err)
	bs[0] = 0
	assert.Equal(t, byte(9), src[0], "decoded bytes are a copy")

	sl, err := lisp.Decode[[]string](lisp.Nil())
	require.NoError(t, err)
	assert.Empty(t, sl)

	p, err := lisp.Decode[*int](lisp.Nil())
	require.NoError(t, err)
	assert.Nil(t, p)
}

func TestDecodeAny(t *testing.T) {
	sym := lisp.Symbol("sym")
	v := lisp.MapOf(
		"i", 1,
		"f", 1.5,
		"s", "x",
		"t", true,
		"l", lisp.QExpr([]*lisp.LVal{lisp.Int(1), lisp.Nil()}),
		"sym", sym,
		"n", lisp.NativeOf(errors.New("boom")),
	)
	got, err := lisp.Decode[any](v)
	require.NoError(t, err)
	m, ok := got.(map[string]any)
	require.True(t, ok)
	assert.Equal(t, int64(1), m["i"])
	assert.InDelta(t, 1.5, m["f"], 0)
	assert.Equal(t, "x", m["s"])
	assert.Equal(t, true, m["t"])
	assert.Equal(t, []any{int64(1), nil}, m["l"])
	assert.Same(t, sym, m["sym"])
	require.EqualError(t, m["n"].(error), "boom") //nolint:forcetypeassert // test
}

func TestDecodeNative(t *testing.T) {
	type s struct {
		Err error `elps:"err"`
	}
	got, err := lisp.Decode[s](lisp.MapOf("err", lisp.NativeOf(errors.New("boom"))))
	require.NoError(t, err)
	require.EqualError(t, got.Err, "boom")
}

func TestDecodeErrors(t *testing.T) {
	_, err := lisp.Decode[codecOuter](lisp.MapOf("items", lisp.QExpr([]*lisp.LVal{
		lisp.MapOf("name", "a"), lisp.MapOf("name", 3),
	})))
	var ce *lisp.CodecError
	require.ErrorAs(t, err, &ce)
	assert.Equal(t, "items[1].name", ce.Path)
	assert.Equal(t, "lisp: items[1].name: cannot store int in string", err.Error())

	_, err = lisp.Decode[codecOuter](lisp.String("x"))
	require.EqualError(t, err, "lisp: cannot store string in lisp_test.codecOuter")

	_, err = lisp.Decode[int](lisp.Errorf("bad"))
	var ev *lisp.ErrorVal
	require.ErrorAs(t, err, &ev)

	_, err = lisp.Decode[[3]int](lisp.QExpr([]*lisp.LVal{lisp.Int(1)}))
	require.Error(t, err)

	_, err = lisp.Decode[error](lisp.Int(1))
	require.Error(t, err)

	assert.Panics(t, func() { _ = lisp.DecodeInto(lisp.Int(1), 3) })
}

func TestCodecDepthLimit(t *testing.T) {
	type node struct {
		Next *node `elps:"next"`
	}
	var n *node
	for range 20_000 {
		n = &node{Next: n}
	}
	_, err := lisp.Encode(n)
	require.ErrorContains(t, err, "nested more than")

	v := lisp.Nil()
	for range 20_000 {
		v = lisp.QExpr([]*lisp.LVal{v})
	}
	_, err = lisp.Decode[any](v)
	require.ErrorContains(t, err, "nested more than")
	assert.Less(t, len(err.Error()), 200_000)
	assert.True(t, strings.HasPrefix(err.Error(), "lisp: [0][0]"))
}

type namedByte uint8

func TestCodecNamedByteSlice(t *testing.T) {
	got, err := lisp.Decode[[]namedByte](lisp.Bytes([]byte{1, 2}))
	require.NoError(t, err)
	assert.Equal(t, []namedByte{1, 2}, got)
	v, err := lisp.Encode([]namedByte{3, 4})
	require.NoError(t, err)
	require.Equal(t, lisp.LBytes, v.Type)
	assert.Equal(t, []byte{3, 4}, v.Bytes())
}

func TestDecodeFastPathTakesNative(t *testing.T) {
	s, err := lisp.Decode[string](lisp.NativeOf("hi"))
	require.NoError(t, err)
	assert.Equal(t, "hi", s)
}

func TestDecodeKeyCollisions(t *testing.T) {
	m := lisp.SortedMap()
	m.MapSetString("a", lisp.Int(1))
	m.MapSetLVal(lisp.Symbol(":a"), lisp.Int(2))
	_, err := lisp.Decode[map[string]int](m)
	require.ErrorContains(t, err, "two keys convert to one string key")
	_, err = lisp.Decode[any](m)
	require.ErrorContains(t, err, "two keys convert to one string key")

	got, err := lisp.Decode[any](lisp.MapOf(":kw", 1))
	require.NoError(t, err)
	assert.Equal(t, map[string]any{"kw": int64(1)}, got)
}

func TestDecodeNilInterface(t *testing.T) {
	type s struct {
		Err error `elps:"err"`
	}
	got, err := lisp.Decode[s](lisp.MapOf("err", lisp.Nil()))
	require.NoError(t, err)
	assert.NoError(t, got.Err)
}

func TestEncodeOpaqueStruct(t *testing.T) {
	type opaque struct{ n int }
	_ = opaque{}.n
	_, err := lisp.Encode(struct{ At opaque }{})
	require.ErrorContains(t, err, "At: cannot encode a lisp_test.opaque: it has no exported fields")
	v, err := lisp.Encode(struct{}{})
	require.NoError(t, err)
	assert.Equal(t, lisp.LSortMap, v.Type)
}

// TestDecodeSharedValueIsLinear decodes a value with 2^60 paths and 61
// distinct containers; without the memo it would not finish.
func TestDecodeSharedValueIsLinear(t *testing.T) {
	type node struct {
		L *node `elps:"l"`
		R *node `elps:"r"`
	}
	list := lisp.Nil()
	m := lisp.SortedMap()
	for range 60 {
		list = lisp.QExpr([]*lisp.LVal{list, list})
		next := lisp.SortedMap()
		next.MapSetString("l", m)
		next.MapSetString("r", m)
		m = next
	}
	_, err := lisp.Decode[any](list)
	require.NoError(t, err)
	_, err = lisp.Decode[[]any](list)
	require.NoError(t, err)
	n, err := lisp.Decode[*node](m)
	require.NoError(t, err)
	assert.Same(t, n.L, n.R)
	_, err = lisp.Decode[map[string]any](m)
	require.NoError(t, err)
}
