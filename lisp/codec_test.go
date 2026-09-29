// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"encoding/hex"
	"math"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func kw(s string) *lisp.LVal { return lisp.Symbol(s) }

func vec(cells ...*lisp.LVal) *lisp.LVal { return lisp.Vector(cells) }

func smap(t *testing.T, kv ...*lisp.LVal) *lisp.LVal {
	t.Helper()
	m := lisp.SortedMap()
	for i := 0; i+1 < len(kv); i += 2 {
		require.NotEqual(t, lisp.LError, m.MapSetLVal(kv[i], kv[i+1]).Type)
	}
	return m
}

// TestCanonicalGolden pins canonical format version 1 byte for byte.  The
// format is frozen: bytes it produces may be stored durably (on a ledger,
// in a cache key), so ANY change to an expected value below is a format
// change and needs a new version byte, not an edit to this table.
func TestCanonicalGolden(t *testing.T) {
	two := lisp.QExpr([]*lisp.LVal{lisp.Int(2), lisp.Int(3)})
	grid := lisp.Array(two, []*lisp.LVal{lisp.Int(1), lisp.Int(2), lisp.Int(3), lisp.Int(4), lisp.Int(5), lisp.Int(6)})
	tests := []struct {
		name string
		v    *lisp.LVal
		hex  string
	}{
		{"int zero", lisp.Int(0), "010100"},
		{"int one", lisp.Int(1), "010102"},
		{"int minus one", lisp.Int(-1), "010101"},
		{"int 300", lisp.Int(300), "0101d804"},
		{"int max", lisp.Int(math.MaxInt64), "0101feffffffffffffffff01"},
		{"int min", lisp.Int(math.MinInt64), "0101ffffffffffffffffff01"},
		{"float 1.5 (float32)", lisp.Float(1.5), "01023fc00000"},
		{"float 0.1 (float64)", lisp.Float(0.1), "01033fb999999999999a"},
		{"float +0", lisp.Float(0), "010200000000"},
		{"float -0", lisp.Float(math.Copysign(0, -1)), "010280000000"},
		{"float +inf", lisp.Float(math.Inf(1)), "01027f800000"},
		{"float -inf", lisp.Float(math.Inf(-1)), "0102ff800000"},
		{"float NaN", lisp.Float(math.NaN()), "01027fc00000"},
		{"float NaN payload canonicalized", lisp.Float(math.Float64frombits(0xfff0000000000001)), "01027fc00000"},
		{"string", lisp.String("hé"), "01040368c3a9"},
		{"empty string", lisp.String(""), "010400"},
		{"bytes", lisp.Bytes([]byte{0, 0xff}), "01050200ff"},
		{"symbol", lisp.Symbol("abc"), "010603616263"},
		{"qualified symbol", lisp.Symbol("lisp:x"), "0106066c6973703a78"},
		{"true", lisp.Symbol("true"), "01060474727565"},
		{"keyword", kw(":ab"), "0107026162"},
		{"nil", lisp.Nil(), "010800"},
		{"empty list", lisp.QExpr(nil), "010800"},
		{"list", lisp.QExpr([]*lisp.LVal{lisp.Int(1), lisp.String("a")}), "0108020102040161"},
		{"unquoted list encodes as list", lisp.SExpr([]*lisp.LVal{lisp.Int(1)}), "0108010102"},
		{"nested list", lisp.QExpr([]*lisp.LVal{lisp.QExpr(nil)}), "010801 0800"},
		{"vector", vec(lisp.Int(1), lisp.Int(2)), "010901 02 0102 0104"},
		{"empty vector", vec(), "0109010 0"},
		{"2x3 array", grid, "010902 0203 0102010401060108010a010c"},
		{"map", smap(t, lisp.String("b"), lisp.Int(2), lisp.Int(7), lisp.Int(0), kw(":a"), lisp.Int(1)),
			"010a03 010e 0100 07 0161 0102 040162 0104"},
		{"empty map", lisp.SortedMap(), "010a00"},
		{"tagged", mustTagged(t, "user:point", lisp.QExpr([]*lisp.LVal{lisp.Int(1), lisp.Int(2)})),
			"010b0a757365723a706f696e74 0802 0102 0104"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			want := strings.ReplaceAll(tt.hex, " ", "")
			b, err := lisp.EncodeCanonical(tt.v)
			require.NoError(t, err)
			assert.Equal(t, want, hex.EncodeToString(b))
			raw, err := hex.DecodeString(want)
			require.NoError(t, err)
			got, err := lisp.DecodeCanonical(raw)
			require.NoError(t, err)
			again, err := lisp.EncodeCanonical(got)
			require.NoError(t, err)
			assert.Equal(t, want, hex.EncodeToString(again), "decode/encode is not the identity on canonical bytes")
		})
	}
}

func mustTagged(t *testing.T, typ string, v *lisp.LVal) *lisp.LVal {
	t.Helper()
	env := lisp.NewEnv(nil)
	tv := env.TaggedValue(lisp.Symbol(typ), v)
	require.Equal(t, lisp.LTaggedVal, tv.Type)
	return tv
}

func TestCanonicalRoundTripValues(t *testing.T) {
	neg0 := math.Copysign(0, -1)
	tests := []*lisp.LVal{
		lisp.Int(42), lisp.Float(math.Pi), lisp.Float(neg0), lisp.Float(1e300), lisp.Float(math.SmallestNonzeroFloat64),
		lisp.String("x\x00y\xff"), lisp.Bytes(nil), lisp.Symbol("a"), kw(":k"),
		lisp.QExpr([]*lisp.LVal{lisp.Int(1), vec(lisp.String("s")), smap(t, lisp.String("k"), lisp.QExpr(nil))}),
	}
	for _, v := range tests {
		b, err := lisp.EncodeCanonical(v)
		require.NoError(t, err)
		got, err := lisp.DecodeCanonical(b)
		require.NoError(t, err)
		assert.Equal(t, v.String(), got.String())
		assert.Equal(t, v.Type, got.Type)
		if v.Type == lisp.LFloat {
			assert.Equal(t, math.Float64bits(v.Float), math.Float64bits(got.Float))
		}
	}
	// Decoded lists are quoted data lists, as (list ...) builds.
	got, err := lisp.DecodeCanonical([]byte{1, 8, 1, 1, 2})
	require.NoError(t, err)
	assert.True(t, got.IsQuoted())
}

func TestCanonicalRejects(t *testing.T) {
	env := testEnv(t)
	cyc := lisp.QExpr([]*lisp.LVal{lisp.Int(1)})
	cyc.Cells[0] = cyc
	fun := env.LoadString("t", "(lambda () 1)")
	require.Equal(t, lisp.LFun, fun.Type)
	tests := []struct {
		name string
		v    *lisp.LVal
		msg  string
	}{
		{"function", fun, "function"},
		{"native", lisp.Native(struct{}{}), "native"},
		{"error", lisp.Errorf("boom"), "error"},
		{"cycle", cyc, "cycle"},
		{"nested quote", lisp.Quote(lisp.Quote(lisp.Symbol("x"))), "quote"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			_, err := lisp.EncodeCanonical(tt.v)
			require.Error(t, err)
			assert.Contains(t, err.Error(), tt.msg)
		})
	}
}

// TestCanonicalSharedSubstructure pins the policy: shared substructure is
// written out in full at every occurrence (no back-references), so encoding
// depends only on the value, never on identity.
func TestCanonicalSharedSubstructure(t *testing.T) {
	shared := lisp.QExpr([]*lisp.LVal{lisp.Int(1)})
	a, err := lisp.EncodeCanonical(lisp.QExpr([]*lisp.LVal{shared, shared}))
	require.NoError(t, err)
	b, err := lisp.EncodeCanonical(lisp.QExpr([]*lisp.LVal{
		lisp.QExpr([]*lisp.LVal{lisp.Int(1)}), lisp.QExpr([]*lisp.LVal{lisp.Int(1)}),
	}))
	require.NoError(t, err)
	assert.Equal(t, a, b)
	got, err := lisp.DecodeCanonical(a)
	require.NoError(t, err)
	assert.NotSame(t, got.Cells[0], got.Cells[1], "decoded values must be fresh, never aliased")

	// A DAG whose tree expansion is exponential stops at the value limit.
	v := lisp.QExpr(nil)
	for range 64 {
		v = lisp.QExpr([]*lisp.LVal{v, v})
	}
	_, err = lisp.EncodeCanonical(v)
	require.Error(t, err)
	assert.Contains(t, err.Error(), "limit")
}

func TestCanonicalNativeCodec(t *testing.T) {
	type point struct{ X, Y byte }
	codec := lisp.NativeCodec{
		Name: "test:point",
		Encode: func(x any) ([]byte, bool, error) {
			p, ok := x.(point)
			if !ok {
				return nil, false, nil
			}
			return []byte{p.X, p.Y}, true, nil
		},
		Decode: func(b []byte) (any, error) {
			if len(b) != 2 {
				return nil, assert.AnError
			}
			return point{b[0], b[1]}, nil
		},
	}
	v := lisp.Native(point{3, 4}) //elpsvet:allow-native test payload
	b, err := lisp.EncodeCanonical(v, lisp.WithNativeCodec(codec))
	require.NoError(t, err)
	assert.Equal(t, "010c0a746573743a706f696e74020304", hex.EncodeToString(b))
	got, err := lisp.DecodeCanonical(b, lisp.WithNativeCodec(codec))
	require.NoError(t, err)
	assert.Equal(t, point{3, 4}, got.Native)
	_, err = lisp.DecodeCanonical(b)
	require.Error(t, err)
	assert.Contains(t, err.Error(), "test:point")
}

// TestCanonicalDecodeHostile checks that malformed and non-canonical input
// is rejected with an error: the decoder accepts exactly the bytes the
// encoder produces.
func TestCanonicalDecodeHostile(t *testing.T) {
	tests := []struct {
		name, hex, msg string
	}{
		{"empty", "", "version"},
		{"bad version", "0201 00", "version"},
		{"no value", "01", "truncated"},
		{"trailing", "010100 00", "trailing"},
		{"unknown tag", "0100", "tag"},
		{"unknown tag high", "01ff", "tag"},
		{"overlong varint", "01018000", "non-minimal"},
		{"varint overflow", "0101ffffffffffffffffff02", "overflow"},
		{"varint truncated", "010180", "truncated"},
		{"float64 fits float32", "01033ff8000000000000", "non-canonical float"},
		{"float64 NaN", "01037ff8000000000000", "non-canonical float"},
		{"float32 NaN payload", "01027fc00001", "NaN"},
		{"float truncated", "01023f", "truncated"},
		{"string length past end", "010405 61", "truncated"},
		{"huge string length", "0104ffffffffffffffff7f", ""},
		{"list count past end", "0108ff7f", "truncated"},
		{"symbol empty", "010600", "symbol"},
		{"symbol with colon prefix", "0106023a61", "keyword"},
		{"map misordered", "010a02 040162 0100 040161 0100", "order"},
		{"map duplicate", "010a02 040161 0100 060161 0100", "order"},
		{"map float key", "010a01 023f800000 0100", "key"},
		{"map int after string", "010a02 040161 0100 0102 0100", "order"},
		{"array dims overflow", "010902 ffffffffffffffff7f ffffffffffffffff7f", ""},
		{"array count mismatch truncated", "010901 05 0100", "truncated"},
		{"native unknown", "010c0178 00", "native"},
		{"tagged empty type", "010b00 0100", "tagged"},
		{"deep nesting", "01" + strings.Repeat("0801", 5000) + "0800", "depth"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			raw, err := hex.DecodeString(strings.ReplaceAll(tt.hex, " ", ""))
			require.NoError(t, err)
			v, err := lisp.DecodeCanonical(raw)
			require.Error(t, err, "decoded %v", v)
			assert.Nil(t, v)
			assert.Contains(t, err.Error(), tt.msg)
		})
	}
}

func TestCanonicalLimits(t *testing.T) {
	deep := lisp.QExpr(nil)
	for range 20 {
		deep = lisp.QExpr([]*lisp.LVal{deep})
	}
	_, err := lisp.EncodeCanonical(deep, lisp.WithCodecMaxDepth(10))
	require.Error(t, err)
	b, err := lisp.EncodeCanonical(deep)
	require.NoError(t, err)
	_, err = lisp.DecodeCanonical(b, lisp.WithCodecMaxDepth(10))
	require.Error(t, err)
	_, err = lisp.EncodeCanonical(lisp.String(strings.Repeat("x", 100)), lisp.WithCodecMaxBytes(50))
	require.Error(t, err)
	_, err = lisp.DecodeCanonical(b, lisp.WithCodecMaxBytes(5))
	require.Error(t, err)
	_, err = lisp.EncodeCanonical(vec(lisp.Int(1), lisp.Int(2), lisp.Int(3)), lisp.WithCodecMaxValues(3))
	require.Error(t, err)
}

// An array with a zero dimension is empty however large its other
// dimensions are; it must encode and round-trip (found by fuzzing).
func TestCanonicalEmptyArrayHugeDims(t *testing.T) {
	v := lisp.Array(lisp.QExpr([]*lisp.LVal{lisp.Int(1 << 40), lisp.Int(1 << 40), lisp.Int(0)}), nil)
	require.Equal(t, lisp.LArray, v.Type, "%v", v)
	b, err := lisp.EncodeCanonical(v)
	require.NoError(t, err)
	got, err := lisp.DecodeCanonical(b)
	require.NoError(t, err)
	again, err := lisp.EncodeCanonical(got)
	require.NoError(t, err)
	assert.Equal(t, b, again)
}

// TestCanonicalTypeFaithful pins the guarantee: the same types and
// structure give the same bytes.  equal? is coarser -- it equates a string
// and a symbol map key of one spelling, and an int and a float of one
// value -- and the encoding deliberately keeps those types apart.
func TestCanonicalTypeFaithful(t *testing.T) {
	env := testEnv(t)
	for _, tc := range []struct{ a, b string }{
		{`(sorted-map "a" 1)`, `(sorted-map 'a 1)`},
		{`1`, `1.0`},
	} {
		a, b := env.LoadString("a", tc.a), env.LoadString("b", tc.b)
		eq := env.LoadString("eq", "(equal? "+tc.a+" "+tc.b+")")
		assert.Equal(t, "true", eq.String(), "%s vs %s", tc.a, tc.b)
		ea, err := lisp.EncodeCanonical(a)
		require.NoError(t, err)
		eb, err := lisp.EncodeCanonical(b)
		require.NoError(t, err)
		assert.NotEqual(t, ea, eb, "%s and %s must encode differently", tc.a, tc.b)
	}
}
