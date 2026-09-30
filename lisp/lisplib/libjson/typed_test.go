// Copyright © 2026 The ELPS authors

package libjson

import (
	"errors"
	"math"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func tsmap(t *testing.T, kv ...*lisp.LVal) *lisp.LVal {
	t.Helper()
	m := lisp.SortedMap()
	for i := 0; i+1 < len(kv); i += 2 {
		require.NotEqual(t, lisp.LError, m.MapSetLVal(kv[i], kv[i+1]).Type)
	}
	return m
}

func ttagged(name string, v *lisp.LVal) *lisp.LVal {
	return &lisp.LVal{Type: lisp.LTaggedVal, Str: name, Cells: []*lisp.LVal{v}}
}

func ints(xs ...int) []*lisp.LVal {
	out := make([]*lisp.LVal, len(xs))
	for i, x := range xs {
		out[i] = lisp.Int(x)
	}
	return out
}

// bs is a backslash, so escapes in expected JSON read plainly.
const bs = "\\"

// TestTypedGolden pins the typed JSON format byte for byte.  Stored
// documents (ledger state, content hashes) depend on it, so ANY change to an
// expected value below is a format change, not an edit.
func TestTypedGolden(t *testing.T) {
	tests := []struct {
		name string
		v    *lisp.LVal
		want string
	}{
		{"int zero", lisp.Int(0), `0`},
		{"int negative", lisp.Int(-42), `-42`},
		{"int 2^53-1", lisp.Int(1<<53 - 1), `9007199254740991`},
		{"int 2^53", lisp.Int(1 << 53), `"~i9007199254740992"`},
		{"int -(2^53)", lisp.Int(-(1 << 53)), `"~i-9007199254740992"`},
		{"int max", lisp.Int(math.MaxInt64), `"~i9223372036854775807"`},
		{"int min", lisp.Int(math.MinInt64), `"~i-9223372036854775808"`},
		{"float 1.0", lisp.Float(1), `1.0`},
		{"float 1.5", lisp.Float(1.5), `1.5`},
		{"float 0.1", lisp.Float(0.1), `0.1`},
		{"float 0", lisp.Float(0), `0.0`},
		{"float -0", lisp.Float(math.Copysign(0, -1)), `-0.0`},
		{"float 1e20", lisp.Float(1e20), `100000000000000000000.0`},
		{"float 1e21", lisp.Float(1e21), `1e+21`},
		{"float 1e-6", lisp.Float(1e-6), `0.000001`},
		{"float 1e-7", lisp.Float(1e-7), `1e-7`},
		{"float -2.5e-300", lisp.Float(-2.5e-300), `-2.5e-300`},
		{"float max", lisp.Float(math.MaxFloat64), `1.7976931348623157e+308`},
		{"float NaN", lisp.Float(math.NaN()), `"~zNaN"`},
		{"float NaN payload", lisp.Float(math.Float64frombits(0xfff0000000000001)), `"~zNaN"`},
		{"float +Inf", lisp.Float(math.Inf(1)), `"~zINF"`},
		{"float -Inf", lisp.Float(math.Inf(-1)), `"~z-INF"`},
		{"string", lisp.String("hé 世界"), `"hé 世界"`},
		{"empty string", lisp.String(""), `""`},
		{"string escapes", lisp.String("a\"b\\c\n\x01\x1f\x7f</>&"), `"a` + bs + `"b` + bs + bs + `c` + bs + `n` + bs + `u0001` + bs + `u001f` + "\x7f</>&\""},
		{"string tilde", lisp.String("~x"), `"~~x"`},
		{"string caret", lisp.String("^ "), `"~^ "`},
		{"string backtick", lisp.String("`a"), "\"~`a\""},
		{"string tilde inside", lisp.String("a~"), `"a~"`},
		{"bytes", lisp.Bytes([]byte{0, 0xff, 1}), `"~bAP8B"`},
		{"bytes padded", lisp.Bytes([]byte{1}), `"~bAQ=="`},
		{"empty bytes", lisp.Bytes(nil), `"~b"`},
		{"symbol", lisp.Symbol("abc"), `"~$abc"`},
		{"qualified symbol", lisp.Symbol("lisp:x"), `"~$lisp:x"`},
		{"true", lisp.Symbol("true"), `true`},
		{"false", lisp.Symbol("false"), `false`},
		{"keyword", lisp.Symbol(":ab"), `"~:ab"`},
		{"nil", lisp.Nil(), `[]`},
		{"list", lisp.QExpr([]*lisp.LVal{lisp.Int(1), lisp.String("a")}), `[1,"a"]`},
		{"unquoted list", lisp.SExpr([]*lisp.LVal{lisp.Int(1)}), `[1]`},
		{"vector", lisp.Vector(ints(1, 2)), `["~#vector",[1,2]]`},
		{"empty vector", lisp.Vector(nil), `["~#vector",[]]`},
		{"2x3 array", lisp.Array(lisp.QExpr(ints(2, 3)), ints(1, 2, 3, 4, 5, 6)), `["~#array",[[2,3],[1,2,3,4,5,6]]]`},
		{"rank 0 array", lisp.Array(lisp.QExpr(nil), ints(7)), `["~#array",[[],[7]]]`},
		{"string-keyed map", tsmap(t, lisp.String("b"), lisp.Int(2), lisp.String("a"), lisp.Int(1)), `{"a":1,"b":2}`},
		{"typed keys", tsmap(t, lisp.String("b"), lisp.Int(2), lisp.Int(7), lisp.Int(0), lisp.Symbol(":a"), lisp.Int(1),
			lisp.Symbol("s"), lisp.Float(1), lisp.Symbol("true"), lisp.Int(3), lisp.String("~t"), lisp.Int(4)),
			`{"b":2,"~$s":1.0,"~:a":1,"~?t":3,"~i7":0,"~~t":4}`},
		{"empty map", lisp.SortedMap(), `{}`},
		{"tagged", ttagged("user:point", lisp.QExpr(ints(1, 2))), `["~#tagged",["user:point",[1,2]]]`},
		{"nested", tsmap(t, lisp.String("xs"), lisp.Vector([]*lisp.LVal{tsmap(t, lisp.String("k"), lisp.Symbol(":v"))})),
			`{"xs":["~#vector",[{"k":"~:v"}]]}`},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			b, err := DumpTyped(tt.v)
			require.NoError(t, err)
			assert.Equal(t, tt.want, string(b))
			back, err := LoadTyped(b)
			require.NoError(t, err)
			again, err := DumpTyped(back)
			require.NoError(t, err)
			assert.Equal(t, tt.want, string(again))
		})
	}
}

// TestTypedJCSKeyOrder: members sort by UTF-16 code units, which puts a
// character above U+FFFF before one in U+E000-U+FFFF (RFC 8785 3.2.3).
func TestTypedJCSKeyOrder(t *testing.T) {
	m := tsmap(t, lisp.String("￿"), lisp.Int(1), lisp.String("😀"), lisp.Int(2), lisp.String("a"), lisp.Int(3))
	b, err := DumpTyped(m)
	require.NoError(t, err)
	assert.Equal(t, `{"a":3,"😀":2,"`+"￿"+`":1}`, string(b))
	_, err = LoadTyped(b)
	require.NoError(t, err)
	_, err = LoadTyped([]byte(`{"a":3,"` + "￿" + `":1,"😀":2}`))
	require.Error(t, err)
}

func TestTypedTypeFaithful(t *testing.T) {
	enc := func(v *lisp.LVal) string {
		b, err := DumpTyped(v)
		require.NoError(t, err)
		return string(b)
	}
	assert.NotEqual(t, enc(lisp.Int(1)), enc(lisp.Float(1)))
	assert.NotEqual(t, enc(lisp.String("a")), enc(lisp.Symbol("a")))
	assert.NotEqual(t, enc(lisp.QExpr(ints(1))), enc(lisp.Vector(ints(1))))
	assert.NotEqual(t, enc(tsmap(t, lisp.String("a"), lisp.Int(1))), enc(tsmap(t, lisp.Symbol("a"), lisp.Int(1))))
	assert.NotEqual(t, enc(tsmap(t, lisp.String("1"), lisp.Int(1))), enc(tsmap(t, lisp.Int(1), lisp.Int(1))))
}

func TestTypedMapOrderIndependent(t *testing.T) {
	a := tsmap(t, lisp.String("x"), lisp.Int(1), lisp.Symbol("y"), lisp.Int(2), lisp.Int(3), lisp.Int(3))
	b := tsmap(t, lisp.Int(3), lisp.Int(3), lisp.Symbol("y"), lisp.Int(2), lisp.String("x"), lisp.Int(1))
	ea, err := DumpTyped(a)
	require.NoError(t, err)
	eb, err := DumpTyped(b)
	require.NoError(t, err)
	assert.Equal(t, ea, eb)
}

func TestTypedRejectsUnencodable(t *testing.T) {
	fn := lisp.Fun("f", lisp.Formals(), func(*lisp.LEnv, *lisp.LVal) *lisp.LVal { return lisp.Nil() })
	cyc := lisp.QExpr([]*lisp.LVal{lisp.Int(1)})
	cyc.Cells = append(cyc.Cells, cyc)
	cm := lisp.SortedMap()
	cm.MapSetLVal(lisp.String("self"), cm)
	for name, v := range map[string]*lisp.LVal{
		"function":     fn,
		"native":       lisp.Native(struct{}{}), //elpsvet:allow-native test value, never shared
		"error":        lisp.Errorf("boom"),
		"cyclic list":  cyc,
		"cyclic map":   cm,
		"invalid utf8": lisp.String("\xff"),
		"empty symbol": lisp.Symbol(""),
		"utf8 symbol":  lisp.Symbol("a\xffb"),
		"utf8 keyword": lisp.Symbol(":a\xffb"),
		"utf8 tag":     ttagged("t\xff", lisp.Int(1)),
		"bad key":      tsmap(t, lisp.String("\xff"), lisp.Int(1)),
	} {
		t.Run(name, func(t *testing.T) {
			_, err := DumpTyped(v)
			require.Error(t, err)
		})
	}
	_, err := DumpTyped(cyc)
	assert.Contains(t, err.Error(), "contains itself")
}

func TestTypedSharedSubstructure(t *testing.T) {
	shared := lisp.QExpr(ints(1, 2))
	b, err := DumpTyped(lisp.Vector([]*lisp.LVal{shared, shared}))
	require.NoError(t, err)
	assert.Equal(t, `["~#vector",[[1,2],[1,2]]]`, string(b))
	// A small DAG that doubles at every level is stopped by the limits.
	v := lisp.Int(0)
	for range 40 {
		v = lisp.Vector([]*lisp.LVal{v, v})
	}
	_, err = DumpTyped(v)
	require.ErrorIs(t, err, ErrTypedLimit)
}

func TestTypedDecodeRejectsNonCanonical(t *testing.T) {
	for _, in := range []string{
		``, ` 1`, `1 `, `[1, 2]`, `{"a" :1}`, `null`, `tru`, `1.50`, `1E5`, `1e5`, `01`, `-0`, `+1`, `1.`, `.5`,
		`1.0e+21`, `100000000000000000000`, `1e+20`, `0.10`, `-0.00`, `9007199254740992`, `"~i5"`, `"~i05"`,
		`"~zInf"`, `"~$"`, `"~$true"`, `"~$:a"`, `"^a"`, "\"`a\"", `"~"`, `"~x"`, `"~#vector"`, `"~#list"`, `"~bAQ"`, `"~bAR=="`, `"~b!!"`,
		`"a` + bs + `/"`, `"` + bs + `u0041"`, `"` + bs + `u000a"`, `"` + bs + `u001F"`, "\"\x01\"", `"` + bs + `x"`, "\"\xff\"",
		`{"b":1,"a":2}`, `{"a":1,"a":2}`, `{"a":1,"~$a":2}`, `{"~#vector":[]}`, `{"~?x":1}`, `{"~i01":1}`, `{"^a":1}`,
		`["~#vector",[1],2]`, `["~#vector",1]`, `["~#list",[1]]`, `["~#set",[1]]`, `["~#cmap",["a",1]]`, `["~#array",[[2],[1,2]]]`,
		`["~#array",[[2,2],[1,2,3]]]`, `["~#array",[[-1,0],[]]]`, `["~#array",[[1.0,1],[1]]]`, `["~#tagged",["",1]]`,
		`["~#tagged",["t"]]`, `["~#tagged",["t",1,2]]`, `[1,]`, `{"a":1,}`, `[1`, `{"a"}`, `"abc`, `[]]`,
	} {
		_, err := LoadTyped([]byte(in))
		assert.Error(t, err, "%q", in)
	}
}

func TestTypedDecodeFresh(t *testing.T) {
	in := []byte(`{"a":"~bAQID","b":"text","c":["x"]}`)
	v, err := LoadTyped(in)
	require.NoError(t, err)
	for i := range in {
		in[i] = 'z'
	}
	assert.Equal(t, []byte{1, 2, 3}, v.MapGet(lisp.String("a")).Bytes())
	assert.Equal(t, "text", v.MapGet(lisp.String("b")).Str)
}

func TestTypedLimits(t *testing.T) {
	deep := strings.Repeat("[", 20) + strings.Repeat("]", 20)
	_, err := LoadTyped([]byte(deep), WithTypedMaxDepth(10))
	require.ErrorIs(t, err, ErrTypedLimit)
	_, err = LoadTyped([]byte(deep), WithTypedMaxDepth(20))
	require.NoError(t, err)
	_, err = LoadTyped([]byte(`[1,2,3,4]`), WithTypedMaxValues(4))
	require.ErrorIs(t, err, ErrTypedLimit)
	_, err = LoadTyped([]byte(`[1,2,3,4]`), WithTypedMaxBytes(8))
	require.ErrorIs(t, err, ErrTypedLimit)
	// The limits of the two directions agree at the boundary.
	b5, err := DumpTyped(lisp.String("abc"), WithTypedMaxBytes(5))
	require.NoError(t, err)
	_, err = LoadTyped(b5, WithTypedMaxBytes(5))
	require.NoError(t, err)
	_, err = DumpTyped(lisp.String("~bc"), WithTypedMaxBytes(5))
	require.ErrorIs(t, err, ErrTypedLimit)
	_, err = DumpTyped(lisp.String(strings.Repeat("x", 100)), WithTypedMaxBytes(50))
	require.ErrorIs(t, err, ErrTypedLimit)
	_, err = DumpTyped(lisp.Vector(ints(1, 2, 3, 4)), WithTypedMaxValues(4))
	require.ErrorIs(t, err, ErrTypedLimit)
	v := lisp.Int(1)
	for range 20 {
		v = lisp.Vector([]*lisp.LVal{v})
	}
	_, err = DumpTyped(v, WithTypedMaxDepth(10))
	require.ErrorIs(t, err, ErrTypedLimit)
	b, err := DumpTyped(v, WithTypedMaxDepth(20))
	require.NoError(t, err)
	_, err = LoadTyped(b, WithTypedMaxDepth(20))
	require.NoError(t, err, "a value at the depth limit must round-trip")
}

func TestTypedHugeEmptyArray(t *testing.T) {
	v := lisp.Array(lisp.QExpr(ints(0, math.MaxInt)), nil)
	b, err := DumpTyped(v)
	require.NoError(t, err)
	back, err := LoadTyped(b)
	require.NoError(t, err)
	again, err := DumpTyped(back)
	require.NoError(t, err)
	assert.Equal(t, b, again)
	_, err = LoadTyped([]byte(`["~#array",[[9223372036854775807,9223372036854775807],[1]]]`))
	require.Error(t, err)
}

func TestTypedChargeDuringEncode(t *testing.T) {
	v := lisp.String(strings.Repeat("x", 5000))
	total := 0
	b, err := DumpTyped(v, WithTypedCharge(func(n int) error { total += n; return nil }))
	require.NoError(t, err)
	assert.Equal(t, (len(b)+1023)/1024, total)
	stop := errors.New("budget")
	calls := 0
	big := make([]*lisp.LVal, 100)
	for i := range big {
		big[i] = lisp.String(strings.Repeat("y", 1000))
	}
	_, err = DumpTyped(lisp.Vector(big), WithTypedCharge(func(int) error {
		calls++
		if calls == 3 {
			return stop
		}
		return nil
	}))
	require.ErrorIs(t, err, stop)
	assert.Equal(t, 3, calls, "the encode stops at the failing charge")
}
