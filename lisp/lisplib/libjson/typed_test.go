// Copyright © 2026 The ELPS authors

package libjson

import (
	"errors"
	"math"
	"strconv"
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

func TestTypedStringEscapeMarker(t *testing.T) {
	for _, tt := range []struct {
		value string
		want  string
	}{
		{"", `""`},
		{"~", `"~~"`},
		{"~x", `"~~x"`},
		{"~~x", `"~~~x"`},
		{"~^x", `"~~^x"`},
		{"~`x", "\"~~`x\""},
		{"^", `"^"`},
		{"^x", `"^x"`},
		{"`", "\"`\""},
		{"`x", "\"`x\""},
		{"x~^`", "\"x~^`\""},
	} {
		t.Run(tt.value, func(t *testing.T) {
			t.Run("value", func(t *testing.T) {
				b, err := DumpTyped(lisp.String(tt.value), WithTypedMaxBytes(len(tt.want)))
				require.NoError(t, err)
				assert.Equal(t, tt.want, string(b))
			})
			t.Run("decode value", func(t *testing.T) {
				v, err := LoadTyped([]byte(tt.want))
				require.NoError(t, err)
				assert.Equal(t, lisp.LString, v.Type)
				assert.Equal(t, tt.value, v.Str)
			})
			t.Run("key", func(t *testing.T) {
				want := `{` + tt.want + `:1}`
				b, err := DumpTyped(tsmap(t, lisp.String(tt.value), lisp.Int(1)))
				require.NoError(t, err)
				assert.Equal(t, want, string(b))
				v, err := LoadTyped([]byte(want))
				require.NoError(t, err)
				entries := v.MapEntries()
				require.Len(t, entries.Cells, 1)
				key := entries.Cells[0].Cells[0]
				assert.Equal(t, lisp.LString, key.Type)
				assert.Equal(t, tt.value, key.Str)
				assert.Equal(t, 1, entries.Cells[0].Cells[1].Int)
			})
		})
	}
}

func TestTypedRejectsUnknownEscapeTags(t *testing.T) {
	for _, tag := range []string{"~^", "~^x", "~`", "~`x"} {
		s := strconv.Quote(tag)
		for _, doc := range []string{s, `[` + s + `]`, `["~#list",[` + s + `]]`, `{"k":` + s + `}`} {
			t.Run(doc, func(t *testing.T) {
				_, err := LoadTyped([]byte(doc))
				require.ErrorContains(t, err, "invalid tagged string")
			})
		}
		t.Run("key "+tag, func(t *testing.T) {
			_, err := LoadTyped([]byte(`{` + s + `:1}`))
			require.ErrorContains(t, err, "invalid tagged key")
		})
	}
}

func TestTypedEscapeMarkerKeyOrder(t *testing.T) {
	m := tsmap(t, lisp.String("~b"), lisp.Int(5), lisp.Symbol("key"), lisp.Int(4),
		lisp.String("b"), lisp.Int(3), lisp.String("`b"), lisp.Int(2), lisp.String("^b"), lisp.Int(1))
	want := "{\"^b\":1,\"`b\":2,\"b\":3,\"~$key\":4,\"~~b\":5}"
	b, err := DumpTyped(m)
	require.NoError(t, err)
	assert.Equal(t, want, string(b))
	back, err := LoadTyped([]byte(want))
	require.NoError(t, err)
	again, err := DumpTyped(back)
	require.NoError(t, err)
	assert.Equal(t, want, string(again))
}

// TestTypedGolden pins the typed JSON format byte for byte.  Stored
// documents and content hashes depend on it, so ANY change to an
// expected value below is a format change, not an edit.
func TestTypedGolden(t *testing.T) {
	int64Value := func(n int64) *lisp.LVal {
		if n < math.MinInt || n > math.MaxInt {
			return nil
		}
		return lisp.Int(int(n))
	}
	tests := []struct {
		name string
		v    *lisp.LVal
		want string
	}{
		{"int zero", lisp.Int(0), `0`},
		{"int negative", lisp.Int(-42), `-42`},
		{"int 2^53-1", int64Value(1<<53 - 1), `9007199254740991`},
		{"int -(2^53-1)", int64Value(-(1<<53 - 1)), `-9007199254740991`},
		{"int 2^53", int64Value(1 << 53), `9007199254740992`},
		{"int -(2^53)", int64Value(-(1 << 53)), `-9007199254740992`},
		{"int 2^53+1", int64Value(1<<53 + 1), `"~n9007199254740993"`},
		{"int -(2^53+1)", int64Value(-(1<<53 + 1)), `"~n-9007199254740993"`},
		{"int max", int64Value(math.MaxInt64), `"~n9223372036854775807"`},
		{"int min", int64Value(math.MinInt64), `"~n-9223372036854775808"`},
		{"float 1.0", lisp.Float(1), `"~d1"`},
		{"float 1.5", lisp.Float(1.5), `1.5`},
		{"float 0.1", lisp.Float(0.1), `0.1`},
		{"float 0", lisp.Float(0), `"~d0"`},
		{"float -0", lisp.Float(math.Copysign(0, -1)), `"~d-0"`},
		{"float 1e20", lisp.Float(1e20), `"~d100000000000000000000"`},
		{"float 1e21", lisp.Float(1e21), `"~d1e+21"`},
		{"float 1e-6", lisp.Float(1e-6), `0.000001`},
		{"float 1e-7", lisp.Float(1e-7), `1e-7`},
		{"float -2.5e-300", lisp.Float(-2.5e-300), `-2.5e-300`},
		{"float max", lisp.Float(math.MaxFloat64), `"~d1.7976931348623157e+308"`},
		{"float NaN", lisp.Float(math.NaN()), `"~zNaN"`},
		{"float NaN payload", lisp.Float(math.Float64frombits(0xfff0000000000001)), `"~zNaN"`},
		{"float +Inf", lisp.Float(math.Inf(1)), `"~zINF"`},
		{"float -Inf", lisp.Float(math.Inf(-1)), `"~z-INF"`},
		{"string", lisp.String("hé 世界"), `"hé 世界"`},
		{"empty string", lisp.String(""), `""`},
		{"string escapes", lisp.String("a\"b\\c\n\x01\x1f\x7f</>&"), `"a` + bs + `"b` + bs + bs + `c` + bs + `n` + bs + `u0001` + bs + `u001f` + "\x7f" + `\u003c/\u003e\u0026"`},
		{"string tilde", lisp.String("~x"), `"~~x"`},
		{"string caret", lisp.String("^ "), `"^ "`},
		{"string backtick", lisp.String("`a"), "\"`a\""},
		{"string tilde inside", lisp.String("a~"), `"a~"`},
		{"bytes", lisp.Bytes([]byte{0, 0xff, 1}), `"~bAP8B"`},
		{"bytes padded", lisp.Bytes([]byte{1}), `"~bAQ=="`},
		{"empty bytes", lisp.Bytes(nil), `"~b"`},
		{"symbol", lisp.Symbol("abc"), `"~$abc"`},
		{"qualified symbol", lisp.Symbol("lisp:x"), `"~$lisp:x"`},
		{"true", lisp.Symbol("true"), `true`},
		{"false", lisp.Symbol("false"), `false`},
		{"keyword", lisp.Symbol(":ab"), `"~:ab"`},
		{"nil", lisp.Nil(), `null`},
		{"empty quoted list", lisp.QExpr(nil), `null`},
		{"empty unquoted list", lisp.SExpr(nil), `null`},
		{"list", lisp.QExpr([]*lisp.LVal{lisp.Int(1), lisp.String("a")}), `["~#list",[1,"a"]]`},
		{"unquoted list", lisp.SExpr([]*lisp.LVal{lisp.Int(1)}), `["~#list",[1]]`},
		{"vector", lisp.Vector(ints(1, 2)), `[1,2]`},
		{"empty vector", lisp.Vector(nil), `[]`},
		{"2x3 array", lisp.Array(lisp.QExpr(ints(2, 3)), ints(1, 2, 3, 4, 5, 6)), `["~#array",[[2,3],[1,2,3,4,5,6]]]`},
		{"rank 0 array", lisp.Array(lisp.QExpr(nil), ints(7)), `["~#array",[[],[7]]]`},
		{"string-keyed map", tsmap(t, lisp.String("b"), lisp.Int(2), lisp.String("a"), lisp.Int(1)), `{"a":1,"b":2}`},
		{"typed keys", tsmap(t, lisp.String("b"), lisp.Int(2), lisp.Int(7), lisp.Int(0), lisp.Symbol(":a"), lisp.Int(1),
			lisp.Symbol("s"), lisp.Float(1), lisp.Symbol("true"), lisp.Int(3), lisp.String("~t"), lisp.Int(4)),
			`{"b":2,"~$s":"~d1","~:a":1,"~?t":3,"~i7":0,"~~t":4}`},
		{"empty map", lisp.SortedMap(), `{}`},
		{"tagged", ttagged("user:point", lisp.QExpr(ints(1, 2))), `["~#tagged",["user:point",["~#list",[1,2]]]]`},
		{"nested", tsmap(t, lisp.String("xs"), lisp.Vector([]*lisp.LVal{tsmap(t, lisp.String("k"), lisp.Symbol(":v"))})),
			`{"xs":[{"k":"~:v"}]}`},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if tt.v == nil {
				_, err := LoadTyped([]byte(tt.want))
				require.Error(t, err, "an int that does not fit must be rejected")
				return
			}
			b, err := DumpTyped(tt.v)
			require.NoError(t, err)
			assert.Equal(t, tt.want, string(b))
			image, err := Tag(tt.v)
			require.NoError(t, err)
			plain, err := Dump(image, false)
			require.NoError(t, err)
			require.Equal(t, b, plain)
			composed, err := Untag(LoadWith(plain, LoadOpts{ExactIntegers: true, Strict: true}))
			require.NoError(t, err)
			composedBytes, err := DumpTyped(composed)
			require.NoError(t, err)
			require.Equal(t, b, composedBytes)
			back, err := LoadTyped(b)
			require.NoError(t, err)
			again, err := DumpTyped(back)
			require.NoError(t, err)
			assert.Equal(t, tt.want, string(again))
		})
	}
}

// Large values use Transit's arbitrary-precision tag, while integer keys
// retain its signed 64-bit integer tag at every magnitude.
func TestTypedIntegerTags(t *testing.T) {
	for _, digits := range []string{
		"0", "-7", "2147483647", "-2147483648", "2147483648",
		"9007199254740991", "-9007199254740991",
		"9007199254740992", "-9007199254740992",
		"9007199254740993", "-9007199254740993",
		"9223372036854775807", "-9223372036854775808",
	} {
		t.Run(digits, func(t *testing.T) {
			n, err := strconv.ParseInt(digits, 10, 64)
			require.NoError(t, err)
			want := digits
			if n < -(1<<53) || n > 1<<53 {
				want = `"~n` + digits + `"`
			}
			mapDoc := `{"~i` + digits + `":` + want + `}`
			for _, doc := range []string{want, mapDoc} {
				v, err := LoadTyped([]byte(doc))
				if n < math.MinInt || n > math.MaxInt {
					require.ErrorContains(t, err, "does not fit in a 32-bit int")
					continue
				}
				require.NoError(t, err)
				if doc == mapDoc {
					require.Equal(t, lisp.LSortMap, v.Type)
					entries := v.MapEntries()
					require.Len(t, entries.Cells, 1)
					key := entries.Cells[0].Cells[0]
					assert.Equal(t, lisp.LInt, key.Type)
					assert.Equal(t, n, int64(key.Int))
					v = entries.Cells[0].Cells[1]
				}
				assert.Equal(t, lisp.LInt, v.Type)
				assert.Equal(t, n, int64(v.Int))
				b, err := DumpTyped(v)
				require.NoError(t, err)
				assert.Equal(t, want, string(b))
				b, err = DumpTyped(tsmap(t, lisp.Int(int(n)), v))
				require.NoError(t, err)
				assert.Equal(t, mapDoc, string(b))
			}
		})
	}
}

func TestTypedRejectsNonCanonicalIntegerTags(t *testing.T) {
	for _, in := range []string{
		`"~i9007199254740992"`, `"~i-9007199254740992"`,
		`"~i9223372036854775807"`, `"~i-9223372036854775808"`,
		`["~i9007199254740992"]`, `{"n":"~i9007199254740992"}`,
		`"~n"`, `"~n0"`, `"~n-0"`, `"~n1"`, `"~n-1"`,
		`"~n9007199254740991"`, `"~n-9007199254740991"`,
		`"~n09007199254740992"`, `"~n-09007199254740992"`, `"~n+9007199254740992"`,
		`"~n9007199254740992.0"`, `"~n9.007199254740992e+15"`, `"~n 9007199254740992"`,
		`"~n9223372036854775808"`, `"~n-9223372036854775809"`,
		`{"~n7":0}`, `{"~n9007199254740992":0}`, `{"~n-9007199254740992":0}`,
		`["~#array",[[0,"~i9007199254740992"],[]]]`,
	} {
		t.Run(in, func(t *testing.T) {
			_, err := LoadTyped([]byte(in))
			require.Error(t, err)
		})
	}
}

func TestTypedSequenceDecode(t *testing.T) {
	for _, tt := range []struct {
		in   string
		want *lisp.LVal
	}{
		{`null`, lisp.Nil()},
		{`[]`, lisp.Vector(nil)},
		{`[1,2]`, lisp.Vector(ints(1, 2))},
		{`["~#list",[1,2]]`, lisp.QExpr(ints(1, 2))},
		{`[null,[],["~#list",[1]]]`, lisp.Vector([]*lisp.LVal{lisp.Nil(), lisp.Vector(nil), lisp.QExpr(ints(1))})},
		{`["~#list",[null,[],[1]]]`, lisp.QExpr([]*lisp.LVal{lisp.Nil(), lisp.Vector(nil), lisp.Vector(ints(1))})},
		{`["~~#list",[1]]`, lisp.Vector([]*lisp.LVal{lisp.String("~#list"), lisp.Vector(ints(1))})},
		{`["~~#unknown",[]]`, lisp.Vector([]*lisp.LVal{lisp.String("~#unknown"), lisp.Vector(nil)})},
	} {
		t.Run(tt.in, func(t *testing.T) {
			v, err := LoadTyped([]byte(tt.in))
			require.NoError(t, err)
			assert.Equal(t, tt.want.Type, v.Type)
			assert.Equal(t, tt.want.String(), v.String())
			b, err := DumpTyped(v)
			require.NoError(t, err)
			assert.Equal(t, tt.in, string(b))
		})
	}
}

// Plain JSON canonization turns lists into vectors and null into nil. The
// typed sequence encodings must preserve that plain JSON shape, including nil.
// Integral numbers and reserved strings have separate typed spelling rules.
func TestTypedCanonizedSequences(t *testing.T) {
	for name, v := range map[string]*lisp.LVal{
		"nil":          lisp.Nil(),
		"empty list":   lisp.QExpr(nil),
		"empty vector": lisp.Vector(nil),
		"list": lisp.QExpr([]*lisp.LVal{
			lisp.Float(1.5), lisp.Nil(), lisp.Vector(nil), lisp.Symbol("true"), lisp.String("text"),
		}),
		"nested": lisp.Vector([]*lisp.LVal{
			lisp.QExpr([]*lisp.LVal{lisp.Nil(), lisp.Vector([]*lisp.LVal{lisp.String("text"), lisp.Symbol("false")})}),
			tsmap(t, lisp.Symbol("items"), lisp.QExpr([]*lisp.LVal{lisp.Nil()})),
		}),
	} {
		t.Run(name, func(t *testing.T) {
			plain, err := Dump(v, false)
			require.NoError(t, err)
			canon := Load(plain, false)
			require.NotEqual(t, lisp.LError, canon.Type)
			typed, err := DumpTyped(canon)
			require.NoError(t, err)
			assert.Equal(t, string(plain), string(typed))
		})
	}
}

// UTF-8 byte order matches plain JSON string-key order. Astral characters
// follow U+E000-U+FFFF, unlike the UTF-16 order specified by RFC 8785.
func TestTypedUTF8KeyOrder(t *testing.T) {
	m := tsmap(t, lisp.String("￿"), lisp.Int(1), lisp.String("😀"), lisp.Int(2), lisp.String("a"), lisp.Int(3))
	b, err := DumpTyped(m)
	require.NoError(t, err)
	assert.Equal(t, `{"a":3,"`+"￿"+`":1,"😀":2}`, string(b))
	plain, err := Dump(m, false)
	require.NoError(t, err)
	assert.Equal(t, plain, b)
	_, err = LoadTyped(b)
	require.NoError(t, err)
	_, err = LoadTyped([]byte(`{"a":3,"😀":2,"` + "￿" + `":1}`))
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
	assert.NotEqual(t, enc(lisp.Nil()), enc(lisp.Vector(nil)))
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
	assert.Equal(t, `[["~#list",[1,2]],["~#list",[1,2]]]`, string(b))
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
		``, ` 1`, `1 `, `[1, 2]`, `{"a" :1}`, `nul`, `null `, `nullnull`, `tru`, `1.50`, `1E5`, `1e5`, `01`, `-0`, `+1`, `1.`, `.5`,
		`1.0e+21`, `100000000000000000000`, `1e+20`, `0.10`, `-0.00`, `9007199254740993`, `"~i5"`, `"~i05"`,
		`"~zInf"`, `"~$"`, `"~$true"`, `"~$:a"`, `"~^a"`, "\"~`a\"", `"~"`, `"~x"`, `"~#unknown"`, `"~#list"`, `"~bAQ"`, `"~bAR=="`, `"~b!!"`,
		`"a` + bs + `/"`, `"` + bs + `u0041"`, `"` + bs + `u000a"`, `"` + bs + `u001F"`, "\"\x01\"", `"` + bs + `x"`, "\"\xff\"",
		`{"b":1,"a":2}`, `{"a":1,"a":2}`, `{"a":1,"~$a":2}`, `{"~#unknown":[]}`, `{"~?x":1}`, `{"~i01":1}`, `{"~^a":1}`, "{\"~`a\":1}",
		`["~#unknown",[1],2]`, `["~#unknown",1]`, `["~#unknown",[1]]`, `["~#unknown",[]]`,
		`["~#list",[]]`, `["~#list",1]`, `["~#list",null]`, `["~#list",[1],2]`, `["~#list",[1,]]`,
		`["~#set",[1]]`, `["~#cmap",["a",1]]`, `["~#array",[[2],[1,2]]]`,
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
	assert.Equal(t, lisp.LArray, v.MapGet(lisp.String("c")).Type)

	nil1, err := LoadTyped([]byte(`null`))
	require.NoError(t, err)
	nil2, err := LoadTyped([]byte(`null`))
	require.NoError(t, err)
	assert.NotSame(t, lisp.Nil(), nil1)
	assert.NotSame(t, nil1, nil2)
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
	// Tags do not add a logical container or values to the limits.
	list := lisp.QExpr(ints(1, 2))
	b, err = DumpTyped(list, WithTypedMaxDepth(1), WithTypedMaxValues(3))
	require.NoError(t, err)
	_, err = LoadTyped(b, WithTypedMaxDepth(1), WithTypedMaxValues(3))
	require.NoError(t, err)
	_, err = DumpTyped(list, WithTypedMaxValues(2))
	require.ErrorIs(t, err, ErrTypedLimit)
	_, err = LoadTyped(b, WithTypedMaxValues(2))
	require.ErrorIs(t, err, ErrTypedLimit)
	// Nil is a leaf and needs no container depth in either direction.
	b, err = DumpTyped(lisp.Nil(), WithTypedMaxDepth(0), WithTypedMaxValues(1))
	require.NoError(t, err)
	_, err = LoadTyped(b, WithTypedMaxDepth(0), WithTypedMaxValues(1))
	require.NoError(t, err)
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
	_, err = LoadTyped([]byte(`["~#array",[["~n9223372036854775807","~n9223372036854775807"],[1]]]`))
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
