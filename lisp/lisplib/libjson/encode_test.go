package libjson

import (
	"crypto/rand"
	"encoding/base64"
	"encoding/json"
	"io"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// this function is internal because users are not supposed to construct
// literal SortedMap values in their applications =\
func literalSortedMap(m SortedMap) *lisp.LVal {
	return lisp.SortedMapFromData(&lisp.MapData{Map: m})
}

type encodeTest struct {
	v  *lisp.LVal
	js string
}

var stdEncodeTests = []encodeTest{
	{lisp.String(""), `""`},
	{lisp.String("\t"), `"\t"`},
	{lisp.String("\x05"), `"\u0005"`},
	{lisp.String("a\u2028\u2029"), `"a\u2028\u2029"`},
	{lisp.String("hello"), `"hello"`},
	{lisp.String("🤷🏽\u200d♀️"), `"🤷🏽‍♀️"`}, // Woman Shrugging: Medium Skin Tone
	{lisp.Symbol(""), `""`},
	{lisp.Symbol("hello"), `"hello"`},
	{lisp.Symbol(lisp.TrueSymbol), `true`},
	{lisp.Symbol(lisp.FalseSymbol), `false`},
	{lisp.Nil(), `null`},
	{lisp.SExpr(nil), `null`},
	{lisp.QExpr(nil), `null`},
	{lisp.Int(0), `0`},
	{lisp.Int(1 << 60), `1152921504606846976`},
	{lisp.Int(-(1 << 60)), `-1152921504606846976`},
	{lisp.Float(0), `0`},
	{lisp.Float(-1.5e-7), `-1.5e-7`},
	{lisp.Float(1.125e21), `1.125e+21`},
	{lisp.Bytes([]byte("hello")), `"aGVsbG8="`},
	{lisp.Bytes([]byte{}), `""`},
	{lisp.Bytes([]byte(nil)), `null`}, // backwards compat with v1.13.0
	{
		// overflow the encoder's internal buffer
		lisp.Bytes([]byte("Lorem ipsum dolor sit amet, consectetur adipiscing elit.")),
		`"TG9yZW0gaXBzdW0gZG9sb3Igc2l0IGFtZXQsIGNvbnNlY3RldHVyIGFkaXBpc2NpbmcgZWxpdC4="`,
	},
	{lisp.SortedMap(), `{}`},
	{lisp.Value(map[string]interface{}{}), `{}`},
	{
		lisp.Value(map[string]interface{}{"a": "1"}),
		`{"a":"1"}`,
	},
	{
		lisp.Value(map[string]interface{}{"a": "1", "b": "2"}),
		`{"a":"1","b":"2"}`,
	},
	{literalSortedMap(SortedMap{}), `{}`},
	{literalSortedMap(nil), `{}`},
	{
		literalSortedMap(SortedMap{"a": lisp.String("1")}),
		`{"a":"1"}`,
	},
	{
		literalSortedMap(SortedMap{"a": lisp.String("1"), "b": lisp.String("2")}),
		`{"a":"1","b":"2"}`,
	},
	{lisp.Vector(nil), `[]`},
	{lisp.Vector([]*lisp.LVal{}), `[]`},
	{lisp.Vector([]*lisp.LVal{lisp.String("a")}), `["a"]`},
	{lisp.Vector([]*lisp.LVal{lisp.Float(1.5e-10), lisp.Int(100)}), `[1.5e-10,100]`},
}

var stringNumberEncodeTests = []encodeTest{
	{lisp.Int(0), `"0"`},
	{lisp.Int(1 << 60), `"1152921504606846976"`},
	{lisp.Int(-(1 << 60)), `"-1152921504606846976"`},
	{lisp.Float(0), `"0"`},
	{lisp.Float(-1.5e-7), `"-1.5e-7"`},
	{lisp.Float(1.125e21), `"1.125e+21"`},
}

func testEncode(t testing.TB) {
	for i, test := range stdEncodeTests {
		enc := newEncoder(false)
		if assert.NoError(t, enc.encode(test.v), "test %d: %v", i, test.v) {
			js := string(enc.bytes())
			assert.Equal(t, test.js, js, "test %d: %v", i, test.v)
		}
	}
}

func testEncode_stringNumbers(t testing.TB) {
	for i, test := range stringNumberEncodeTests {
		enc := newEncoder(true)
		if assert.NoError(t, enc.encode(test.v), "test %d: %v", i, test.v) {
			js := string(enc.bytes())
			assert.Equal(t, test.js, js, "test %d: %v", i, test.v)
		}
	}
}

func TestEncode(t *testing.T)               { testEncode(t) }
func TestEncode_stringNumbers(t *testing.T) { testEncode_stringNumbers(t) }

func BenchmarkEncode(b *testing.B) {
	for i := 0; i < b.N; i++ {
		testEncode(b)
	}
}

func BenchmarkEncode_stringNumbers(b *testing.B) {
	for i := 0; i < b.N; i++ {
		testEncode_stringNumbers(b)
	}
}

func TestEncode_largeBytes(t *testing.T) {
	data := make([]byte, 4096)
	_, err := io.ReadFull(rand.Reader, data)
	require.NoError(t, err)
	enc := newEncoder(false)
	require.NoError(t, enc.encode(lisp.Bytes(data)))
	var s string
	err = json.Unmarshal(enc.bytes(), &s)
	require.NoError(t, err)
	decoded, err := base64.StdEncoding.DecodeString(s)
	require.NoError(t, err)
	require.Equal(t, data, decoded)
}
