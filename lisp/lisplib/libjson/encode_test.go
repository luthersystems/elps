package libjson

import (
	"crypto/rand"
	"encoding/base64"
	"encoding/json"
	"io"
	mathrand "math/rand"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// this function is internal because users are not supposed to construct
// literal SortedMap values in their applications =\
func literalSortedMap(m SortedMap) *lisp.LVal {
	return lisp.SortedMapFromData(lisp.NewMapData(m))
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
	{lisp.String("a&<>"), `"a\u0026\u003c\u003e"`},
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

// unencodableTypes lists every lisp.LType the JSON encoder deliberately
// refuses.  Anything not listed here must have an entry in encoderFuncs.
var unencodableTypes = map[lisp.LType]string{
	lisp.LInvalid:       "not a real type",
	lisp.LError:         "errors are a control-flow value, not data",
	lisp.LQSymbol:       "legacy type with no JSON meaning",
	lisp.LFun:           "functions have no serialized form",
	lisp.LMarkTerminal:  "interpreter-internal marker",
	lisp.LMarkTailRec:   "interpreter-internal marker",
	lisp.LMarkMacExpand: "interpreter-internal marker",
}

// TestEncoderTypeCoverage is a drift guard on the JSON serialization surface.
//
// The encoder dispatches on the encoderFuncs table rather than a switch, so
// the exhaustive linter cannot see it: a newly added lisp.LType would get a
// nil table entry with nothing flagging it at build time.  Downstream this
// output is chaincode state, so a type that quietly serialized to nothing
// would be written to a ledger.  This test forces the choice -- register an
// encoder, or record here why the type has no JSON form -- and pins the
// runtime behaviour for the refused types (an error, never empty output).
func TestEncoderTypeCoverage(t *testing.T) {
	for typ := lisp.LInvalid; typ < lisp.LTypeMax; typ++ {
		reason, refused := unencodableTypes[typ]
		hasFunc := encoderFuncs[typ] != nil
		switch {
		case refused && hasFunc:
			t.Errorf("LType %v is listed as unencodable (%s) but has an encoder", typ, reason)
		case !refused && !hasFunc:
			t.Errorf("LType %v has no encoder and is not listed in unencodableTypes: "+
				"register an encoder in encode.go or add it to the list with a reason", typ)
		}
	}
}

// TestEncodeUnregisteredTypeErrors proves the refused types fail loudly.
func TestEncodeUnregisteredTypeErrors(t *testing.T) {
	for typ, reason := range unencodableTypes {
		enc := newEncoder(false)
		err := enc.encode(&lisp.LVal{Type: typ})
		require.Error(t, err, "encoding %v (%s) must fail", typ, reason)
		assert.Contains(t, err.Error(), "invalid type encountered")
		assert.Empty(t, enc.bytes(), "a refused type must not write partial output")
	}
}

func BenchmarkEncode(b *testing.B) {
	for range b.N {
		testEncode(b)
	}
}

func BenchmarkEncode_stringNumbers(b *testing.B) {
	for range b.N {
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

func TestEncoded_stringCompat(t *testing.T) {
	rand := mathrand.New(mathrand.NewSource(1234)) //#nosec G404
	for range 100 {
		s := string(randBytes(rand, 1024))
		canonical, err := json.Marshal(s)
		if !assert.NoError(t, err, "canonical encoding of %q", s) {
			continue
		}
		enc := newEncoder(true)
		if assert.NoError(t, enc.encode(lisp.String(s)), "elps encoding of %q", s) {
			assert.Equal(t, enc.bytes(), canonical)
		}
	}
}

func randBytes(r *mathrand.Rand, maxLen int) []byte {
	n := r.Intn(maxLen)
	b := make([]byte, n)
	for i := range b {
		b[i] = byte(r.Intn(256))
	}
	return b
}

func TestLoadMaxAllocArray(t *testing.T) {
	s := DefaultSerializer()

	t.Run("exceeds", func(t *testing.T) {
		// JSON array with 5 elements, limit 3.
		result := s.LoadMax([]byte(`[1,2,3,4,5]`), false, 3)
		require.Equal(t, lisp.LError, result.Type, "expected error, got: %v", result)
		assert.Contains(t, result.String(), "allocation size 5 exceeds maximum (3)")
	})

	t.Run("within limit", func(t *testing.T) {
		result := s.LoadMax([]byte(`[1,2,3]`), false, 10)
		require.NotEqual(t, lisp.LError, result.Type, "unexpected error: %v", result)
		assert.Equal(t, 3, result.Len())
	})
}

func TestLoadMaxAllocMap(t *testing.T) {
	s := DefaultSerializer()

	t.Run("exceeds", func(t *testing.T) {
		// JSON object with 3 keys, limit 2.
		result := s.LoadMax([]byte(`{"a":1,"b":2,"c":3}`), false, 2)
		require.Equal(t, lisp.LError, result.Type, "expected error, got: %v", result)
		assert.Contains(t, result.String(), "allocation size 3 exceeds maximum (2)")
	})

	t.Run("within limit", func(t *testing.T) {
		result := s.LoadMax([]byte(`{"a":1,"b":2}`), false, 10)
		require.NotEqual(t, lisp.LError, result.Type, "unexpected error: %v", result)
		assert.Equal(t, 2, result.Len())
	})
}
