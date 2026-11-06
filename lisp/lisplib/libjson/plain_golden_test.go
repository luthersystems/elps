// Copyright © 2026 The ELPS authors

package libjson

import (
	"bytes"
	"encoding/json"
	"math"
	"testing"
	"unicode/utf8"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/require"
)

// TestPlainStringBytesMatchEncodingJSON pins plain-mode string output to
// encoding/json's (HTML escaping, U+2028/9), so the escaper shared with the
// typed encoder cannot change plain output.  Each invalid UTF-8 byte is the
// six-byte escape of the JSON v1 engine on both engines; the v2 engine
// writes a raw U+FFFD there (TestStringEscapingIsEngineIndependent).
func TestPlainStringBytesMatchEncodingJSON(t *testing.T) {
	strs := []string{"", "abc", "a\"b\\c", "<script>&</script>", "  ",
		"\x00\x01\x1f\x7f", "\b\f\n\r\t", "hé 世界 😀", "\xff\xfe", "a\xc3", "~^`",
		string([]byte{0xed, 0xa0, 0x80}), "\u007f\u0080￿"}
	for i := range 256 {
		strs = append(strs, string(rune(i)), "x"+string([]byte{byte(i)})+"y")
	}
	strs = append(strs, "\xef\xbf\xbd\xff") // a valid U+FFFD, then a bad byte
	for _, s := range strs {
		want := marshalStringV1(t, s)
		got, err := Dump(lisp.String(s), false)
		require.NoError(t, err)
		require.Equal(t, string(want), string(got), "%q", s)
	}
}

// marshalStringV1 is json.Marshal(s) as the JSON v1 engine writes it, on
// either engine: encoding/json escapes each run of valid UTF-8, and each
// invalid byte is the six-byte escape.
func marshalStringV1(t *testing.T, s string) []byte {
	t.Helper()
	orig := s
	out := []byte{'"'}
	for len(s) > 0 {
		n := 0
		for n < len(s) {
			r, size := utf8.DecodeRuneInString(s[n:])
			if r == utf8.RuneError && size == 1 {
				break
			}
			n += size
		}
		b, err := json.Marshal(s[:n])
		require.NoError(t, err)
		out = append(out, b[1:len(b)-1]...)
		if n < len(s) {
			out = append(out, "\\ufffd"...)
			n++
		}
		s = s[n:]
	}
	out = append(out, '"')
	if !JSONV2EngineForTest() {
		// On the v1 engine the reference must be json.Marshal itself.
		v1, err := json.Marshal(orig)
		require.NoError(t, err)
		require.Equal(t, string(v1), string(out), "%q", orig)
	}
	return out
}

// TestPlainGoldenDocument pins a mixed plain-mode document byte for byte.
func TestPlainGoldenDocument(t *testing.T) {
	m := lisp.SortedMap()
	m.MapSetLVal(lisp.String("b<"), lisp.Float(1e21))
	m.MapSetLVal(lisp.Symbol("a"), lisp.QExpr([]*lisp.LVal{lisp.Int(1), lisp.Float(0.1), lisp.Float(1e-7), lisp.Float(math.Copysign(0, -1))}))
	m.MapSetLVal(lisp.String("c"), lisp.Bytes([]byte{1, 2, 3}))
	m.MapSetLVal(lisp.String("d"), lisp.Symbol("true"))
	m.MapSetLVal(lisp.String("e"), lisp.Symbol(":kw"))
	got, err := Dump(m, false)
	require.NoError(t, err)
	// Byte-exact, not JSONEq: the point is that the bytes do not change.
	want := `{"a":[1,0.1,1e-7,-0],"b\u003c":1e+21,"c":"AQID","d":true,"e":":kw"}`
	require.True(t, bytes.Equal([]byte(want), got), "got %s", got)
}
