// Copyright © 2026 The ELPS authors

package libjson

import (
	"bytes"
	"encoding/json"
	"math"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/require"
)

// TestPlainStringBytesMatchEncodingJSON pins plain-mode string output to
// encoding/json's (HTML escaping, U+2028/9, invalid UTF-8 as U+FFFD), so the
// escaper shared with the typed encoder cannot change plain output.
func TestPlainStringBytesMatchEncodingJSON(t *testing.T) {
	strs := []string{"", "abc", "a\"b\\c", "<script>&</script>", "  ",
		"\x00\x01\x1f\x7f", "\b\f\n\r\t", "hé 世界 😀", "\xff\xfe", "a\xc3", "~^`",
		string([]byte{0xed, 0xa0, 0x80}), "\u007f\u0080￿"}
	for i := range 256 {
		strs = append(strs, string(rune(i)), "x"+string([]byte{byte(i)})+"y")
	}
	for _, s := range strs {
		want, err := json.Marshal(s)
		require.NoError(t, err)
		got, err := Dump(lisp.String(s), false)
		require.NoError(t, err)
		require.Equal(t, string(want), string(got), "%q", s)
	}
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
