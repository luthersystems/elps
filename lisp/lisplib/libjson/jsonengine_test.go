// Copyright © 2026 The ELPS authors

package libjson

import (
	"encoding/json"
	"sync"
	"testing"

	"github.com/stretchr/testify/require"

	"github.com/luthersystems/elps/lisp"
)

// JSONV2EngineForTest reports whether encoding/json runs on Go's JSON v2
// engine, Go 1.27's default.  GOEXPERIMENT=nojsonv2 selects the v1 engine.
// One result of this package still differs by engine
// (luthersystems/elps#928): TestNativeTextMarshalerInvalidUTF8 pins it.
// The function looks at the output of encoding/json, so it stays right on a
// Go that drops the experiment.
func JSONV2EngineForTest() bool { return jsonV2Engine() }

var jsonV2Engine = sync.OnceValue(func() bool {
	b, err := json.Marshal("\xff")
	return err != nil || string(b) != `"\ufffd"`
})

// TestStringEscapingIsEngineIndependent pins the bytes libjson writes for an
// lisp string with invalid UTF-8 to `\ufffd`, on both JSON engines.  A
// phylum can build such a string from bytes, and substrate hashes the
// output, so these bytes must not move with the engine.
//
// A Go string with invalid UTF-8 inside a native is refused: encoding/json
// would write it, and its bytes follow the engine.
func TestStringEscapingIsEngineIndependent(t *testing.T) {
	m := lisp.SortedMap()
	m.MapSetLVal(lisp.String("k\xfe"), lisp.String("v\xff"))
	// Byte for byte: JSONEq would accept a raw U+FFFD.
	wantMap := []byte(`{"k\ufffd":"v\ufffd"}`)
	for _, sn := range []bool{false, true} {
		got, err := Dump(lisp.String("a\xffb\xc3"), sn)
		require.NoError(t, err)
		require.Equal(t, `"a\ufffdb\ufffd"`, string(got))

		got, err = Dump(m, sn)
		require.NoError(t, err)
		require.Equal(t, wantMap, got)

		got, err = Dump(lisp.QExpr([]*lisp.LVal{lisp.Symbol("s\xff"), lisp.String("\xed\xa0\x80")}), sn)
		require.NoError(t, err)
		require.Equal(t, `["s\ufffd","\ufffd\ufffd\ufffd"]`, string(got))
	}
	// A message embedded in a native is libjson's own output, which
	// encoding/json copies.
	msg, err := Dump(m, false)
	require.NoError(t, err)
	got, err := Dump(lisp.Native(&ownMessage{msg: msg, loadable: true}), false) //elpsvet:allow-native test owns this message
	require.NoError(t, err)
	require.Equal(t, wantMap, got)

	_, err = Dump(lisp.Native("n\xff"), false) //elpsvet:allow-native test owns this native string
	require.EqualError(t, err, "unable to encode native value: a string is not valid UTF-8")
}

// engineBadText is a value whose MarshalText returns invalid UTF-8.
type engineBadText struct{}

func (engineBadText) MarshalText() ([]byte, error) { return []byte("t\xff"), nil }

// TestNativeTextMarshalerInvalidUTF8 pins the one native result that still
// depends on the JSON engine (luthersystems/elps#928): a MarshalText method
// of a value, not of a map key, that returns invalid UTF-8.
//
// The walk does not see the text.  It calls no marshal method of a value, so
// that encoding/json runs each one exactly once.  And the output does not
// show it on the v2 engine, where the bad byte becomes a U+FFFD that a valid
// text can hold too.  When this test fails because the two branches agree,
// delete JSONV2EngineForTest.
func TestNativeTextMarshalerInvalidUTF8(t *testing.T) {
	got, err := Dump(lisp.Native([]any{engineBadText{}}), false) //elpsvet:allow-native test owns this native
	require.NoError(t, err)
	if JSONV2EngineForTest() {
		require.Equal(t, "[\"t\xef\xbf\xbd\"]", string(got)) // U+FFFD
	} else {
		require.Equal(t, `["t\ufffd"]`, string(got))
	}
	// As a map key the walk has the text, and refuses it on both engines.
	_, err = Dump(lisp.Native(map[engineBadText]int{{}: 1}), false) //elpsvet:allow-native test owns this native
	require.EqualError(t, err, "unable to encode native value: a map key is not valid UTF-8")
}
