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
// Tests use it for the native cases whose bytes or errors come from
// encoding/json and differ by engine (luthersystems/elps#928).  It looks at
// the output, so it stays right on a Go that drops the experiment.  It
// probes encoding/json itself rather than read nativeInvalidByteLen, so a
// wrong probe in the walk fails TestNativeInvalidByteLen.
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
// A Go string inside a native is written by encoding/json, so its bytes
// follow the engine.  The second half of the test records that difference.
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

	got, err = Dump(lisp.Native("n\xff"), false) //elpsvet:allow-native test owns this native string
	require.NoError(t, err)
	if JSONV2EngineForTest() {
		require.Equal(t, "\"n\xef\xbf\xbd\"", string(got)) // U+FFFD
	} else {
		require.Equal(t, `"n\ufffd"`, string(got))
	}
}

// TestNativeInvalidByteLen pins the native walk's count for one invalid
// UTF-8 byte to what each engine writes.  On the v1 engine it must stay 6,
// or the walk's step charges change.
func TestNativeInvalidByteLen(t *testing.T) {
	if JSONV2EngineForTest() {
		require.Equal(t, 3, nativeInvalidByteLen)
	} else {
		require.Equal(t, 6, nativeInvalidByteLen)
	}
	b, err := json.Marshal("a\xff")
	require.NoError(t, err)
	require.Equal(t, len(b), nativeStringLen("a\xff"))
}
