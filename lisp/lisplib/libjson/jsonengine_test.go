// Copyright © 2026 The ELPS authors

package libjson

import (
	"encoding"
	"encoding/json"
	"fmt"
	"sync"
	"testing"

	"github.com/stretchr/testify/require"

	"github.com/luthersystems/elps/lisp"
)

// JSONV2EngineForTest reports whether encoding/json runs on Go's JSON v2
// engine, Go 1.27's default.  GOEXPERIMENT=nojsonv2 selects the v1 engine.
// No result of this package differs by engine (luthersystems/elps#928,
// luthersystems/elps#950).
// Tests use the function to pick a reference: encoding/json itself is the
// reference only on the v1 engine.  The function looks at the output of
// encoding/json, so it stays right on a Go that drops the experiment.
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

// TestNativeTextMarshalerIsEngineIndependent pins one result, on both JSON
// engines, for a MarshalText method of a value that returns invalid UTF-8
// (luthersystems/elps#950).  libjson quotes the text itself, so each bad
// byte is the escape \ufffd, as the v1 engine writes it.  The v2 engine
// alone would write a raw U+FFFD, which a valid text can hold too.
//
// As a map key the walk has the text before anything is written, and
// refuses it on both engines.
func TestNativeTextMarshalerIsEngineIndependent(t *testing.T) {
	for _, native := range []any{
		[]any{engineBadText{}},
		[]engineBadText{{}},
		[]*engineBadText{{}},
		struct{ T engineBadText }{},
		map[string]encoding.TextMarshaler{"k": engineBadText{}},
	} {
		got, err := Dump(lisp.Native(native), false) //elpsvet:allow-native test owns each native
		require.NoError(t, err)
		require.Contains(t, string(got), `"t\ufffd"`, "%T", native)
		require.NotContains(t, string(got), "\ufffd", "%T", native)
	}
	_, err := Dump(lisp.Native(map[engineBadText]int{{}: 1}), false) //elpsvet:allow-native test owns this native
	require.EqualError(t, err, "unable to encode native value: a map key is not valid UTF-8")
}

// engineText counts its MarshalText calls.  Its pointer receiver means
// encoding/json calls it only on an addressable value.
type engineText struct {
	S     string
	calls *int
}

func (t *engineText) MarshalText() ([]byte, error) {
	*t.calls++
	return []byte(t.S), nil
}

// engineValueText has a value receiver: encoding/json calls it on any value.
type engineValueText string

func (t engineValueText) MarshalText() ([]byte, error) { return []byte("v:" + t), nil }

// engineIntText is a map key type of int kind whose text names the key.
type engineIntText int

func (t engineIntText) MarshalText() ([]byte, error) { return fmt.Appendf(nil, "i:%d", int(t)), nil }

// engineBothMarshalers has both methods.  MarshalJSON wins in both engines.
type engineBothMarshalers struct{}

func (engineBothMarshalers) MarshalJSON() ([]byte, error) { return []byte(`"json"`), nil }
func (engineBothMarshalers) MarshalText() ([]byte, error) { return []byte("text"), nil }

// enginePtrJSONValueText has MarshalJSON on the pointer and MarshalText on
// the value.  An addressable value uses MarshalJSON.  On an unaddressable
// one the v1 engine uses neither pointer method, so it uses MarshalText.
type enginePtrJSONValueText struct{ N int }

func (*enginePtrJSONValueText) MarshalJSON() ([]byte, error) { return []byte(`"json"`), nil }
func (enginePtrJSONValueText) MarshalText() ([]byte, error)  { return []byte("text"), nil }

// engineNilMarshalers has both methods on its pointer, and they work on a
// nil pointer.
type engineNilMarshalers struct{}

func (*engineNilMarshalers) MarshalJSON() ([]byte, error) { return []byte(`"called"`), nil }
func (*engineNilMarshalers) MarshalText() ([]byte, error) { return []byte("text"), nil }

type engineTextFields struct {
	P  engineText             `json:"p"`
	PP *engineText            `json:"pp"`
	V  engineValueText        `json:"v"`
	Q  engineValueText        `json:"q,string"`
	I  encoding.TextMarshaler `json:"i"`
	N  encoding.TextMarshaler `json:"n"`
	B  engineBothMarshalers   `json:"b"`
	X  enginePtrJSONValueText `json:"x"`
}

// TestNativeTextMarshalerMatchesEncodingJSON pins that libjson calls each
// MarshalText method where the v1 engine calls it, once, and writes the
// v1 engine's bytes for a valid text.  The cases cover receivers,
// addressability, nil values, methods that MarshalJSON beats and text that
// needs escaping.
func TestNativeTextMarshalerMatchesEncodingJSON(t *testing.T) {
	calls := 0
	text := func(s string) engineText { return engineText{S: s, calls: &calls} }
	ptr := func(s string) *engineText { x := text(s); return &x }
	for _, tc := range []struct {
		name      string
		native    any
		want      string
		wantCalls int
	}{
		{name: "pointer receiver, addressable", native: []engineText{text("a"), text("<&>\u2028")}, want: `["a","\u003c\u0026\u003e\u2028"]`, wantCalls: 2},
		{name: "pointer receiver, unaddressable", native: []any{text("a")}, want: `[{"S":"a"}]`},
		{name: "pointer receiver in a map value", native: map[string]engineText{"k": text("a")}, want: `{"k":{"S":"a"}}`},
		{name: "pointer", native: []any{ptr("a"), (*engineText)(nil)}, want: `["a",null]`, wantCalls: 1},
		{name: "value receiver", native: []any{engineValueText("a"), (*engineValueText)(nil)}, want: `["v:a",null]`},
		{name: "fields through a pointer", native: &engineTextFields{P: text("p"), PP: ptr("pp"), V: "v", Q: "q", I: engineValueText("i"), X: enginePtrJSONValueText{}},
			want: `{"p":"p","pp":"pp","v":"v:v","q":"v:q","i":"v:i","n":null,"b":"json","x":"json"}`, wantCalls: 2},
		{name: "fields of a value", native: engineTextFields{P: text("p"), I: ptr("i")},
			want: `{"p":{"S":"p"},"pp":null,"v":"v:","q":"v:","i":"i","n":null,"b":"json","x":"text"}`, wantCalls: 1},
		// encoding/json calls no MarshalText of a key of string kind.
		{name: "string keys and text values", native: map[engineValueText]engineValueText{"b": "1", "a": "2"}, want: `{"a":"v:2","b":"v:1"}`},
		{name: "text keys and values", native: map[engineIntText]engineValueText{10: "1", 9: "2"}, want: `{"i:10":"v:1","i:9":"v:2"}`},
		{name: "top level", native: engineValueText("<"), want: `"v:\u003c"`},
		{name: "empty text", native: []engineText{text("")}, want: `[""]`, wantCalls: 1},
		// An interface whose type has the method holds a nil pointer: the
		// v1 engine calls the method on it.  A pointer field, or an
		// interface without the method, writes null.
		{name: "nil pointer in a Marshaler interface", native: struct{ F json.Marshaler }{(*engineNilMarshalers)(nil)}, want: `{"F":"called"}`},
		{name: "nil pointers in Marshaler interfaces", native: []json.Marshaler{(*engineNilMarshalers)(nil), &engineNilMarshalers{}}, want: `["called","called"]`},
		{name: "nil pointer in a Marshaler map value", native: map[string]json.Marshaler{"k": (*engineNilMarshalers)(nil)}, want: `{"k":"called"}`},
		{name: "nil pointer in a TextMarshaler interface", native: struct{ F encoding.TextMarshaler }{(*engineNilMarshalers)(nil)}, want: `{"F":"text"}`},
		{name: "nil pointer as a TextMarshaler key", native: map[encoding.TextMarshaler]int{(*engineNilMarshalers)(nil): 1}, want: `{"text":1}`},
		{name: "nil pointers elsewhere", native: []any{(*engineNilMarshalers)(nil), []*engineNilMarshalers{nil}, struct{ P *engineNilMarshalers }{}}, want: `[null,[null],{"P":null}]`},
	} {
		t.Run(tc.name, func(t *testing.T) {
			calls = 0
			got, err := Dump(lisp.Native(tc.native), false) //elpsvet:allow-native test owns each native
			require.NoError(t, err)
			require.Equal(t, tc.want, string(got))
			require.Equal(t, tc.wantCalls, calls, "MarshalText calls")
			calls = 0
			ref, err := json.Marshal(tc.native)
			require.NoError(t, err)
			require.Equal(t, string(ref), string(got), "encoding/json")
		})
	}
}
