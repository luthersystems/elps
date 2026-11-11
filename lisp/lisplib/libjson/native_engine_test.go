// Copyright © 2026 The ELPS authors

package libjson

import (
	"encoding/json"
	"errors"
	"math"
	"strings"
	"testing"
	"time"

	"github.com/stretchr/testify/require"

	"github.com/luthersystems/elps/lisp"
)

type engineFailJSON struct{}

func (engineFailJSON) MarshalJSON() ([]byte, error) { return nil, errors.New("boom") }

type engineFailText struct{}

func (engineFailText) MarshalText() ([]byte, error) { return nil, errors.New("text boom") }

// engineRawJSON returns its text from MarshalJSON, valid or not.
type engineRawJSON string

func (r engineRawJSON) MarshalJSON() ([]byte, error) { return []byte(r), nil }

type engineRing struct{ Next *engineRing }

// engineNest wraps v in n slices.
func engineNest(v any, n int) any {
	for range n {
		v = []any{v}
	}
	return v
}

// TestNativeResultsAreEngineIndependent pins one result, on both JSON
// engines, for each native that used to give a result per engine
// (luthersystems/elps#928).  The walk refuses these natives itself, or this
// package reports the marshal method's failure in its own words, so no row
// carries text or bytes of encoding/json.
//
// CI runs this test on the v2 engine and, in the json-v1 job, on v1.  A row
// that needs JSONV2EngineForTest would be a regression.
func TestNativeResultsAreEngineIndependent(t *testing.T) {
	const prefix = "unable to encode native value: "
	const badMethodJSON = prefix + "a marshal method returned JSON that is not valid or nests more than 10000 levels deep"
	const badString = prefix + "a string is not valid UTF-8"
	const badKey = prefix + "a map key is not valid UTF-8"
	nest := func(n int) string { return strings.Repeat("[", n) + strings.Repeat("]", n) }
	ring := &engineRing{}
	ring.Next = &engineRing{Next: ring}
	self := map[string]any{}
	self["self"] = self
	epoch := time.Unix(0, 0).UTC()
	farEast := time.Date(2000, 1, 1, 0, 0, 0, 0, time.FixedZone("", 86400))
	for _, tc := range []struct {
		name    string
		native  any
		want    string // the bytes, when wantErr is empty
		wantErr string
	}{
		// Map key kinds.  The v2 engine writes the first three, and the
		// empty complex map.
		{name: "float keys", native: map[float64]int{1.5: 1}, wantErr: prefix + "unsupported type map[float64]int"},
		{name: "nil float keys", native: map[float32]int(nil), wantErr: prefix + "unsupported type map[float32]int"},
		{name: "nil bool keys", native: map[bool]int(nil), wantErr: prefix + "unsupported type map[bool]int"},
		{name: "bool keys", native: map[bool]int{true: 1}, wantErr: prefix + "unsupported type map[bool]int"},
		{name: "empty complex keys", native: map[complex64]int{}, wantErr: prefix + "unsupported type map[complex64]int"},
		{name: "nested float keys", native: []any{1, map[float64]any{}}, wantErr: prefix + "unsupported type map[float64]interface {}"},
		{name: "int keys", native: map[int]int{10: 1, 9: 2}, want: `{"10":1,"9":2}`},

		// Types and numbers with no JSON form.
		{name: "chan", native: []any{make(chan int)}, wantErr: prefix + "unsupported type chan int"},
		{name: "NaN", native: []float64{1, math.NaN()}, wantErr: prefix + "unsupported number NaN"},
		{name: "infinity", native: float32(math.Inf(-1)), wantErr: prefix + "unsupported number -Inf"},

		// Cycles and depth.
		{name: "pointer cycle", native: ring, wantErr: prefix + "value of type *libjson.engineRing contains itself"},
		{name: "map cycle", native: self, wantErr: prefix + "value of type map[string]interface {} contains itself"},
		{name: "nested 10000", native: engineNest(1, 10_000), want: nest(10_000)[:10_000] + "1" + nest(10_000)[10_000:]},
		{name: "nested 10001", native: engineNest([]any{}, 10_000), wantErr: prefix + "JSON nests more than 10000 levels deep"},
		{name: "nested 30000", native: engineNest(1, 30_000), wantErr: prefix + "JSON nests more than 10000 levels deep"},

		// json.Number and json.RawMessage.
		{name: "invalid number", native: json.Number("1x"), wantErr: prefix + `invalid json.Number "1x"`},
		{name: "empty number", native: json.Number(""), want: "0"},
		{name: "invalid raw message", native: json.RawMessage(`{"a"`), wantErr: badMethodJSON},
		{name: "empty raw message", native: json.RawMessage{}, wantErr: badMethodJSON},
		{name: "deep raw message", native: json.RawMessage(nest(10_001)), wantErr: badMethodJSON},
		{name: "invalid raw message pointer", native: []any{1, new(json.RawMessage("tru"))}, wantErr: badMethodJSON},
		{name: "raw message too deep where it sits", native: engineNest(json.RawMessage(nest(10)), 9995), wantErr: badMethodJSON},
		{name: "raw number out of range", native: []any{json.RawMessage("1E1000")}, wantErr: prefix + "json: cannot unmarshal number 1E1000 into Go value of type float64"},
		{name: "number out of range", native: map[string]any{"a": json.Number("-1e999")}, wantErr: prefix + "json: cannot unmarshal number -1e999 into Go value of type float64"},

		// time.Time.  The v2 engine formats it without calling MarshalJSON.
		{name: "time", native: []any{time.Unix(0, 0).UTC(), &epoch, (*time.Time)(nil)}, want: `["1970-01-01T00:00:00Z","1970-01-01T00:00:00Z",null]`},
		{name: "time year", native: time.Date(10_000, 1, 1, 0, 0, 0, 0, time.UTC), wantErr: prefix + "time.Time has year 10000, outside of 0 to 9999"},
		{name: "time negative year", native: struct{ T time.Time }{time.Date(-1, 1, 1, 0, 0, 0, 0, time.UTC)}, wantErr: prefix + "time.Time has year -1, outside of 0 to 9999"},
		{name: "time zone", native: &farEast, wantErr: prefix + "time.Time has a zone 86400 seconds from UTC, which is 24 hours or more"},
		{name: "time zone at the limit", native: time.Date(2000, 1, 1, 0, 0, 0, 0, time.FixedZone("", -86399)), want: `"2000-01-01T00:00:00-23:59"`},

		// Invalid UTF-8.  The v1 engine writes the escape \ufffd for a bad
		// byte and the v2 engine a raw U+FFFD.
		{name: "invalid UTF-8 string", native: "a\xff", wantErr: badString},
		{name: "invalid UTF-8 string in a slice", native: []any{"ok", "\xed\xa0\x80"}, wantErr: badString},
		{name: "invalid UTF-8 string, typed", native: []engineString{"a\xc3"}, wantErr: badString},
		{name: "invalid UTF-8 string in a field", native: engineTagged{S: "\xfe"}, wantErr: badString},
		{name: "invalid UTF-8 quoted string field", native: engineTagged{Q: "\xfe"}, wantErr: badString},
		{name: "invalid UTF-8 key", native: map[string]any{"k\xff": 1}, wantErr: badKey},
		{name: "invalid UTF-8 key, typed", native: map[engineString]int{"k\xff": 1}, wantErr: badKey},
		{name: "invalid UTF-8 key from MarshalText", native: map[engineBadText]int{{}: 1}, wantErr: badKey},
		// libjson quotes the text of a MarshalText value itself, so it
		// writes each bad byte as the v1 engine does.
		{name: "invalid UTF-8 from MarshalText", native: []any{engineBadText{}}, want: `["t\ufffd"]`},
		{name: "invalid UTF-8 from MarshalText in a field", native: struct{ T *engineBadText }{&engineBadText{}}, want: `{"T":"t\ufffd"}`},
		{name: "replacement character", native: map[string]any{"k\ufffd": "v\ufffd"}, want: "{\"k\xef\xbf\xbd\":\"v\xef\xbf\xbd\"}"},
		{name: "invalid UTF-8 in bytes", native: []byte{0xff}, want: `"/w=="`},
		{name: "invalid UTF-8 in an omitted field", native: engineTagged{O: ""}, want: `{"s":"","Q":"\"\""}`},

		// Struct tags the engines read differently.
		{name: "tag with a format option", native: engineFormatTag{}, wantErr: prefix + "struct field libjson.engineFormatTag.T has a json tag with a format option"},
		{name: "tag with a format option, in a pointer", native: []any{&engineFormatTag{}}, wantErr: prefix + "struct field libjson.engineFormatTag.T has a json tag with a format option"},
		{name: "tag with a format option, embedded", native: engineEmbedsFormat{}, wantErr: prefix + "struct field libjson.engineFormatTag.T has a json tag with a format option"},
		{name: "tag with a format option, nil pointer", native: struct{ P *engineFormatTag }{}, want: `{"P":null}`},
		{name: "tag name with a quote", native: engineQuoteTag{}, wantErr: prefix + `struct field libjson.engineQuoteTag.A has the json tag name "a'b", which encoding/json does not accept`},
		{name: "tag name with a backslash", native: engineBackslashTag{}, wantErr: prefix + `struct field libjson.engineBackslashTag.A has the json tag name "a\\b", which encoding/json does not accept`},
		// reflect reads the bad byte of this tag as U+FFFD.
		{name: "tag name with invalid UTF-8", native: engineUTF8Tag{}, wantErr: prefix + `struct field libjson.engineUTF8Tag.A has the json tag name "a�b", which encoding/json does not accept`},
		{name: "tags both engines read alike", native: engineGoodTags{}, want: `{" a b":0,"é":0,"!#$%\u0026()*+-./:;\u003c=\u003e?@[]^_{|}~":0,"d":0,"e":0}`},

		// Marshal methods.
		{name: "failing MarshalJSON", native: engineFailJSON{}, wantErr: prefix + "a marshal method failed: boom"},
		{name: "failing MarshalJSON in a slice", native: []engineFailJSON{{}}, wantErr: prefix + "a marshal method failed: boom"},
		{name: "failing MarshalText", native: &engineFailText{}, wantErr: prefix + "a marshal method failed: text boom"},
		{name: "failing key MarshalText", native: map[engineFailText]int{{}: 1}, wantErr: prefix + "map key MarshalText failed: text boom"},
		{name: "invalid MarshalJSON output", native: engineRawJSON("[1 2]"), wantErr: badMethodJSON},
		{name: "empty MarshalJSON output", native: engineRawJSON(""), wantErr: badMethodJSON},
		{name: "MarshalJSON output too deep where it sits", native: engineNest(engineRawJSON(nest(10)), 9995), wantErr: badMethodJSON},
		// Two marshal problems: the first in the output's order is reported.
		{name: "invalid raw message before a failing method", native: []any{json.RawMessage("tru"), engineFailJSON{}}, wantErr: badMethodJSON},
		{name: "failing method before an invalid raw message", native: map[string]any{"a": engineFailJSON{}, "b": json.RawMessage("tru")}, wantErr: prefix + "a marshal method failed: boom"},
		{name: "invalid raw message in a map before a failing method", native: map[string]any{"b": engineFailJSON{}, "a": json.RawMessage("tru"), "c": &engineFailText{}}, wantErr: badMethodJSON},
		// A problem the walk sees wins over both, wherever it sits.
		{name: "walk problem after marshal problems", native: []any{json.RawMessage("tru"), engineFailJSON{}, math.NaN()}, wantErr: prefix + "unsupported number NaN"},
		{name: "MarshalJSON output", native: engineRawJSON(` [ 1 , "a" ] `), want: `[1,"a"]`},
	} {
		t.Run(tc.name, func(t *testing.T) {
			for _, sn := range []bool{false, true} {
				got, err := Dump(lisp.Native(tc.native), sn) //elpsvet:allow-native test owns each native input
				if sn && strings.Contains(tc.wantErr, "cannot unmarshal number") {
					// :string-numbers loads the literal back as text.
					require.NoError(t, err)
					continue
				}
				if tc.wantErr != "" {
					require.EqualError(t, err, tc.wantErr)
					var refused encodeUnloadableNativeError
					require.ErrorAs(t, err, &refused)
					continue
				}
				require.NoError(t, err)
				require.Equal(t, tc.want, string(got))
			}
		})
	}
}

type engineString string

type engineTagged struct {
	S string `json:"s"`
	Q string `json:",string"`
	O string `json:",omitempty"`
}

type engineFormatTag struct {
	A int
	T time.Time `json:"t,format:RFC822"`
}

type engineEmbedsFormat struct {
	engineFormatTag
	B int
}

type engineQuoteTag struct {
	A int `json:"a'b"` //nolint:staticcheck // the malformed tag is what the test is about
}

type engineBackslashTag struct {
	A int `json:"a\\b"` //nolint:staticcheck // the malformed tag is what the test is about
}

type engineUTF8Tag struct {
	A int "json:\"a\xffb\""
}

// engineGoodTags has the tags closest to the refused ones that both engines
// read alike.
type engineGoodTags struct {
	A int `json:" a b"`
	B int `json:"é"`
	C int `json:"!#$%&()*+-./:;<=>?@[]^_{|}~"`
	D int `json:"d,formatx"` //nolint:staticcheck // the malformed tag is what the test is about
	E int `json:"e,format"`  //nolint:staticcheck // the malformed tag is what the test is about
	F int `json:"-"`
}

// engineCountJSON counts its MarshalJSON calls.
type engineCountJSON struct{ calls *int }

func (c engineCountJSON) MarshalJSON() ([]byte, error) {
	*c.calls++
	return []byte("1"), nil
}

// A value marshaler runs once in a dump that succeeds, and not at all when
// the walk refuses the native.  The walk used to call marshalers to learn
// which error came first, and encoding/json then called them again.
func TestNativeMarshalerRunsAtMostOnce(t *testing.T) {
	calls := 0
	m := engineCountJSON{&calls}
	got, err := Dump(lisp.Native([]any{m, map[string]any{"k": m}}), false) //elpsvet:allow-native test owns this native
	require.NoError(t, err)
	require.Equal(t, `[1,{"k":1}]`, string(got))
	require.Equal(t, 2, calls, "one call for each place the marshaler sits")

	self := map[string]any{}
	self["self"] = self
	for name, native := range map[string]any{
		"cycle":       []any{m, self},
		"bad map key": []any{m, map[engineFailText]int{{}: 1}},
		"float keys":  []any{m, map[float64]int{}},
		"NaN":         []any{m, math.NaN()},
		"too deep":    []any{m, engineNest(1, 10_001)},
		"too big":     []any{m, strings.Repeat("x", 4096)},
	} {
		calls = 0
		enc := getEncoder(false)
		err := enc.encodeLimit(lisp.Native(native), lisp.MaxValueDepth, encodeBudget{maxBytes: 1024}, encodeMeter{}) //elpsvet:allow-native test owns each native input
		putEncoder(enc)
		require.Error(t, err, name)
		require.Zero(t, calls, name)
	}
}
