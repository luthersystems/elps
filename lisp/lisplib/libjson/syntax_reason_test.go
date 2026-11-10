// Copyright © 2026 The ELPS authors

package libjson

import (
	"encoding/json"
	"errors"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// syntaxLoadModes are the load paths that read a document with encoding/json.
var syntaxLoadModes = map[string]LoadOpts{
	"default":        {},
	"string-numbers": {StringNumbers: true},
	"exact-integers": {ExactIntegers: true},
}

// TestLoadSyntaxErrorTextIsEngineIndependent pins the text of
// json:syntax-error for each kind of malformed document.  The text is this
// package's, so it is the same on both JSON engines and on every load path
// (luthersystems/elps#928).  CI runs this test on both engines.
func TestLoadSyntaxErrorTextIsEngineIndependent(t *testing.T) {
	deep := func(n int) string { return strings.Repeat("[", n) + strings.Repeat("]", n) }
	for _, tc := range []struct {
		doc  string
		want string
	}{
		{``, "offset 0: unexpected end of input"},
		{" \n", "offset 2: unexpected end of input"},
		{`{`, "offset 1: unexpected end of input"},
		{`{"a":[1,2`, "offset 9: unexpected end of input"},
		{`"abc`, "offset 4: unexpected end of input"},
		{`"ab\`, "offset 4: unexpected end of input"},
		{`"\u12`, "offset 5: unexpected end of input"},
		{`tru`, "offset 3: unexpected end of input"},
		{`-`, "offset 1: unexpected end of input"},
		{`1.`, "offset 2: unexpected end of input"},
		{`[1e`, "offset 3: unexpected end of input"},
		{`{"a"`, "offset 4: unexpected end of input"},
		{`{"a":`, "offset 5: unexpected end of input"},
		{`[1,`, "offset 3: unexpected end of input"},

		{`x`, "offset 0: expected a value"},
		{`[1,]`, "offset 3: expected a value"},
		{`{"a":}`, "offset 5: expected a value"},
		{`[}`, "offset 1: expected a value"},
		{`'a'`, "offset 0: expected a value"},
		{`+1`, "offset 0: expected a value"},
		{`.5`, "offset 0: expected a value"},
		{"\xef\xbb\xbf1", "offset 0: expected a value"},
		{`NaN`, "offset 0: expected a value"},

		{`trux`, "offset 3: invalid literal"},
		{`[nul]`, "offset 4: invalid literal"},
		{`False`, "offset 0: expected a value"},
		{`falsy`, "offset 4: invalid literal"},

		{`-x`, "offset 1: invalid number"},
		{`[1.x]`, "offset 3: invalid number"},
		{`1e+,`, "offset 3: invalid number"},
		{`[-]`, "offset 2: invalid number"},

		{"\"a\nb\"", "offset 2: control character in string"},
		{"[\"\x00\"]", "offset 2: control character in string"},
		{`"\x"`, "offset 2: invalid escape in string"},
		{`"\u12g4"`, "offset 5: invalid escape in string"},
		{`{"a\q":1}`, "offset 4: invalid escape in string"},

		{`{1:2}`, "offset 1: expected a string key"},
		{`{"a":1,}`, "offset 7: expected a string key"},
		{`{]`, "offset 1: expected a string key"},
		{`{"a" 1}`, "offset 5: expected ':' after the key"},
		{`{"a":1 "b":2}`, "offset 7: expected ',' or '}' in object"},
		{`{"a":1]`, "offset 6: expected ',' or '}' in object"},
		{`[1 2]`, "offset 3: expected ',' or ']' in array"},
		{`[1}`, "offset 2: expected ',' or ']' in array"},
		{`[01]`, "offset 2: expected ',' or ']' in array"},

		{`1 2`, "offset 2: unexpected data after the top-level value"},
		{`{}x`, "offset 2: unexpected data after the top-level value"},
		{`[]]`, "offset 2: unexpected data after the top-level value"},
		{`01`, "offset 1: unexpected data after the top-level value"},
		{`1x`, "offset 1: unexpected data after the top-level value"},
		{`nullnull`, "offset 4: unexpected data after the top-level value"},

		{deep(10_001), "offset 10000: nests more than 10000 levels deep"},
		{strings.Repeat(`{"a":`, 10_001), "offset 50000: nests more than 10000 levels deep"},
	} {
		name := tc.doc
		if len(name) > 30 {
			name = name[:30]
		}
		t.Run(name, func(t *testing.T) {
			for mode, opts := range syntaxLoadModes {
				v := LoadWith([]byte(tc.doc), opts)
				require.Equal(t, lisp.LError, v.Type, "%s: %v", mode, v)
				assert.Equal(t, "json:syntax-error", v.Str, mode)
				require.Len(t, v.Cells, 1, mode)
				err, ok := v.Cells[0].Native.(error)
				require.True(t, ok, "%s: %v", mode, v.Cells[0])
				assert.Equal(t, "json: invalid JSON at "+tc.want, err.Error(), mode)
			}
		})
	}
	// The limit is a nesting of 10000, and the documents at it load.
	for mode, opts := range syntaxLoadModes {
		for _, doc := range []string{deep(10_000), strings.Repeat(`{"a":`, 10_000) + "1" + strings.Repeat("}", 10_000)} {
			_, reason := jsonFault([]byte(doc))
			require.Empty(t, reason, mode)
			require.NoError(t, DefaultSerializer().jsonDecodeOpts([]byte(doc), new(any), opts), mode)
		}
	}
}

// engineRefusesSyntax reports whether encoding/json refuses doc as malformed
// on the load path of opts.  A number that does not fit a float is not a
// syntax error.
func engineRefusesSyntax(t testing.TB, doc []byte, opts LoadOpts) bool {
	t.Helper()
	var x any
	err := DefaultSerializer().jsonDecodeOpts(doc, &x, opts)
	if err == nil {
		return false
	}
	var rangeErr floatRangeError
	if errors.As(err, &rangeErr) {
		return false
	}
	var syntaxErr *json.SyntaxError
	var exactErr syntaxError
	if !errors.As(err, &syntaxErr) && !errors.As(err, &exactErr) {
		t.Fatalf("%q: an error that is neither a range nor a syntax error: %T %v", doc, err, err)
	}
	return true
}

// FuzzSyntaxReasonMatchesEngine checks that jsonFault and encoding/json agree
// on which documents are malformed, on every load path.  So a document the
// engine refuses always gets a reason, and never the fixed fallback text of
// invalidJSONError.  Run on both JSON engines, it also shows that the two
// engines refuse the same documents (luthersystems/elps#928).
func FuzzSyntaxReasonMatchesEngine(f *testing.F) {
	for _, seed := range []string{
		``, ` `, `1`, `-0`, `1.5e+3`, `01`, `1.`, `-`, `1e`, `"a"`, `"\u00e9\n"`, `"\ud800"`, `"\ud800\udc00"`,
		"\"\xff\"", "\"\xed\xa0\x80\"", `"\x"`, "\"\t\"", `true`, `nul`, `[]`, `[1,2]`, `[1,]`, `{}`, `{"a":1}`,
		`{"a":1,"a":2}`, `{"a"}`, `{1:2}`, `1 2`, `[1]]`, "\xef\xbb\xbf{}", `{"a":[{"b":null}]}`, "[1\x00]",
		`1e400`, `[1E-400]`, `123456789012345678901234567890`, ` [ 1 , "x" ] `, "\x0c1", `/**/1`, `[1,/**/2]`,
		strings.Repeat("[", 50) + strings.Repeat("]", 50), "\"\\u\"", "\"\x7f\"", "1\n", "\"a\"\"b\"",
	} {
		f.Add([]byte(seed))
	}
	f.Fuzz(func(t *testing.T, doc []byte) {
		offset, reason := jsonFault(doc)
		if offset < 0 || offset > len(doc) {
			t.Fatalf("%q: offset %d is outside of the document", doc, offset)
		}
		for mode, opts := range syntaxLoadModes {
			if refused := engineRefusesSyntax(t, doc, opts); refused != (reason != "") {
				t.Fatalf("%q, %s: encoding/json refuses it: %v; jsonFault reason: %q at %d", doc, mode, refused, reason, offset)
			}
		}
	})
}
