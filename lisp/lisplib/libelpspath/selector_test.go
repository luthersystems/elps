// Copyright © 2026 The ELPS authors

package libelpspath

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
)

// TestParseSelector is the jq-selector corpus, ported with the parser from
// luthersystems/substrate (issue #564).
//
// It crosses every selector shape with all seven Path operations and checks
// the result as JSON, which makes roughly a fifth of it a test of the ENGINE
// rather than of the parser: iterator collapse, range splice arity,
// delete-of-root, in-place versus copy. That half was being spent downstream
// on code that lives here.
//
// The expectations were checked against jq (https://jqplay.org) where the
// two agree. Where they do not, the case says so -- the iterator cases are
// the ones that differ, because jq raises on an element a selector cannot be
// applied to and elpspath yields null for it.
//
// PathMethod, the method constants and jsonEqual are shared with
// query_test.go, which drives the same seven operations from positional
// steps instead of from a string.
func TestParseSelector(t *testing.T) {
	t.Parallel()
	testCases := []struct {
		Name     string
		In       string
		Path     string
		Expected string
		Method   PathMethod
		NewIn    string
		HasError bool
	}{
		{
			Name:     "empty",
			Method:   Get,
			In:       `{"hello":"world"}`,
			Path:     "",
			HasError: true,
		},
		{
			Name:     "dot",
			Method:   Get,
			In:       `{"hello":"world"}`,
			Path:     ".",
			Expected: `{"hello":"world"}`,
		},
		{
			Name:     "dot",
			Method:   Set,
			In:       `{"hello":"world"}`,
			NewIn:    `{"fnord":"fnord"}`,
			Path:     ".",
			Expected: `{"fnord":"fnord"}`,
		},
		{
			// Deleting the root deletes everything, and the engine now
			// says so with a lisp nil rather than a Go nil *LVal. The
			// old Go nil was not a value a builtin could return: it
			// reached the evaluator as a nil pointer.
			Name:     "dot",
			Method:   Del,
			In:       `{"hello":"world"}`,
			Path:     ".",
			Expected: `null`,
		},
		{
			Name:     "dot",
			Method:   SetMutate,
			In:       `{"hello":"world"}`,
			NewIn:    `{"fnord":"fnord"}`,
			Path:     ".",
			HasError: true,
		},
		{
			Name:     "nothing",
			Method:   Get,
			In:       `{"hello":"world"}`,
			Path:     "",
			HasError: true,
		},
		{
			Name:     "array no dot",
			Method:   Get,
			In:       `["hello"]`,
			Path:     `[0]`,
			HasError: true,
		},
		{
			Name:     "simple",
			Method:   Get,
			In:       `{"hello":"world"}`,
			Path:     ".hello",
			Expected: `"world"`,
		},
		{
			Name:     "simple special",
			Method:   Get,
			In:       `{"$private":"world"}`,
			Path:     `.["$private"]`,
			Expected: `"world"`,
		},
		{
			Name:     "simple underscore special",
			Method:   Get,
			In:       `{"_private":"world"}`,
			Path:     `._private`,
			Expected: `"world"`,
		},
		{
			Name:     "simple quote special",
			Method:   Get,
			In:       `{"\"\n":"world"}`,
			Path:     `.["\"\n"]`,
			Expected: `"world"`,
		},
		{
			Name:     "simple set!",
			Method:   SetMutate,
			In:       `{"hello":"world"}`,
			Path:     ".hello",
			NewIn:    "42",
			Expected: `{"hello":42}`,
		},
		{
			Name:     "simple set",
			Method:   Set,
			In:       `{"hello":"world"}`,
			Path:     ".hello",
			NewIn:    "42",
			Expected: `{"hello":42}`,
		},
		{
			Name:     "simple delete!",
			Method:   DelMutate,
			In:       `{"hello":"world"}`,
			Path:     ".hello",
			Expected: `{}`,
		},
		{
			Name:     "simple delete",
			Method:   Del,
			In:       `{"hello":"world"}`,
			Path:     ".hello",
			Expected: `{}`,
		},
		{
			Name:     "simple set! new key",
			Method:   SetMutate,
			In:       `{"hello":"world"}`,
			Path:     ".foo",
			NewIn:    `"bar"`,
			Expected: `{"foo":"bar","hello":"world"}`,
		},
		{
			Name:     "simple set new key",
			Method:   SetMutate,
			In:       `{"hello":"world"}`,
			Path:     ".foo",
			NewIn:    `"bar"`,
			Expected: `{"foo":"bar","hello":"world"}`,
		},
		{
			Name:     "simple missing ok",
			Method:   Get,
			In:       `{"hello":"world"}`,
			Path:     ".wut",
			Expected: `null`,
		},
		{
			Name:     "simple key error",
			Method:   Get,
			In:       `{"hello":"world"}`,
			Path:     ".hello.wut",
			HasError: true,
		},
		{
			Name:     "special key error",
			Method:   Get,
			In:       `{"$private":"world"}`,
			Path:     ".$private",
			HasError: true,
		},
		{
			Name:     "numeric key error",
			Method:   Get,
			In:       `{"0":"world"}`,
			Path:     ".0",
			HasError: true,
		},
		{
			Name:     "simple key set! error",
			Method:   SetMutate,
			In:       `{"hello":"world"}`,
			Path:     ".hello.wut",
			NewIn:    "42",
			HasError: true,
		},
		{
			Name:     "simple key set error",
			Method:   Set,
			In:       `{"hello":"world"}`,
			Path:     ".hello.wut",
			NewIn:    "42",
			HasError: true,
		},
		{
			Name:     "nested",
			Method:   Get,
			In:       `{"a":{"b":"world"}}`,
			Path:     ".a.b",
			Expected: `"world"`,
		},
		{
			Name:     "nested set!",
			Method:   SetMutate,
			In:       `{"a":{"b":"world"}}`,
			Path:     ".a.b",
			NewIn:    "23",
			Expected: `{"a":{"b":23}}`,
		},
		{
			Name:     "nested set",
			Method:   Set,
			In:       `{"a":{"b":"world"}}`,
			Path:     ".a.b",
			NewIn:    "23",
			Expected: `{"a":{"b":23}}`,
		},
		{
			Name:     "nested delete!",
			Method:   DelMutate,
			In:       `{"a":{"b":"world"}}`,
			Path:     ".a.b",
			Expected: `{"a":{}}`,
		},
		{
			Name:     "nested delete",
			Method:   Del,
			In:       `{"a":{"b":"world"}}`,
			Path:     ".a.b",
			Expected: `{"a":{}}`,
		},
		{
			Name:     "index",
			Method:   Get,
			In:       `["a","b","c"]`,
			Path:     ".[1]",
			Expected: `"b"`,
		},
		{
			Name:     "index neg zero",
			Method:   Get,
			In:       `["a","b","c"]`,
			Path:     ".[-0]",
			Expected: `"a"`,
		},
		{
			Name:     "index set!",
			Method:   SetMutate,
			In:       `["a","b","c"]`,
			Path:     ".[1]",
			NewIn:    `"d"`,
			Expected: `["a","d","c"]`,
		},
		{
			Name:     "index set",
			Method:   Set,
			In:       `["a","b","c"]`,
			Path:     ".[1]",
			NewIn:    `"d"`,
			Expected: `["a","d","c"]`,
		},
		{
			Name:     "index delete!",
			Method:   DelMutate,
			In:       `["a","b","c"]`,
			Path:     ".[1]",
			Expected: `["a","c"]`,
		},
		{
			Name:     "index delete",
			Method:   Del,
			In:       `["a","b","c"]`,
			Path:     ".[1]",
			Expected: `["a","c"]`,
		},
		{
			Name:     "index set! error",
			Method:   SetMutate,
			In:       `{"hello":"world"}`,
			Path:     ".[1]",
			NewIn:    `"d"`,
			HasError: true,
		},
		{
			Name:     "index set error",
			Method:   Set,
			In:       `{"hello":"world"}`,
			Path:     ".[1]",
			NewIn:    `"d"`,
			HasError: true,
		},
		{
			Name:     "index missing",
			Method:   Get,
			In:       `["a","b","c"]`,
			Path:     ".[23]",
			Expected: `null`,
		},
		{
			Name:     "index neg",
			Method:   Get,
			In:       `["a","b","c"]`,
			Path:     ".[-1]",
			Expected: `"c"`,
		},
		{
			Name:     "range",
			Method:   Get,
			In:       `["a","b","c"]`,
			Path:     ".[1:3]",
			Expected: `["b","c"]`,
		},
		{
			Name:     "all implicit",
			Method:   Get,
			In:       `["hello","world"]`,
			Path:     ".[:]",
			Expected: `["hello","world"]`,
		},
		{
			Name:     "range implicit start",
			Method:   Get,
			In:       `["a","b","c"]`,
			Path:     ".[:2]",
			Expected: `["a","b"]`,
		},
		{
			Name:     "range implicit end",
			Method:   Get,
			In:       `["a","b","c"]`,
			Path:     ".[1:]",
			Expected: `["b","c"]`,
		},
		{
			Name:     "range (inner)",
			Method:   Get,
			In:       `["a","b","c"]`,
			Path:     ".[1:2]",
			Expected: `["b"]`,
		},
		{
			Name:     "range set!",
			Method:   SetMutate,
			In:       `["a","b","c"]`,
			Path:     ".[1:3]",
			NewIn:    `["d","e"]`,
			Expected: `["a","d","e"]`,
		},
		{
			Name:     "range set",
			Method:   Set,
			In:       `["a","b","c"]`,
			Path:     ".[1:3]",
			NewIn:    `["d","e"]`,
			Expected: `["a","d","e"]`,
		},
		{
			Name:     "range delete!",
			Method:   DelMutate,
			In:       `["a","b","c"]`,
			Path:     ".[1:3]",
			Expected: `["a"]`,
		},
		{
			Name:     "range delete",
			Method:   Del,
			In:       `["a","b","c"]`,
			Path:     ".[1:3]",
			Expected: `["a"]`,
		},
		{
			Name:     "range delete error!",
			Method:   DelMutate,
			In:       `["a","b","c"]`,
			Path:     ".[1:4]",
			Expected: `["a"]`,
			HasError: true,
		},
		{
			Name:     "range delete error",
			Method:   Del,
			In:       `["a","b","c"]`,
			Path:     ".[1:4]",
			Expected: `["a"]`,
			HasError: true,
		},
		{
			Name:     "range delete (inner range)!",
			Method:   DelMutate,
			In:       `["a","b","c"]`,
			Path:     ".[1:2]",
			Expected: `["a","c"]`,
		},
		{
			Name:     "range delete (inner range)",
			Method:   Del,
			In:       `["a","b","c"]`,
			Path:     ".[1:2]",
			Expected: `["a","c"]`,
		},
		{
			// NOTE: the input is malformed JSON -- `1"` -- and has been
			// since the case was written. It still asserts something (a
			// path operation over the error value libjson.Load returns
			// must report, not panic) but not what the name says, so the
			// intended case is spelled out separately below rather than
			// silently repaired: both are worth having.
			Name:     "range error (malformed input document)",
			Method:   Get,
			In:       `[{"k":1"},{"k":2}]`,
			Path:     ".[1:2].k",
			HasError: true,
		},
		{
			// The case the one above meant to be: a key step applied to
			// the ARRAY a range yields. jq refuses to index an array
			// with a string; the engine refuses it as "first argument is
			// not a map". (The malformed case above reports "argument is
			// not an array" instead, which is how the two were told
			// apart.)
			Name:     "range error",
			Method:   Get,
			In:       `[{"k":1},{"k":2}]`,
			Path:     ".[1:2].k",
			HasError: true,
		},
		{
			Name:     "range set! error",
			Method:   SetMutate,
			In:       `["a","b","c"]`,
			Path:     ".[1:3]",
			NewIn:    `["d","e", "f"]`,
			Expected: `["a","d","e","f"]`,
		},
		{
			Name:     "range set error",
			Method:   Set,
			In:       `["a","b","c"]`,
			Path:     ".[1:3]",
			NewIn:    `["d","e","f"]`,
			Expected: `["a","d","e","f"]`,
		},
		{
			Name:     "range neg",
			Method:   Get,
			In:       `["a","b","c"]`,
			Path:     ".[-2:-1]",
			Expected: `["b"]`,
		},
		{
			Name:     "nested index",
			Method:   Get,
			In:       `{"abc":"-","def":["a","b","c"]}`,
			Path:     ".def[1]",
			Expected: `"b"`,
		},
		{
			Name:     "nested index has error",
			Method:   Get,
			In:       `{"abc":"-","def":["a","b","c"]}`,
			Path:     ".abc[1]",
			HasError: true,
		},
		{
			Name:     "nested range",
			Method:   Get,
			In:       `{"abc":"-","def":["a","b","c"]}`,
			Path:     ".def[1:3]",
			Expected: `["b","c"]`,
		},
		{
			Name:     "range query implicit start",
			Method:   Get,
			In:       `[{"a":[1,2]},{"b":[3,3]},{"a":[4]}]`,
			Path:     ".[0:]",
			Expected: `[{"a":[1,2]},{"b":[3,3]},{"a":[4]}]`,
		},
		{
			Name:     "iterator query",
			Method:   Get,
			In:       `[{"a":[1,2]},{"b":[3,3]},{"a":[4]}]`,
			Path:     ".[].a",
			Expected: `[[1,2],null,[4]]`,
		},
		{
			Name:     "iterator query over chain",
			Method:   Get,
			In:       `[{"a":[1,2]},{"b":[3,3]},{"a":[4]}]`,
			Path:     ".[].a[0]",
			Expected: `[1,null,4]`,
		},
		{
			Name:     "chain on iterator",
			Method:   Get,
			In:       `[{"a":[1,2]},{"b":[3,3]},{"a":[4]}]`,
			Path:     ".[0].a[]",
			Expected: `[1,2]`,
		},
		{
			Name:   "chain on iterator chain",
			Method: Get,
			In:     `{"a":[ { "b": 2 }, 0, {"b": 3 } ] }`,
			Path:   ".a[].b",
			// IMPORTANT: this semantics differs from `jq`, where `jq` would
			// return 2 and then throw an error.
			Expected: `[2,null,3]`,
		},
		{
			Name:     "iterator on iterator (collapse results)",
			Method:   Get,
			In:       `[ {"a":[ { "b": 2 }, {"b": 3 } ] }, {"a":[ { "b": 4 } ]} ]`,
			Path:     ".[].a[].b",
			Expected: `[2,3,4]`,
		},
		{
			Name:     "iterator get array (no collapse paths)",
			Method:   Get,
			In:       `[{"a":[1, 2]}, {"a":[3, 4]}]`,
			Path:     ".[].a",
			Expected: `[[1,2], [3,4]]`,
		},
		{
			Name:     "iterator null mutate",
			Method:   NilMutate,
			In:       `{"a":[ { "b": 2 }, 0, {"b": 3 } ] }`,
			Path:     ".a[].b",
			Expected: `{"a":[ { "b": null }, 0, {"b": null } ] }`,
		},
		{
			Name:     "iterator null",
			Method:   Nil,
			In:       `{"a":[ { "b": 2 }, 0, {"b": 3 } ] }`,
			Path:     ".a[].b",
			Expected: `{"a":[ { "b": null }, 0, {"b": null } ] }`,
		},
		{
			Name:     "iterator set mutate",
			Method:   SetMutate,
			In:       `{"a":[ { "b": 2 }, 0, {"b": 3 }, {"c": 4 } ] }`,
			Path:     ".a[].b",
			NewIn:    "42",
			Expected: `{"a":[ { "b": 42 }, 0, {"b": 42 }, {"c": 4, "b": 42 } ] }`,
		},
		{
			Name:     "iterator set",
			Method:   Set,
			In:       `{"a":[ { "b": 2 }, 0, {"b": 3 }, {"c": 4 } ] }`,
			Path:     ".a[].b",
			NewIn:    "42",
			Expected: `{"a":[ { "b": 42 }, 0, {"b": 42 }, {"c": 4, "b": 42 } ] }`,
		},
		{
			Name:     "iterator delete mutate",
			Method:   DelMutate,
			In:       `{"a":[ { "b": 2 }, 0, {"b": 3 }, {"c": 4 } ] }`,
			Path:     ".a[].b",
			Expected: `{"a":[ {}, 0, {}, {"c": 4} ] }`,
		},
		{
			Name:     "iterator delete",
			Method:   Del,
			In:       `{"a":[ { "b": 2 }, 0, {"b": 3 }, {"c": 4 } ] }`,
			Path:     ".a[].b",
			Expected: `{"a":[ {}, 0, {}, {"c": 4} ] }`,
		},
		{
			Name:     "dot null",
			Method:   Nil,
			In:       `{"hello":"world"}`,
			Path:     ".",
			Expected: `null`,
		},
		{
			Name:     "dot null mutate",
			Method:   NilMutate,
			In:       `{"hello":"world"}`,
			Path:     ".",
			HasError: true,
		},
		{
			Name:     "simple key null",
			Method:   Nil,
			In:       `{"hello":"world"}`,
			Path:     ".hello",
			Expected: `{"hello":null}`,
		},
		{
			Name:     "simple key null mutate",
			Method:   NilMutate,
			In:       `{"hello":"world"}`,
			Path:     ".hello",
			Expected: `{"hello":null}`,
		},
		{
			Name:     "simple index null",
			Method:   Nil,
			In:       `["fnord", "FNORD"]`,
			Path:     ".[1]",
			Expected: `["fnord",null]`,
		},
		{
			Name:     "simple index null mutate",
			Method:   NilMutate,
			In:       `["fnord", "FNORD"]`,
			Path:     ".[1]",
			Expected: `["fnord",null]`,
		},
		{
			Name:     "simple range null",
			Method:   Nil,
			In:       `["fnord", "FNORD"]`,
			Path:     ".[0:]",
			Expected: `[null,null]`,
		},
		{
			Name:     "simple range null mutate all",
			Method:   NilMutate,
			In:       `["fnord", "FNORD"]`,
			Path:     ".[0:]",
			Expected: `[null,null]`,
		},
		{
			Name:     "simple range null mutate some",
			Method:   NilMutate,
			In:       `["fnord", "FNORD", "f-nord"]`,
			Path:     ".[1:]",
			Expected: `["fnord",null,null]`,
		},
		{
			Name:     "simple chain null",
			Method:   Nil,
			In:       `{"fubar":["fnord", "FNORD", "f-nord"]}`,
			Path:     ".fubar[1:]",
			Expected: `{"fubar":["fnord",null,null]}`,
		},
		{
			Name:     "simple chain null mutate",
			Method:   NilMutate,
			In:       `{"fubar":["fnord", "FNORD", "f-nord"]}`,
			Path:     ".fubar[1:]",
			Expected: `{"fubar":["fnord",null,null]}`,
		},
	}

	for _, tc := range testCases {
		t.Run(tc.Name, func(t *testing.T) {
			t.Parallel()
			path, err := ParseSelector(tc.Path)
			if !tc.HasError && err != nil {
				t.Fatalf("unexpected parser error: %v", err)
			}
			var data *lisp.LVal
			if err == nil {
				lval := libjson.Load([]byte(tc.In), false)
				switch method := tc.Method; method {
				case Get:
					data, err = path.Get(lval)
				case Set:
					newlval := libjson.Load([]byte(tc.NewIn), false)
					data, err = path.Set(lval, newlval)
				case SetMutate:
					newlval := libjson.Load([]byte(tc.NewIn), false)
					data, err = path.SetMutate(lval, newlval)
				case Del:
					data, err = path.Delete(lval)
				case DelMutate:
					data, err = path.DeleteMutate(lval)
				case Nil:
					data, err = path.Nil(lval)
				case NilMutate:
					data, err = path.NilMutate(lval)
				default:
					t.Fatalf("unknown method: %v", method)
				}
			}
			if tc.HasError {
				if err == nil {
					t.Fatal("expected error")
				}
			} else {
				if err != nil {
					t.Fatalf("unexpected error: %v", err)
				}
				var ejson []byte
				if data != nil {
					ejson, err = libjson.Dump(data, false)
				}
				if err != nil {
					t.FailNow()
				}
				if string(ejson) != tc.Expected && !jsonEqual(string(ejson), tc.Expected) {
					t.Fatalf("\ngot:\n%s\n----\nexpected:\n%s\n", string(ejson), tc.Expected)
				}
			}
		})
	}
}

// TestParseSelectorPathString asserts the PARSE, not the operation.
//
// TestParseSelector above checks what a parsed path DOES to a document,
// which a subtly different path can satisfy: ".[:2]" and ".[0:2]" are
// distinct parses with identical behaviour on every document, and until
// issue #563 an implicit end and an empty slice printed the same way. This
// table pins the Path each selector actually produces, through String(),
// which is the only introspection the Path interface offers.
func TestParseSelectorPathString(t *testing.T) {
	t.Parallel()
	testCases := []struct {
		Name     string
		In       string
		Expected string
		HasError bool
	}{
		{Name: "bare key", In: ".hello", Expected: `.["hello"]`},
		{Name: "leading underscore key", In: "._private", Expected: `.["_private"]`},
		{Name: "quoted key", In: `.["$private"]`, Expected: `.["$private"]`},
		{Name: "quoted key with escapes", In: `.["\"\n"]`, Expected: `.["\"\n"]`},
		{Name: "chained bare keys", In: ".a.b", Expected: `.["a"]["b"]`},
		{Name: "key then index", In: ".def[1]", Expected: `.["def"][1]`},
		{Name: "index", In: ".[1]", Expected: ".[1]"},
		{Name: "negative index", In: ".[-1]", Expected: ".[-1]"},
		{
			// Atoi("-0") is 0, so the sign is discarded here and ".[-0]"
			// is the FIRST element. The corpus asserts that behaviour
			// against a document; this asserts it against the path.
			Name: "negative zero index", In: ".[-0]", Expected: ".[0]",
		},
		{Name: "range", In: ".[1:3]", Expected: ".[1:3]"},
		{
			// An absent "from" is a literal 0, NOT an implicit bound: the
			// two ends are not symmetric. Only the end can be implicit,
			// because only the end needs the document's length.
			Name: "range implicit start", In: ".[:2]", Expected: ".[0:2]",
		},
		{Name: "range implicit end", In: ".[1:]", Expected: ".[1:]"},
		{Name: "range both implicit", In: ".[:]", Expected: ".[0:]"},
		{Name: "range negative bounds", In: ".[-2:-1]", Expected: ".[-2:-1]"},
		{Name: "iterator", In: ".[]", Expected: ".[]"},
		{Name: "iterator then key", In: ".[].a", Expected: `.[]["a"]`},
		{Name: "key then iterator then key", In: ".a[].b", Expected: `.["a"][]["b"]`},
		{Name: "nested iterators", In: ".[].a[].b", Expected: `.[]["a"][]["b"]`},
		{
			// The "?" of jq's optional selectors is matched by all three
			// regexps and read by none of them, so it is accepted and
			// discarded: ".a?" is exactly ".a", errors included. See
			// ParseSelector's doc comment.
			Name: "optional suffix is discarded", In: ".a?", Expected: `.["a"]`,
		},
		{Name: "optional suffix on index", In: ".[0]?", Expected: ".[0]"},
		{Name: "whitespace is insignificant", In: ".[ 0 : 1 ] ", Expected: ".[0:1]"},
		{Name: "whitespace after the dot", In: ".  wut", Expected: `.["wut"]`},

		// --- rejected ---
		{Name: "empty", In: "", HasError: true},
		{Name: "whitespace only", In: "   ", HasError: true},
		{Name: "no leading dot", In: "[0]", HasError: true},
		{Name: "bare key may not start with a digit", In: ".0", HasError: true},
		{Name: "bare key may not contain $", In: ".$private", HasError: true},
		{
			// The regexp matches any run of digits; Atoi is what refuses
			// this, and the message names the offending text.
			Name: "index too large for an int", In: ".[99999999999999999999]", HasError: true,
		},
		{Name: "range end too large for an int", In: ".[0:99999999999999999999]", HasError: true},
		{Name: "unterminated bracket", In: ".[0", HasError: true},
		{Name: "bad string escape in a key", In: `.["\q"]`, HasError: true},
		{Name: "trailing garbage", In: ".a!!", HasError: true},
	}
	for _, tc := range testCases {
		t.Run(tc.Name, func(t *testing.T) {
			t.Parallel()
			path, err := ParseSelector(tc.In)
			if tc.HasError {
				if err == nil {
					t.Fatalf("expected error, got path %q", path.String())
				}
				if path != nil {
					t.Fatalf("a rejected selector must return a nil Path, got %v", path)
				}
				return
			}
			if err != nil {
				t.Fatalf("unexpected error: %v", err)
			}
			if got := path.String(); got != tc.Expected {
				t.Fatalf("path.String() = %q, want %q", got, tc.Expected)
			}
		})
	}
}

// TestParseSelectorRoundTrip asserts that String() emits a selector that
// parses back to the SAME PATH -- same behaviour, not merely the same text.
//
// This is the test the upstream/downstream split made unwritable, and issue
// #563 is what it catches: rangePath.String() ignored implicitTo, so ".[1:]"
// printed as ".[1:0]", which parses to an EMPTY slice. Only this parser ever
// built an implicitTo path, so the defect lived in path.go and was reachable
// only from substrate.
//
// Text equality is NOT sufficient to catch that, and asserting it alone was
// the first draft of this test: ".[1:0]" prints as ".[1:0]", so a printed
// form that means something else is perfectly stable as text. The assertion
// with teeth is that the reprinted path answers every document the same way
// the original does. The text check stays as the other half -- it catches a
// printer that emits something the parser cannot read at all.
//
// The comparison is over Get only. The mutating operations would need a
// fresh document per call and add nothing: a path that reads the same
// locations writes to the same locations.
func TestParseSelectorRoundTrip(t *testing.T) {
	t.Parallel()
	// Documents chosen so that every selector below lands somewhere in at
	// least one of them, and so that an off-by-one in a bound is visible
	// (distinct elements, not repeats).
	docs := []string{
		`["a","b","c"]`,
		`["a","b","c","d","e"]`,
		`[]`,
		`{"hello":"world","_private":1,"$private":2,"a":{"b":"world"},"def":["a","b","c"],"fubar":[1,2,3]}`,
		`[{"a":[1,2]},{"b":[3,3]},{"a":[4]}]`,
		`{"a":[{"b":2},0,{"b":3}]}`,
		`"scalar"`,
	}
	selectors := []string{
		".hello", "._private", `.["$private"]`, `.["\"\n"]`, `.["a\"b"]`,
		".[0]", ".[1]", ".[-1]", ".[-0]", ".[23]",
		".[1:3]", ".[:2]", ".[1:]", ".[:]", ".[0:]", ".[-2:-1]", ".[1:2]",
		// The open-ended forms are the seam this branch's two commits meet
		// at: only this parser produces an implicitTo path, and before
		// issue #563 String() rendered one as its stored `to`, which is 0.
		// Measured on that code: ".[1:]" printed as ".[1:0]", whose
		// REPARSE raises "end before start"; ".[0:]" printed as ".[0:0]",
		// which empties the array; ".[-2:]" printed as ".[-2:0]". None of
		// those three is caught by comparing printed text, which is why
		// the comparison below is behavioural.
		".[-2:]", ".[-1:]", ".[-5:]", ".[:-1]", ".[-3:-1]", ".[3:]",
		".[]", ".[].a", ".[0].a[]", ".a[]", ".def[1]", ".def[1:3]",
		".fubar[1:]", ".a?", ".[0]?",
		// Two or more keys -- see the note above (issue #566).
		".a.b", ".a[].b", ".def[0]", `.["a"]["b"]`, ".a.b.c",
		`.["$private"].x`, ".hello.there", `.["a\"b"].c`,
	}
	// The two-key selectors below could not be in this list before issue
	// #566 was fixed: String() renders every key bracketed, so ".a.b"
	// prints as `.["a"]["b"]`, and the greedy key body could not read that
	// back. They are the round-trip cases the defect was hiding.
	// describe renders a Get outcome so that a value and an error compare
	// as different things rather than both as "".
	describe := func(t *testing.T, p Path, doc *lisp.LVal) string {
		t.Helper()
		v, err := p.Get(doc)
		if err != nil {
			return "error: " + err.Error()
		}
		if v == nil {
			return "<nil LVal>"
		}
		b, derr := libjson.Dump(v, false)
		if derr != nil {
			return "undumpable: " + derr.Error()
		}
		return "value: " + string(b)
	}
	for _, sel := range selectors {
		t.Run(sel, func(t *testing.T) {
			t.Parallel()
			path, err := ParseSelector(sel)
			if err != nil {
				t.Fatalf("unexpected error: %v", err)
			}
			printed := path.String()
			again, err := ParseSelector(printed)
			if err != nil {
				t.Fatalf("String() produced %q, which does not parse: %v", printed, err)
			}
			if got := again.String(); got != printed {
				t.Fatalf("round trip is not stable as text: %q -> %q -> %q", sel, printed, got)
			}
			for _, src := range docs {
				want := describe(t, path, libjson.Load([]byte(src), false))
				got := describe(t, again, libjson.Load([]byte(src), false))
				if got != want {
					t.Errorf("%q printed as %q, which is a DIFFERENT path\n  document: %s\n  original: %s\n  reparsed: %s",
						sel, printed, src, want, got)
				}
			}
		})
	}
}

// TestParseSelectorRootSpelling pins the identity selector's odd result so
// that it stays a decision rather than becoming an accident.
//
// ParseSelector(".") returns Chain(), not Root(Chain()). The two are the
// same path -- rootPath proxies all seven operations and adds only a leading
// "." to String() -- so nothing a caller can DO with them differs, but
// Chain().String() is the empty string, which is not a selector. ArgsToPath
// spells the identical path Root(Chain()) and prints ".".
//
// Preserved on the port (issue #564) because substrate's builtins have
// returned this path since the beginning and a port is not the place to
// change what they return. It is the one selector TestParseSelectorRoundTrip
// cannot cover.
func TestParseSelectorRootSpelling(t *testing.T) {
	t.Parallel()
	path, err := ParseSelector(".")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if got := path.String(); got != "" {
		t.Fatalf(`ParseSelector(".").String() = %q, want "" (see this test's comment)`, got)
	}
	if _, err := ParseSelector(path.String()); err == nil {
		t.Fatal("the empty string is expected NOT to parse; if it now does, this test is stale")
	}

	// The behavioural half: identical to the path ArgsToPath builds for an
	// empty step list, which is what makes the difference cosmetic.
	args, err := ArgsToPath(nil)
	if err != nil {
		t.Fatalf("unexpected ArgsToPath error: %v", err)
	}
	if got := args.String(); got != "." {
		t.Fatalf("ArgsToPath(nil).String() = %q, want %q", got, ".")
	}
	doc := libjson.Load([]byte(`{"hello":"world"}`), false)
	for _, p := range []Path{path, args} {
		got, err := p.Get(doc)
		if err != nil {
			t.Fatalf("%q: unexpected Get error: %v", p.String(), err)
		}
		if got != doc {
			t.Fatalf("%q: Get must be the identity at the root", p.String())
		}
	}
}

// TestParseSelectorTwoQuotedKeys covers the grammar fix for issue #566.
//
// reArrayKey's body used to be `(?:\"|[^"])*`, in which `\"` is a plain
// escaped quote -- so the alternation was `"` OR `not "`, i.e. every
// character, and the group ran greedily to the last quote in the selector.
// Given `.["a"]["b"]` it captured `"a"]["b"` and handed that to
// strconv.Unquote, which rejected the interior quote. A selector could
// therefore carry at most ONE bracketed key.
//
// That was not only an input restriction. String() renders every map key
// bracketed and quoted, so `.a.b` printed as `.["a"]["b"]` -- this parser
// emitting output it could not itself read. The round-trip test had to
// exclude every two-key selector; it carries them now.
//
// The body is now `(?:\\.|[^"\\])*`: an escape sequence, or a character
// that is neither a quote nor a backslash. Verified not to change any
// selector that parsed before -- the previous behaviour on all of these was
// an error, and the one-key and escape cases below are unchanged.
func TestParseSelectorTwoQuotedKeys(t *testing.T) {
	t.Parallel()
	for _, sel := range []string{
		`.["a"]["b"]`,
		`.["first name"]["last name"]`,
		`.["a"][0]["b"]`,
		`.["a"][]["b"]`,
		`.["a"] ["b"]`,
		`.["a"]["b"]["c"]`,
	} {
		if _, err := ParseSelector(sel); err != nil {
			t.Errorf("%q must parse: %v", sel, err)
		}
	}
	// Unchanged by the fix: one bracketed key, and the escape forms whose
	// grammar the new body has to keep getting right.
	for _, sel := range []string{
		`.["a"]`, `.["a"].b`, `.a["b"]`, `.["a\"b"]`, `.["\"\n"]`,
		`.[""]`, `.["]"]`, `.["a\\"]`, `.["a\\"]["b"]`,
	} {
		if _, err := ParseSelector(sel); err != nil {
			t.Errorf("%q must parse: %v", sel, err)
		}
	}
	// The key itself must survive intact, not merely parse.
	p, err := ParseSelector(`.["first name"]["last name"]`)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if got, want := p.String(), `.["first name"]["last name"]`; got != want {
		t.Errorf("String() = %q, want %q", got, want)
	}
}
