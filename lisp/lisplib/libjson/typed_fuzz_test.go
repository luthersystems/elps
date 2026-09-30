// Copyright © 2026 The ELPS authors

package libjson

import (
	"bytes"
	"testing"

	"github.com/luthersystems/elps/internal/fuzzseed"
	"github.com/luthersystems/elps/lisp"
)

// FuzzTypedJSON feeds arbitrary bytes to LoadTyped.
//
// ASSERTIONS
//
//  1. No panic escapes (LoadTyped has no recover of its own).
//  2. On error the value is nil; on success it is not.
//  3. Canonicality: whatever decodes re-encodes to exactly the input bytes.
//     The decoder accepts only what the encoder writes, so any alias it
//     accepted (reordered members, 1.50, an extra escape, whitespace) fails
//     here.
//  4. Round trip: decoding the re-encoding gives the same bytes again.
//
// Decoding is pure Go bounded by the typed limits, so no watchdog or step
// budget is needed.
func FuzzTypedJSON(f *testing.F) {
	for _, s := range []string{
		`0`, `-42`, `9007199254740992`, `-9007199254740992`, `"~n9007199254740993"`, `"~n-9007199254740993"`, `1.0`, `-0.0`, `1e+21`, `1e-7`, `0.000001`, `"~zNaN"`, `"~z-INF"`,
		`"hé"`, `"~~x"`, `"~^ "`, `"~bAP8B"`, `"~b"`, `"~$abc"`, `"~:ab"`, `true`, `false`,
		`null`, `[]`, `[1,"a"]`, `["~#list",[1,2]]`, `["~#list",[null,[]]]`, `[null,[],["~#list",[1]]]`, `["~#array",[[2,3],[1,2,3,4,5,6]]]`, `["~#array",[[],[7]]]`,
		`["~#array",[[0,"~n9223372036854775807"],[]]]`, `{"~i9007199254740992":9007199254740992}`, `{"a":1,"b":2}`, `{"b":2,"~$s":1.0,"~:a":1,"~?t":3,"~i7":0,"~~t":4}`,
		`{}`, `["~#tagged",["user:point",["~#list",[1,2]]]]`, `{"xs":[{"k":"~:v"}]}`, "\"\\u0001\\n\"",
		`{"a":1, "b":2}`, `[1.50]`, `{"b":1,"a":2}`, `["~#unknown",[1,2]]`, `["~#unknown",[]]`, `["~#list",[]]`,
		`"~i9007199254740992"`, `"~n7"`, `"~n9007199254740992"`, `"~n-9007199254740992"`, `{"~n9007199254740992":0}`,
		`"\u003c\u003e\u0026\u2028\u2029"`, `"<>&"`, "\"\u2028\u2029\"",
		`"x\n<"`, `"x\n\u2028"`, `"\u003C"`, `"\u0041"`, `"\u000a"`,
		`{"\u003c":1,"Z":2}`, `{"Z":2,"\u003c":1}`,
		"{\"\ue000\":1,\"\uffff\":2,\"𐀀\":3,\"😀\":4}",
		"{\"𐀀\":3,\"😀\":4,\"\ue000\":1,\"\uffff\":2}",
	} {
		f.Add([]byte(s))
	}
	for _, s := range fuzzseed.Adversarial() {
		f.Add(s)
	}
	f.Fuzz(func(t *testing.T, data []byte) {
		v, err := LoadTyped(data)
		optionValue := LoadWith(data, LoadOpts{Typed: true, ExactIntegers: true})
		if err != nil {
			if v != nil {
				t.Fatalf("error %v with non-nil value", err)
			}
			if optionValue.Type != lisp.LError {
				t.Fatalf("typed option accepted input rejected by LoadTyped: %q", data)
			}
			return
		}
		if v == nil {
			t.Fatal("nil value without error")
		}
		if optionValue.Type == lisp.LError {
			t.Fatalf("typed option rejected valid input: %v", optionValue)
		}
		enc, err := DumpWith(optionValue, DumpOpts{Typed: true})
		if err != nil {
			t.Fatalf("decoded value does not re-encode: %v (%v)", err, v)
		}
		if !bytes.Equal(enc, data) {
			t.Fatalf("non-canonical input accepted:\n in  %q\n out %q", data, enc)
		}
		v2, err := LoadTyped(enc)
		if err != nil {
			t.Fatalf("re-encoding does not decode: %v", err)
		}
		enc2, err := DumpWith(v2, DumpOpts{Typed: true})
		if err != nil || !bytes.Equal(enc2, enc) {
			t.Fatalf("round trip unstable: %q vs %q (%v)", enc2, enc, err)
		}
	})
}
