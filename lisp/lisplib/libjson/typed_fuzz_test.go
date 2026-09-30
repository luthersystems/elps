// Copyright © 2026 The ELPS authors

package libjson

import (
	"bytes"
	"testing"

	"github.com/luthersystems/elps/internal/fuzzseed"
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
		`0`, `-42`, `"~i9007199254740992"`, `1.0`, `-0.0`, `1e+21`, `1e-7`, `0.000001`, `"~zNaN"`, `"~z-INF"`,
		`"hé"`, `"~~x"`, `"~^ "`, `"~bAP8B"`, `"~b"`, `"~$abc"`, `"~:ab"`, `true`, `false`,
		`[]`, `[1,"a"]`, `["~#vector",[1,2]]`, `["~#vector",[]]`, `["~#list",[1]]`, `["~#array",[[2,3],[1,2,3,4,5,6]]]`, `["~#array",[[],[7]]]`,
		`["~#array",[[0,9223372036854775807],[]]]`, `{"a":1,"b":2}`, `{"b":2,"~$s":1.0,"~:a":1,"~?t":3,"~i7":0,"~~t":4}`,
		`{}`, `["~#tagged",["user:point",[1,2]]]`, `{"xs":["~#vector",[{"k":"~:v"}]]}`, "\"\\u0001\\n\"",
		`{"a":1, "b":2}`, `[1.50]`, `{"b":1,"a":2}`, `null`,
	} {
		f.Add([]byte(s))
	}
	for _, s := range fuzzseed.Adversarial() {
		f.Add(s)
	}
	f.Fuzz(func(t *testing.T, data []byte) {
		v, err := LoadTyped(data)
		if err != nil {
			if v != nil {
				t.Fatalf("error %v with non-nil value", err)
			}
			return
		}
		if v == nil {
			t.Fatal("nil value without error")
		}
		enc, err := DumpTyped(v)
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
		enc2, err := DumpTyped(v2)
		if err != nil || !bytes.Equal(enc2, enc) {
			t.Fatalf("round trip unstable: %q vs %q (%v)", enc2, enc, err)
		}
	})
}
