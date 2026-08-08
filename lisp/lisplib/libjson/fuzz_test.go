// Copyright © 2026 The ELPS authors

package libjson_test

import (
	"bytes"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
)

// libjson decodes untrusted JSON: in substrate this is chaincode state and
// connector payloads, neither of which the phylum author controls.  Load must
// therefore survive any byte string, and the values it produces must survive
// being handed straight back to Dump, which is what every read-modify-write
// path in a phylum does.
//
// Two invariants:
//
//   - Load never panics and never returns nil.  It returns an LError for bad
//     input; that is a value, not a failure.
//
//   - Dump/Load agree on everything Load can produce.  Load emits only
//     strings, floats, bools, nil, arrays and sorted-maps, and Dump handles
//     all six, so a successful Load must re-encode and re-decode to an equal
//     value.  Equality is checked on lisp.LVal.String() of the SECOND decode
//     against the first, which is the strongest form that is actually
//     guaranteed -- see the note on integers below.
//
// NOT asserted: that Load(b) preserves the numeric text of b.  encoding/json
// decodes every JSON number into a float64 here, so integers beyond 2^53 are
// rounded on the way in.  That is a known, deliberately out-of-scope defect;
// asserting text preservation would turn it into permanent fuzz noise.
// Comparing decode-of-re-encode against the FIRST decode sidesteps it: the
// rounding has already happened before the comparison starts, and float64 ->
// shortest-repr -> float64 is exact.

func stringNumberModes() []bool { return []bool{false, true} }

func FuzzLoadJSON(f *testing.F) {
	seeds := []string{
		"", " ", "null", "true", "false",
		"0", "-0", "1", "-1", "1.5", "1e400", "-1e400", "1e-400",
		"9007199254740993", // 2^53+1: rounds on decode, see the note above
		"12345678901234567890123456789",
		`""`, `"a"`, "\"\\u0000\"", `"\ud800"`, `"\udc00"`, `"😀"`,
		"[]", "{}", "[1,2,3]", `{"a":1}`, `{"a":{"b":[1,{"c":null}]}}`,
		`{"a":1,"a":2}`, // duplicate keys
		`{"":1}`,
		"[1,2", `{"a":`, `{"a":1,}`, "[,]", "{,}",
		"1 2",   // trailing content after a complete value
		"nulll", // prefix of a literal
		"\xff",  // invalid utf-8
		"\"\xff\"",
		"[[[[[[[[[[]]]]]]]]]]",
		strings.Repeat("[", 200) + strings.Repeat("]", 200),
		strings.Repeat("[", 200), // unbalanced, deep
		`{"a":` + strings.Repeat("[", 200),
		"\x00",
		`{"a":"` + strings.Repeat("x", 1024) + `"}`,
	}
	for _, s := range seeds {
		for _, mode := range stringNumberModes() {
			f.Add([]byte(s), mode)
		}
	}
	f.Fuzz(func(t *testing.T, data []byte, stringNums bool) {
		v := libjson.Load(data, stringNums)
		if v == nil {
			t.Fatal("Load returned a nil LVal")
		}
		_ = v.String()
		if v.Type == lisp.LError {
			return
		}

		enc, err := libjson.Dump(v, stringNums)
		if err != nil {
			t.Fatalf("Dump rejected a value Load produced: %v\n--- value ---\n%s", err, v)
		}

		again := libjson.Load(enc, stringNums)
		if again == nil {
			t.Fatal("Load returned a nil LVal for re-encoded output")
		}
		if again.Type == lisp.LError {
			t.Fatalf("Load rejected Dump's output: %s\n--- re-encoded ---\n%s", again, enc)
		}
		if got, want := again.String(), v.String(); got != want {
			t.Fatalf("Dump/Load round-trip changed the value\n--- first ---\n%s\n--- second ---\n%s\n--- re-encoded ---\n%s",
				want, got, enc)
		}

		// Dump must also be deterministic: a phylum that hashes or compares
		// serialised state depends on it.  Sorted-map ordering is the part
		// that could drift, since Go map iteration is randomised.
		enc2, err := libjson.Dump(v, stringNums)
		if err != nil {
			t.Fatalf("second Dump of the same value failed: %v", err)
		}
		if !bytes.Equal(enc, enc2) {
			t.Fatalf("Dump is not deterministic\n--- run 1 ---\n%s\n--- run 2 ---\n%s", enc, enc2)
		}
	})
}
