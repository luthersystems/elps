// Copyright © 2026 The ELPS authors

package libjson_test

import (
	"bytes"
	"strings"
	"testing"

	"github.com/luthersystems/elps/internal/fuzzval"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
	"github.com/luthersystems/elps/parser"
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
// NOT asserted HERE: that Load(b) preserves the numeric text of b.  In the two
// modes this target covers, encoding/json decodes every JSON number into a
// float64, so integers beyond 2^53 are rounded on the way in (issue #350).
// This target measures the DEFAULT, which still rounds on purpose, so
// asserting text preservation would be permanent noise.  Comparing
// decode-of-re-encode against the FIRST decode sidesteps it: the rounding has
// already happened before the comparison starts, and float64 -> shortest-repr
// -> float64 is exact.
//
// FuzzLoadExactIntegers, below, covers the mode where text preservation IS the
// contract and asserts it directly.

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

// FuzzDumpJSON fuzzes the ENCODE direction, which FuzzLoadJSON structurally
// cannot reach.
//
// FuzzLoadJSON only ever hands Dump a value that Load produced, and Load emits
// exactly six shapes: string, float, bool, nil, array, sorted-map. Everything
// else a phylum can put in front of json:dump-string -- a tagged-value from
// deftype/new, an LNative handed in by host code, a multi-dimensional array,
// an integer, a byte slice, a function, an error value, a symbol-keyed
// sorted-map -- has never been encoded under fuzzing. In substrate the value
// being serialised is phylum state, so its shape is chosen by the phylum
// author, not by the decoder.
//
// INVARIANTS
//
//  1. Dump does not panic. An unsupported type must come back as an error.
//  2. Dump is deterministic. Sorted-map iteration is the part that could
//     drift, and a phylum that hashes or compares serialised state depends on
//     it.
//  3. Whatever Dump accepts, Load must be able to read back, and the value
//     must then be STABLE under further round trips. Emitting bytes that the
//     package's own decoder rejects is the strongest form of encoder bug
//     there is.
//  4. The shared singletons are unmutated.
//
// NOT ASSERTED: that Load's result equals the ORIGINAL value, and not that
// re-encoding reproduces the ORIGINAL bytes. Neither holds, for reasons that
// are known and out of scope:
//
//   - A tagged-value, an integer or a byte slice does not survive a JSON round
//     trip as itself.
//   - json.Load rounds every number through float64 (the >2^53 rounding).
//   - encoding/json replaces invalid UTF-8 with U+FFFD, and escapes it as
//     \ufffd on the way out; decoding that yields a real U+FFFD rune, which
//     re-encodes as the literal character. So Dump("\xfd") != Dump(Load(Dump(
//     "\xfd"))) at the byte level even though the VALUE is stable. Measured on
//     the seed corpus, so asserting byte identity against the first encode
//     would be permanent noise rather than a finding.
//
// The comparison is therefore made on the far side of every lossy conversion:
// the SECOND decode against the FIRST, exactly as FuzzLoadJSON does.
func FuzzDumpJSON(f *testing.F) {
	for _, seed := range fuzzval.Seeds() {
		for _, mode := range stringNumberModes() {
			f.Add(seed, mode)
		}
	}
	f.Fuzz(func(t *testing.T, data []byte, stringNums bool) {
		before := lisp.TakeSingletonSnapshot()

		env := newJSONEnv(t)
		gen := fuzzval.New(data, env)
		v := gen.Value()

		enc, err := libjson.Dump(v, stringNums)
		if err != nil {
			// Refusing a value it cannot represent is correct behaviour, not
			// a finding. Rendering the value is still exercised: every error
			// path in the interpreter formats its operands.
			_ = v.String()
			return
		}

		enc2, err := libjson.Dump(v, stringNums)
		if err != nil {
			t.Fatalf("Dump succeeded then failed on the same value: %v\n--- value ---\n%s", err, v)
		}
		if !bytes.Equal(enc, enc2) {
			t.Fatalf("Dump is not deterministic\n--- run 1 ---\n%s\n--- run 2 ---\n%s\n--- value ---\n%s",
				enc, enc2, v)
		}

		back := libjson.Load(enc, stringNums)
		if back == nil {
			t.Fatalf("Load returned a nil LVal for Dump's output\n--- encoded ---\n%s", enc)
		}
		if back.Type == lisp.LError {
			t.Fatalf("Load rejected Dump's own output: %s\n--- encoded ---\n%s\n--- value ---\n%s",
				back, enc, v)
		}
		reenc, err := libjson.Dump(back, stringNums)
		if err != nil {
			t.Fatalf("Dump rejected the value its own output decoded to: %v\n--- encoded ---\n%s", err, enc)
		}
		again := libjson.Load(reenc, stringNums)
		if again == nil || again.Type == lisp.LError {
			t.Fatalf("Load rejected the re-encoded output: %v\n--- re-encoded ---\n%s", again, reenc)
		}
		if got, want := again.String(), back.String(); got != want {
			t.Fatalf("the value drifted across a second round trip\n--- first decode ---\n%s\n--- second decode ---\n%s\n--- encoded ---\n%s\n--- re-encoded ---\n%s",
				want, got, enc, reenc)
		}

		if drift := before.Verify(); drift != "" {
			t.Fatalf("Dump/Load mutated the shared singleton %s\n--- value ---\n%s", drift, v)
		}
	})
}

// newJSONEnv exists only so the generator can build tagged-values, which need
// an LEnv to stamp a source location. libjson itself is package-level.
func newJSONEnv(tb testing.TB) *lisp.LEnv {
	tb.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	if rc := lisp.InitializeUserEnv(env); rc.Type == lisp.LError {
		tb.Fatalf("initialize-user-env: %v", rc)
	}
	if rc := libjson.LoadPackage(env); rc.Type == lisp.LError {
		tb.Fatalf("load libjson: %v", rc)
	}
	if rc := env.InPackage(lisp.String(lisp.DefaultUserPackage)); rc.Type == lisp.LError {
		tb.Fatalf("in-package: %v", rc)
	}
	return env
}

// FuzzLoadExactIntegers covers the :exact-integers decode path (issue #350),
// which FuzzLoadJSON does not reach: that target measures the default, where
// every number is a float64 and the >2^53 rounding is deliberate.
//
// This one is where text preservation IS the contract, so it is asserted
// directly rather than sidestepped.
//
// INVARIANTS
//
//  1. Load never panics and never returns nil, exactly as in the default mode.
//
//  2. A document that is a bare integer literal either round-trips
//     BYTE-IDENTICALLY or fails loudly. There is no third outcome, and a
//     silently rounded value is precisely the outcome the option exists to
//     remove. This is the assertion with teeth.
//
//  3. Dump/Load agree: whatever the option decodes, Dump must encode and the
//     option must read that back, and the value must then be STABLE under
//     further round trips -- the read-modify-write shape every consumer of
//     this package has. Emitting bytes the package's own decoder rejects is
//     the strongest form of encoder bug there is, and the option can commit
//     it in a way the default cannot, because the option is allowed to refuse
//     a number.
//
//     Stability is asserted from the SECOND decode, exactly as FuzzLoadJSON
//     and FuzzDumpJSON do, and for a reason specific to this mode: the rule
//     for what is an integer is SYNTACTIC, and Dump normalises a float's text.
//     "100e7" decodes to the float 1e9, which Dump writes as "1000000000",
//     which IS an integer literal and so decodes to an int the second time.
//     The value is right at every step and stable from the second decode on;
//     it is the document that changed, not the rule. LoadOpts.ExactIntegers
//     documents this.
//
//  4. The opt-in never ACCEPTS a document the default rejects. A node that has
//     turned the option on and a node that has not must not disagree about
//     whether replicated state is well formed; disagreeing about the VALUE of
//     a large integer is the known, documented cost of the option, but
//     disagreeing about validity would be a new defect introduced by the fix.
//     The converse is allowed and expected: an integer too large for a lisp
//     int is an error under the option and a rounded float without it.
func FuzzLoadExactIntegers(f *testing.F) {
	seeds := []string{
		"", " ", "null", "true", "false",
		"0", "-0", "1", "-1", "1.5", "1e2", "1E2", "1.0", "1e400", "-1e400", "1e-400",
		"9007199254740991", // 2^53-1: exact as a float too
		"9007199254740992", // 2^53
		"9007199254740993", // 2^53+1: the value in the issue
		"-9007199254740993",
		"9223372036854775807",  // int64 max
		"-9223372036854775808", // int64 min
		"9223372036854775808",  // int64 max + 1: must be an error, not a float
		"-9223372036854775809",
		"12345678901234567890123456789",
		"00", "01", "-", "+1", "1.", ".1", "1e", "1e+", "0e0",
		`""`, `"9007199254740993"`, `"😀"`,
		"[]", "{}", "[1,2,3]", `{"a":1}`,
		`{"id":9007199254740993}`,
		`[9007199254740993,9223372036854775807,-9223372036854775808]`,
		`{"a":{"b":[{"c":9223372036854775808}]}}`,
		"[1,2", `{"a":`, "1 2", "nulll", "\xff", "\x00",
		strings.Repeat("[", 200) + strings.Repeat("]", 200),
		strings.Repeat("9", 400),
	}
	for _, s := range seeds {
		f.Add([]byte(s))
	}
	f.Fuzz(func(t *testing.T, data []byte) {
		opts := libjson.LoadOpts{ExactIntegers: true}
		v := libjson.LoadWith(data, opts)
		if v == nil {
			t.Fatal("LoadWith returned a nil LVal")
		}
		_ = v.String()

		def := libjson.Load(data, false)
		if def == nil {
			t.Fatal("Load returned a nil LVal")
		}

		if v.Type == lisp.LError {
			// Invariant 2, the loud half: a bare integer literal that does not
			// round-trip must have failed, and it did.
			return
		}

		// Invariant 4.
		if def.Type == lisp.LError {
			t.Fatalf("the option ACCEPTED a document the default rejects: %s\n--- default ---\n%s\n--- input ---\n%q",
				v, def, data)
		}

		enc, err := libjson.Dump(v, false)
		if err != nil {
			t.Fatalf("Dump rejected a value LoadWith produced: %v\n--- value ---\n%s", err, v)
		}

		// Invariant 2, the exact half.
		//
		// The assertion is on the BYTES, not on the type. An integer that fits
		// a lisp int becomes one; an integer too large for a lisp int is
		// accepted as a float only when the literal is already that float's
		// canonical text, which by construction re-encodes to the same bytes.
		// Both outcomes preserve the document exactly, which is the contract.
		// Anything that would not have been reached the third branch above and
		// returned an error.
		if lit, ok := bareJSONInteger(data); ok && string(enc) != lit {
			t.Fatalf("integer literal did not round-trip byte-identically\n--- in  ---\n%s\n--- out ---\n%s\n--- decoded as ---\n%s (%s)",
				lit, enc, lisp.GetType(v).Str, v)
		}

		// Invariant 3.
		back := libjson.LoadWith(enc, opts)
		if back == nil || back.Type == lisp.LError {
			t.Fatalf("LoadWith rejected Dump's output: %v\n--- re-encoded ---\n%s", back, enc)
		}
		reenc, err := libjson.Dump(back, false)
		if err != nil {
			t.Fatalf("Dump rejected the value its own output decoded to: %v\n--- re-encoded ---\n%s", err, enc)
		}
		again := libjson.LoadWith(reenc, opts)
		if again == nil || again.Type == lisp.LError {
			t.Fatalf("LoadWith rejected the re-encoded output: %v\n--- re-re-encoded ---\n%s", again, reenc)
		}
		if got, want := again.String(), back.String(); got != want {
			t.Fatalf("the value drifted across a second round trip\n--- first decode ---\n%s\n--- second decode ---\n%s\n--- encoded ---\n%s\n--- re-encoded ---\n%s",
				want, got, enc, reenc)
		}
		enc2, err := libjson.Dump(v, false)
		if err != nil {
			t.Fatalf("second Dump of the same value failed: %v", err)
		}
		if !bytes.Equal(enc, enc2) {
			t.Fatalf("Dump is not deterministic\n--- run 1 ---\n%s\n--- run 2 ---\n%s", enc, enc2)
		}
	})
}

// bareJSONInteger reports whether data is a whole JSON document consisting of
// nothing but an integer literal, and returns that literal. It is deliberately
// stricter than the JSON grammar -- it rejects anything it is not certain
// about -- because it gates an assertion, and a false positive there is a
// spurious failure rather than a finding.
func bareJSONInteger(data []byte) (string, bool) {
	s := strings.TrimSpace(string(data))
	if s == "" || s == "-" || s == "-0" {
		return "", false
	}
	digits := s
	if digits[0] == '-' {
		digits = digits[1:]
	}
	if digits == "" || (len(digits) > 1 && digits[0] == '0') {
		return "", false // JSON forbids leading zeros
	}
	for i := range len(digits) {
		if digits[i] < '0' || digits[i] > '9' {
			return "", false
		}
	}
	return s, true
}
