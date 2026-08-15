// Copyright © 2018 The ELPS authors

package libjson_test

import (
	"encoding/json"
	"math/big"
	"strconv"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
)

// TestDumpRefusesNativeItCannotLoad pins the invariant elps#410 broke: whatever
// Dump emits, Load must accept.
//
// A json.RawMessage is a json.Marshaler that emits its bytes verbatim, so an
// embedder's bytes reach the output without libjson having produced them. JSON
// puts no bound on an exponent, so `1E1000` is valid syntax and marshals
// straight through -- it only fails at unmarshal time, where the target is a
// float64 and the value overflows. Before the fix this produced a document
// json:dump wrote and json:load then refused, which for a phylum persisting
// state is a liveness bug rather than a corruption one.
func TestDumpRefusesNativeItCannotLoad(t *testing.T) {
	// Every case is SYNTACTICALLY valid JSON. json.Valid accepts all of them,
	// which is why a syntax check would not have caught this.
	unloadable := []string{
		"1E1000",           // the fuzzer's find (FuzzDumpJSON/f30697df7ba6591e)
		"-1E1000",          // and its negation
		"1e999999",         // lower-case exponent
		"[1E1000]",         // nested inside an array
		`{"k":1E1000}`,     // nested inside an object
		"1E-1000000000000", // underflow is accepted by Go, kept as a guard below
	}

	for _, src := range unloadable {
		t.Run(src, func(t *testing.T) {
			raw := json.RawMessage(src)
			v := lisp.Native(&raw)

			// Establish what Load does with these bytes, so the test asserts a
			// RELATIONSHIP rather than a hard-coded verdict. Underflow decodes
			// to 0 rather than failing, so that row exercises the accept path.
			loadable := libjson.Load([]byte(src), false).Type != lisp.LError

			enc, err := libjson.Dump(v, false)
			if loadable {
				if err != nil {
					t.Fatalf("Dump refused %s, which Load accepts: %v", src, err)
				}
				if back := libjson.Load(enc, false); back.Type == lisp.LError {
					t.Fatalf("Load rejected Dump's own output %s: %v", enc, back)
				}
				return
			}
			if err == nil {
				t.Fatalf("Dump emitted %s, which Load rejects -- elps#410", enc)
			}
			if !strings.Contains(err.Error(), "unable to encode native value") {
				t.Fatalf("unexpected error for %s: %v", src, err)
			}
		})
	}
}

// TestDumpAcceptsOrdinaryNatives guards against the fix over-reaching: the
// check must not start refusing native values that were always fine.  Without
// this, "refuse everything" would pass the test above.
func TestDumpAcceptsOrdinaryNatives(t *testing.T) {
	ok := []string{
		`{"a":1,"b":[2,3]}`,
		`"a string"`,
		`1.5`,
		`9007199254740993`, // rounds on load, but loads -- not this bug
		`null`,
		`true`,
		`[]`,
		`{}`,
	}
	for _, src := range ok {
		t.Run(src, func(t *testing.T) {
			raw := json.RawMessage(src)
			enc, err := libjson.Dump(lisp.Native(&raw), false)
			if err != nil {
				t.Fatalf("Dump refused an ordinary native %s: %v", src, err)
			}
			if back := libjson.Load(enc, false); back.Type == lisp.LError {
				t.Fatalf("Load rejected Dump's output for %s: %v", src, back)
			}
		})
	}
}

// TestDumpNativeHonoursStringNumbers pins the mode-dependence rather than
// leaving it implicit.  Under :string-numbers the decoder uses UseNumber, which
// keeps a number as text and never converts it to a float64 -- so `1E1000` is
// loadable in that mode and Dump must NOT refuse it.  A check that ignored the
// mode would reject a document the caller can read back perfectly well.
func TestDumpNativeHonoursStringNumbers(t *testing.T) {
	raw := json.RawMessage("1E1000")

	if _, err := libjson.Dump(lisp.Native(&raw), false); err == nil {
		t.Fatal("stringNums=false: expected Dump to refuse 1E1000")
	}

	enc, err := libjson.Dump(lisp.Native(&raw), true)
	if err != nil {
		t.Fatalf("stringNums=true: Dump refused 1E1000, which Load accepts there: %v", err)
	}
	if back := libjson.Load(enc, true); back.Type == lisp.LError {
		t.Fatalf("stringNums=true: Load rejected Dump's own output %s: %v", enc, back)
	}
}

// unloadableMarshaler is the smallest embedder type that reproduces elps#410
// without going anywhere near json.RawMessage: encoding/json calls MarshalJSON
// and compacts whatever comes back, and compaction checks syntax, not range.
type unloadableMarshaler struct{}

func (unloadableMarshaler) MarshalJSON() ([]byte, error) { return []byte("1E1000"), nil }

// TestDumpRefusesUnloadableNativeBeyondRawMessage is the evidence against
// narrowing this check to a json.RawMessage/json.Number type switch.
//
// The tempting optimisation is to validate only when the native IS one of the
// two known pass-through types, on the theory that anything else is a Go value
// encoding/json marshals itself and therefore cannot produce an out-of-range
// literal -- Marshal already rejects Inf and NaN, and no float64 or uint64
// prints outside float64 range.
//
// That theory is false, and cheaply so. ANY json.Marshaler writes its own
// bytes, and the standard library ships one: math/big.Int marshals as raw
// decimal digits, so a big.Int above ~1.8e308 dumps a literal that will not
// load, while being an ordinary Go value of neither pass-through type.
//
// Worse for a type switch, the Marshaler need not be the native's own type. It
// can sit in a struct field, a map value or a slice element, at any depth,
// behind an interface whose static type says nothing -- the last three rows
// here. That is why the check runs on the marshalled BYTES: they are the only
// place the property is actually decidable.
func TestDumpRefusesUnloadableNativeBeyondRawMessage(t *testing.T) {
	huge := new(big.Int).Exp(big.NewInt(10), big.NewInt(400), nil)

	cases := []struct {
		name string
		v    interface{}
	}{
		{"custom json.Marshaler", unloadableMarshaler{}},
		{"big.Int", huge},
		{"big.Int in a struct field", struct {
			N *big.Int `json:"n"`
		}{huge}},
		{"big.Int in a map value", map[string]interface{}{"n": huge}},
		{"big.Int in a slice element", []*big.Int{huge}},
		{"RawMessage in a struct field", struct {
			P json.RawMessage `json:"p"`
		}{json.RawMessage("1E1000")}},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Establish the premise rather than assuming it: these values
			// marshal without complaint, and the bytes are valid JSON...
			b, err := json.Marshal(tc.v)
			if err != nil {
				t.Fatalf("premise broken: the value does not marshal: %v", err)
			}
			if !json.Valid(b) {
				t.Fatalf("premise broken: %s is not valid JSON", b)
			}
			// ...and yet Load refuses them. That gap is the bug.
			if libjson.Load(b, false).Type != lisp.LError {
				t.Fatalf("premise broken: Load accepts %s, so this is not an "+
					"unloadable native and the row proves nothing", b)
			}

			// So Dump must refuse them too, rather than emitting a document
			// this package will not read back.
			enc, err := libjson.Dump(lisp.Native(tc.v), false)
			if err == nil {
				t.Fatalf("Dump emitted %s, which Load rejects -- elps#410", enc)
			}
			if !strings.Contains(err.Error(), "unable to encode native value") {
				t.Fatalf("unexpected error: %v", err)
			}
		})
	}
}

// deepNative builds a native Go value nested n levels deep. It is deliberately
// NOT a json.RawMessage: the point of the rows below is a value encoding/json
// serializes itself.
func deepNative(n int) interface{} {
	var v interface{} = 1
	for range n {
		v = []interface{}{v}
	}
	return v
}

// TestDumpRefusesNativeTooDeepToLoad is the second way a native can dump into a
// document that will not load, and the one a syntax check does not catch.
//
// encoding/json applies a nesting limit of 10000 when it PARSES -- inside
// Unmarshal, and inside the compaction it runs over any json.Marshaler's
// output. It applies no such limit when it SERIALIZES a plain Go value, which
// it walks structurally and never parses. So a native that is an ordinary
// nested slice marshals happily at any depth and produces a document Load then
// refuses, exactly as `1E1000` does.
//
// This is why the check cannot be narrowed to number literals: the depth rows
// here and the range rows above are independent holes in the same invariant,
// and json.Marshal closes neither of them for a value of this shape. The two
// rows either side of the limit are what pin the boundary, so a check that is
// off by one shows up here.
func TestDumpRefusesNativeTooDeepToLoad(t *testing.T) {
	for _, depth := range []int{64, 9999, 10000, 10001, 12000} {
		t.Run(strconv.Itoa(depth), func(t *testing.T) {
			v := deepNative(depth)

			// Establish the premise rather than assuming it: json.Marshal is
			// perfectly happy with the value at every depth here, so nothing
			// upstream of the check has refused it.
			b, err := json.Marshal(v)
			if err != nil {
				t.Fatalf("premise broken: the value does not marshal: %v", err)
			}

			// Whatever Load makes of those bytes is what Dump must agree with.
			loadable := libjson.Load(b, false).Type != lisp.LError

			enc, derr := libjson.Dump(lisp.Native(v), false)
			if loadable {
				if derr != nil {
					t.Fatalf("Dump refused a %d-deep native, which Load accepts: %v", depth, derr)
				}
				if back := libjson.Load(enc, false); back.Type == lisp.LError {
					t.Fatalf("Load rejected Dump's own output at depth %d: %v", depth, back)
				}
				return
			}
			if derr == nil {
				t.Fatalf("Dump emitted a %d-deep document, which Load rejects -- elps#410", depth)
			}
			if !strings.Contains(derr.Error(), "unable to encode native value") {
				t.Fatalf("unexpected error at depth %d: %v", depth, derr)
			}
		})
	}
}
