// Copyright © 2018 The ELPS authors

package libjson

import (
	"encoding/json"
	"fmt"
	"strconv"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// loadableCase is one input to the parity table below.
//
// wellFormed records whether src is syntactically well-formed JSON APART FROM
// nesting depth. It is written down rather than computed with json.Valid
// because json.Valid enforces the depth limit too, and the rows either side of
// that limit are the ones the check is most likely to get wrong -- computing
// the flag would quietly drop exactly those rows from the direct comparison.
//
// It is what decides which of the two comparisons a row gets:
//
//   - every row is run through the SHIPPED path, Dump on a native holding
//     exactly these bytes, and must agree with Load;
//   - a well-formed row is additionally compared against checkLoadable
//     directly, because checkLoadable is only a total predicate on input that
//     is already well-formed -- see its comment for why syntax is established
//     before it runs.
type loadableCase struct {
	src        string
	wellFormed bool
}

func ok(src string) loadableCase  { return loadableCase{src: src, wellFormed: true} }
func bad(src string) loadableCase { return loadableCase{src: src} }

// loadableCases is the parity table: inputs where "does libjson accept this?"
// is decided, chosen so that a byte scanner which gets any of them wrong shows
// up here rather than in production.
func loadableCases() []loadableCase {
	cases := []loadableCase{
		// The #410 family: valid syntax, out of float64 range.
		ok("1E1000"),
		ok("-1E1000"),
		ok("1e999999"),
		ok("[1E1000]"),
		ok(`{"k":1E1000}`),
		ok("1E-1000000000000"), // underflows to 0; Go accepts it

		// Ordinary numbers.
		ok("1.5"),
		ok("0"),
		ok("-0"),
		ok("-0.0"),
		ok("9007199254740993"),
		ok("9223372036854775807"),
		ok("-9223372036854775808"),

		// Exponent edge cases, bracketing the float64 ceiling from both
		// sides. A range check that is off by one order of magnitude, or that
		// mishandles a '+' sign or an upper-case marker, fails here.
		ok("1e0"),
		ok("1e308"),
		ok("1e309"),
		ok("-1e309"),
		ok("1e+308"),
		ok("1e+309"),
		ok("1E308"),
		ok("1E309"),
		ok("1.7976931348623157e308"), // the largest float64
		ok("1.7976931348623159e308"), // just past it
		ok("17976931348623157e292"),  // same value, mantissa carried into the exponent
		ok("17976931348623159e292"),
		ok("100000e304"),  // 1e309 spelled with the magnitude split across both parts
		ok("1000000e303"), // ditto, one digit further over
		ok("0.00001e314"), // magnitude hidden behind a leading zero fraction
		ok("0.1e310"),
		ok("1.0e309"),
		ok("0e999999"),   // zero mantissa: no exponent can lift it out of range
		ok("0.0e999999"), // ditto with a fractional zero
		ok("-0e999999"),
		ok("1e-400"),                      // underflow, which Go accepts
		ok("1e-999999999999999999999999"), // exponent far past what int64 holds

		// Long digit runs with no exponent at all. The magnitude is carried
		// entirely by the number of digits, so a check that only looks for an
		// exponent marker misses these.
		ok(strings.Repeat("9", 308)),
		ok(strings.Repeat("9", 309)),
		ok(strings.Repeat("9", 400)),
		ok("-" + strings.Repeat("9", 400)),
		ok("0." + strings.Repeat("9", 400)),    // 400 digits, value below 1
		ok(strings.Repeat("9", 400) + "e-500"), // 400 digits pulled back into range

		// Numbers that are NOT numbers: string contents and object keys. A
		// scan that ignores string context refuses these, and every one of
		// them loads perfectly well.
		ok(`"1E1000"`),
		ok(`"a 1E1000 b"`),
		ok(`["a 1E1000 b"]`),
		ok(`{"1E1000":1}`),
		ok(`{"1E1000":1E1000}`),
		ok(`{"k":"1E1000"}`),
		ok(`"` + strings.Repeat("9", 400) + `"`),
		ok(`{"` + strings.Repeat("9", 400) + `":1}`),

		// Escapes. A scan that skips strings must honour backslashes, or it
		// loses track of where the string ends and starts reading text as
		// numbers (or the reverse).
		ok(`["\"1E1000",1]`), // an escaped quote immediately before a number
		ok(`["\"1E1000",1E1000]`),
		ok(`["\\",1E1000]`),    // string ends in an escaped backslash
		ok(`["\\\\",1E1000]`),  // two escaped backslashes
		ok(`["\\\"1E1000",1]`), // backslash then escaped quote
		ok(`["a\"b",1E1000]`),
		ok(`["1",1E1000]`),
		ok(`{"\"":1E1000}`),
		ok(`["\\"]`),
		ok(`["\""]`),

		// Adjacent and repeated numbers, where a scanner can lose a literal by
		// mis-computing how far to advance past the previous one.
		ok("[1,2,3]"),
		ok("[1E1000,1]"),
		ok("[1,1E1000]"),
		ok("[1,2,1E1000,3]"),
		ok("[-1,-2,-1E1000]"),
		ok("[1.5,2.5,1E1000]"),
		ok("[1e2,1e3,1e4]"),
		ok(`{"a":1,"b":1E1000}`),

		// Nesting, so a number is found well below the top level.
		ok("[[[[1E1000]]]]"),
		ok(`{"a":{"b":{"c":1E1000}}}`),
		ok(`{"a":[{"b":[1E1000]}]}`),
		ok("[[[[1]]]]"),

		// Whitespace around a literal.
		ok("[ 1E1000 ]"),
		ok("  1E1000  "),
		ok("[\n\t1,\n\t1E1000\n]"),

		// Ordinary documents.
		ok(`{"a":1,"b":[2,3],"c":"str"}`),
		ok(`"plain string"`),
		ok("null"),
		ok("true"),
		ok("false"),
		ok("[]"),
		ok("{}"),
		ok(`[null,true,false]`), // bare literals contain 'e' and 'a'..'s'

		// Malformed, to check the two agree on rejection as well as
		// acceptance.
		bad("{"),
		bad("["),
		bad("[1,]"),
		bad("tru"),
		bad(""),
		bad("\t\n "),
		bad(`{"a":`),
		bad(`"unterminated`),
		bad("[[]"),
		bad("1 2"),
		bad(`{"a":1} trailing`),
	}

	// Depths bracketing encoding/json's limit. The interesting rows are the
	// two either side of it: a bare token scan accepted both, Load rejects the
	// second, and that disagreement was a real hole rather than a
	// hypothetical. These are well-formed by construction -- balanced brackets
	// and nothing else -- which is why the flag is written down instead of
	// asking json.Valid, since json.Valid applies the very limit under test.
	for _, depth := range []int{1, 64, 9999, 10000, 10001, 20000} {
		cases = append(cases,
			ok(strings.Repeat("[", depth)+strings.Repeat("]", depth)),
			ok(strings.Repeat(`{"k":`, depth)+"1"+strings.Repeat("}", depth)))
	}
	// A number that only becomes reachable after the depth limit is passed,
	// so depth and range cannot mask each other.
	cases = append(cases,
		ok(strings.Repeat("[", 10001)+"1E1000"+strings.Repeat("]", 10001)),
		ok(strings.Repeat("[", 9999)+"1E1000"+strings.Repeat("]", 9999)))

	return cases
}

// TestCheckLoadableMatchesLoad pins the property the #410 fix rests on:
// whatever Dump emits, Load accepts.
//
// It compares two things against Load, for every row of the table:
//
//   - the SHIPPED path -- Dump on a native holding exactly these bytes, which
//     is json.Marshal followed by checkLoadable, the composition that actually
//     decides what libjson emits. Every row gets this comparison, malformed
//     rows included, and when Dump accepts a row its own output is loaded back
//     to close the loop.
//   - checkLoadable on its own, for rows that are well-formed. checkLoadable
//     is a total predicate there and nowhere else: json.Marshal establishes
//     syntax before it runs, so it is not asked about `{` or `tru` on the
//     shipped path and does not answer for them here either.
//
// This is the whole safety net for replacing the implementation of
// checkLoadable, so the table is deliberately much wider than the bug. It was
// first written as a hand-rolled token scan, and this test caught it accepting
// `""` and `{` and accepting documents nested past the limit Unmarshal
// enforces. The rows covering string context, escapes, object keys, adjacent
// literals and long digit runs are there for the byte scanner that replaced
// the decode: each of them is a way for a scan to lose track of where a number
// literal begins and ends.
//
// If the standard library moves its nesting limit or changes number handling,
// this fails here instead of shipping a document that dumps and will not load.
func TestCheckLoadableMatchesLoad(t *testing.T) {
	for _, stringNums := range []bool{false, true} {
		for _, tc := range loadableCases() {
			name := fmt.Sprintf("stringNums=%v/%s", stringNums, label(tc.src))
			t.Run(name, func(t *testing.T) {
				loadOK := Load([]byte(tc.src), stringNums).Type != lisp.LError

				if tc.wellFormed {
					checkOK := newEncoder(stringNums).checkLoadable([]byte(tc.src)) == nil
					if loadOK != checkOK {
						t.Fatalf("checkLoadable and Load disagree\n  input:          %s\n  Load accepts:   %v\n  check accepts:  %v",
							label(tc.src), loadOK, checkOK)
					}
				}

				raw := json.RawMessage(tc.src)
				out, err := Dump(lisp.Native(&raw), stringNums)
				if loadOK != (err == nil) {
					t.Fatalf("Dump and Load disagree\n  input:          %s\n  Load accepts:   %v\n  Dump accepts:   %v (%v)",
						label(tc.src), loadOK, err == nil, err)
				}
				if err == nil {
					if back := Load(out, stringNums); back.Type == lisp.LError {
						t.Fatalf("Load rejected Dump's own output for %s: %v", label(tc.src), back)
					}
				}
			})
		}
	}
}

// TestCheckLoadableIsTheDecoder concentrates the parity property on the short
// list of inputs a cheaper implementation is most likely to get wrong, so a
// failure points at the mistake instead of at one row of a large table.
//
// These go through Dump rather than checkLoadable because most of them are
// malformed, and malformed input is json.Marshal's to refuse: empty and
// whitespace-only documents, unterminated containers and strings, trailing
// data, and the #410 literal itself.
func TestCheckLoadableIsTheDecoder(t *testing.T) {
	tricky := []string{"", "{", "[", `{"a":`, `"unterminated`, "[[]", "1E1000", "\t\n ", "1 2"}
	for _, stringNums := range []bool{false, true} {
		for _, src := range tricky {
			loadOK := Load([]byte(src), stringNums).Type != lisp.LError
			raw := json.RawMessage(src)
			_, err := Dump(lisp.Native(&raw), stringNums)
			if loadOK != (err == nil) {
				t.Errorf("stringNums=%v %s: Load accepts=%v Dump accepts=%v",
					stringNums, label(src), loadOK, err == nil)
			}
		}
	}
}

// TestLoadNestingLimitIsWhereWeThinkItIs pins the boundary the depth check
// copies, so a change in the standard library fails here -- next to the
// constant that has to move -- rather than as a mysterious parity failure.
//
// The check cannot ask encoding/json what its limit is: maxNestingDepth is
// unexported and there is no accessor. So the constant is restated, and this
// test is the thing that keeps the restatement honest by finding the boundary
// empirically and asserting it is exactly where the constant says.
func TestLoadNestingLimitIsWhereWeThinkItIs(t *testing.T) {
	load := func(depth int) bool {
		src := strings.Repeat("[", depth) + strings.Repeat("]", depth)
		return Load([]byte(src), false).Type != lisp.LError
	}
	if !load(maxLoadDepth) {
		t.Errorf("Load rejects depth %d, but maxLoadDepth says it is the deepest acceptable", maxLoadDepth)
	}
	if load(maxLoadDepth + 1) {
		t.Errorf("Load accepts depth %d, so the limit has moved past maxLoadDepth (%d)",
			maxLoadDepth+1, maxLoadDepth)
	}
}

func label(s string) string {
	if len(s) <= 40 {
		if s == "" {
			return "<empty>"
		}
		return s
	}
	return fmt.Sprintf("<%d bytes, starts %q>", len(s), s[:12])
}

// TestMarshalRefusesWhatItCannotParse pins the premise checkLoadable rests on:
// json.Marshal does not return bytes that are not well-formed JSON.
//
// checkLoadable is called on Marshal's output and nothing else, and it does not
// check syntax -- it counts brackets and range-checks numbers, which are the
// two questions Marshal leaves open. That is only sound because Marshal
// compacts every json.Marshaler's output through the same scanner json.Valid
// uses, and generates everything else itself. The rows below are exactly the
// inputs the parity table marks as malformed, handed to Marshal through a
// Marshaler that returns them verbatim.
//
// If a future encoding/json stopped validating, dump would start emitting
// documents load refuses, and the failure would show up here -- next to the
// reasoning it invalidates -- rather than as a puzzling parity failure.
func TestMarshalRefusesWhatItCannotParse(t *testing.T) {
	for _, tc := range loadableCases() {
		if tc.wellFormed {
			continue
		}
		t.Run(label(tc.src), func(t *testing.T) {
			raw := json.RawMessage(tc.src)
			if _, err := json.Marshal(&raw); err == nil {
				t.Fatalf("json.Marshal accepted malformed JSON %s, so checkLoadable "+
					"can no longer assume its input is well formed", label(tc.src))
			}
		})
	}
}

// TestCheckLoadableDoesNotAllocate is the guard on the cost that made the
// original implementation untenable.
//
// The decode this replaced allocated in proportion to the DOCUMENT -- 1159
// allocations for the 4KiB document BenchmarkEncodeNativeLarge encodes,
// because decoding into an interface{} materialises every value only to throw
// it away. The scan reads the bytes where they lie. The budget is generous on
// purpose: what is being pinned is the order of magnitude, so that an edit
// which quietly reintroduces a decode fails loudly here instead of showing up
// as a downstream latency report.
func TestCheckLoadableDoesNotAllocate(t *testing.T) {
	var large strings.Builder
	large.WriteString(`{"users":[`)
	for i := range 60 {
		if i > 0 {
			large.WriteByte(',')
		}
		large.WriteString(`{"id":12345,"name":"a name here","tags":["x","y","z"],"score":1.5}`)
	}
	large.WriteString(`]}`)

	docs := []struct {
		name string
		src  string
	}{
		{"small", `{"a":1,"b":[2,3],"c":"str"}`},
		{"large", large.String()},
		{"deeply nested", strings.Repeat("[", 9999) + "1" + strings.Repeat("]", 9999)},
		{"long digit runs", "[" + strings.Repeat(strings.Repeat("9", 400)+",", 20) + "1]"},
		{"exponents", "[" + strings.Repeat("1.5e300,", 50) + "1]"},
		{"numbers inside strings", `["1E1000","1E1000","1E1000"]`},
		{"refused", "[1,2,1E1000]"},
	}

	const budget = 2
	for _, mode := range []bool{false, true} {
		enc := newEncoder(mode)
		for _, doc := range docs {
			b := []byte(doc.src)
			name := fmt.Sprintf("stringNums=%v/%s", mode, doc.name)
			t.Run(name, func(t *testing.T) {
				if n := testing.AllocsPerRun(50, func() { _ = enc.checkLoadable(b) }); n > budget {
					t.Errorf("checkLoadable allocated %v times per run over %d bytes (budget %d)",
						n, len(b), budget)
				}
			})
		}
	}
}

// TestNumberInFloat64RangeMatchesParseFloat checks the range decision against
// the conversion Load actually performs, over far more literals than the
// parity table can carry.
//
// numberInFloat64Range answers from the literal's SHAPE wherever the shape
// settles it, and converts only in the single decade that straddles
// MaxFloat64. That is an argument about decimal exponents, and an argument is
// not evidence: this sweeps mantissas against exponents either side of the
// ceiling and requires the shape-based answer to agree with strconv.ParseFloat
// on every one. Underflow is included deliberately, because ParseFloat treats
// it as success and a check that confused "tiny" with "out of range" would
// refuse documents that load.
func TestNumberInFloat64RangeMatchesParseFloat(t *testing.T) {
	mantissas := []string{
		"0", "1", "2", "5", "9", "10", "17", "18",
		"1.5", "9.9", "0.1", "0.9", "0.0001", "0.00001",
		"1.7976931348623157", "1.7976931348623159",
		"17976931348623157", "17976931348623159",
		"100000", "1000000", "0.0", "0.000",
		strings.Repeat("9", 20), strings.Repeat("9", 308),
		strings.Repeat("9", 309), strings.Repeat("9", 400),
		"0." + strings.Repeat("9", 400),
	}
	exponents := []string{""}
	for e := -320; e <= 320; e++ {
		exponents = append(exponents, "e"+strconv.Itoa(e))
	}
	exponents = append(exponents,
		"e+308", "e+309", "E308", "E309", "e0", "e-0", "e+0",
		"e999999", "e-999999", "e99999999999999999999999",
		"e-99999999999999999999999")

	checked := 0
	for _, m := range mantissas {
		for _, e := range exponents {
			for _, sign := range []string{"", "-"} {
				lit := sign + m + e
				want := true
				if _, err := strconv.ParseFloat(lit, 64); err != nil {
					want = false
				}
				n, got := numberInFloat64Range([]byte(lit))
				if n != len(lit) {
					t.Fatalf("%s: scanned %d of %d bytes", lit, n, len(lit))
				}
				if got != want {
					t.Errorf("%s: ParseFloat in range=%v, scan says %v", lit, want, got)
				}
				checked++
			}
		}
	}
	t.Logf("checked %d literals", checked)
}
