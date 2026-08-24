// Copyright © 2018 The ELPS authors

package libjson

import (
	"encoding/json"
	"fmt"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// loadableCase is one input to the parity table below.
//
// EVERY row gets both comparisons: against the shipped path, Dump on a native
// holding exactly these bytes, and against checkLoadable directly.
// checkLoadable decodes, so it is a total predicate on arbitrary bytes and
// there is no class of input it declines to answer for.
//
// wellFormed records whether src is syntactically well-formed JSON APART FROM
// nesting depth. It survives because a second test still needs the split:
// TestMarshalRefusesWhatItCannotParse hands exactly the malformed rows to
// json.Marshal and requires it to refuse them. It is written down rather than
// computed with json.Valid because json.Valid enforces the depth limit too,
// and the rows either side of that limit are the ones a check is most likely
// to get wrong -- computing the flag would silently reclassify exactly those.
type loadableCase struct {
	src        string
	wellFormed bool
}

func ok(src string) loadableCase  { return loadableCase{src: src, wellFormed: true} }
func bad(src string) loadableCase { return loadableCase{src: src} }

// loadableCases is the parity table: inputs where "does libjson accept this?"
// is decided, chosen so that a check which gets any of them wrong shows up here
// rather than in production.
//
// The table is much wider than the bug on purpose. It is what makes the
// implementation of checkLoadable REPLACEABLE: it has already caught a token
// scan accepting `""` and `{`, and it is what a future attempt at a cheaper
// check -- see elps#412 for the one worth making -- would have to pass.
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

		// Numbers that are NOT numbers: string contents and object keys.
		// Every one of these loads perfectly well, and a check that read the
		// bytes without tracking string context would refuse them all.
		ok(`"1E1000"`),
		ok(`"a 1E1000 b"`),
		ok(`["a 1E1000 b"]`),
		ok(`{"1E1000":1}`),
		ok(`{"1E1000":1E1000}`),
		ok(`{"k":"1E1000"}`),
		ok(`"` + strings.Repeat("9", 400) + `"`),
		ok(`{"` + strings.Repeat("9", 400) + `":1}`),

		// Escapes. Any check that skips over strings has to honour
		// backslashes, or it loses track of where the string ends and starts
		// reading text as numbers (or the reverse).
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

		// Adjacent and repeated numbers, where a check that advances literal
		// by literal can lose one by mis-computing where the previous ended.
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
//   - checkLoadable on its own. It decodes with the decoder's own function, so
//     it is total: every row gets this comparison, malformed rows included.
//   - the SHIPPED path -- Dump on a native holding exactly these bytes, which
//     is json.Marshal followed by checkLoadable, the composition that actually
//     decides what libjson emits. When Dump accepts a row its own output is
//     loaded back to close the loop.
//
// Both comparisons are kept even though the first now implies most of the
// second, because they can come apart: the shipped path is a composition, and
// a change to encodeNative -- to what it marshals, or to whether it checks at
// all -- breaks it while leaving checkLoadable itself correct.
//
// This is the safety net that makes the implementation of checkLoadable
// replaceable, so the table is deliberately much wider than the bug. It has
// already caught a hand-rolled token scan accepting `""` and `{`, and
// accepting documents nested past the limit Unmarshal enforces. The rows
// covering string context, escapes, object keys, adjacent literals and long
// digit runs are the ones a byte-level check gets wrong: each is a way to lose
// track of where a number literal begins and ends. They cost nothing to keep
// and they are precisely what the next attempt would have to survive.
//
// If the standard library moves its nesting limit or changes number handling,
// this fails here instead of shipping a document that dumps and will not load.
func TestCheckLoadableMatchesLoad(t *testing.T) {
	for _, stringNums := range []bool{false, true} {
		for _, tc := range loadableCases() {
			name := fmt.Sprintf("stringNums=%v/%s", stringNums, label(tc.src))
			t.Run(name, func(t *testing.T) {
				loadOK := Load([]byte(tc.src), stringNums).Type != lisp.LError

				checkOK := getEncoder(stringNums).checkLoadable([]byte(tc.src)) == nil
				if loadOK != checkOK {
					t.Fatalf("checkLoadable and Load disagree\n  input:          %s\n  Load accepts:   %v\n  check accepts:  %v",
						label(tc.src), loadOK, checkOK)
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
// These go through Dump, the composition, rather than through checkLoadable
// alone: most of them are malformed, and on the shipped path malformed input
// is json.Marshal's to refuse, so this is the arrangement that has to agree
// with Load whatever the check itself does. The rows are empty and
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

// maxLoadDepth is the deepest document Load accepts. It restates
// encoding/json's maxNestingDepth, which is unexported and has no accessor.
//
// checkLoadable does not need it -- the decode applies the limit itself -- so
// it lives here, in the tests that DO depend on knowing where the boundary
// falls: the depth rows of the parity table above and
// TestDumpRefusesNativeTooDeepToLoad both straddle exactly this value, and are
// only meaningful if it is where they assume.
const maxLoadDepth = 10000

// TestLoadNestingLimitIsWhereWeThinkItIs finds the nesting boundary
// empirically and asserts it is where the constant above says, so a standard
// library that moves its limit fails here -- next to the number that has to
// move with it -- rather than as a set of depth rows that quietly stop testing
// a boundary.
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

// TestMarshalRefusesWhatItCannotParse pins where syntax is settled on the path
// that emits a native: json.Marshal does not return bytes that are not
// well-formed JSON.
//
// checkLoadable decodes, so it does not RELY on this -- it would refuse
// malformed bytes on its own, and the parity table checks that it does. What
// this pins is the division of labour, which is what any cheaper check would
// have to lean on: Marshal compacts every json.Marshaler's output through the
// same scanner json.Valid uses, and generates everything else itself, so by
// the time bytes reach the check the only open questions are nesting and
// number range. The rows below are exactly the inputs the parity table marks
// as malformed, handed to Marshal through a Marshaler that returns them
// verbatim.
//
// If a future encoding/json stopped validating, that division would no longer
// hold, and the failure shows up here -- next to the reasoning it invalidates
// -- rather than inside whatever check was built on it.
func TestMarshalRefusesWhatItCannotParse(t *testing.T) {
	for _, tc := range loadableCases() {
		if tc.wellFormed {
			continue
		}
		t.Run(label(tc.src), func(t *testing.T) {
			raw := json.RawMessage(tc.src)
			if _, err := json.Marshal(&raw); err == nil {
				t.Fatalf("json.Marshal accepted malformed JSON %s, so a check on its "+
					"output can no longer assume its input is well formed", label(tc.src))
			}
		})
	}
}
