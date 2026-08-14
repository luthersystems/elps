// Copyright © 2018 The ELPS authors

package libjson

import (
	"fmt"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// TestCheckLoadableMatchesLoad pins the property the fix rests on: checkLoadable
// accepts exactly what Load accepts.
//
// Today that holds by construction -- checkLoadable calls jsonDecode, the same
// function Load decodes with -- so on the current implementation this test can
// only fail if the two paths drift apart. That is precisely its job. The
// obvious future edit is to replace the decode with something cheaper, and this
// test is what makes such an attempt safe to make: it compares the two directly
// over the inputs where a cheaper check goes wrong, rather than trusting an
// argument about why it should not.
//
//   - number literals, where the overflow that is elps#410 lives;
//   - nesting depth, where encoding/json applies a limit inside Unmarshal that
//     a streaming scan does not see -- the gap that sank the first attempt;
//   - both stringNums modes, since UseNumber changes the answer for numbers.
//
// This is not hypothetical. checkLoadable was first written as a hand-rolled
// token scan, and this test caught it accepting `""` and `{` (an unterminated
// container simply ends the scan at EOF) and accepting documents nested past
// the limit Unmarshal enforces. See the comment on checkLoadable for why that
// approach was abandoned rather than repaired.
//
// If the standard library moves its limit or changes number handling, this
// fails here instead of shipping a document that dumps and will not load.
func TestCheckLoadableMatchesLoad(t *testing.T) {
	cases := []string{
		// numbers -- the #410 family
		"1E1000",
		"-1E1000",
		"1e999999",
		"[1E1000]",
		`{"k":1E1000}`,
		"1E-1000000000000", // underflows to 0; Go accepts it
		"1.5",
		"0",
		"-0",
		"9007199254740993",
		"9223372036854775807",
		// ordinary documents
		`{"a":1,"b":[2,3],"c":"str"}`,
		`"plain string"`,
		"null",
		"true",
		"[]",
		"{}",
		// malformed, to check the two agree on rejection as well as acceptance
		"{",
		"[1,]",
		"tru",
		"",
		`{"a":1} trailing`,
	}

	// Depths bracketing encoding/json's limit. The interesting rows are the two
	// either side of it: a bare token scan accepted both, Unmarshal rejects the
	// second, and that disagreement was a real hole rather than a hypothetical.
	for _, depth := range []int{1, 64, 9999, 10000, 10001, 20000} {
		cases = append(cases,
			strings.Repeat("[", depth)+strings.Repeat("]", depth))
	}

	for _, stringNums := range []bool{false, true} {
		for _, src := range cases {
			name := fmt.Sprintf("stringNums=%v/%s", stringNums, label(src))
			t.Run(name, func(t *testing.T) {
				loadOK := Load([]byte(src), stringNums).Type != lisp.LError
				checkOK := newEncoder(stringNums).checkLoadable([]byte(src)) == nil

				if loadOK != checkOK {
					t.Fatalf("checkLoadable and Load disagree\n  input:          %s\n  Load accepts:   %v\n  check accepts:  %v",
						label(src), loadOK, checkOK)
				}
			})
		}
	}
}

// TestCheckLoadableIsTheDecoder concentrates the parity property on the short
// list of inputs a cheaper implementation is most likely to get wrong, so a
// failure points at the mistake instead of at one row of a large table.
//
// checkLoadable costs a full decode and the standing temptation is to hand-roll
// something cheaper. The first attempt was exactly that, and it got `""` and
// `{` wrong while allocating MORE than the decode it replaced. These are those
// inputs: empty and whitespace-only documents, unterminated containers and
// strings, trailing data, and the #410 literal itself.
func TestCheckLoadableIsTheDecoder(t *testing.T) {
	tricky := []string{"", "{", "[", `{"a":`, `"unterminated`, "[[]", "1E1000", "\t\n ", "1 2"}
	for _, stringNums := range []bool{false, true} {
		for _, src := range tricky {
			loadOK := Load([]byte(src), stringNums).Type != lisp.LError
			checkOK := newEncoder(stringNums).checkLoadable([]byte(src)) == nil
			if loadOK != checkOK {
				t.Errorf("stringNums=%v %s: Load accepts=%v check accepts=%v",
					stringNums, label(src), loadOK, checkOK)
			}
		}
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
