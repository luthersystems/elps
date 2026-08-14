// Copyright © 2018 The ELPS authors

package libjson

import (
	"encoding/json"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// FuzzCheckLoadableMatchesLoad is the differential form of
// TestCheckLoadableMatchesLoad: same property, inputs it did not have to be
// told about.
//
// The property is the one elps#410 broke -- whatever dump emits, load must
// accept -- and it is checked as an EQUIVALENCE in both directions, because
// the two ways to be wrong are opposite and equally bad. Accepting bytes Load
// refuses is #410 itself: a document written and then unreadable. Refusing
// bytes Load accepts is the opposite failure, and it turns a working phylum
// into a broken one just as effectively.
//
// checkLoadable decodes with the decoder's own function, so today the
// equivalence holds by construction and this target cannot fail on the
// arithmetic of number literals. It is kept for the two things construction
// does not cover. First, it drives the SHIPPED composition -- json.Marshal
// then checkLoadable, which is what actually decides whether libjson emits a
// native -- so a change to encodeNative that stops checking, or checks the
// wrong bytes, is caught here. Second, it is the differential harness any
// cheaper implementation would be judged against, and a cheaper implementation
// is exactly what elps#412 contemplates; a check that reads the bytes rather
// than decoding them CAN mistake string content or an object key for a number
// literal, and this is where that shows up on inputs nobody thought to write
// down.
//
// The enumerated table remains the right tool for the nesting boundary, which
// is a hard edge at exactly 10000 that no fuzzer will find by chance. This is
// the right tool for everything else: escapes, string context, number shapes
// and the interactions between them.
func FuzzCheckLoadableMatchesLoad(f *testing.F) {
	for _, tc := range loadableCases() {
		if len(tc.src) > 4096 {
			// The depth rows are enumerated for a boundary a fuzzer cannot
			// reach, and seeding megabyte inputs only slows the corpus down.
			continue
		}
		f.Add([]byte(tc.src), false)
		f.Add([]byte(tc.src), true)
	}
	// Inputs where the encoder and the decoder plausibly disagree for reasons
	// that have nothing to do with numbers, so a divergence would be a finding
	// about the check's scope rather than about its arithmetic.
	for _, seed := range []string{
		"\"\xff\xfe\"",           // invalid UTF-8, which encoding/json rewrites
		`"\ud800"`,               // a lone surrogate
		`{"a":1,"a":2}`,          // duplicate keys
		"1.7976931348623157e308", // the largest float64
		"1.7976931348623159e308", // the first one past it
		`["\\\\\\"]`,             // a run of escaped backslashes
		`{"1E1000":1E1000}`,      // a number-shaped KEY beside a real number
		"[" + strings.Repeat("[", 64) + "1E1000" + strings.Repeat("]", 64) + "]",
	} {
		f.Add([]byte(seed), false)
		f.Add([]byte(seed), true)
	}

	f.Fuzz(func(t *testing.T, b []byte, stringNums bool) {
		loadOK := Load(b, stringNums).Type != lisp.LError

		// The shipped path, end to end: a native holding exactly these bytes.
		raw := json.RawMessage(b)
		out, err := Dump(lisp.Native(&raw), stringNums)
		if loadOK != (err == nil) {
			t.Fatalf("Dump and Load disagree\n  stringNums: %v\n  input:      %q\n  Load accepts: %v\n  Dump accepts: %v (%v)",
				stringNums, b, loadOK, err == nil, err)
		}
		if err == nil {
			if back := Load(out, stringNums); back.Type == lisp.LError {
				t.Fatalf("Load rejected Dump's own output\n  stringNums: %v\n  input:      %q\n  emitted:    %q\n  error:      %v",
					stringNums, b, out, back)
			}
		}

		// checkLoadable on its own, on arbitrary bytes: it decodes, so it is
		// total and owes an answer for malformed input too.
		checkOK := newEncoder(stringNums).checkLoadable(b) == nil
		if loadOK != checkOK {
			t.Fatalf("checkLoadable and Load disagree\n  stringNums: %v\n  input:      %q\n  Load accepts:  %v\n  check accepts: %v",
				stringNums, b, loadOK, checkOK)
		}
	})
}
