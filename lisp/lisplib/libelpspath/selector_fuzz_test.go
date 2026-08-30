// Copyright © 2026 The ELPS authors

package libelpspath

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
)

// FuzzParseSelector fuzzes the jq-string selector front end (issue #564).
//
// WHY IT IS SEPARATE FROM FuzzPathEngine. That target generates STEPS as
// structured lisp values and never sees a selector string; the grammar --
// three regexps, a hand-rolled scan loop and strconv.Unquote -- is upstream
// of everything it exercises. Both defects this target was written for live
// in that gap.
//
// THE INVARIANT WITH TEETH is not "does not panic". It is:
//
//	a Path this parser produces must PRINT to a selector this parser reads
//	back as the same path.
//
// String() is the only thing that turns a Path into a selector, ParseSelector
// the only thing that reads one, and they are now in the same package, so a
// disagreement between them is a self-contradiction rather than a
// cross-repository question. Issue #566 was exactly that: reArrayKey's body
// matched every character, so a selector could carry at most one bracketed
// key, while String() brackets EVERY key -- `.a.b` printed as `.["a"]["b"]`,
// which this parser rejected. A plain no-panic target would have run green
// over that forever.
//
// "The same path" is compared BEHAVIOURALLY, on documents, not by comparing
// printed forms. Printed forms compare equal in the one case that matters
// least: `.[1:0]` prints as itself and means the wrong thing, which is how
// issue #563's String() defect survived. So both paths are applied to each
// document and their outcomes -- value or error text -- must agree.
//
// Termination is an assertion here too, not an assumption. Before issue
// #565, Path CONSTRUCTION was exponential in the number of iterator steps: a
// 45-byte ".[][][]..." cost 1.1s and 24 steps allocated 134 million times,
// before any document was touched. The fuzzer would have found that as a
// hang with no crasher to minimise, so the length cap below keeps a
// pathological input from being reported as a timeout instead of the
// regression it would be.
func FuzzParseSelector(f *testing.F) {
	seeds := []string{
		".", ".a", ".a.b", ".a.b.c", `.["a"]`, `.["a"]["b"]`,
		`.["first name"]["last name"]`, `.["a\"b"]`, `.["\"\n"]`, `.[""]`,
		`.["]"]`, `.["["]`, `.["a\\"]`, `.["a\\"]["b"]`,
		".[0]", ".[-1]", ".[-0]", ".[999999]", ".a[0].b",
		".[1:3]", ".[:2]", ".[1:]", ".[:]", ".[0:]", ".[-2:]", ".[-2:-1]",
		".[]", ".[].a", ".a[]", ".a[].b", ".[][]", ".[][][]",
		".a?", ".[0]?", ".[1:2]?", `.["a"]?`,
		// Malformed, to keep the error paths in the corpus.
		"", "a", "..", ".[", ".]", ".[:", ".[a]", ".[1:2:3]", `.["a`,
		".[--1]", ".[+1]", ".[ 1 : 2 ]", ".\t.a", ".[1:]]",
	}
	for _, s := range seeds {
		f.Add(s)
	}

	docSrcs := []string{
		`{"a":{"b":{"c":1}},"first name":"x","]":1,"[":2,"":3}`,
		`["a","b","c","d","e"]`,
		`[]`,
		`[{"a":[1,2]},{"b":[3]},{"a":[4]}]`,
		`{"a":[{"b":2},0,{"b":3}]}`,
		`"scalar"`,
	}
	// Fail loudly here rather than inside the fuzz body, where a broken
	// fixture would look like a finding.
	for _, src := range docSrcs {
		if v := libjson.Load([]byte(src), false); v.Type == lisp.LError {
			f.Fatalf("fixture %q did not load: %v", src, v)
		}
	}

	f.Fuzz(func(t *testing.T, sel string) {
		// See the header: an unbounded selector is a cost question, and
		// #565 is pinned by its own allocation test rather than by a
		// fuzz timeout that would name no defect.
		if len(sel) > 512 {
			return
		}
		path, err := ParseSelector(sel)
		if err != nil {
			// A rejection is a fine outcome; it must just be an error
			// rather than a nil path with a nil error.
			return
		}
		if path == nil {
			t.Fatalf("ParseSelector(%q) returned a nil path and a nil error", sel)
		}

		// stepCapHint presizes the scan's step slice, and the whole of
		// that is an UPPER-BOUND claim over a grammar. Nothing here reads
		// the difference -- an under-estimate would cost one growth, not a
		// wrong answer -- but the claim is cheap to check on every input
		// the fuzzer gets accepted, which is a far wider corpus than the
		// table in TestStepCapHintBoundsStepCount.
		steps, stepsErr := selectorPaths(sel)
		if stepsErr != nil {
			t.Fatalf("ParseSelector(%q) succeeded but selectorPaths did not: %v", sel, stepsErr)
		}
		if hint := stepCapHint(selectorBody(strings.TrimSpace(sel))); hint < len(steps) {
			t.Fatalf("stepCapHint(%q) = %d, below the %d steps the selector names",
				sel, hint, len(steps))
		}

		printed := path.String()

		// THE INVARIANT. Anything this parser accepts must print to
		// something it accepts. Issue #566 broke exactly this.
		again, err := ParseSelector(printed)
		if err != nil {
			t.Fatalf("ParseSelector(%q) printed %q, which does not parse: %v",
				sel, printed, err)
		}

		// ...and mean the same thing. Compared on documents, because
		// equal printed forms do not imply equal paths (issue #563).
		// Three of the four NON-MUTATING operations. Get alone would
		// leave the delete and nil paths -- which have their own range and
		// iterator arithmetic -- outside the invariant.
		//
		// Set is the fourth and is absent for its own reason: it needs a
		// replacement VALUE, and drawing one from the fuzzer would make
		// this target a value generator as well as a selector generator,
		// which is FuzzPathEngine's job. The mutating "!" variants are
		// absent for a different reason again -- they rework their input
		// in place, so comparing two runs means reasoning about aliasing
		// rather than about the parser.
		ops := []struct {
			name string
			run  func(Path, *lisp.LVal) (*lisp.LVal, error)
		}{
			{"get", func(p Path, d *lisp.LVal) (*lisp.LVal, error) { return p.Get(d) }},
			{"del", func(p Path, d *lisp.LVal) (*lisp.LVal, error) { return p.Delete(d) }},
			{"nil", func(p Path, d *lisp.LVal) (*lisp.LVal, error) { return p.Nil(d) }},
		}
		for _, src := range docSrcs {
			for _, op := range ops {
				// A fresh document per side: these hand back values that
				// can alias their input, so the two must not share one.
				wantV, wantErr := op.run(path, libjson.Load([]byte(src), false))
				gotV, gotErr := op.run(again, libjson.Load([]byte(src), false))

				switch {
				case (wantErr == nil) != (gotErr == nil):
					t.Fatalf("selector %q printed %q: %s on %s -- one errored and the other "+
						"did not (orig err=%v, reparsed err=%v)",
						sel, printed, op.name, src, wantErr, gotErr)
				case wantErr != nil:
					if wantErr.Error() != gotErr.Error() {
						t.Fatalf("selector %q printed %q: %s on %s -- errors differ: %q vs %q",
							sel, printed, op.name, src, wantErr, gotErr)
					}
				default:
					w, g := render(wantV), render(gotV)
					if w != g {
						t.Fatalf("selector %q printed %q: %s on %s -- results differ: %s vs %s",
							sel, printed, op.name, src, w, g)
					}
				}
			}
		}

		// Printing is stable: a third pass adds nothing. This is weaker
		// than the behavioural check above and catches a different
		// thing -- a String() that keeps rewriting its own output.
		if p3 := again.String(); p3 != printed {
			t.Fatalf("selector %q: String() is not stable: %q then %q", sel, printed, p3)
		}
	})
}

// render turns a Get outcome into a comparable string.
func render(v *lisp.LVal) string {
	if v == nil {
		return "<nil>"
	}
	return strings.TrimSpace(v.String())
}
