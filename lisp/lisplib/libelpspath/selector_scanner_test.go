// Copyright © 2026 The ELPS authors

package libelpspath

import (
	"strings"
	"testing"
)

// The step readers, pinned at the level selectorPaths actually uses them: the
// step each one produces AND the number of bytes it consumes.
//
// The byte count is load-bearing and invisible at the call site, which is why
// every case asserts it. A reader that produced the right step but
// UNDER-counted would leave its own tail in the remainder, and the scan would
// either stall on a valid selector or read that tail as another step; one
// that OVER-counted would swallow the step after it. `.a[0]` would still
// parse, just as some other path.
//
// These tables replace the three that pinned the CAPTURE GROUP INDICES of the
// regexps this scanner was written to replace, case for case: the inputs are
// the same, and what was asserted as match[1]/match[3]/match[4] is asserted
// here as the step, the consumed length, and -- for the "?" suffix -- the
// fact that the length covers it and the step does not mention it.
//
// A reader returns (0, nil, nil) for a form it does not own. That is not an
// error: selectorPaths owns the message, because only it knows the whole
// remainder.

// TestScanSubscript covers the bracket forms that are not a quoted key.
func TestScanSubscript(t *testing.T) {
	t.Parallel()
	testCases := []struct {
		Name string
		In   string
		Step string
		N    int
	}{
		{Name: "simple", In: `[0]`, Step: `[0]`, N: 3},
		{Name: "simple opt", In: `[0]?`, Step: `[0]`, N: 4},
		{Name: "simple neg", In: `[-1]`, Step: `[-1]`, N: 4},
		{Name: "range", In: `[0:1]`, Step: `[0:1]`, N: 5},
		{Name: "implicit start", In: `[:1]`, Step: `[0:1]`, N: 4},
		{Name: "all implicit", In: `[:]`, Step: `[0:]`, N: 3},
		{Name: "implicit end", In: `[1:]`, Step: `[1:]`, N: 4},
		{Name: "space after", In: `[0:1] `, Step: `[0:1]`, N: 5},
		{Name: "space from", In: `[ 0 :1] `, Step: `[0:1]`, N: 7},
		{Name: "space to", In: `[0: 1 ] `, Step: `[0:1]`, N: 7},
		{Name: "space before opt", In: `[0] ?`, Step: `[0]`, N: 5},
		{Name: "iterator", In: `[]`, Step: `[]`, N: 2},
		{
			// -0 is not 0 to strconv.Atoi's caller by inspection, but it is
			// by value, and the corpus depends on it: ".[-0]" is the FIRST
			// element, not the last. The digit scan has to hand the sign
			// through rather than reject it.
			Name: "neg zero", In: `[-0]`, Step: `[0]`, N: 4,
		},
		{Name: "range both negative", In: `[-2:-1]`, Step: `[-2:-1]`, N: 7},
		// The tail is left for the next round, untouched.
		{Name: "step then key", In: `[0].a`, Step: `[0]`, N: 3},
		{Name: "step then step", In: `[1:2][3]`, Step: `[1:2]`, N: 5},
	}
	for _, tc := range testCases {
		t.Run(tc.Name, func(t *testing.T) {
			t.Parallel()
			n, step, err := scanSubscript(tc.In)
			if err != nil {
				t.Fatalf("scanSubscript(%q): unexpected error: %v", tc.In, err)
			}
			if step == nil {
				t.Fatalf("scanSubscript(%q) claimed nothing", tc.In)
			}
			if got := step.String(); got != tc.Step {
				t.Errorf("scanSubscript(%q) = %s, want %s", tc.In, got, tc.Step)
			}
			if n != tc.N {
				t.Errorf("scanSubscript(%q) consumed %d bytes, want %d", tc.In, n, tc.N)
			}
		})
	}
}

// TestScanSubscriptRejects covers the two ways a subscript ends badly: a
// shape this reader does not own (no error, nothing consumed, so the scan
// reports the stall) and a bound too large for an int (an error, named).
func TestScanSubscriptRejects(t *testing.T) {
	t.Parallel()
	for _, in := range []string{
		`[`, `[0`, `]`, `[-]`, `[--]`, `[+1]`, `[1-]`, `[1:2:3]`, `[::]`,
		`[0 1]`, `["a"]`, `[a]`, `.foo`, ``, `x`,
	} {
		t.Run(in, func(t *testing.T) {
			t.Parallel()
			n, step, err := scanSubscript(in)
			if n != 0 || step != nil || err != nil {
				t.Errorf("scanSubscript(%q) = (%d, %v, %v), want (0, nil, nil)",
					in, n, step, err)
			}
		})
	}
	for _, tc := range []struct{ in, msg string }{
		{`[99999999999999999999]`, "fail to parse array index: 99999999999999999999"},
		{`[0:99999999999999999999]`, "fail to parse second array index: 99999999999999999999"},
	} {
		t.Run(tc.in, func(t *testing.T) {
			t.Parallel()
			n, step, err := scanSubscript(tc.in)
			if err == nil {
				t.Fatalf("scanSubscript(%q) = (%d, %v, nil), want an error", tc.in, n, step)
			}
			if err.Error() != tc.msg {
				t.Errorf("scanSubscript(%q) reported %q, want %q", tc.in, err, tc.msg)
			}
		})
	}
	// An overflowing bound in an UNTERMINATED bracket is a stall, not an
	// overflow: the digits are converted only once the form is recognised, so
	// the message names what is actually wrong.
	if n, _, err := scanSubscript(`[99999999999999999999`); n != 0 || err != nil {
		t.Errorf("an unterminated bracket must stall, got (%d, %v)", n, err)
	}
}

// TestScanQuotedKey covers key access using bracket-and-quote notation.
func TestScanQuotedKey(t *testing.T) {
	t.Parallel()
	testCases := []struct {
		Name string
		In   string
		Step string
		N    int
	}{
		{Name: "simple", In: `["0"]`, Step: `["0"]`, N: 5},
		{Name: "simple space", In: `[  "a1"  ]`, Step: `["a1"]`, N: 10},
		{Name: "simple opt", In: `["0"]?`, Step: `["0"]`, N: 6},
		{Name: "empty key", In: `[""]`, Step: `[""]`, N: 4},
		{
			// The literal keeps its Go escapes for strconv.Unquote, which is
			// the inverse of the %q dotPath.String() renders a key with. The
			// two-byte skip after a backslash takes the interior \" as an
			// escape SEQUENCE and carries on to the real closing quote. The
			// regexp this replaced read the body as `(?:\"|[^"])*` until
			// issue #566, in which `\"` is a plain escaped quote -- so the
			// alternation matched every character and ran greedily to the
			// last quote in the whole selector.
			Name: "escaped quote and newline", In: `["\"\n"]`, Step: `["\"\n"]`, N: 8,
		},
		{Name: "key containing a bracket", In: `["a]b"]`, Step: `["a]b"]`, N: 7},
		{Name: "trailing backslash escape", In: `["a\\"]`, Step: `["a\\"]`, N: 7},
		// The tail is left for the next round: this is the selector issue
		// #566 made unreadable.
		{Name: "two keys", In: `["a"]["b"]`, Step: `["a"]`, N: 5},
	}
	for _, tc := range testCases {
		t.Run(tc.Name, func(t *testing.T) {
			t.Parallel()
			n, step, err := scanQuotedKey(tc.In)
			if err != nil {
				t.Fatalf("scanQuotedKey(%q): unexpected error: %v", tc.In, err)
			}
			if step == nil {
				t.Fatalf("scanQuotedKey(%q) claimed nothing", tc.In)
			}
			if got := step.String(); got != tc.Step {
				t.Errorf("scanQuotedKey(%q) = %s, want %s", tc.In, got, tc.Step)
			}
			if n != tc.N {
				t.Errorf("scanQuotedKey(%q) consumed %d bytes, want %d", tc.In, n, tc.N)
			}
		})
	}
}

// TestScanQuotedKeyRejects pins the unterminated forms as stalls and the
// malformed escape as the one error this reader raises.
//
// The order matters and is asserted by the last case: strconv.Unquote runs
// only after the closing "]" is found, so `.["\q"` -- bad escape AND no
// bracket -- stalls rather than reporting a decoding error about text that
// was never a key.
func TestScanQuotedKeyRejects(t *testing.T) {
	t.Parallel()
	for _, in := range []string{
		`[0]`, `[]`, `[1:3]`, `[:]`, `.foo`, ``, `["a`, `["a"`, `[a"]`,
		`["a` + "\\", `["a\` + "\n" + `"]`, `["\q"`,
	} {
		t.Run(in, func(t *testing.T) {
			t.Parallel()
			n, step, err := scanQuotedKey(in)
			if n != 0 || step != nil || err != nil {
				t.Errorf("scanQuotedKey(%q) = (%d, %v, %v), want (0, nil, nil)",
					in, n, step, err)
			}
		})
	}
	if _, _, err := scanQuotedKey(`["\q"]`); err == nil {
		t.Error(`scanQuotedKey(["\q"]) accepted an invalid escape`)
	}
}

// TestScanDotKey covers key access with dot notation.
func TestScanDotKey(t *testing.T) {
	t.Parallel()
	testCases := []struct {
		Name string
		In   string
		Step string
		N    int
	}{
		{Name: "simple", In: `.wut`, Step: `["wut"]`, N: 4},
		{Name: "simple space", In: `.  wut`, Step: `["wut"]`, N: 6},
		{Name: "simple opt", In: `.wut?`, Step: `["wut"]`, N: 5},
		{Name: "leading underscore", In: `._private`, Step: `["_private"]`, N: 9},
		{Name: "digits after the first byte", In: `.a0_9`, Step: `["a0_9"]`, N: 5},
		{
			// The identifier rule stops at the next selector's dot, so a
			// chain does not collapse into one key.
			Name: "stops at the next dot", In: `.a.b`, Step: `["a"]`, N: 2,
		},
		{Name: "stops at a bracket", In: `.a[0]`, Step: `["a"]`, N: 2},
	}
	for _, tc := range testCases {
		t.Run(tc.Name, func(t *testing.T) {
			t.Parallel()
			n, step, err := scanDotKey(tc.In)
			if err != nil {
				t.Fatalf("scanDotKey(%q): unexpected error: %v", tc.In, err)
			}
			if step == nil {
				t.Fatalf("scanDotKey(%q) claimed nothing", tc.In)
			}
			if got := step.String(); got != tc.Step {
				t.Errorf("scanDotKey(%q) = %s, want %s", tc.In, got, tc.Step)
			}
			if n != tc.N {
				t.Errorf("scanDotKey(%q) consumed %d bytes, want %d", tc.In, n, tc.N)
			}
		})
	}
	// The narrow identifier rule, which is what keeps a key from swallowing
	// the dot of the step after it.
	for _, in := range []string{`.0`, `.$private`, `.`, `.-x`, `.9`, `[0]`, ``} {
		if n, step, err := scanDotKey(in); n != 0 || step != nil || err != nil {
			t.Errorf("scanDotKey(%q) = (%d, %v, %v), want (0, nil, nil)", in, n, step, err)
		}
	}
}

// TestSelectorStepFormsAreDisjoint pins the assumption that makes ONE
// left-to-right pass enough, which is the invariant the regexp version needed
// for the fixed order of its parser slice.
//
// scanStep dispatches on a single byte and scanBracketStep on the first
// non-blank byte inside the bracket. That dispatch is only sound if no reader
// can claim a form another one owns -- otherwise a selector would be read as
// a different path, silently, rather than rejected. Each reader is asked
// directly about the forms it does NOT own here, which is stronger than
// testing the dispatch: it says the dispatch could be reordered or replaced
// by trying each in turn without changing an answer.
func TestSelectorStepFormsAreDisjoint(t *testing.T) {
	t.Parallel()
	subscripts := []string{`[0]`, `[]`, `[1:3]`, `[:]`, `[-1]`, `[ 0 : 1 ]`}
	keys := []string{`["0"]`, `["a1"]`, `[""]`, `["a]b"]`, `["[0]"]`, `[ "x" ]`}
	dots := []string{`.foo`, `._p`, `.a.b`, `.  wut`}

	for _, in := range keys {
		if n, _, err := scanSubscript(in); n != 0 || err != nil {
			t.Errorf("scanSubscript must not claim the quoted key %q (got %d, %v)", in, n, err)
		}
	}
	for _, in := range subscripts {
		if n, _, err := scanQuotedKey(in); n != 0 || err != nil {
			t.Errorf("scanQuotedKey must not claim the subscript %q (got %d, %v)", in, n, err)
		}
	}
	for _, in := range append(append([]string{}, subscripts...), keys...) {
		if n, _, err := scanDotKey(in); n != 0 || err != nil {
			t.Errorf("scanDotKey must not claim the bracket form %q (got %d, %v)", in, n, err)
		}
	}
	for _, in := range dots {
		if n, _, err := scanSubscript(in); n != 0 || err != nil {
			t.Errorf("scanSubscript must not claim the dot key %q (got %d, %v)", in, n, err)
		}
		if n, _, err := scanQuotedKey(in); n != 0 || err != nil {
			t.Errorf("scanQuotedKey must not claim the dot key %q (got %d, %v)", in, n, err)
		}
	}

	// ...and the dispatch does route each form to the reader that owns it.
	for _, tc := range []struct{ in, want string }{
		{`[0]`, `[0]`}, {`[]`, `[]`}, {`[1:3]`, `[1:3]`},
		{`["a"]`, `["a"]`}, {`[ "x" ]`, `["x"]`}, {`.foo`, `["foo"]`},
	} {
		n, step, err := scanStep(tc.in)
		if err != nil || step == nil {
			t.Errorf("scanStep(%q) = (%d, %v, %v), want a step", tc.in, n, step, err)
			continue
		}
		if got := step.String(); got != tc.want {
			t.Errorf("scanStep(%q) = %s, want %s", tc.in, got, tc.want)
		}
	}
	// Nothing else begins a step.
	for _, in := range []string{``, `foo`, `0`, `"a"`, `:`, `]`, `?`} {
		if n, step, err := scanStep(in); n != 0 || step != nil || err != nil {
			t.Errorf("scanStep(%q) = (%d, %v, %v), want (0, nil, nil)", in, n, step, err)
		}
	}
}

// TestScannerWhitespaceSets pins the two whitespace sets apart.
//
// The readers treat only [\t\n\f\r ] as blank -- the set the grammar was
// specified with -- while the scan loop trims the REMAINDER with
// strings.TrimSpace, which is unicode-aware. So a vertical tab or a
// non-breaking space separates two steps and cannot appear inside one. It
// looks like an inconsistency and is a deliberate one: widening the readers
// would accept selectors no other implementation of this grammar accepts.
func TestScannerWhitespaceSets(t *testing.T) {
	t.Parallel()
	for _, sel := range []string{".a\v.b", ".a\u00a0.b", ".a\u2003.b", ".a \t.b"} {
		steps, err := selectorPaths(sel)
		if err != nil {
			t.Errorf("%q: unexpected error: %v", sel, err)
			continue
		}
		if len(steps) != 2 {
			t.Errorf("%q: got %d steps, want 2", sel, len(steps))
		}
	}
	for _, sel := range []string{".\va", ".\u00a0a", ".[\v0]", ".[0\u00a0]"} {
		if _, err := selectorPaths(sel); err == nil {
			t.Errorf("%q parsed, but its whitespace is not blank inside a step", sel)
		}
	}
	// The narrow set really is blank inside a step.
	for _, sel := range []string{".\ta", ".[\t0\f]", ".[\r0 :\f1]", ".[0 ]"} {
		if _, err := selectorPaths(sel); err != nil {
			t.Errorf("%q: unexpected error: %v", sel, err)
		}
	}
}

// TestSelectorBodyStopsAtANewline pins a WART, so that it is a decision
// rather than an accident.
//
// selectorBody implements the ".[x]" leading-bracket rule, and a bracket-led
// selector is cut at its first newline: ".[0]\n.a" is the path ".[0]" and the
// rest is DISCARDED -- not parsed, not rejected. That is what the regexp this
// scanner replaced did (its ".*" did not match a newline), selectors are not
// written across lines, and a grammar that means one thing here and another
// downstream would be worse than a wart that means the same thing in both
// places.
//
// The contrast is the point: a selector that does NOT lead with a bracket is
// not cut, because the rule is only about where the scan starts.
func TestSelectorBodyStopsAtANewline(t *testing.T) {
	t.Parallel()
	if got, want := selectorBody(".[0]\n.a"), "[0]"; got != want {
		t.Errorf("selectorBody(%q) = %q, want %q", ".[0]\n.a", got, want)
	}
	path, err := ParseSelector(".[0]\n.a")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if got, want := path.String(), ".[0]"; got != want {
		t.Errorf("ParseSelector(%q) = %q, want %q -- the tail is discarded", ".[0]\n.a", got, want)
	}
	if got, want := selectorBody(".a\n.b"), ".a\n.b"; got != want {
		t.Errorf("selectorBody(%q) = %q, want it uncut", ".a\n.b", got)
	}
	path, err = ParseSelector(".a\n.b")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if got, want := path.String(), `.["a"]["b"]`; got != want {
		t.Errorf("ParseSelector(%q) = %q, want %q", ".a\n.b", got, want)
	}
	// The leading-bracket rule itself: whitespace may precede the bracket,
	// and only a bracket triggers it.
	for _, tc := range []struct{ in, want string }{
		{".[0]", "[0]"},
		{". \t\n [0]", "[0]"},
		{".a[0]", ".a[0]"},
		{".", "."},
		{"", ""},
	} {
		if got := selectorBody(tc.in); got != tc.want {
			t.Errorf("selectorBody(%q) = %q, want %q", tc.in, got, tc.want)
		}
	}
}

// TestScanStepConsumesWhatItReports is the property the per-reader byte
// counts add up to: re-scanning from the reported offset yields the REST of
// the selector, step for step, so no reader can quietly borrow or leave a
// byte.
//
// It is a cheap check over a table rather than a proof, but it covers the
// combinations the individual tables do not: a key after a range, a "?" in
// the middle, blanks between steps.
func TestScanStepConsumesWhatItReports(t *testing.T) {
	t.Parallel()
	for _, tc := range []struct {
		sel  string
		want []string
	}{
		{".a.b.c", []string{`["a"]`, `["b"]`, `["c"]`}},
		{".items[0].id", []string{`["items"]`, `[0]`, `["id"]`}},
		{`.["first name"].address.city`, []string{`["first name"]`, `["address"]`, `["city"]`}},
		{".items[1:3]", []string{`["items"]`, `[1:3]`}},
		{".items[1:]?.id?", []string{`["items"]`, `[1:]`, `["id"]`}},
		{".a[]['", nil},
		{`.a [0] ["b"] .c`, []string{`["a"]`, `[0]`, `["b"]`, `["c"]`}},
		{"." + strings.Repeat("[]", 4), []string{`[]`, `[]`, `[]`, `[]`}},
	} {
		t.Run(tc.sel, func(t *testing.T) {
			t.Parallel()
			steps, err := selectorPaths(tc.sel)
			if tc.want == nil {
				if err == nil {
					t.Fatalf("selectorPaths(%q) accepted %d steps, want a stall", tc.sel, len(steps))
				}
				return
			}
			if err != nil {
				t.Fatalf("selectorPaths(%q): %v", tc.sel, err)
			}
			got := make([]string, 0, len(steps))
			for _, s := range steps {
				got = append(got, s.String())
			}
			if strings.Join(got, "") != strings.Join(tc.want, "") {
				t.Errorf("selectorPaths(%q) = %v, want %v", tc.sel, got, tc.want)
			}
		})
	}
}
