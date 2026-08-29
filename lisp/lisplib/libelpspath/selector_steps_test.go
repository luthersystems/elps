// Copyright © 2026 The ELPS authors

package libelpspath

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
)

// TestSelectorStepsRendering pins the step spelling each grammar form takes.
func TestSelectorStepsRendering(t *testing.T) {
	t.Parallel()
	for _, tc := range []struct{ sel, want string }{
		{".", "'()"},
		{".a", `'("a")`},
		{".a.b", `'("a" "b")`},
		{`.["first name"]`, `'("first name")`},
		{".a[0]", `'("a" 0)`},
		{".a[-1]", `'("a" -1)`},
		// The iterator step is a bare symbol VALUE, so it renders without
		// a quote mark. Nothing evaluates it -- apply passes the list's
		// elements through as they are and argToStep reads the symbol --
		// so the missing quote is a printing detail, not a lookup waiting
		// to happen. TestSelectorStepsMatchParseSelector covers it here,
		// and the lisp test "parse-path steps apply into the ? family"
		// covers it end to end through apply.
		{".a[]", `'("a" *)`},
		{".a[].b", `'("a" * "b")`},
		{".a[1:3]", `'("a" '(range 1 3))`},
		{".a[:2]", `'("a" '(range 0 2))`},
		{".a[1:]", `'("a" '(range 1))`},
		{".a[:]", `'("a" '(range 0))`},
		{".a[-2:]", `'("a" '(range -2))`},
		{".a?", `'("a")`},
	} {
		steps, err := SelectorSteps(tc.sel)
		if err != nil {
			t.Errorf("%q: unexpected error: %v", tc.sel, err)
			continue
		}
		if got := lisp.QExpr(steps).String(); got != tc.want {
			t.Errorf("SelectorSteps(%q) = %s, want %s", tc.sel, got, tc.want)
		}
	}
}

// TestSelectorStepsMatchParseSelector is the property that makes the two
// surfaces one grammar rather than two.
//
// They share selectorPaths, so they agree on WHAT was parsed by
// construction. What is not free is that the steps MEAN the same thing:
// SelectorSteps renders each leaf back into a lisp value and ArgsToPath
// parses that value again, so a rendering that does not invert argToStep
// would produce a different path from the same selector. Both routes are
// applied to documents here and required to give the same answer.
func TestSelectorStepsMatchParseSelector(t *testing.T) {
	t.Parallel()
	docs := []string{
		`{"a":{"b":{"c":1}},"items":[{"id":1},{"id":2},{"id":3}],"first name":"fn","":0}`,
		`["a","b","c","d","e"]`,
		`[]`,
		`[{"a":[1,2]},{"a":[3]}]`,
		`"scalar"`,
	}
	sels := []string{
		".", ".a", ".a.b", ".a.b.c", `.["first name"]`, `.[""]`,
		".[0]", ".[1]", ".[-1]", ".[9]", ".a[0]", ".items[1].id",
		".[1:3]", ".[:2]", ".[1:]", ".[:]", ".[0:]", ".[-2:]", ".[-2:-1]",
		".[]", ".[].a", ".items[].id", ".a[]", ".[][]",
		".a?", ".[0]?", `.["a"]["b"]`, ".items[1:].id",
	}
	for _, sel := range sels {
		t.Run(sel, func(t *testing.T) {
			t.Parallel()
			direct, derr := ParseSelector(sel)
			steps, serr := SelectorSteps(sel)
			if (derr == nil) != (serr == nil) {
				t.Fatalf("%q: ParseSelector err=%v but SelectorSteps err=%v", sel, derr, serr)
			}
			if derr != nil {
				return
			}
			viaSteps, err := ArgsToPath(steps)
			if err != nil {
				t.Fatalf("%q: steps %s did not convert back: %v",
					sel, lisp.QExpr(steps), err)
			}
			if got, want := viaSteps.String(), direct.String(); got != want {
				t.Errorf("%q: via steps prints %q, direct prints %q", sel, got, want)
			}
			for _, src := range docs {
				wv, we := direct.Get(libjson.Load([]byte(src), false))
				gv, ge := viaSteps.Get(libjson.Load([]byte(src), false))
				switch {
				case (we == nil) != (ge == nil):
					t.Fatalf("%q on %s: direct err=%v, via steps err=%v", sel, src, we, ge)
				case we != nil:
					if we.Error() != ge.Error() {
						t.Errorf("%q on %s: errors differ: %q vs %q", sel, src, we, ge)
					}
				default:
					if wv.String() != gv.String() {
						t.Errorf("%q on %s: results differ: %s vs %s", sel, src, wv, gv)
					}
				}
			}
		})
	}
}

// TestSelectorStepsRejectsWhatParseSelectorRejects keeps the two error
// surfaces together, in both senses: the same selectors are rejected AND
// with the same message.
//
// The message half is not decoration. BuiltinParsePath re-emits whatever
// SelectorSteps returns, verbatim, so a divergence would reach users as a
// different error from the same malformed selector depending on which
// surface they came through. Comparing only "did both error" leaves that
// invisible.
//
// The rejection half matters more: a selector that is not a path must not
// become a step list, because an empty step list is the IDENTITY path and
// would silently address the whole document.
func TestSelectorStepsRejectsWhatParseSelectorRejects(t *testing.T) {
	t.Parallel()
	for _, sel := range []string{
		"", "a", "..", ".[", ".]", ".[a]", ".[1:2:3]", `.["a`,
		".my-key", " ", "[0]", ".café",
	} {
		_, perr := ParseSelector(sel)
		steps, serr := SelectorSteps(sel)
		switch {
		case (perr == nil) != (serr == nil):
			t.Errorf("%q: ParseSelector err=%v but SelectorSteps err=%v", sel, perr, serr)
		case perr == nil:
			t.Errorf("%q: expected both to reject, both accepted", sel)
		case perr.Error() != serr.Error():
			t.Errorf("%q: messages differ: ParseSelector %q, SelectorSteps %q",
				sel, perr, serr)
		case steps != nil:
			t.Errorf("%q: rejected but still returned %d steps -- an empty step "+
				"list is the identity path", sel, len(steps))
		}
	}
}
