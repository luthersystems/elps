// Copyright © 2026 The ELPS authors

// In-package controls for the comparators the exported oracle is built on.
//
// The negative controls in aliasguard_broken_test.go drive the oracle from
// outside, through deliberately broken walkers.  That reaches everything a
// lisp value can express — but the adversarial review of #599 found one arm
// it cannot reach: making the alias-class comparison permissive left the
// entire suite green, because every de-aliasing shape the guard can build
// is also caught by the fingerprint, which runs first.
//
// So the comparator gets a control here instead, in-package, where it can
// be called directly.
package elpstest

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// TestSameIndexSetIsNotPermissive is the negative control for the
// alias-class comparison.  Making sameIndexSet return true unconditionally
// — the exact weakening the review applied — must fail here even though no
// end-to-end shape can see it.
func TestSameIndexSetIsNotPermissive(t *testing.T) {
	t.Parallel()
	cases := []struct {
		name string
		a, b []int
		want bool
	}{
		{"identical", []int{0, 1}, []int{0, 1}, true},
		{"identical empty", nil, nil, true},
		{"the copy sees one site fewer", []int{0, 1}, []int{0}, false},
		{"the copy sees one site more", []int{0}, []int{0, 1}, false},
		{"same size, different sites", []int{0}, []int{1}, false},
		{"disjoint", []int{0, 1}, []int{2, 3}, false},
		{"source saw nothing, copy saw something", nil, []int{0}, false},
	}
	for _, tc := range cases {
		if got := sameIndexSet(tc.a, tc.b); got != tc.want {
			t.Errorf("sameIndexSet(%v, %v) = %t, want %t (%s).\n"+
				"The alias equivalence classes a walker produced and the one it was given are\n"+
				"compared with this function. A permissive comparison switches off the mutation-probe\n"+
				"sweep — the centrepiece of the guard — and no end-to-end test can see it, because\n"+
				"the fingerprint catches every shape lisp can express before the sweep is consulted.",
				tc.a, tc.b, got, tc.want, tc.name)
		}
	}
}

// TestQuoteKeyDoesNotDoubleQuote pins the witness rendering.  A string
// key's String() is already quoted, so the obvious strconv.Quote(k.String())
// renders `map entry "\"k\""` where the doc comment, the witnesses and the
// revert-proof transcripts all say `map entry "k"`.
func TestQuoteKeyDoesNotDoubleQuote(t *testing.T) {
	t.Parallel()
	if got, want := quoteKey(lisp.String("k")), `"k"`; got != want {
		t.Errorf("quoteKey(string k) = %s, want %s", got, want)
	}
	if got, want := quoteKey(lisp.Int(3)), `"3"`; got != want {
		t.Errorf("quoteKey(int 3) = %s, want %s", got, want)
	}
}
