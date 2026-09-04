// Copyright © 2026 The ELPS authors

// In-package controls for the comparators the exported oracle is built on.
//
// The negative controls in aliasguard_broken_test.go drive the oracle from
// outside, through deliberately broken walkers.  This file adds direct
// unit controls for the comparators those checks are built from, where a
// weakening can be aimed at one function rather than at a whole graph.
//
// A retraction, kept because the mistake is instructive: this comment used
// to say the alias-class arm could NOT be reached end to end, since every
// de-aliasing shape lisp can build is also caught by the fingerprint.  That
// is false.  It covered DE-aliasing only and missed over-aliasing at the
// backing-array level, which the fingerprint cannot see — see
// TestGuardDetectsACopyThatInternsEqualBuffers, which fails alongside the
// test below when sameIndexSet is made permissive.
package elpstest

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// TestSameIndexSetIsNotPermissive is the direct control for the
// alias-class comparison: making sameIndexSet return true unconditionally
// must fail here.  It is deliberately redundant with the end-to-end
// control (TestGuardDetectsACopyThatInternsEqualBuffers) — this one names
// the function, that one proves the arm earns its place in the oracle.
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
				"compared with this function. A permissive comparison switches off the alias-class\n"+
				"half of the sweep, the only coverage of OVER-aliasing at the backing-array level:\n"+
				"two distinct *[]byte headers over one array get two distinct identity ordinals, so\n"+
				"the fingerprint reports them as unshared while the memory is shared.",
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
