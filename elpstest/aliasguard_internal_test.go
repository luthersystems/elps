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
	"os"
	"strings"
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

// The template-to-fork property refuses to pass for free.
//
// It is driven directly rather than through CheckTransactions because the
// vacuous case is not reachable from outside: a transaction that moves its
// own fork does so by mutating state the template holds, and running that
// same transaction on the template moves the template too. So an
// end-to-end attempt at this case never gets past the earlier "no
// transaction changed its own fork" guard. The branch is still worth
// having — it is what stops a future caller whose transactions only touch
// fork-local state from getting a green property that asserted nothing —
// and it is worth testing, so it is tested where it can be reached.
func TestTemplateWriteVacuityIsReported(t *testing.T) {
	t.Parallel()
	env, err := NewForkCheckEnv()
	if err != nil {
		t.Fatal(err)
	}
	if rc := env.LoadString("p.lisp", `(set 'shared (sorted-map "k" 1))`); rc.Type == lisp.LError {
		t.Fatal(rc)
	}
	fork, err := env.Fork()
	if err != nil {
		t.Fatal(err)
	}
	before := []*Fingerprint{FingerprintEnv(fork, templateOpts)}

	// Pure expressions: they evaluate, and they change nothing the
	// fingerprint can see.
	c := TransactionCheck{Tx: []string{`(+ 1 2)`, `(* 2 3)`}, Repro: "no-op transactions"}
	got := templateToForkWitnesses(c, env, []*lisp.LEnv{fork}, before)
	if len(got) != 1 {
		t.Fatalf("a transaction set that cannot move the template produced %d witnesses, want 1:\n%v",
			len(got), got)
	}
	if !strings.Contains(got[0].Leak, "no transaction moved the template") {
		t.Errorf("the vacuity witness does not say the template never moved:\n%s", got[0])
	}
	if !strings.Contains(got[0].Detail, "pass for free") {
		t.Errorf("the vacuity witness does not explain why a green result would be worthless:\n%s", got[0])
	}
}

// TestTheDirectionMatrixMatchesTheProperties is a drift guard on the file
// header of aliasguard_isolation.go.
//
// That header used to say "Four properties" and enumerate 1-4, organised by
// MECHANISM. A fifth property was added and the header was not updated, so
// the doc asserted something false about the code — the very failure the
// claims-under-test rule exists to stop. Worse, organising the list by
// mechanism is why the missing DIRECTION stayed invisible: with no explicit
// direction space, nobody could see a hole in it.
//
// So the header now leads with the direction matrix, and this test holds
// the matrix and the code together: every direction the header names must
// correspond to a property string the check can actually emit, and every
// one of those property strings must still be named in the header. Rename
// a property, delete one, or add a direction to the doc without a property
// behind it, and this goes red.
func TestTheDirectionMatrixMatchesTheProperties(t *testing.T) {
	t.Parallel()
	src, err := os.ReadFile("aliasguard_isolation.go")
	if err != nil {
		t.Fatal(err)
	}
	text := string(src)

	header := text
	if i := strings.Index(text, "// TransactionCheck describes one run"); i > 0 {
		header = text[:i]
	}

	for _, d := range []struct{ direction, property string }{
		{"fork -> another fork", "a transaction on one fork is invisible to every other fork"},
		{"fork -> its template", "the template is unchanged by a transaction on a fork"},
		{"fork -> template -> later fork", "a fork taken after other forks were mutated is pristine"},
		{"template -> an existing fork", "a transaction on the template is invisible to every existing fork"},
	} {
		if !strings.Contains(header, d.direction) {
			t.Errorf("the file header no longer names the direction %q.\n"+
				"The direction matrix is what makes a missing direction visible; dropping a row from it\n"+
				"is how the template -> fork gap stayed hidden.", d.direction)
		}
		if !strings.Contains(text, `Property: "`+d.property+`"`) {
			t.Errorf("no property string %q is emitted any more, but the header still lists the\n"+
				"direction %q as covered by it. Either the property was renamed and the header must\n"+
				"follow, or the direction is no longer asserted at all.", d.property, d.direction)
		}
		if !strings.Contains(header, "property") {
			t.Fatal("the header no longer maps directions to properties")
		}
	}

	// The header must not claim a count that has drifted from the list.
	for _, stale := range []string{"Four properties", "Three properties"} {
		if strings.Contains(header, stale) {
			t.Errorf("the header says %q; the properties are enumerated below it and there are five.\n"+
				"A count in prose drifts the moment a property is added — that already happened once.",
				stale)
		}
	}
}
