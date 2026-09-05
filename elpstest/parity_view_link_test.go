// Copyright © 2026 The ELPS authors

// In-package: parityFingerprint and templateOpts are unexported.

package elpstest

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// TestForkParity_ViewLinkIsInvisibleToTheOracle pins, by running it, that
// the cell-view link PR #602 records on every view header (Native = the
// root *LVal, Int = the offset, on LSExpr headers only; the convention on
// lisp.cellsView) is invisible to this oracle's two comparators.  A
// program with views of every producing shape at load scope is loaded
// cold and loaded into a template that is then forked: the two arms must
// fingerprint identically under the parity options, and a transaction
// sequence that sorts through the roots, observes the views, and takes a
// fresh view inside the transaction must produce no witness.
//
// The fingerprint keys headers on the *LVal and emits an LSExpr's cells,
// never its Native or Int, and the fork remaps the link onto the fork's
// own root, so nothing in either arm's tokens names the other's memory.
// The premise is checked rather than assumed: every view in both arms must
// actually carry a live link, or the test would pass vacuously on a tree
// that recorded nothing.
func TestForkParity_ViewLinkIsInvisibleToTheOracle(t *testing.T) {
	const program = `
(set 'l (list 30 10 20 40))
(set 'tail (cdr l))
(set 'tail2 (cdr (cdr l)))
(set 'sl (slice 'list l 1 3))
(set 'v (vector 3 1 2))
(set 'r (rest v))
(set 'sv (slice 'vector v 0 2))
(set 'w (append 'vector v))
`
	views := []string{"tail", "tail2", "sl", "r"}
	vectorViews := []string{"sv", "w"}

	load := func(name string) *lisp.LEnv {
		env, err := NewForkCheckEnv()
		if err != nil {
			t.Fatalf("%s: env: %v", name, err)
		}
		if rc := env.LoadString("program.lisp", program); rc.Type == lisp.LError {
			t.Fatalf("%s: program: %v", name, rc)
		}
		return env
	}
	template := load("template")
	fork, err := template.Fork()
	if err != nil {
		t.Fatalf("fork: %v", err)
	}
	cold := load("cold")

	// Premise: the links are there, live, in both arms.
	for _, env := range []struct {
		name string
		env  *lisp.LEnv
	}{{"cold", cold}, {"fork", fork}} {
		for _, name := range views {
			v := env.env.Get(lisp.Symbol(name))
			if !v.IsCellView() {
				t.Fatalf("%s: %s carries no view link; the premise of this test is wrong", env.name, name)
			}
			if _, _, ok := v.CellView(); !ok {
				t.Fatalf("%s: %s's view link is not live", env.name, name)
			}
		}
		for _, name := range vectorViews {
			holder := env.env.Get(lisp.Symbol(name)).Cells[1]
			if _, _, ok := holder.CellView(); !ok {
				t.Fatalf("%s: %s's data holder carries no live view link", env.name, name)
			}
		}
	}
	// A fork-side link must name the fork's root, never the template's.
	if root, _, _ := fork.Get(lisp.Symbol("tail")).CellView(); root != fork.Get(lisp.Symbol("l")) || root == template.Get(lisp.Symbol("l")) {
		t.Fatalf("fork's view links to %p, want the fork's own list %p (template's is %p)", root, fork.Get(lisp.Symbol("l")), template.Get(lisp.Symbol("l")))
	}

	// The comparator: byte-identical post-load state.
	cfp, ffp := parityFingerprint(cold), parityFingerprint(fork)
	if !cfp.Equal(ffp) {
		t.Fatalf("a view at load scope fingerprints differently cold vs forked:\n%s", cfp.Diff(ffp))
	}

	// The oracle end to end: sorts through every root, observed through
	// every view, plus a view taken inside the transaction.
	RunParityCheck(t, ParityCheck{
		Program: program,
		Tx: [][]string{
			{"(stable-sort < l) tail", "tail2", "sl", "(set 'in (cdr l)) (stable-sort > l) in", "tail"},
			{"(stable-sort < v) r", "sv", "w", "(stable-sort > v) (list r sv w)"},
		},
		Repro: "TestForkParity_ViewLinkIsInvisibleToTheOracle",
	})
	RunParityCheck(t, ParityCheck{
		Program:    program,
		Tx:         [][]string{{"(stable-sort < l) tail"}, {"(stable-sort < v) r"}, {"tail2 sl sv w"}},
		Interleave: true,
		Hops:       2,
		Repro:      "TestForkParity_ViewLinkIsInvisibleToTheOracle/interleaved-two-hop",
	})
}
