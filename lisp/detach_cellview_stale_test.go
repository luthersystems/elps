// Copyright © 2026 The ELPS authors

package lisp

import "testing"

// TestDetachDropsAStaleCellViewLinkSilently is the evidence for detach's
// refusal to be LOUD about a *LVal payload on an LSExpr that is not a live
// cell view.
//
// The tempting rule is "an LSExpr whose Native is a *LVal is a cell view;
// anything else there is an embedder scribbling on a field it does not own,
// so refuse it the way the default arm refuses every other unrecognised
// payload".  It cannot be implemented, because a STALE link is that same
// shape and is legitimate: the convention on cellsView says so in as many
// words ("A stale link is therefore never a correctness hazard, only a lost
// optimisation"), and this test reaches one from ordinary lisp -- (rest v)
// takes a view, (append! v 5) reallocates the root past its exact capacity,
// and the view's link now describes memory neither header holds.
//
// CellView, the validated resolver, answers false for BOTH -- a stale link
// and an embedder-set payload are the same bytes -- so a loud arm would
// reject this program.  Dropping the link silently is therefore the only
// available behaviour, not a lapse.  Reinstating the refusal must fail
// here.
func TestDetachDropsAStaleCellViewLinkSilently(t *testing.T) {
	env := newForkTestEnv(t)
	v := setGlobal(t, env, "v", "vector", ints(30, 10, 20)...)
	before := v.Cells[1].Cells
	r := setGlobal(t, env, "r", "rest", Symbol("v"))
	// (vector ...) allocates exact capacity, so this append! reallocates
	// the root and strands r's link.
	call(t, env, "append!", Symbol("v"), Int(5))
	if &v.Cells[1].Cells[0] == &before[0] {
		t.Skip("append! did not reallocate; nothing to pin")
	}
	if root, _ := r.cellsView(); root == nil {
		t.Fatal("premise: r carries no link at all")
	}
	if _, _, ok := r.CellView(); ok {
		t.Fatal("premise: r's link is still live, so it is not the stale shape")
	}

	for _, c := range []struct {
		name string
		run  func() (*LVal, error)
	}{
		{"Detach", func() (*LVal, error) { return Detach(r) }},
		{"deepCopy", func() (*LVal, error) {
			cp := call(t, env, "copy", Symbol("r"))
			return cp, nil
		}},
	} {
		t.Run(c.name, func(t *testing.T) {
			cp, err := c.run()
			if err != nil {
				t.Fatalf("a stale cell-view link was refused: %v", err)
			}
			if cp.Native != nil {
				t.Errorf("the stale link survived: Native = %T", cp.Native)
			}
			if got := intsOf(t, cp); !eqInts(got, []int{10, 20}) {
				t.Errorf("copy = %v, want [10 20]", got)
			}
		})
	}
}
