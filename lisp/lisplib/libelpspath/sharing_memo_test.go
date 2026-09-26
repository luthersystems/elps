// Copyright © 2026 The ELPS authors

package libelpspath

import (
	"errors"
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// Go-side properties of the elpspath sharing memos (cycleState.valid and
// cycleState.copies).  The lisp-level regressions are in
// sharing_bomb_test.go.

func memoChain(n int, leaf *lisp.LVal) *lisp.LVal {
	for i := range n {
		leaf = lisp.SExpr([]*lisp.LVal{leaf})
		if i%7 == 3 {
			leaf = lisp.Quote(lisp.Quote(leaf)) // a quote wrapper on the path
		}
	}
	return leaf
}

func memoFiller(n int) *lisp.LVal {
	cells := make([]*lisp.LVal, n)
	for i := range cells {
		cells[i] = lisp.SExpr([]*lisp.LVal{lisp.Int(i)})
	}
	return lisp.SExpr(cells)
}

// memoDepthCase is (list filler x (chain k x)): x first reached one level
// down, then again k+1 levels down; a distinct second x when shared is false.
func memoDepthCase(k int, shared bool) *lisp.LVal {
	x := memoChain(500, lisp.Int(7))
	second := x
	if !shared {
		second = memoChain(500, lisp.Int(7))
	}
	return lisp.SExpr([]*lisp.LVal{memoFiller(sharedWalkBudget + 10), x, memoChain(k, second)})
}

// A copy memo hit answers a shared container without copying it again, so it
// must fail the value depth limit exactly where the re-copy would.  The
// oracle is the same value built as a tree.
func TestCopyMemoHitHonoursValueDepthLimit(t *testing.T) {
	const limit = 1024
	run := func(v *lisp.LVal) error {
		_, err := copyLVal(v, limit)
		return err
	}
	fails := func(k int) bool { return run(memoDepthCase(k, false)) != nil }
	lo, hi := 0, 1100
	if fails(lo) || !fails(hi) {
		t.Fatal("the range does not straddle the depth limit")
	}
	for hi-lo > 1 {
		if mid := (lo + hi) / 2; fails(mid) {
			hi = mid
		} else {
			lo = mid
		}
	}
	for k := hi - 4; k <= hi+4; k++ {
		tree, dag := run(memoDepthCase(k, false)), run(memoDepthCase(k, true))
		if (tree == nil) != (dag == nil) || (tree != nil && tree.Error() != dag.Error()) {
			t.Fatalf("k=%d (tree fails from %d): tree %v, shared %v", k, hi, tree, dag)
		}
	}
}

// A tree larger than the budget takes the memo's code path and must be copied
// exactly as before: equal, one distinct copy per container, none shared
// with the source.
func TestCopyMemoLeavesLargeTreesUnshared(t *testing.T) {
	in := memoFiller(3 * sharedWalkBudget)
	out, err := copyLVal(in, 0)
	if err != nil {
		t.Fatal(err)
	}
	if !lisp.True(out.Equal(in)) {
		t.Fatal("copy differs")
	}
	seen := map[*lisp.LVal]bool{}
	for i, c := range out.Cells {
		if seen[c] || c == in.Cells[i] {
			t.Fatalf("cell %d is shared", i)
		}
		seen[c] = true
	}
}

// The validator's memo skips only containers that PASSED: a cycle, and an
// invalid container, reached after the memo switched on -- including one
// under a container reached before it -- are still refused.
func TestValidMemoStillRefuses(t *testing.T) {
	x := memoChain(20, lisp.Int(1))
	cyc := lisp.SExpr([]*lisp.LVal{lisp.Int(1)})
	cyc.Cells = append(cyc.Cells, cyc) //nolint:gocritic // building a cycle on purpose
	multi := lisp.Array(lisp.SExpr([]*lisp.LVal{lisp.Int(1), lisp.Int(1)}), []*lisp.LVal{lisp.Int(0)})
	for name, bad := range map[string]*lisp.LVal{"cycle": cyc, "multi-dimensional array": multi} {
		t.Run(name, func(t *testing.T) {
			v := lisp.SExpr([]*lisp.LVal{memoFiller(3 * sharedWalkBudget), x, x, lisp.SExpr([]*lisp.LVal{x, bad})})
			err := okSimpleType(v)
			if err == nil {
				t.Fatal("accepted")
			}
			want := okSimpleType(lisp.SExpr([]*lisp.LVal{bad}))
			if err.Error() != want.Error() {
				t.Fatalf("got %v, want %v", err, want)
			}
			if name == "cycle" && !errors.Is(err, errCyclicValue) {
				t.Fatalf("got %v", err)
			}
		})
	}
}

// Both memos count cells, not just containers: a handful of wide containers
// is past the budget, so a wide list reached again is not re-walked.
func TestMemosCountCells(t *testing.T) {
	wide := make([]*lisp.LVal, sharedWalkBudget)
	for i := range wide {
		wide[i] = lisp.Int(i)
	}
	w := lisp.SExpr(wide)
	v := lisp.SExpr([]*lisp.LVal{w, w, w})

	var st cycleState
	if err := okSimpleTypeGuarded(v, newCycleGuard(&st)); err != nil {
		t.Fatal(err)
	}
	if st.valid == nil {
		t.Fatalf("validator: %d work over 4 containers did not switch the memo on", st.work)
	}

	op := newCopyOp(0)
	if _, err := copyLValOp(v, op); err != nil {
		t.Fatal(err)
	}
	if op.copies == nil {
		t.Fatalf("copy: %d work over 4 containers did not switch the memo on", op.work)
	}
}
