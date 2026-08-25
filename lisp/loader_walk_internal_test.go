// Copyright © 2026 The ELPS authors

package lisp

import (
	"errors"
	"testing"
)

// In-package tests for the admission walk's stream-level bookkeeping.
//
// They live inside package lisp because they PRE-SEED loaderWalk.distinct.
// The behaviour under test only begins past loaderWalkHardMaxNodes — 4.19M
// distinct nodes — and building a reader output that large costs ~500MB of
// *LVal plus a memo entry each.  Seeding the counter reaches the same code
// with a four-node tree, which is the difference between a test that pins the
// rule and a test nobody dares run.  Everything else about the walk is
// exercised from outside the package, through env.Load and a LoadCache.

// walkTree returns a two-level tree and its interior node.  Four distinct
// nodes: root, inner, and inner's two leaves.
func walkTree() (root, inner *LVal) {
	inner = SExpr([]*LVal{Int(2), Int(3)})
	root = SExpr([]*LVal{inner, Int(1)})
	return root, inner
}

// primedWalk returns a strict walk whose node budget is n nodes from being
// exhausted.
func primedWalk(n int64) *loaderWalk {
	w := newLoaderWalk(true)
	w.distinct = loaderWalkHardMaxNodes - n
	return w
}

// TestLoaderWalkUnwindsOnPathMarksOnError is round-four suspicious 1 at the
// mechanism.  check marks a node on-path before descending and clears the
// mark on the way back up — but it used to clear it only on the SUCCESS
// return, so a walk that abandoned partway left every ancestor of the
// abandonment point marked as "currently on the path" forever.  Since a
// revisit to an on-path node is by definition a cycle, the stale marks turned
// any later reference to those nodes into a cycle report.
func TestLoaderWalkUnwindsOnPathMarksOnError(t *testing.T) {
	root, inner := walkTree()
	// root, inner and inner's first leaf fit; inner's second leaf trips the
	// hard cap, abandoning the walk with root and inner both marked.
	w := primedWalk(3)
	if _, err := w.check(root, 0); !errors.Is(err, errReaderTreeTooLarge) {
		t.Fatalf("expected the hard cap to stop this walk, got %v", err)
	}
	for v, info := range w.sizes {
		if info.onPath {
			t.Errorf("node %p left marked on-path by an abandoned walk", v)
		}
	}
	if _, marked := w.sizes[root]; marked {
		t.Error("root kept a memo entry although its sizes were never computed")
	}
	if _, marked := w.sizes[inner]; marked {
		t.Error("inner kept a memo entry although its sizes were never computed")
	}
	// The consequence, stated directly: reaching either node again reports
	// the budget, not a cycle it does not have.
	for name, v := range map[string]*LVal{"root": root, "inner": inner} {
		if _, err := w.check(v, 0); !errors.Is(err, errReaderTreeTooLarge) {
			t.Errorf("revisiting %s after an abandoned walk: got %v, want the budget sentinel", name, err)
		}
	}
}

// The same finding end to end through the stream admission, which is where it
// mattered: the hard cap is the ONE error newProgramAdmitted continues past,
// so a stale mark from the abandoned expression is read by the expressions
// after it.  A deduping Reader that returns the same big expression twice
// loads fine with no cache installed; with a cache it reported
// errReaderTreeUnbounded, which readCached turns into a hard load failure
// rather than an uncached fall-back.
func TestLoaderWalkHardCapDoesNotPoisonLaterExpressions(t *testing.T) {
	root, inner := walkTree()
	for name, exprs := range map[string][]*LVal{
		"same expression twice":  {root, root},
		"a marked interior node": {root, inner},
	} {
		t.Run(name, func(t *testing.T) {
			_, err := newProgramAdmitted(exprs, primedWalk(3))
			if errors.Is(err, errReaderTreeUnbounded) {
				t.Fatal("a stream with no cycle in it was refused as unbounded")
			}
			if !errors.Is(err, errReaderTreeTooLarge) {
				t.Fatalf("want the budget sentinel (an uncached fall-back), got %v", err)
			}
		})
	}
}

// TestLoaderWalkHardCapDegradesCycleDetection pins the chosen answer to
// round-four minor 1 rather than fixing it, and the comment on
// loaderWalkHardMaxNodes says why.
//
// "A cycle outranks the budget" holds for the SOFT budget —
// TestLoadCacheCycleOutranksBudgetAcrossStream pins that — and it stops at
// the hard cap: past 4.19M distinct nodes the walk refuses every further node
// without inspecting it, so a cycle behind such an expression is not seen and
// the stream verdict is TooLarge.
//
// That is bounded rather than lucky.  TooLarge is the FALL-BACK sentinel, so
// the load runs uncached — which is exactly what it does with no cache
// installed, and with no cache installed nothing inspects reader output for
// cycles at all.  The cycle then meets the evaluator's own nesting cap, the
// same backstop that has always caught it.  Nothing is stored either way.
func TestLoaderWalkHardCapDegradesCycleDetection(t *testing.T) {
	cycInner := SExpr([]*LVal{Symbol("progn")})
	cyc := SExpr([]*LVal{Symbol("progn"), cycInner})
	cycInner.Cells = append(cycInner.Cells, cyc)

	big, _ := walkTree()
	_, err := newProgramAdmitted([]*LVal{big, cyc}, primedWalk(3))
	if !errors.Is(err, errReaderTreeTooLarge) {
		t.Fatalf("want the budget sentinel, got %v", err)
	}
	// The same cycle, with the budget intact, is still refused as a cycle.
	if _, err := newProgramAdmitted([]*LVal{cyc}, newLoaderWalk(true)); !errors.Is(err, errReaderTreeUnbounded) {
		t.Fatalf("a cycle under the budget must still be refused as one, got %v", err)
	}
}

// TestLoaderWalkQuoteDiscount pins the two counts the round-four blocker fix
// separated, on the four shapes that must differ.  raw is what the walks
// downstream of admission cost (Copy, firstUnsealed, the fingerprint); eval is
// what the EVALUATOR costs, and only eval feeds the Unbounded hard failure.
func TestLoaderWalkQuoteDiscount(t *testing.T) {
	// A depth-10 doubling DAG: 11 distinct nodes, 2^11-1 = 2047 unfolded.
	dag := func() *LVal {
		node := Int(7)
		for range 10 {
			node = SExpr([]*LVal{node, node})
		}
		return node
	}
	const dagRaw = 2047

	tests := []struct {
		name          string
		build         func() *LVal
		wantRaw, want int64
	}{
		{"bare", func() *LVal { return dag() }, dagRaw, dagRaw},
		{"quote form", func() *LVal {
			return SExpr([]*LVal{Symbol("quote"), dag()})
		}, dagRaw + 2, 3},
		{"qualified quote form", func() *LVal {
			return SExpr([]*LVal{Symbol("lisp:quote"), dag()})
		}, dagRaw + 2, 3},
		{"quoted flag", func() *LVal { return Quote(dag()) }, dagRaw, 1},
		{"quasiquote form", func() *LVal {
			return SExpr([]*LVal{Symbol("quasiquote"), dag()})
		}, dagRaw + 2, dagRaw + 2},
		{"quasiquote over a quoted payload", func() *LVal {
			// No discount survives beneath a quasiquote: findAndUnquote
			// descends through quote levels, so the payload is charged its
			// quote-blind size.
			return SExpr([]*LVal{Symbol("quasiquote"), Quote(dag())})
		}, dagRaw + 2, dagRaw + 2},
		{"a quote the head symbol does not name", func() *LVal {
			return SExpr([]*LVal{Symbol("list"), dag()})
		}, dagRaw + 2, dagRaw + 2},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			w := newLoaderWalk(true)
			info, err := w.check(tt.build(), 0)
			if err != nil {
				t.Fatalf("check: %v", err)
			}
			if info.raw != tt.wantRaw {
				t.Errorf("raw = %d, want %d", info.raw, tt.wantRaw)
			}
			if info.eval != tt.want {
				t.Errorf("eval = %d, want %d", info.eval, tt.want)
			}
		})
	}
}

// TestLoaderWalkMemoRecordsHeight is round-four minor 2 at the mechanism: the
// memo entry has to carry enough for a HIT to answer the depth cap, which
// means the subtree's height and not only its size.
func TestLoaderWalkMemoRecordsHeight(t *testing.T) {
	leaf := Int(1)
	mid := SExpr([]*LVal{leaf})
	top := SExpr([]*LVal{mid})

	w := newLoaderWalk(true)
	if _, err := w.check(top, 0); err != nil {
		t.Fatalf("check: %v", err)
	}
	for name, want := range map[string]struct {
		v *LVal
		h int64
	}{
		"leaf": {leaf, 1}, "mid": {mid, 2}, "top": {top, 3},
	} {
		if got := w.sizes[want.v].height; got != want.h {
			t.Errorf("%s height = %d, want %d", name, got, want.h)
		}
	}
	// A hit at a depth that would put the subtree's own interior past the cap
	// is refused, without the subtree being walked again.
	if _, err := w.check(top, loaderWalkMaxDepth-1); !errors.Is(err, errReaderTreeUnbounded) {
		t.Fatalf("a memo hit smuggled a 3-deep subtree past the depth cap: %v", err)
	}
	// And one that fits is not.
	if _, err := w.check(top, loaderWalkMaxDepth-3); err != nil {
		t.Fatalf("a memo hit that fits under the cap was refused: %v", err)
	}
}
