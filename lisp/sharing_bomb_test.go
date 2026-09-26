// Copyright © 2026 The ELPS authors

package lisp

import (
	"fmt"
	"math"
	"testing"
	"time"

	"github.com/luthersystems/elps/internal/testdeadline"

	"github.com/luthersystems/elps/parser/token"
)

// Go-side properties of the sharing memos (lisp/sharing.go).  The lisp-level
// regressions are in sharing_bomb_eval_test.go.

// chain returns n nested fresh one-cell lists around leaf.
func chain(n int, leaf *LVal) *LVal {
	for range n {
		leaf = SExpr([]*LVal{leaf})
	}
	return leaf
}

// fillerTree returns a fresh tree of n+1 distinct containers: enough to push
// a walk past sharedWalkBudget before it reaches what follows it.
func fillerTree(n int) *LVal {
	cells := make([]*LVal, n)
	for i := range cells {
		cells[i] = SExpr([]*LVal{Int(i)})
	}
	return SExpr(cells)
}

// countContainers returns the number of distinct containers reachable from
// v and the number of container visits a tree walk would make.
func countContainers(v *LVal) (distinct, paths int) {
	seen := map[*LVal]bool{}
	var walk func(*LVal)
	walk = func(v *LVal) {
		if v == nil || len(v.Cells) == 0 {
			return
		}
		paths++
		if !seen[v] {
			seen[v] = true
			distinct++
		}
		for _, c := range v.Cells {
			walk(c)
		}
	}
	walk(v)
	return distinct, paths
}

// chainQuoted is chain with quote wrappers mixed in: every third level also
// carries the quoted flag, and every fifth is additionally wrapped in an
// LQuote node, so the quasiquote walker's quote-edge arithmetic is on the
// shared path, down to the leaf.
func chainQuoted(n int, leaf *LVal) *LVal {
	leaf = Quote(Quote(leaf)) // a wrapped leaf at the bottom of the path
	for i := range n {
		leaf = SExpr([]*LVal{leaf})
		switch {
		case i%5 == 2:
			leaf = Quote(Quote(leaf))
		case i%3 == 1:
			leaf = Quote(leaf)
		}
	}
	return leaf
}

// sharedDepthCase builds (list filler x (chain k x)), where x is a 500-level
// chain, first reached one level down and then again k+1 levels down.  When
// shared is false the second x is a distinct copy, so the value is a tree
// and no walk can take a memo hit on it.  With the filler first, a memoising
// walk has switched its memo on before it reaches either x.
func sharedDepthCase(k int, shared, quoted bool) *LVal {
	mk := chain
	if quoted {
		mk = chainQuoted
	}
	x := mk(500, Int(7))
	second := x
	if !shared {
		second = mk(500, Int(7))
	}
	return SExpr([]*LVal{fillerTree(sharedWalkBudget + 10), x, mk(k, second)})
}

// checkMemoDepthLimit is the oracle for a memo hit and the value depth
// limit: a memo hit answers a shared container without walking it, so it
// must fail exactly where re-walking it would.  It finds the smallest k at
// which run fails on the TREE (the value built with a distinct second x,
// which no memo can hit), then requires the DAG to agree -- same error text,
// or both succeed -- at every k around it.  check inspects a DAG result
// that succeeded.
func checkMemoDepthLimit(t *testing.T, quoted bool, run func(*LVal) *LVal, check func(k int, dag *LVal)) {
	t.Helper()
	fails := func(k int) bool { return run(sharedDepthCase(k, false, quoted)).Type == LError }
	lo, hi := 0, 1100
	if fails(lo) || !fails(hi) {
		t.Fatalf("the range [%d, %d] does not straddle the depth limit", lo, hi)
	}
	for hi-lo > 1 {
		if mid := (lo + hi) / 2; fails(mid) {
			hi = mid
		} else {
			lo = mid
		}
	}
	for k := hi - 4; k <= hi+4; k++ {
		tree := run(sharedDepthCase(k, false, quoted))
		dag := run(sharedDepthCase(k, true, quoted))
		treeErr, dagErr := tree.Type == LError, dag.Type == LError
		if treeErr != dagErr || (treeErr && tree.Str != dag.Str) {
			t.Fatalf("k=%d (tree fails from %d): tree %v, shared %v", k, hi, tree, dag)
		}
		if !dagErr && check != nil {
			check(k, dag)
		}
	}
}

func TestSharingMemoHitHonoursValueDepthLimit(t *testing.T) {
	const limit = 1024
	type walker struct {
		name string
		run  func(*LVal) *LVal
	}
	walkers := []walker{
		{"stamp", func(v *LVal) *LVal {
			rt := StandardRuntime()
			rt.MaxValueDepth = limit
			return stampMacroExpansion(v, &token.Location{File: "depth", Pos: 1}, nil, rt)
		}},
		{"quasiquote", func(v *LVal) *LVal {
			env := initSafetyTestEnv(t)
			env.Runtime.MaxValueDepth = limit
			return findAndUnquote(env, v, 0)
		}},
	}
	for _, w := range walkers {
		for _, quoted := range []bool{false, true} {
			t.Run(fmt.Sprintf("%s/quoted=%v", w.name, quoted), func(t *testing.T) {
				checkMemoDepthLimit(t, quoted, w.run, func(k int, dag *LVal) {
					if quoted {
						return // the wrappers make the path irregular; the error check above is the point
					}
					// The shared x is walked once: its counterpart is the
					// same node in both places.
					second := dag.Cells[2]
					for range k {
						second = second.Cells[0]
					}
					if dag.Cells[1] != second {
						t.Fatalf("k=%d: the shared chain was rebuilt per path", k)
					}
				})
			})
		}
	}
}

// A tree larger than the budget still takes the memo's code path, and must
// come out exactly as the tree walk builds it: equal, with one distinct
// output container per input container and nothing shared that was not.
func TestSharingMemoLeavesLargeTreesUnshared(t *testing.T) {
	build := func() *LVal {
		cells := make([]*LVal, 3*sharedWalkBudget)
		for i := range cells {
			cells[i] = SExpr([]*LVal{Int(i), SExpr([]*LVal{Symbol("a")})})
		}
		return SExpr(cells)
	}
	stamp := func(v *LVal) *LVal {
		return stampMacroExpansion(v, &token.Location{File: "tree", Pos: 1}, nil, StandardRuntime())
	}
	quasi := func(v *LVal) *LVal { return findAndUnquote(initSafetyTestEnv(t), v, 0) }
	for name, run := range map[string]func(*LVal) *LVal{"stamp": stamp, "quasiquote": quasi} {
		t.Run(name, func(t *testing.T) {
			in := build()
			out := run(in)
			if out.Type == LError {
				t.Fatal(out)
			}
			if eq := out.Equal(in); !True(eq) {
				t.Fatal("output differs from input")
			}
			inD, inP := countContainers(in)
			outD, outP := countContainers(out)
			if inD != outD || inP != outP || outD != outP {
				t.Fatalf("input %d distinct/%d paths, output %d distinct/%d paths", inD, inP, outD, outP)
			}
		})
	}
}

// bomb returns a depth-level sharing chain over leaf.
func bomb(depth int, leaf *LVal) *LVal {
	for range depth {
		leaf = SExpr([]*LVal{leaf, leaf})
	}
	return leaf
}

// NaN is not equal to itself, and a memoised pair must not change that: the
// memo records pairs of CONTAINERS as they are entered, so a leaf pair is
// always compared, even when both sides are the same node.
func TestEqualPairMemoKeepsNaNUnequal(t *testing.T) {
	nan := Float(math.NaN())
	v := bomb(40, nan)
	if got := v.Equal(v); got.Type == LError || True(got) {
		t.Fatalf("a value holding NaN compared equal to itself: %v", got)
	}
	// Past the budget, with the NaN at the far end of a wide filler.
	w := SExpr([]*LVal{fillerTree(3 * sharedWalkBudget), bomb(40, Int(1)), nan})
	if got := w.Equal(w); got.Type == LError || True(got) {
		t.Fatalf("NaN after the memo switched on compared equal: %v", got)
	}
}

// Trees larger than the budget take the memo's path through both passes
// and must answer exactly as before: equal, and unequal at the last leaf,
// with string and symbol map keys compared by name.
func TestEqualLargeTreesUnchanged(t *testing.T) {
	build := func(last int) *LVal {
		cells := make([]*LVal, 3*sharedWalkBudget)
		for i := range cells {
			m := SortedMap()
			m.MapSetLVal(String("k"), SExpr([]*LVal{Int(i)}))
			cells[i] = SExpr([]*LVal{Int(i), m})
		}
		cells[len(cells)-1] = SExpr([]*LVal{Int(last)})
		return SExpr(cells)
	}
	symKeyed := func(last int) *LVal {
		v := build(last)
		for _, c := range v.Cells[:len(v.Cells)-1] {
			m := SortedMap()
			m.MapSetLVal(Symbol("k"), c.Cells[1].MapGet(String("k")))
			c.Cells[1] = m
		}
		return v
	}
	if got := build(1).Equal(build(1)); !True(got) {
		t.Fatalf("equal trees: %v", got)
	}
	if got := build(1).Equal(symKeyed(1)); !True(got) {
		t.Fatalf("string- and symbol-keyed trees: %v", got)
	}
	if got := build(1).Equal(build(2)); got.Type == LError || True(got) {
		t.Fatalf("trees differing at the last leaf: %v", got)
	}
}

// Past the budget, a pair repeated through sharing is skipped, so a DAG
// compares in time linear in its distinct pairs.  The step budget is not
// involved: equal? is one step.
func TestEqualSharedValuesAreLinear(t *testing.T) {
	a, b := bomb(60, Int(1)), bomb(60, Int(1))
	var got *LVal
	testdeadline.Watch("equal? over two 60-level sharing chains", 20*time.Second, 1<<30, func() {
		got = a.Equal(b)
	})
	if !True(got) {
		t.Fatalf("got %v", got)
	}
}

// equal?'s pair memo answers a repeated pair without walking it, so it must
// fail the value depth limit exactly where the re-walk would.  Both sides
// share their x, so the second (x, x) pair is a memo hit; the oracle is the
// same comparison over two trees.
func TestEqualMemoHitHonoursValueDepthLimit(t *testing.T) {
	rt := StandardRuntime()
	rt.MaxValueDepth = 1024
	for _, quoted := range []bool{false, true} {
		t.Run(fmt.Sprintf("quoted=%v", quoted), func(t *testing.T) {
			// run compares v with an identically built, separately
			// allocated value: the shape of v, found from its cells.
			run := func(v *LVal) *LVal {
				return v.EqualWithRuntime(v.Copy(), rt)
			}
			checkMemoDepthLimit(t, quoted, run, func(k int, dag *LVal) {
				if !True(dag) {
					t.Fatalf("k=%d: equal values compared unequal: %v", k, dag)
				}
			})
		})
	}
}
