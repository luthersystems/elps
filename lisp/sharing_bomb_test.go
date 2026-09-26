// Copyright © 2026 The ELPS authors

package lisp

import (
	"testing"

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

// sharedDepthCase builds (list filler x (chain k x)), where x is a 500-level
// chain, first reached one level down and then again k+1 levels down.  When
// shared is false the second x is a distinct copy, so the value is a tree
// and no walk can take a memo hit on it.  With the filler first, a memoising
// walk has switched its memo on before it reaches either x.
func sharedDepthCase(k int, shared bool) *LVal {
	x := chain(500, Int(7))
	second := x
	if !shared {
		second = chain(500, Int(7))
	}
	return SExpr([]*LVal{fillerTree(sharedWalkBudget + 10), x, chain(k, second)})
}

// A memo hit answers a shared container without walking it, so it must fail
// exactly where re-walking it would: at the value depth limit.  The oracle is
// the same value built as a tree, which the memo can never hit.
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
		t.Run(w.name, func(t *testing.T) {
			flips := 0
			var prev bool
			for k := 500; k <= 540; k++ {
				tree := w.run(sharedDepthCase(k, false))
				dag := w.run(sharedDepthCase(k, true))
				treeErr, dagErr := tree.Type == LError, dag.Type == LError
				if treeErr != dagErr {
					t.Fatalf("k=%d: tree walk error=%v (%v), shared walk error=%v (%v)", k, treeErr, tree, dagErr, dag)
				}
				if treeErr && tree.Str != dag.Str {
					t.Fatalf("k=%d: tree error %q, shared error %q", k, tree.Str, dag.Str)
				}
				if k > 500 && treeErr != prev {
					flips++
				}
				prev = treeErr
				if !dagErr {
					// The shared x is walked once: its counterpart is the
					// same node in both places.
					second := dag.Cells[2]
					for range k {
						second = second.Cells[0]
					}
					if dag.Cells[1] != second {
						t.Fatalf("k=%d: the shared chain was rebuilt per path", k)
					}
				}
			}
			if flips != 1 {
				t.Fatalf("the depth limit flipped %d times over the range, want once: the range does not straddle it", flips)
			}
		})
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
