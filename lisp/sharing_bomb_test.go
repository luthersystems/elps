// Copyright © 2026 The ELPS authors

package lisp

import (
	"context"
	"fmt"
	"math"
	"strconv"
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
	// Past equalShallowBudget, so the iterative pass and its memo run too.
	build := func(last int) *LVal {
		cells := make([]*LVal, equalShallowBudget/2)
		for i := range cells {
			if i%64 != 0 {
				cells[i] = SExpr([]*LVal{Int(i)})
				continue
			}
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
			if len(c.Cells) < 2 {
				continue
			}
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

// equal?'s pair memo answers only pairs whose walk stays above
// cycleGuardDepth.  Deeper sharing is left to the depth-tracked seen set and
// the strict restart, exactly as before, so a value that shares a pair both
// shallow and deep past the value depth limit compares as it always did:
// the restart skips the repeated pair and never reaches the limit.  These
// results are origin/main's.
func TestEqualMemoLeavesDeepSharingToStrictRestart(t *testing.T) {
	rt := StandardRuntime()
	rt.MaxValueDepth = 1024
	for _, k := range []int{400, 523, 524, 530, 600} {
		for _, quoted := range []bool{false, true} {
			v := sharedDepthCase(k, true, quoted)
			if got := v.EqualWithRuntime(v.Copy(), rt); !True(got) {
				t.Fatalf("k=%d quoted=%v: %v, want true", k, quoted, got)
			}
		}
	}
	p := chain(1000, Int(7))
	v := SExpr([]*LVal{fillerTree(sharedWalkBudget + 10), p, chain(60, p)})
	if got := v.EqualWithRuntime(v.Copy(), rt); !True(got) {
		t.Fatalf("%v, want true", got)
	}
	// A pair the memo records near the root, reached again right at the
	// depth limit: the re-walk fails there, so the memo must not answer it.
	// The oracle is the same value with a distinct second copy.
	wide := func() *LVal {
		cells := make([]*LVal, 2*equalMemoGrain)
		for i := range cells {
			cells[i] = Int(i)
		}
		return SExpr([]*LVal{SExpr(cells)}) // height 1: its re-walk goes one level deeper
	}
	for k := 1018; k <= 1026; k++ {
		q := wide()
		dag := SExpr([]*LVal{fillerTree(sharedWalkBudget + 10), q, chain(k, q)})
		tree := SExpr([]*LVal{fillerTree(sharedWalkBudget + 10), wide(), chain(k, wide())})
		d, tr := dag.EqualWithRuntime(dag.Copy(), rt), tree.EqualWithRuntime(tree.Copy(), rt)
		if d.Type != tr.Type || d.String() != tr.String() {
			t.Fatalf("k=%d: shared %v, tree %v", k, d, tr)
		}
	}
}

// A sharing bomb is linear wherever it sits: above cycleGuardDepth the memo
// answers repeats, below it the strict restart does.
func TestEqualSharingBombAtAnyDepth(t *testing.T) {
	for _, at := range []int{0, 10, 30, 50, 63, 64, 70, 200} {
		a := SExpr([]*LVal{fillerTree(sharedWalkBudget + 10), chain(at, bomb(40, Int(1)))})
		b := SExpr([]*LVal{fillerTree(sharedWalkBudget + 10), chain(at, bomb(40, Int(1)))})
		var got *LVal
		testdeadline.Watch("equal? over a bomb at depth "+strconv.Itoa(at), 20*time.Second, 1<<30, func() { got = a.Equal(b) })
		if !True(got) {
			t.Fatalf("at=%d: %v", at, got)
		}
	}
}

// The memo's cycleGuardDepth boundary, pinned to origin/main.  A pair p of
// height h is finished near the root, reached again with its top at depth
// dp, and a third time with its bottom exactly at the value depth limit.  On
// main, the second visit records p's pairs in seen when any of them lies at
// or below cycleGuardDepth (dp+h >= 65, given the filler's level), the third
// visit repeats one, and the strict restart skips it: true.  Otherwise the
// third visit walks p and fails the limit.  The memo must answer the second
// visit only where main's re-walk would have recorded nothing, which is
// what makes every row here identical to main -- and why heights must be
// exact.
func TestEqualMemoDepthBoundaryMatchesMain(t *testing.T) {
	rt := StandardRuntime()
	rt.MaxValueDepth = 1024
	wide := func() *LVal {
		cells := make([]*LVal, 2*equalMemoGrain)
		for i := range cells {
			cells[i] = Int(i)
		}
		return SExpr(cells)
	}
	for _, h := range []int{3, 5} {
		for dp := 54; dp <= 66; dp++ {
			p := chain(h, wide())
			v := SExpr([]*LVal{fillerTree(sharedWalkBudget + 10), p, chain(dp-1, p), chain(1023-h, p)})
			got := v.EqualWithRuntime(v.Copy(), rt)
			if wantErr := dp+h <= 64; (got.Type == LError) != wantErr || (!wantErr && !True(got)) {
				t.Fatalf("h=%d dp=%d: got %v, want error=%v (origin/main)", h, dp, got, wantErr)
			}
		}
	}
}

// Two values shared DIFFERENTLY can have |a|x|b| distinct pairs, which no
// memo removes.  EqualWithEnv polls the context as it goes -- in the strict
// restart too, which sharing below cycleGuardDepth sends it to -- so a
// deadline still stops it.
func TestEqualWithEnvPollsDifferentlySharedValues(t *testing.T) {
	build := func(n, m, depth int) *LVal {
		level := make([]*LVal, n)
		for j := range level {
			level[j] = Int(1)
		}
		for range depth {
			next := make([]*LVal, n)
			for j := range next {
				next[j] = SExpr([]*LVal{level[(j*m)%n], level[(j*m+1)%n]})
			}
			level = next
		}
		return SExpr(level)
	}
	for _, depth := range []int{40, 100} {
		a, b := build(200, 2, depth), build(200, 3, depth)
		env := NewEnv(nil)
		ctx, cancel := context.WithTimeout(context.Background(), 50*time.Millisecond)
		env.evalCtx = ctx
		var got *LVal
		testdeadline.Watch("differently shared equal? under a deadline", 20*time.Second, 1<<30, func() { got = a.EqualWithEnv(b, env) })
		cancel()
		if got.Type != LError || got.Str != CondContextCancelled {
			t.Fatalf("depth %d: got %v, want the context to stop it", depth, got)
		}
	}
}

// GoValue memoises past the budget too: a shared container converts once,
// and the Go value shares the conversion where the LVal shared the
// container.
func TestGoValueKeepsSharing(t *testing.T) {
	var got any
	v := bomb(40, Int(1))
	testdeadline.Watch("GoValue over a 40-level sharing bomb", 20*time.Second, 1<<30, func() { got = GoValue(v) })
	for i := range 40 - 13 {
		s, ok := got.([]any)
		if !ok || len(s) != 2 {
			t.Fatalf("level %d: got %T", i, got)
		}
		a, b := s[0].([]any), s[1].([]any)
		if &a[0] != &b[0] {
			t.Fatalf("level %d: the conversion unshared the value", i)
		}
		got = s[0]
	}
}

// A GoValue memo hit fails the value depth limit exactly where
// re-converting would; the oracle is the same value as a tree.
func TestGoValueMemoHitHonoursValueDepthLimit(t *testing.T) {
	rt := StandardRuntime()
	rt.MaxValueDepth = 1024
	for _, quoted := range []bool{false, true} {
		t.Run(fmt.Sprintf("quoted=%v", quoted), func(t *testing.T) {
			checkMemoDepthLimit(t, quoted, func(v *LVal) *LVal {
				if err, ok := GoValueWithRuntime(rt, v).(error); ok {
					return Error(err)
				}
				return Nil()
			}, nil)
		})
	}
}

// A tree larger than the budget converts exactly as before: every Go
// container distinct.
func TestGoValueLargeTreeUnshared(t *testing.T) {
	cells := make([]*LVal, 3*sharedWalkBudget)
	for i := range cells {
		cells[i] = SExpr([]*LVal{Int(i)})
	}
	got, ok := GoValue(SExpr(cells)).([]any)
	if !ok || len(got) != len(cells) {
		t.Fatalf("got %T", got)
	}
	seen := map[*any]bool{}
	for i, c := range got {
		s := c.([]any)
		if seen[&s[0]] || s[0] != i {
			t.Fatalf("element %d shared or wrong: %v", i, s)
		}
		seen[&s[0]] = true
	}
}
