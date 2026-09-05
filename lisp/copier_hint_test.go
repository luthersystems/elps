// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"fmt"
	"strings"
	"testing"

	"github.com/luthersystems/elps/elpstest"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
)

// The controls for the copier's size hint (lisp/copier.go, copyWithHint):
// a hinted walk is Copy with its header memo reserved up front, and nothing
// else.  Same output tree, same aliasing, same cycle behaviour; the hint
// decides only where the memo lives.

// copierHints are the hints every hinted test runs under: one inside the
// inline array (the hint is ignored and the walk IS Copy), one just past it
// (a map from the start, far smaller than any real tree), and one far past
// any fixture here (the map is reserved larger than the walk needs).
var copierHints = []int{1, lisp.CopierSmallMemo + 1, 1 << 16}

// copierHintedWalker is copierWalker with the copy made through the hint.
func copierHintedWalker(n int) elpstest.Walker {
	w := copierWalker()
	w.Name = fmt.Sprintf("LVal.copyWithHint(%d)", n)
	w.Copy = func(_ *lisp.LEnv, v *lisp.LVal) (*lisp.LVal, error) { return lisp.CopyWithHint(v, n), nil }
	return w
}

// TestCopyWithHintMeetsTheAliasGuard drives the hinted walk through the same
// CheckWalker TestCopyMeetsTheAliasGuard drives Copy through, under every
// hint in copierHints: the same values and sharing as the source, the same
// mutable payloads, every probe write seen on the same sites.
func TestCopyWithHintMeetsTheAliasGuard(t *testing.T) {
	t.Parallel()
	for _, n := range copierHints {
		t.Run(fmt.Sprintf("hint=%d", n), func(t *testing.T) {
			t.Parallel()
			got, err := elpstest.CheckWalker(copierHintedWalker(n), elpstest.AliasCheck{
				NewEnv: func() (*lisp.LEnv, error) {
					env := lisp.NewEnv(nil)
					env.Runtime.Reader = parser.NewReader()
					if rc := lisp.InitializeUserEnv(env); rc.Type == lisp.LError {
						return nil, lisp.GoError(rc)
					}
					if rc := env.InPackage(lisp.String(lisp.DefaultUserPackage)); rc.Type == lisp.LError {
						return nil, lisp.GoError(rc)
					}
					return env, nil
				},
				Program: copierProgram,
			})
			if err != nil {
				t.Fatalf("harness error: %v", err)
			}
			for _, wit := range got {
				t.Errorf("%s", wit)
			}
		})
	}
}

// sameCopy walks a and b in lockstep and reports the first place they differ:
// a field, a position, or the alias graph (pairs maps a's headers to b's, so
// a header reached twice in a must be reached twice in b at the same
// places, and a cycle in a must close at the same node in b).
func sameCopy(a, b *lisp.LVal, pairs map[*lisp.LVal]*lisp.LVal) error {
	if (a == nil) != (b == nil) {
		return fmt.Errorf("nil mismatch: %v vs %v", a, b)
	}
	if a == nil {
		return nil
	}
	if want, seen := pairs[a]; seen {
		if want != b {
			return fmt.Errorf("alias graph differs at %v: %p vs %p", a, want, b)
		}
		return nil
	}
	pairs[a] = b
	if a.Type != b.Type || a.Str != b.Str || a.Int != b.Int || a.Float != b.Float ||
		a.IsQuoted() != b.IsQuoted() || len(a.Cells) != len(b.Cells) {
		return fmt.Errorf("node differs: %v vs %v", a, b)
	}
	la, oka := a.Source()
	lb, okb := b.Source()
	if oka != okb || la != lb {
		return fmt.Errorf("position differs at %v: %v vs %v", a, la, lb)
	}
	if lisp.IsSealedForTest(a) != lisp.IsSealedForTest(b) {
		return fmt.Errorf("seal differs at %v", a)
	}
	for i := range a.Cells {
		if err := sameCopy(a.Cells[i], b.Cells[i], pairs); err != nil {
			return err
		}
	}
	return nil
}

// TestCopyWithHintProducesCopysTree: on a parsed, sealed, library-shaped
// source -- the tree TextLoader copies per load -- every hinted copy is
// node-for-node the tree Copy produces: same fields, same positions, same
// alias graph, and, as Copy's, unsealed and disjoint from the source.
func TestCopyWithHintProducesCopysTree(t *testing.T) {
	t.Parallel()
	exprs, err := parser.NewReader().Read("hint.lisp", strings.NewReader(synthLoaderSource(8*1024)))
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	if len(exprs) < 2 {
		t.Fatalf("fixture parsed to %d expressions, want several", len(exprs))
	}
	for _, n := range copierHints {
		for i, expr := range exprs {
			want := expr.Copy()
			got := lisp.CopyWithHint(expr, n)
			if got == want || got == expr {
				t.Fatalf("hint %d, expr %d: the hinted copy is not a fresh tree", n, i)
			}
			if err := sameCopy(want, got, map[*lisp.LVal]*lisp.LVal{}); err != nil {
				t.Errorf("hint %d, expr %d: %v", n, i, err)
			}
			// Disjoint from the source: no header of the copy is a header
			// of the sealed tree.
			src := map[*lisp.LVal]bool{}
			copierReachable(expr, src)
			cp := map[*lisp.LVal]bool{}
			copierReachable(got, cp)
			for h := range cp {
				if src[h] {
					t.Errorf("hint %d, expr %d: the copy holds the source's node %v", n, i, h)
					break
				}
			}
		}
	}
}

// TestCopyWithHintSmallerThanTheTreeIsHarmless is
// TestCopyMemoSpillsPastTheInlineArray under hints that do not fit the
// walk: past the inline array the map grows from the hint as it would from
// empty, so a subtree shared past the reserved size is still copied once
// and a cycle still closes onto the copy.  The hint that fits the inline
// array is the control: it is ignored, and the walk is Copy's.
func TestCopyWithHintSmallerThanTheTreeIsHarmless(t *testing.T) {
	t.Parallel()
	shared := lisp.QExpr([]*lisp.LVal{lisp.Int(0)})
	cells := make([]*lisp.LVal, 0, 260)
	cells = append(cells, shared)
	for i := range 256 {
		cells = append(cells, lisp.Int(i))
	}
	cells = append(cells, shared)
	big := lisp.QExpr(cells)
	big.Cells = append(big.Cells, big)
	last := len(big.Cells) - 1
	for _, n := range copierHints {
		cp := lisp.CopyWithHint(big, n)
		if cp.Cells[0] != cp.Cells[last-1] {
			t.Errorf("hint %d: a subtree shared past the reserved memo was copied twice", n)
		}
		if cp.Cells[0] == shared {
			t.Errorf("hint %d: the copy holds the source's subtree", n)
		}
		if cp.Cells[last] != cp {
			t.Errorf("hint %d: the cycle closes onto %p, want the copy %p", n, cp.Cells[last], cp)
		}
	}
}

// TestCopyWithHintReservesTheMemoOnce is the property the hint exists for,
// at Copy's own boundary: on a tree well past the inline memo, a walk
// hinted at the tree's size allocates strictly fewer times than the
// unhinted walk, because the map is built once at its final size rather
// than grown through every doubling.  The two trees are the same (the test
// above), so the difference is the memo's growth and nothing else.
func TestCopyWithHintReservesTheMemoOnce(t *testing.T) {
	cells := make([]*lisp.LVal, 0, 512)
	for i := range 512 {
		cells = append(cells, lisp.Int(i))
	}
	list := lisp.QExpr(cells)
	nodes := len(cells) + 1
	unhinted := testing.AllocsPerRun(50, func() { list.Copy() })
	hinted := testing.AllocsPerRun(50, func() { lisp.CopyWithHint(list, nodes) })
	if hinted >= unhinted {
		t.Errorf("copying %d nodes allocated %v times unhinted and %v times hinted at %d; the hint must spare the memo's growth", nodes, unhinted, hinted, nodes)
	}
	// And an ignored hint costs what Copy costs: the inline array, no map.
	small := lisp.QExpr([]*lisp.LVal{lisp.Int(1), lisp.Int(2), lisp.Int(3)})
	if n := testing.AllocsPerRun(200, func() { lisp.CopyWithHint(small, 1) }); n != 5 {
		t.Errorf("copying a 3-element list under an inline-sized hint allocated %v times, want 5", n)
	}
}

// TestTextLoaderRecordsAMemoHintPerExpression: TextLoader's admission walk
// records, for every top-level expression, the number of headers the
// per-load copy will memoise -- for a parser's tree (no sharing) exactly
// the headers reachable from the expression -- and records it once, at
// admission, so the Loader carries it into every load.
func TestTextLoaderRecordsAMemoHintPerExpression(t *testing.T) {
	t.Parallel()
	exprs, err := parser.NewReader().Read("hint.lisp", strings.NewReader(synthLoaderSource(8*1024)))
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	hints, err := lisp.TextLoaderCopyHints(exprs)
	if err != nil {
		t.Fatalf("admission: %v", err)
	}
	if len(hints) != len(exprs) {
		t.Fatalf("%d hints for %d expressions", len(hints), len(exprs))
	}
	for i, expr := range exprs {
		seen := map[*lisp.LVal]bool{}
		copierReachable(expr, seen)
		if hints[i] != len(seen) {
			t.Errorf("expr %d: hint %d, want the %d headers the copy memoises", i, hints[i], len(seen))
		}
		if hints[i] <= lisp.CopierSmallMemo {
			t.Errorf("expr %d: hint %d fits the inline memo; the fixture must be library-shaped", i, hints[i])
		}
	}
	// A Loader built over the same source still evaluates it: the hinted
	// copy is the tree the environment sees.
	env := copierEnv(t)
	loader, err := lisp.TextLoader(parser.NewReader(), "hint.lisp", strings.NewReader(synthLoaderSource(8*1024)))
	if err != nil {
		t.Fatalf("TextLoader: %v", err)
	}
	if rc := loader(env); rc.Type == lisp.LError {
		t.Fatalf("load: %v", rc)
	}
	if rc := copierEval(t, env, `(loader-bench-handler-0 "case-0" 0)`); rc.String() != "'(1 2 3 0)" {
		t.Errorf("loaded function evaluated to %v", rc)
	}
}
