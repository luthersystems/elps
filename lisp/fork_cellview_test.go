// Copyright © 2026 The ELPS authors

package lisp

import (
	"fmt"
	"testing"
	"unsafe"
)

// Tests for cell views across Fork -- the link a view producer records
// (linkCellsView) and the O(1) resolution forker.val performs with it.  The
// language-level statement of the property lives in
// elpstest/fork_cellalias_test.go (TestForkPreservesCellSlotAliasing);
// these pin the mechanism: which producers link, what the link points at,
// and what Fork does with a link that no longer describes the template.
//
// Package lisp cannot import the parser, so calls are built by hand and
// evaluated, the way fork_cells_test.go's sealedDefun does.

// call evaluates (fn args...) against env and fails the test on an error.
func call(t testing.TB, env *LEnv, fn string, args ...*LVal) *LVal {
	t.Helper()
	cells := append([]*LVal{Symbol(fn)}, args...)
	res := env.Eval(SExpr(cells))
	if res.Type == LError {
		t.Fatalf("(%s ...): %v", fn, res)
	}
	return res
}

// setGlobal binds name to the value of (fn args...) and returns the stored
// value, read back from the package so the test holds exactly the header
// the environment holds.
func setGlobal(t testing.TB, env *LEnv, name, fn string, args ...*LVal) *LVal {
	t.Helper()
	cells := append([]*LVal{Symbol(fn)}, args...)
	call(t, env, "set", Quote(Symbol(name)), SExpr(cells))
	return packageSymbol(t, env, name)
}

func ints(xs ...int) []*LVal {
	out := make([]*LVal, len(xs))
	for i, x := range xs {
		out[i] = Int(x)
	}
	return out
}

func intsOf(t testing.TB, v *LVal) []int {
	t.Helper()
	cells := v.Cells
	if v.Type == LArray {
		cells = v.Cells[1].Cells
	}
	out := make([]int, len(cells))
	for i, c := range cells {
		if c.Type != LInt {
			t.Fatalf("cell %d is %v, want int", i, c.Type)
		}
		out[i] = c.Int
	}
	return out
}

func eqInts(a, b []int) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}

// sharesSlot reports whether view's element 0 is root's element off -- the
// slot-identity check forker.val performs, from the outside.
func sharesSlot(root, view *LVal, off int) bool {
	rc, vc := seqCells(root), seqCells(view)
	if len(vc) == 0 || off < 0 || off >= len(rc) {
		return false
	}
	return &rc[off] == &vc[0]
}

// TestCellViewLinkNeedsNoField pins the layout claim in lisp.go ("Cell
// views"): the link rides in Native and Int, and LVal does not grow.
func TestCellViewLinkNeedsNoField(t *testing.T) {
	if s := unsafe.Sizeof(LVal{}); s != 112 {
		t.Fatalf("LVal is %d bytes, want 112: the view link must not grow the struct", s)
	}
}

// TestCellViewProducers enumerates every producer of a shared-slot view
// and checks each records a link to the ROOT with the right offset.  A
// producer missing from this list, or one that stops linking, fails here
// before it fails a fork.
func TestCellViewProducers(t *testing.T) {
	env := newForkTestEnv(t)
	l := setGlobal(t, env, "l", "list", ints(1, 2, 3, 4, 5)...)
	v := setGlobal(t, env, "v", "vector", ints(1, 2, 3, 4, 5)...)
	holder := v.Cells[1]

	cases := []struct {
		name string
		view *LVal
		root *LVal
		off  int
	}{
		{"cdr", call(t, env, "cdr", Symbol("l")), l, 1},
		{"rest/list", call(t, env, "rest", Symbol("l")), l, 1},
		{"rest/vector", call(t, env, "rest", Symbol("v")), holder, 1},
		{"slice/list", call(t, env, "slice", Quote(Symbol("list")), Symbol("l"), Int(2), Int(4)), l, 2},
		{"slice/vector", call(t, env, "slice", Quote(Symbol("list")), Symbol("v"), Int(2), Int(4)), holder, 2},
		{"slice/vector-out", call(t, env, "slice", Quote(Symbol("vector")), Symbol("v"), Int(2), Int(4)).Cells[1], holder, 2},
		{"append/vector-no-values", call(t, env, "append", Quote(Symbol("vector")), Symbol("v")).Cells[1], holder, 0},
		// Views of views link to the root, never to the intermediate.
		{"cdr/cdr", call(t, env, "cdr", SExpr([]*LVal{Symbol("cdr"), Symbol("l")})), l, 2},
		{"cdr/cdr/cdr", call(t, env, "cdr", SExpr([]*LVal{Symbol("cdr"), SExpr([]*LVal{Symbol("cdr"), Symbol("l")})})), l, 3},
		{"slice/cdr", call(t, env, "slice", Quote(Symbol("list")), SExpr([]*LVal{Symbol("cdr"), Symbol("l")}), Int(1), Int(3)), l, 2},
		{"rest/slice/vector", call(t, env, "rest", SExpr([]*LVal{Symbol("slice"), Quote(Symbol("vector")), Symbol("v"), Int(1), Int(4)})), holder, 2},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			root, off := c.view.cellsView()
			if root != c.root || off != c.off {
				t.Fatalf("link = (%p, %d), want (%p, %d)", root, off, c.root, c.off)
			}
			if !sharesSlot(root, c.view, off) {
				t.Fatalf("the link does not describe the view: element 0 is not root.Cells[%d]", off)
			}
		})
	}

	// Non-views carry no link: the constructors, and the copying
	// producers (append with values, concat, copy).
	for _, c := range []struct {
		name string
		v    *LVal
	}{
		{"list", l},
		{"vector-holder", holder},
		{"append/vector-with-values", call(t, env, "append", Quote(Symbol("vector")), Symbol("v"), Int(9)).Cells[1]},
		{"append/list", call(t, env, "append", Quote(Symbol("list")), Symbol("l"), Int(9))},
		{"concat", call(t, env, "concat", Quote(Symbol("list")), Symbol("l"), Symbol("l"))},
		{"copy/cdr", call(t, env, "copy", SExpr([]*LVal{Symbol("cdr"), Symbol("l")}))},
	} {
		if root, _ := c.v.cellsView(); root != nil {
			t.Errorf("%s: unexpected view link to %p", c.name, root)
		}
	}
}

// TestCellViewSealedParentNotLinked: a view of a sealed list is itself
// sealed and shared by Fork outright, so linking it would only retain the
// parent.
func TestCellViewSealedParentNotLinked(t *testing.T) {
	env := newForkTestEnv(t)
	lit := QExpr(ints(1, 2, 3))
	lit.SealAST()
	env.PutGlobal(Symbol("lit"), lit)
	view := call(t, env, "cdr", Symbol("lit"))
	if !view.sealed {
		t.Fatalf("cdr of a sealed list is not sealed; the premise of this test is wrong")
	}
	if root, _ := view.cellsView(); root != nil {
		t.Errorf("sealed view carries a link to %p", root)
	}
}

// forkOf forks env and returns the fork plus a lookup for its package
// symbols.
func forkOf(t testing.TB, env *LEnv) (*LEnv, func(string) *LVal) {
	t.Helper()
	fork, err := env.Fork()
	if err != nil {
		t.Fatalf("fork: %v", err)
	}
	return fork, func(name string) *LVal { return packageSymbol(t, fork, name) }
}

// TestForkCellViewSharesRootCopy is the mechanism behind
// TestForkPreservesCellSlotAliasing: the fork's view is a window onto the
// fork's root copy, the link is remapped onto the fork's root, and neither
// touches template memory.  Checked for one and two hops, for a vector
// root, and for the vector-producing slice.
func TestForkCellViewSharesRootCopy(t *testing.T) {
	env := newForkTestEnv(t)
	setGlobal(t, env, "l", "list", ints(30, 10, 20, 40)...)
	setGlobal(t, env, "v", "vector", ints(30, 10, 20, 40)...)
	setGlobal(t, env, "tail", "cdr", Symbol("l"))
	setGlobal(t, env, "tail2", "cdr", SExpr([]*LVal{Symbol("cdr"), Symbol("l")}))
	setGlobal(t, env, "vrest", "rest", Symbol("v"))
	setGlobal(t, env, "vslice", "slice", Quote(Symbol("vector")), Symbol("v"), Int(1), Int(3))

	fork, get := forkOf(t, env)
	l, v := get("l"), get("v")
	for _, c := range []struct {
		name string
		view *LVal
		root *LVal
		off  int
	}{
		{"tail", get("tail"), l, 1},
		{"tail2", get("tail2"), l, 2},
		{"vrest", get("vrest"), v.Cells[1], 1},
		{"vslice", get("vslice").Cells[1], v.Cells[1], 1},
	} {
		t.Run(c.name, func(t *testing.T) {
			root, off := c.view.cellsView()
			if root != c.root || off != c.off {
				t.Fatalf("fork-side link = (%p, %d), want the fork's root (%p, %d)", root, off, c.root, c.off)
			}
			if root == packageSymbol(t, env, "l") || root == packageSymbol(t, env, "v").Cells[1] {
				t.Fatalf("fork-side link points at the template's root")
			}
			if !sharesSlot(c.root, c.view, c.off) {
				t.Fatalf("fork's view is not a window onto the fork's root copy")
			}
			if cap(seqCells(c.view)) != len(seqCells(c.view)) {
				t.Errorf("fork's view has spare capacity (len %d, cap %d): the #373 clamp was lost", len(seqCells(c.view)), cap(seqCells(c.view)))
			}
			if sameCellsBacking(seqHolder(c.view), seqHolder(packageSymbol(t, env, c.name))) {
				t.Errorf("fork's view shares the template's backing array")
			}
		})
	}

	// Behaviour, both directions: sorting through the fork's root is seen
	// through the fork's views and not by the template.
	call(t, fork, "stable-sort", Symbol("<"), Symbol("l"))
	if got := intsOf(t, get("tail2")); !eqInts(got, []int{30, 40}) {
		t.Errorf("fork's two-hop view after sorting the root = %v, want [30 40]", got)
	}
	if got := intsOf(t, packageSymbol(t, env, "tail2")); !eqInts(got, []int{20, 40}) {
		t.Errorf("template's view changed when the fork sorted its root: %v", got)
	}
	call(t, fork, "stable-sort", Symbol("<"), Symbol("vrest"))
	if got := intsOf(t, get("v")); !eqInts(got, []int{30, 10, 20, 40}) {
		t.Errorf("fork's vector after sorting its rest view = %v, want [30 10 20 40]", got)
	}
}

// TestForkCellViewUnreachableRoot: the root is garbage except through the
// view (the list was never bound).  The fork must still produce a private,
// correct view -- and does so by copying the root through the link.
func TestForkCellViewUnreachableRoot(t *testing.T) {
	env := newForkTestEnv(t)
	tmpl := setGlobal(t, env, "tail", "cdr", SExpr([]*LVal{Symbol("list"), Int(3), Int(1), Int(2)}))
	root, _ := tmpl.cellsView()
	if root == nil {
		t.Fatal("cdr of an unbound list carries no link; premise wrong")
	}

	fork, get := forkOf(t, env)
	ftail := get("tail")
	if got := intsOf(t, ftail); !eqInts(got, []int{1, 2}) {
		t.Fatalf("fork's view = %v, want [1 2]", got)
	}
	if sameCellsBacking(ftail, tmpl) {
		t.Fatalf("fork's view shares the template's backing array")
	}
	froot, _ := ftail.cellsView()
	if froot == nil || froot == root {
		t.Fatalf("fork's link = %p, want a fork-side copy of the template root %p", froot, root)
	}
	if !sharesSlot(froot, ftail, 1) {
		t.Errorf("fork's view is not a window onto its copied root")
	}
	call(t, fork, "stable-sort", Symbol("<"), Symbol("tail"))
	if got := intsOf(t, ftail); !eqInts(got, []int{1, 2}) {
		t.Errorf("fork's view after sort = %v", got)
	}
	if got := intsOf(t, tmpl); !eqInts(got, []int{1, 2}) {
		t.Errorf("template's view changed: %v", got)
	}
}

// TestForkCellViewMadeInTransaction: a view produced INSIDE a fork links to
// the fork's own root, never to template memory.
func TestForkCellViewMadeInTransaction(t *testing.T) {
	env := newForkTestEnv(t)
	tl := setGlobal(t, env, "l", "list", ints(30, 10, 20)...)
	fork, get := forkOf(t, env)
	fl := get("l")
	if fl == tl {
		t.Fatal("fork shares the template's list header")
	}
	view := setGlobal(t, fork, "tail", "cdr", Symbol("l"))
	root, off := view.cellsView()
	if root != fl || off != 1 {
		t.Fatalf("in-fork view links to (%p, %d), want the fork's own list (%p, 1)", root, off, fl)
	}
	if root == tl || sameCellsBacking(view, tl) {
		t.Fatalf("in-fork view reaches template memory")
	}
	// A fork of the fork resolves it like any other view.
	fork2, get2 := forkOf(t, fork)
	call(t, fork2, "stable-sort", Symbol("<"), Symbol("l"))
	if got := intsOf(t, get2("tail")); !eqInts(got, []int{20, 30}) {
		t.Errorf("second-generation fork's view = %v, want [20 30]", got)
	}
	if got := intsOf(t, view); !eqInts(got, []int{10, 20}) {
		t.Errorf("first fork's view changed when its fork sorted: %v", got)
	}
}

// TestForkCellViewStaleLinkCopiesPrivately pins the advisory nature of the
// link: a header whose Cells was reassigned after the link was recorded
// has parted from its root in the template, and the fork must copy it
// privately -- the old contents, on its own array -- rather than re-point
// it onto the root's copy.  Two shapes: the VIEW's Cells replaced, and the
// ROOT's Cells replaced (append! growing a vector past its capacity does
// the second one from lisp).
func TestForkCellViewStaleLinkCopiesPrivately(t *testing.T) {
	t.Run("view-reassigned", func(t *testing.T) {
		env := newForkTestEnv(t)
		setGlobal(t, env, "l", "list", ints(30, 10, 20)...)
		tail := setGlobal(t, env, "tail", "cdr", Symbol("l"))
		tail.Cells = ints(7, 8) //elps:mutates the template on purpose, to arm the stale link
		fork, get := forkOf(t, env)
		ftail := get("tail")
		if got := intsOf(t, ftail); !eqInts(got, []int{7, 8}) {
			t.Fatalf("fork's view = %v, want the reassigned contents [7 8]", got)
		}
		if root, _ := ftail.cellsView(); root != nil {
			t.Errorf("stale link survived the fork: %p", root)
		}
		if sameCellsBacking(ftail, tail) {
			t.Errorf("fork's copy shares the template's array")
		}
		call(t, fork, "stable-sort", Symbol("<"), Symbol("l"))
		if got := intsOf(t, ftail); !eqInts(got, []int{7, 8}) {
			t.Errorf("sorting the fork's root reached the parted view: %v", got)
		}
	})
	t.Run("root-reassigned-by-append!", func(t *testing.T) {
		env := newForkTestEnv(t)
		v := setGlobal(t, env, "v", "vector", ints(30, 10, 20)...)
		before := v.Cells[1].Cells
		setGlobal(t, env, "r", "rest", Symbol("v"))
		// (vector ...) has exact capacity, so this append! reallocates.
		call(t, env, "append!", Symbol("v"), Int(5))
		if &v.Cells[1].Cells[0] == &before[0] {
			t.Skip("append! did not reallocate; nothing to pin")
		}
		fork, get := forkOf(t, env)
		fr := get("r")
		if got := intsOf(t, fr); !eqInts(got, []int{10, 20}) {
			t.Fatalf("fork's view = %v, want [10 20]", got)
		}
		if root, _ := fr.cellsView(); root != nil {
			t.Errorf("link survived a fork after the root reallocated: %p", root)
		}
		call(t, fork, "stable-sort", Symbol("<"), Symbol("v"))
		if got := intsOf(t, fr); !eqInts(got, []int{10, 20}) {
			t.Errorf("fork's parted view followed the root's sort: %v", got)
		}
	})
}

// TestForkCellViewInsideItsOwnRoot: a vector that holds a slice of itself,
// appended in place so the slice still windows the vector's array, and the
// slice is bound NOWHERE ELSE -- the walk can only reach it through the
// root's own cells, while the root's copy is still being filled.  That is
// why forker.val publishes a root's cell slice before filling it: with the
// publish after the loop, the view finds a root copy with no Cells yet and
// falls back to a private copy, and this test goes red.  (Binding the view
// to a symbol as well would let map iteration order reach it first and
// hide the defect on a coin flip.)
func TestForkCellViewInsideItsOwnRoot(t *testing.T) {
	env := newForkTestEnv(t)
	v := setGlobal(t, env, "v", "vector", Int(1))
	call(t, env, "append!", Symbol("v"), Int(2))
	call(t, env, "append!", Symbol("v"), Int(3))
	holder := v.Cells[1]
	if cap(holder.Cells) <= len(holder.Cells) {
		t.Skip("append! left no spare capacity; the in-place shape cannot be built")
	}
	call(t, env, "append!", Symbol("v"), SExpr([]*LVal{Symbol("slice"), Quote(Symbol("list")), Symbol("v"), Int(0), Int(2)}))
	s := holder.Cells[3]
	if root, off := s.cellsView(); root != holder || off != 0 || !sharesSlot(holder, s, 0) {
		t.Skip("the vector does not hold its own view in place; shape not built")
	}

	// Several forks, so an order-dependent defect cannot pass on a coin
	// flip; the view is reachable through the root alone, so every fork
	// takes the root-first path.
	for i := range 8 {
		fork, err := env.Fork()
		if err != nil {
			t.Fatalf("fork %d: %v", i, err)
		}
		fv := packageSymbol(t, fork, "v")
		fs := fv.Cells[1].Cells[3]
		if fs == s {
			t.Fatalf("fork %d shares the template's view header", i)
		}
		if !sharesSlot(fv.Cells[1], fs, 0) {
			t.Fatalf("fork %d: the self-held view is not a window onto the fork's vector", i)
		}
		if root, _ := fs.cellsView(); root != fv.Cells[1] {
			t.Fatalf("fork %d: view links to %p, want the fork's holder %p", i, root, fv.Cells[1])
		}
		if sameCellsBacking(fs, s) || sameCellsBacking(fv.Cells[1], holder) {
			t.Fatalf("fork %d shares template memory", i)
		}
	}
}

// TestCopyAndDetachDropCellViewLink: copy and detach allocate fresh storage
// and do not preserve backing-array sharing
// (TestCopyDoesNotPreserveBackingArraySharing), so the link must not travel
// -- a copy carrying it would tell a later Fork that it windows a root it
// does not.
func TestCopyAndDetachDropCellViewLink(t *testing.T) {
	env := newForkTestEnv(t)
	l := setGlobal(t, env, "l", "list", ints(30, 10, 20)...)
	tail := setGlobal(t, env, "tail", "cdr", Symbol("l"))
	if root, _ := tail.cellsView(); root != l {
		t.Fatal("premise: tail is not a view of l")
	}
	for _, c := range []struct {
		name string
		cp   *LVal
	}{
		{"copy-builtin", call(t, env, "copy", Symbol("tail"))},
		{"Copy", tail.Copy()},
		{"detach", func() *LVal {
			cp, err := Detach(tail)
			if err != nil {
				t.Fatalf("detach: %v", err)
			}
			return cp
		}()},
	} {
		t.Run(c.name, func(t *testing.T) {
			if root, off := c.cp.cellsView(); root != nil || off != 0 {
				t.Errorf("link survived: (%p, %d)", root, off)
			}
			if c.cp.Native != nil {
				t.Errorf("Native = %T, want nil", c.cp.Native)
			}
			if sameCellsBacking(c.cp, tail) {
				t.Errorf("copy shares the view's backing array")
			}
			if got := intsOf(t, c.cp); !eqInts(got, []int{10, 20}) {
				t.Errorf("copy = %v, want [10 20]", got)
			}
		})
	}
	// A view held INSIDE a copied container gets the same treatment.
	cp := call(t, env, "copy", SExpr([]*LVal{Symbol("list"), Symbol("l"), Symbol("tail")}))
	if root, _ := cp.Cells[1].cellsView(); root != nil {
		t.Errorf("nested view's link survived copy: %p", root)
	}
}

// TestForkManyViewsResolveInConstantWork: the resolution is per view, not
// per template value -- a template with many views forks with one extra
// header lookup per view and no sort.  Pinned as a bound on allocations
// per fork relative to the same template without the views, since that is
// what an address sort or a record slice would move
// (BenchmarkForkViews measures the time).
func TestForkManyViewsResolveInConstantWork(t *testing.T) {
	const n = 200
	build := func(withViews bool) *LEnv {
		env := newForkTestEnv(t)
		for i := range n {
			setGlobal(t, env, fmt.Sprintf("l%d", i), "list", ints(i, i+1, i+2, i+3)...)
			if withViews {
				setGlobal(t, env, fmt.Sprintf("t%d", i), "cdr", Symbol(fmt.Sprintf("l%d", i)))
			}
		}
		return env
	}
	plain, views := build(false), build(true)
	allocs := func(env *LEnv) float64 {
		return testing.AllocsPerRun(20, func() {
			if _, err := env.Fork(); err != nil {
				t.Fatal(err)
			}
		})
	}
	a0, a1 := allocs(plain), allocs(views)
	// Each view costs its header copy (one LVal) and its package-symbol
	// entry; a re-pointed window allocates nothing of its own.
	if extra := a1 - a0; extra > 3*n {
		t.Errorf("%d views added %.0f allocations per fork (want at most %d): a view is costing more than a header", n, extra, 3*n)
	}
}
