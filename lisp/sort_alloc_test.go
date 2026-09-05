// Copyright © 2026 The ELPS authors

package lisp

import (
	"fmt"
	"testing"
)

// TestStableSortAllocationsDoNotDependOnMapWidth pins that stable-sort's
// per-call allocation count is a function of the comparisons it makes and
// not of the size of the elements it compares: sorting eight one-entry maps
// and sorting eight nine-entry maps, with the same keys in the same order,
// allocate identically.
//
// Before #604 the comparator copied both elements on every comparison, so
// the wide list cost a nine-entry map walk per element per comparison more
// than the narrow one; the two counts differed by hundreds.  The count is
// also pinned as an equality, in the style of TestVectorBuiltinAllocations
// (same Go-implemented predicate discipline, same warm-up: AllocsPerRun's
// first call sorts the list, and the measured calls sort an already sorted
// list, which insertion-sorts with a fixed number of comparisons).
func TestStableSortAllocationsDoNotDependOnMapWidth(t *testing.T) {
	env := NewEnv(nil)
	if rc := InitializeUserEnv(env); rc.Type == LError {
		t.Fatalf("initialize-user-env: %v", rc)
	}
	key := String("k")
	// A Go-implemented predicate keeps the evaluator's own allocations,
	// which this test does not measure, out of the count; it reads the
	// sort key from each map exactly as (lambda (a b) (< (get a "k") (get b "k")))
	// would.
	lessK := FunInPackage(DefaultUserPackage, "less-k?", Formals("a", "b"), func(env *LEnv, args *LVal) *LVal {
		x, _ := args.Cells[0].Map().Get(key)
		y, _ := args.Cells[1].Map().Get(key)
		return Bool(x.Int < y.Int)
	})
	const n = 8
	// mapList builds n maps keyed "k" = n..1 (reverse order, so the
	// warm-up sort moves every element) with width-1 further entries each.
	mapList := func(width int) *LVal {
		cells := make([]*LVal, 0, n)
		for i := n; i > 0; i-- {
			m := SortedMap()
			m.Map().Set(key, Int(i))
			for f := 1; f < width; f++ {
				m.Map().Set(String(fmt.Sprintf("f%d", f)), Int(f))
			}
			cells = append(cells, m)
		}
		return QExpr(cells)
	}
	narrow := mapList(1)
	wide := mapList(9)
	sortNarrow := QExpr([]*LVal{lessK, narrow})
	sortWide := QExpr([]*LVal{lessK, wide})

	// Anti-vacuity for the width axis: a wide map costs more to copy than a
	// narrow one, so a per-comparison copy would separate the two counts.
	copyNarrow := testing.AllocsPerRun(20, func() { narrow.Cells[0].Copy() })
	copyWide := testing.AllocsPerRun(20, func() { wide.Cells[0].Copy() })
	if copyWide <= copyNarrow {
		t.Fatalf("fixture: copying a wide map allocated %v times, a narrow one %v; the width axis would not show a copy", copyWide, copyNarrow)
	}

	allocsNarrow := testing.AllocsPerRun(200, func() { builtinSortStable(env, sortNarrow) })
	allocsWide := testing.AllocsPerRun(200, func() { builtinSortStable(env, sortWide) })
	// Anti-vacuity for the sort itself: both lists came out sorted.
	for _, list := range []*LVal{narrow, wide} {
		for i, m := range list.Cells {
			v, _ := m.Map().Get(key)
			if v.Int != i+1 {
				t.Fatalf("list not sorted: element %d has k=%d", i, v.Int)
			}
		}
	}
	if allocsNarrow != allocsWide {
		t.Errorf("stable-sort allocated %v times per call over one-entry maps and %v over nine-entry maps; the count must not depend on element size", allocsNarrow, allocsWide)
	}
	// Measured on the commit that removed the per-comparison copy: the
	// seven insertion-sort comparisons of an already sorted eight-element
	// list, each an evaluated (less-k? a b) form.
	const want = 43
	if int(allocsNarrow) != want {
		t.Errorf("stable-sort allocated %v times per call, want %d", allocsNarrow, want)
	}
}
