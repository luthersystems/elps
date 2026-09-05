// Copyright © 2026 The ELPS authors

// !race && !elpscheck for the same reason as TestVectorBuiltinAllocations:
// the checked build adds ownership bookkeeping to every eval
// (lisp/ownership_check_elpscheck.go), which moves this exact count (52
// where the release build measures 43), and the race detector's
// instrumentation does the same.  The width-independence property those
// builds still hold is asserted, unconstrained, in
// TestStableSortAllocationsDoNotDependOnMapWidth.

//go:build !race && !elpscheck

package lisp

import "testing"

// TestStableSortAllocationCount pins stable-sort's per-call allocation
// count on an already sorted eight-element list of maps, in the style of
// TestVectorBuiltinAllocations: AllocsPerRun's first call sorts the list,
// and the measured calls insertion-sort a sorted list with a fixed seven
// comparisons, each an evaluated (less-k? a b) form.
func TestStableSortAllocationCount(t *testing.T) {
	env, lessK, key := stableSortAllocFixture(t)
	list := stableSortAllocMaps(key, 1)
	args := QExpr([]*LVal{lessK, list})
	// Anti-vacuity: the call must have sorted the list, or an early error
	// return would allocate less and pass a bound.
	if got := builtinSortStable(env, args); got.Type == LError {
		t.Fatalf("stable-sort: %v", got)
	}
	for i, m := range list.Cells {
		v, _ := m.Map().Get(key)
		if v.Int != i+1 {
			t.Fatalf("list not sorted: element %d has k=%d", i, v.Int)
		}
	}
	// Measured on the commit that removed the per-comparison copy.
	const want = 43
	if n := testing.AllocsPerRun(200, func() { builtinSortStable(env, args) }); int(n) != want {
		t.Errorf("stable-sort allocated %v times per call, want %d", n, want)
	}
}
