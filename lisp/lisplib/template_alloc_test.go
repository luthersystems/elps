// Copyright © 2026 The ELPS authors

//go:build !race && !elpscheck

package lisplib_test

import (
	"strconv"
	"testing"
)

func TestTemplatePublicationAllocationBudget(t *testing.T) {
	for _, tc := range []struct {
		functions int
		base      float64
	}{{0, 520}, {250, 1779}} {
		t.Run(strconv.Itoa(tc.functions), func(t *testing.T) {
			env := templatePlanBenchmarkFixture(t, tc.functions)
			allocations := testing.AllocsPerRun(20, func() { snapshotFixture(t, env) })
			// PR base 2b20622 (414 / 1673). Lazy instantiation added a flat
			// 106: every package, not only the frozen ones, gets a shared
			// base (name->slot index, packageBase, refs) so its bindings can
			// start unmaterialized in each VM. It is per package, not per
			// binding, and is paid once at publication; NewVM drops from
			// thousands of allocations to tens. Traversal scratch must not
			// add allocations per binding, builtin environment, or sealed
			// scalar.
			if allocations > tc.base*1.05 {
				t.Fatalf("publication used %g allocations, want within 5%% of %g", allocations, tc.base)
			}
		})
	}
}
