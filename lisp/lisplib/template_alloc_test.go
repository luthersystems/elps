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
	}{{0, 414}, {250, 1673}} {
		t.Run(strconv.Itoa(tc.functions), func(t *testing.T) {
			env := templatePlanBenchmarkFixture(t, tc.functions)
			allocations := testing.AllocsPerRun(20, func() { snapshotFixture(t, env) })
			// PR base 2b20622. Traversal scratch must not add allocations
			// per binding, builtin environment, or sealed scalar.
			if allocations > tc.base*1.05 {
				t.Fatalf("publication used %g allocations, want within 5%% of %g", allocations, tc.base)
			}
		})
	}
}
