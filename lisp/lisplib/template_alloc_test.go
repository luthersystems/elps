// Copyright © 2026 The ELPS authors

//go:build !race && !elpscheck

package lisplib_test

import (
	"runtime"
	"strconv"
	"testing"

	"github.com/luthersystems/elps/lisp"
)

func TestTemplatePublicationAllocationBudget(t *testing.T) {
	for _, tc := range []struct {
		functions int
		base      float64
	}{{0, 514}, {250, 1774}} {
		t.Run(strconv.Itoa(tc.functions), func(t *testing.T) {
			env := templatePlanBenchmarkFixture(t, tc.functions)
			allocations := testing.AllocsPerRun(20, func() { snapshotFixture(t, env) })
			// Lazy bases retain the compiler's binding descriptors and adopt
			// the private export copy: neither needs a second slice. Scratch
			// must not add allocations per binding, builtin environment or
			// sealed scalar. Keep budgets at or below the measured baseline.
			if allocations > tc.base*1.05 {
				t.Fatalf("publication used %g allocations, want within 5%% of %g", allocations, tc.base)
			}
		})
	}
}

func TestTemplateForkAllocationBudget(t *testing.T) {
	for _, tc := range []struct {
		functions int
		base      float64
	}{{0, 27}, {250, 32}} {
		t.Run(strconv.Itoa(tc.functions), func(t *testing.T) {
			tmpl := snapshotFixture(t, templatePlanBenchmarkFixture(t, tc.functions))
			allocations := testing.AllocsPerRun(100, func() {
				runtime.KeepAlive(forkTemplateFixture(t, tmpl))
			})
			if allocations > tc.base {
				t.Fatalf("fork used %g allocations, want at most %g", allocations, tc.base)
			}
		})
	}
}

// Adding an unused package costs its independent mutable shell alone. Shared
// bindings and pending per-VM values must not add eager slot/link allocations.
func TestTemplateForkExtraPackageAllocationBudget(t *testing.T) {
	for _, kind := range []string{"empty", "shared", "pending"} {
		t.Run(kind, func(t *testing.T) {
			env := templatePlanBenchmarkFixture(t, 0)
			measure := func() float64 {
				tmpl := snapshotFixture(t, env)
				return testing.AllocsPerRun(100, func() {
					runtime.KeepAlive(forkTemplateFixture(t, tmpl))
				})
			}
			before := measure()
			pkg := env.Runtime.Registry.DefinePackage("extra")
			switch kind {
			case "shared":
				pkg.Put(lisp.Symbol("value"), lisp.Nil())
			case "pending":
				pkg.Put(lisp.Symbol("value"), lisp.Int(1))
			}
			if extra := measure() - before; extra > 1 {
				t.Fatalf("extra package costs %g allocations per fork, want only its shell (1)", extra)
			}
		})
	}
}
