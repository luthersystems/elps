// Copyright © 2026 The ELPS authors

// Exact allocation pins exclude race and ownership instrumentation, as in
// sort_alloc_pin_test.go. Behavioral coverage lives in
// template_builtin_cells_test.go.
//go:build !race && !elpscheck

package lisp

import "testing"

// TestTemplateBuiltinReadAllocs pins the cost of the first reads of
// registered builtins from a frozen package in a forked VM (issue #870).
func TestTemplateBuiltinReadAllocs(t *testing.T) {
	_, tmpl, syms := builtinCellsTemplate(t)
	var err error
	// Each read now costs what a Lisp function's does: the function value and
	// its function data.  One more allocation fills the package's slot values.
	const runs = 50
	vms := make([]*LEnv, runs+1)
	for i := range vms {
		if vms[i], err = tmpl.NewVM(); err != nil {
			t.Fatal(err)
		}
	}
	next := 0
	allocs := testing.AllocsPerRun(runs, func() {
		pkg := vms[next].Runtime.Registry.packages[DefaultLangPackage]
		next++
		for _, sym := range syms {
			pkg.Get(sym)
		}
	})
	if limit := float64(2*len(syms) + 1); allocs > limit {
		t.Errorf("first reads of %d builtins allocated %v objects, want at most %v", len(syms), allocs, limit)
	}
}
