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
	// The VM's tables of built objects are paged (lazyTable), so the reads
	// also allocate each page they are the first to store into.
	inst := lazyInstanceOf(vms[0])
	pages := lazyTablePages(&inst.values) + lazyTablePages(&inst.functions)
	if most := (len(syms) + 1) / 2; pages > most {
		t.Errorf("first reads of %d builtins stored into %d table pages, want at most %d", len(syms), pages, most)
	}
	if limit := float64(2*len(syms) + 1 + pages); allocs > limit {
		t.Errorf("first reads of %d builtins allocated %v objects, want at most %v", len(syms), allocs, limit)
	}
}

// lazyTablePages returns the number of pages a VM has allocated in one of
// its tables of built objects.
func lazyTablePages[T any](table *lazyTable[T]) int {
	n := 0
	for _, page := range table.pages {
		if page != nil {
			n++
		}
	}
	return n
}
