// Copyright © 2026 The ELPS authors

package lisp

import "testing"

// TestTemplateBuiltinCellsShared checks that a VM forked from a template
// shares a registered builtin's formals and docstring with the template, as it
// shares a Lisp function's formals and body.  Before issue #870 the docstring
// cell was unsealed, so the plan gave the cells a per-VM storage group and
// every first read of a builtin from a frozen package (use-package reads every
// export) allocated the group and the docstring value.
func TestTemplateBuiltinCellsShared(t *testing.T) {
	source := NewEnv(nil)
	if rc := InitializeUserEnv(source); rc.Type == LError {
		t.Fatal(rc)
	}
	tmpl, err := NewTemplate(source, TemplateWithBuiltinPolicy(func(*LVal) bool { return true }),
		TemplateWithFrozenPackages(DefaultLangPackage))
	if err != nil {
		t.Fatal(err)
	}
	names := []string{"map", "car", "cdr", "list", "concat", "foldl", "cons", "reverse", "length", "nth"}
	syms := make([]*LVal, len(names))
	for i, name := range names {
		syms[i] = Symbol(name)
	}
	lang := source.Runtime.Registry.packages[DefaultLangPackage]

	vm, err := tmpl.NewVM()
	if err != nil {
		t.Fatal(err)
	}
	for _, sym := range syms {
		want := lang.Get(sym)
		got := vm.Runtime.Registry.packages[DefaultLangPackage].Get(sym)
		if got.Type != LFun || len(got.Cells) != 2 {
			t.Fatalf("%s: forked value is %v", sym.Str, got)
		}
		if got == want {
			t.Fatalf("%s: forked VM shares the function value itself", sym.Str)
		}
		if !want.Cells[1].IsSealed() {
			t.Errorf("%s: registered docstring cell is not sealed", sym.Str)
		}
		if got.Cells[0] != want.Cells[0] || got.Cells[1] != want.Cells[1] {
			t.Errorf("%s: forked VM copied the builtin's formals or docstring", sym.Str)
		}
	}

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
