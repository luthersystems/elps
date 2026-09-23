// Copyright © 2026 The ELPS authors

package lisp

import (
	"reflect"
	"slices"
	"strings"
	"testing"
)

func clonePackageBase(b *packageBase) packageBase {
	out := packageBase{index: make(map[string]int, len(b.index)), externals: slices.Clone(b.externals)}
	for k, v := range b.index {
		out.index[k] = v
	}
	out.funNames = cloneFrozenStrings(b.funNames)
	out.symbolDocs = cloneFrozenStrings(b.symbolDocs)
	return out
}

func cloneFrozenStrings(m map[string]string) map[string]string {
	if m == nil {
		return nil
	}
	out := make(map[string]string, len(m))
	for k, v := range m {
		out[k] = v
	}
	return out
}

func expectFrozenPanic(t *testing.T, what string, f func()) {
	t.Helper()
	defer func() {
		t.Helper()
		r := recover()
		msg, _ := r.(string)
		if !strings.HasPrefix(msg, "cannot modify frozen package frozen: symbol ") {
			t.Fatalf("%s: want frozen-package panic, got %v", what, r)
		}
	}()
	f()
}

// TestTemplateFrozenPackageBaseImmutable drives every Package mutator at a
// frozen package in template VMs: each is refused, and the published base
// is byte-for-byte unchanged afterwards.
func TestTemplateFrozenPackageBaseImmutable(t *testing.T) {
	source := templateOwnershipEnv()
	frozen := source.Runtime.Registry.DefinePackage("frozen")
	frozen.setSymbolDoc("value", "original doc")
	fn := source.Lambda(Formals(), []*LVal{Int(1)})
	frozen.Put(Symbol("fn"), fn)
	frozen.Put(Symbol("value"), Int(1))
	frozen.Export("zeta", "value") // deliberately unsorted
	source.Runtime.Package.Put(Symbol("mine"), Int(1))
	if _, err := NewTemplate(source, TemplateWithFrozenPackages("missing")); err == nil || !strings.Contains(err.Error(), `frozen package "missing" is not registered`) {
		t.Fatalf("unregistered frozen package accepted: %v", err)
	}
	tmpl, err := NewTemplate(source, TemplateWithFrozenPackages("frozen"))
	if err != nil {
		t.Fatal(err)
	}
	var before packageBase
	for _, p := range tmpl.plan.packages {
		if (p.base != nil) != (p.name == "frozen") {
			t.Fatalf("package %s frozen=%v", p.name, p.base != nil)
		}
		if p.base != nil {
			before = clonePackageBase(p.base)
		}
	}
	for range 3 {
		vm, err := tmpl.NewVM()
		if err != nil {
			t.Fatal(err)
		}
		p := vm.Runtime.Registry.Package("frozen")
		if !p.Frozen() || p.symbols != nil || p.funNames != nil || p.symbolDocs != nil {
			t.Fatal("VM package does not read the shared base")
		}
		if v, _ := p.Symbol("value"); v.Int != 1 || p.SymbolDoc("value") != "original doc" || !slices.Equal(p.SymbolNames(), []string{"fn", "value"}) {
			t.Fatal("frozen package reads changed")
		}
		if f, _ := p.Symbol("fn"); p.GetFunName(f.FID()) != "fn" || f == fn {
			t.Fatal("function naming or isolation changed")
		}
		fn2 := vm.Lambda(Formals(), []*LVal{Int(2)})
		for name, r := range map[string]*LVal{
			"Put": p.Put(Symbol("value"), Int(2)), "Update": p.Update(Symbol("fn"), fn2), "Put new": p.Put(Symbol("new"), fn2),
			"PutGlobal qualified": vm.PutGlobal(Symbol("frozen:value"), Int(3)),
		} {
			if r.Type != LError || !strings.Contains(r.String(), "cannot modify frozen package frozen: symbol ") {
				t.Fatalf("%s: want frozen error, got %v", name, r)
			}
		}
		expectFrozenPanic(t, "setSymbolDoc", func() { p.setSymbolDoc("value", "vm doc") })
		expectFrozenPanic(t, "Exports one", func() { p.Exports("alpha") })
		expectFrozenPanic(t, "Exports many", func() { p.Exports("b", "a") })
		expectFrozenPanic(t, "Export", func() { p.Export("appended") })
		expectFrozenPanic(t, "appendExternal", func() { p.appendExternal("more") })
		expectFrozenPanic(t, "putName", func() { p.putName("more", Int(1)) })
		if r := vm.InPackage(String("frozen")); r.Type == LError {
			t.Fatal(r)
		}
		if r := vm.UsePackage(String("frozen")); r.Type != LError || !strings.Contains(r.String(), "cannot modify frozen package frozen: symbol zeta") {
			t.Fatalf("use-package into a frozen package: %v", r)
		}
		if r := vm.InPackage(String("user")); r.Type == LError {
			t.Fatal(r)
		}
		// Unfrozen packages keep ordinary per-VM writes.
		if r := vm.Runtime.Package.Put(Symbol("mine"), Int(9)); r.Type == LError {
			t.Fatal(r)
		}
	}
	for _, p := range tmpl.plan.packages {
		if p.base != nil && !reflect.DeepEqual(before, clonePackageBase(p.base)) {
			t.Fatalf("package %s: published base was written", p.name)
		}
	}
	if !slices.Equal(before.externals, []string{"zeta", "value"}) {
		t.Fatalf("shared export list reordered: %v", before.externals)
	}
	vm, _ := tmpl.NewVM()
	if v, _ := vm.Runtime.Package.Symbol("mine"); v.Int != 1 {
		t.Fatal("unfrozen write leaked into the template")
	}
}

// Zero-argument mutator calls on a frozen package are refused at entry: a
// no-name Exports() used to sort the shared export list in place.
func TestTemplateFrozenPackageEmptyArgumentMutators(t *testing.T) {
	source := templateOwnershipEnv()
	frozen := source.Runtime.Registry.DefinePackage("frozen")
	frozen.Put(Symbol("a"), Int(1))
	frozen.Export("zeta", "a") // unsorted
	tmpl, err := NewTemplate(source, TemplateWithFrozenPackages("frozen"))
	if err != nil {
		t.Fatal(err)
	}
	vm, _ := tmpl.NewVM()
	p := vm.Runtime.Registry.Package("frozen")
	expectFrozenPanic(t, "Exports()", func() { p.Exports() })
	expectFrozenPanic(t, "Export()", func() { p.Export() })
	expectFrozenPanic(t, "Exports(empty slice)", func() { p.Exports([]string{}...) })
	expectFrozenPanic(t, "setSymbolDoc empty", func() { p.setSymbolDoc("", "") })
	expectFrozenPanic(t, "putName empty", func() { p.putName("", Int(1)) })
	expectFrozenPanic(t, "appendExternal empty", func() { p.appendExternal("") })
	if got := tmpl.plan.packages[0].base.externals; !slices.Equal(got, []string{"zeta", "a"}) {
		t.Fatalf("shared export list was reordered: %v", got)
	}
}
