// Copyright © 2026 The ELPS authors

package lisp

import (
	"reflect"
	"runtime"
	"slices"
	"strconv"
	"strings"
	"testing"
)

type packageBaseSnapshot struct {
	index                map[string]int
	funNames, symbolDocs map[string]string
	externals            []string
}

func clonePackageBase(b *packageBase) packageBaseSnapshot {
	return packageBaseSnapshot{
		index: b.index.Copy(), funNames: b.funNames.Copy(),
		symbolDocs: b.symbolDocs.Copy(), externals: b.externals.Copy(),
	}
}

func frozenFixture(t *testing.T) (*Template, *LVal) {
	t.Helper()
	source := templateOwnershipEnv()
	frozen := source.Runtime.Registry.DefinePackage("frozen")
	frozen.setSymbolDoc("value", "original doc")
	fn := source.Lambda(Formals(), []*LVal{Int(1)})
	frozen.Put(Symbol("fn"), fn)
	frozen.Put(Symbol("value"), Int(1))
	frozen.Export("zeta", "value") // deliberately unsorted
	big := source.Runtime.Registry.DefinePackage("big")
	for n := range 500 {
		big.Put(Symbol("b"+strconv.Itoa(n)), Int(n))
	}
	source.Runtime.Package.Put(Symbol("mine"), Int(1))
	source.Runtime.Package.Export("mine")
	tmpl, err := NewTemplate(source, TemplateWithFrozenPackages("frozen", "big"))
	if err != nil {
		t.Fatal(err)
	}
	return tmpl, fn
}

// TestTemplateFrozenPackageThawMutators drives every Package mutator, with
// and without arguments, at a frozen package: each thaws a private copy for
// its VM and succeeds, and the published base is unchanged afterwards.
func TestTemplateFrozenPackageThawMutators(t *testing.T) {
	tmpl, fn := frozenFixture(t)
	if _, err := NewTemplate(templateOwnershipEnv(), TemplateWithFrozenPackages("missing")); err == nil || !strings.Contains(err.Error(), `frozen package "missing" is not registered`) {
		t.Fatalf("unregistered frozen package accepted: %v", err)
	}
	before := map[string]packageBaseSnapshot{}
	for _, p := range tmpl.plan.packages {
		// A lazy plan gives every package a shared base so its bindings can
		// start unmaterialized; only the named ones are frozen.
		if frozen := p.base != nil && !p.unfrozen; frozen != (p.name == "frozen" || p.name == "big") {
			t.Fatalf("package %s frozen=%v", p.name, frozen)
		}
		if p.base == nil {
			t.Fatalf("package %s has no base in a lazy plan", p.name)
		}
		if p.base != nil {
			before[p.name] = clonePackageBase(p.base)
		}
	}
	mutators := map[string]func(vm *LEnv, p *Package){
		"Put new fn":         func(vm *LEnv, p *Package) { p.Put(Symbol("newfn"), vm.Lambda(Formals(), []*LVal{Int(2)})) },
		"Put new":            func(_ *LEnv, p *Package) { p.Put(Symbol("new"), Int(2)) },
		"PutGlobal":          func(vm *LEnv, _ *Package) { vm.PutGlobal(Symbol("frozen:fresh"), Int(3)) },
		"setSymbolDoc":       func(_ *LEnv, p *Package) { p.setSymbolDoc("value", "vm doc") },
		"setSymbolDoc empty": func(_ *LEnv, p *Package) { p.setSymbolDoc("", "") },
		"Exports one":        func(_ *LEnv, p *Package) { p.Exports("alpha") },
		"Exports many":       func(_ *LEnv, p *Package) { p.Exports("b", "a") },
		"Exports none":       func(_ *LEnv, p *Package) { p.Exports() },
		"Export":             func(_ *LEnv, p *Package) { p.Export("appended") },
		"Export none":        func(_ *LEnv, p *Package) { p.Export() },
		"appendExternal":     func(_ *LEnv, p *Package) { p.appendExternal("more") },
		"putName":            func(_ *LEnv, p *Package) { p.putName("more", Int(1)) },
		"AddBuiltins": func(vm *LEnv, _ *Package) {
			vm.InPackage(String("frozen"))
			vm.AddBuiltins(true, testBuiltin{"tb"})
			vm.InPackage(String("user"))
		},
		"UsePackage": func(vm *LEnv, _ *Package) {
			vm.InPackage(String("frozen"))
			vm.UsePackage(String("user"))
			vm.InPackage(String("user"))
		},
	}
	for name, mutate := range mutators {
		vm, err := tmpl.NewVM()
		if err != nil {
			t.Fatal(err)
		}
		p := vm.Runtime.Registry.Package("frozen")
		if !p.Frozen() || p.symbols != nil {
			t.Fatal("fresh VM package does not read the shared base")
		}
		mutate(vm, p)
		if p.Frozen() || p.symbols == nil || p.funNames == nil {
			t.Fatalf("%s: package did not thaw", name)
		}
		if f, _ := p.Symbol("fn"); f == fn || p.GetFunName(f.FID()) != "fn" {
			t.Fatalf("%s: thaw lost function naming or isolation", name)
		}
		if name != "setSymbolDoc" && p.SymbolDoc("value") != "original doc" {
			t.Fatalf("%s: thaw lost symbol docs", name)
		}
		if !vm.Runtime.Registry.Package("big").Frozen() {
			t.Fatalf("%s: an unrelated package thawed", name)
		}
		if other, _ := tmpl.NewVM(); !other.Runtime.Registry.Package("frozen").Frozen() {
			t.Fatalf("%s: a later VM starts thawed", name)
		}
	}
	for _, p := range tmpl.plan.packages {
		if p.base != nil && !reflect.DeepEqual(before[p.name], clonePackageBase(p.base)) {
			t.Fatalf("package %s: published base was written", p.name)
		}
	}
	if !slices.Equal(before["frozen"].externals, []string{"zeta", "value"}) {
		t.Fatalf("shared export list reordered: %v", before["frozen"].externals)
	}
}

type testBuiltin struct{ name string }

func (b testBuiltin) Name() string                     { return b.name }
func (b testBuiltin) Formals() *LVal                   { return Formals() }
func (b testBuiltin) Eval(env *LEnv, args *LVal) *LVal { return Nil() }
func (b testBuiltin) Docstring() string                { return "test builtin" }

// thawBytes reports the bytes one NewVM plus one write to the named frozen
// package allocates beyond a bare NewVM, averaged over n runs.
func thawBytes(tmpl *Template, name string, n int) float64 {
	measure := func(write bool) float64 {
		var before, after runtime.MemStats
		runtime.GC()
		runtime.ReadMemStats(&before)
		for range n {
			vm, _ := tmpl.NewVM()
			if write {
				vm.Runtime.Registry.Package(name).Put(Symbol("x"), Int(1))
			}
		}
		runtime.ReadMemStats(&after)
		return float64(after.TotalAlloc-before.TotalAlloc) / float64(n)
	}
	return measure(true) - measure(false)
}

// A VM that writes one frozen package pays for that package only: thawing a
// 3-symbol package beside a 500-symbol frozen package costs a few hundred
// bytes, while thawing the big one costs in proportion to its size.
func TestTemplateFrozenPackageThawCostIsPerPackage(t *testing.T) {
	tmpl, _ := frozenFixture(t)
	small, big := thawBytes(tmpl, "frozen", 200), thawBytes(tmpl, "big", 200)
	if small > 2048 {
		t.Fatalf("thawing a 3-symbol package cost %.0f bytes", small)
	}
	if big < 10*small || big < 8192 {
		t.Fatalf("thaw cost is not per package: small=%.0f big=%.0f bytes", small, big)
	}
}
