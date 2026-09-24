// Copyright © 2026 The ELPS authors

package lisp

import (
	"slices"
	"testing"
)

func TestFrozenPackageReadCopies(t *testing.T) {
	source := templateOwnershipEnv()
	p := source.Runtime.Registry.DefinePackage("frozen")
	fn := source.Lambda(Formals(), []*LVal{Int(1)})
	p.Put(Symbol("fn"), fn)
	p.setSymbolDoc("fn", "original")
	p.Export("z", "fn")
	tmpl, err := NewTemplate(source, TemplateWithFrozenPackages("frozen"))
	if err != nil {
		t.Fatal(err)
	}
	first, err := tmpl.NewVM()
	if err != nil {
		t.Fatal(err)
	}
	p = first.Runtime.Registry.Package("frozen")
	p.funNameTable()[fn.FID()] = "corrupted"
	p.symbolDocTable()["fn"] = "corrupted"
	p.symbolTable()["fn"] = Int(9)
	names := p.SymbolNames()
	names[0] = "corrupted"
	exports := p.Externals()
	exports[0] = "corrupted"
	second, err := tmpl.NewVM()
	if err != nil {
		t.Fatal(err)
	}
	for _, vm := range []*LEnv{first, second} {
		p := vm.Runtime.Registry.Package("frozen")
		if p.GetFunName(fn.FID()) != "fn" || p.SymbolDoc("fn") != "original" {
			t.Error("copy-out maps changed shared tables")
		}
		if v, _ := p.Symbol("fn"); v.Type != LFun || !slices.Equal(p.SymbolNames(), []string{"fn"}) {
			t.Error("copy-out symbol table changed bindings")
		}
		if !slices.Equal(p.Externals(), []string{"z", "fn"}) || p.NumExternals() != 2 {
			t.Error("copy-out slice changed exports")
		}
	}
}

// Admission and republication must read frozen exports through the base: the
// Package's own externals slice is deliberately nil.
func TestFrozenPackageReadmission(t *testing.T) {
	source := templateOwnershipEnv()
	p := source.Runtime.Registry.DefinePackage("frozen")
	p.Put(Symbol("value"), Int(1))
	p.setSymbolDoc("value", "doc")
	p.Export("value", "value")
	tmpl, err := NewTemplate(source, TemplateWithFrozenPackages("frozen"))
	if err != nil {
		t.Fatal(err)
	}
	vm, err := tmpl.NewVM()
	if err != nil {
		t.Fatal(err)
	}
	frozen := vm.Runtime.Registry.Package("frozen")
	registry := NewRegistry()
	if !registry.AddPackage(frozen) {
		t.Fatal("admission failed")
	}
	admitted := registry.Package("frozen")
	if admitted.Frozen() || !slices.Equal(admitted.Externals(), []string{"value", "value"}) || admitted.SymbolDoc("value") != "doc" {
		t.Fatal("admission lost frozen package metadata")
	}
	admitted.Export("private")
	admitted.setSymbolDoc("value", "private")
	for _, freeze := range []bool{false, true} {
		var opts []TemplateOption
		if freeze {
			opts = append(opts, TemplateWithFrozenPackages("frozen"))
		}
		child, err := NewTemplate(vm, opts...)
		if err != nil {
			t.Fatal(err)
		}
		grandchild, err := child.NewVM()
		if err != nil {
			t.Fatal(err)
		}
		pkg := grandchild.Runtime.Registry.Package("frozen")
		if pkg.Frozen() != freeze || pkg.SymbolDoc("value") != "doc" || !slices.Equal(pkg.Externals(), []string{"value", "value"}) {
			t.Fatal("republication lost or aliased frozen package metadata")
		}
	}
}
