// Copyright © 2026 The ELPS authors

package lisp

import (
	"math/rand"
	"strconv"
	"testing"
)

// firstNameOfScan is FirstNameOf as it was before the FID index
// (luthersystems/elps#847): a read of every binding.
func firstNameOfScan(pkg *Package, fn *LVal) (string, bool) {
	fd, ok := fn.Native.(*funData)
	if !ok {
		return "", false
	}
	best, found := "", false
	for _, name := range pkg.SymbolNames() {
		if pkg.bindingIsFun(name, fd) && (!found || name < best) {
			best, found = name, true
		}
	}
	return best, found
}

// boundToFID counts the names pkg binds to a function with the FID fid,
// by a read of every binding.
func boundToFID(pkg *Package, fid string) int {
	n := 0
	for _, name := range pkg.SymbolNames() {
		if f, ok := identityFID(pkg.bindingIdentity(name)); ok && f == fid {
			n++
		}
	}
	return n
}

// checkFIDIndex checks FunNameByFID and FirstNameOf against a scan of pkg
// for each function in fns, and that neither builds a lazy value.
func checkFIDIndex(t *testing.T, vm *LEnv, pkg *Package, fns []*LVal) {
	t.Helper()
	before := 0
	if lazyInstanceOf(vm) != nil {
		before = lazyInstanceOf(vm).count
	}
	scan, _ := pkg.FunNamesByFID()
	for _, fn := range fns {
		fid := fn.FID()
		name, ok, read := pkg.funNameByFID(fid)
		if want, wantOK := scan[fid]; name != want || ok != wantOK {
			t.Fatalf("FunNameByFID(%s) = %q %v, scan %q %v", fid, name, ok, want, wantOK)
		}
		if n := boundToFID(pkg, fid); read > n {
			t.Fatalf("FunNameByFID(%s) read %d bindings, %d are bound to it", fid, read, n)
		}
		name, ok, read = pkg.FirstNameOf(fn)
		if want, wantOK := firstNameOfScan(pkg, fn); name != want || ok != wantOK {
			t.Fatalf("FirstNameOf(%s) = %q %v, scan %q %v", fid, name, ok, want, wantOK)
		}
		if n := boundToFID(pkg, fid); read > n {
			t.Fatalf("FirstNameOf(%s) read %d bindings, %d are bound to it", fid, read, n)
		}
	}
	if lazyInstanceOf(vm) != nil {
		if after := lazyInstanceOf(vm).count; after != before {
			t.Fatalf("the index lookups materialized %d values", after-before)
		}
	}
}

// TestFIDIndexMatchesScan rebinds names of a package at random, on a cold
// package and on template VMs of each kind (eager, lazy, frozen, thawed,
// admitted), and checks the FID index against a read of every binding
// after each write (luthersystems/elps#847).
func TestFIDIndexMatchesScan(t *testing.T) {
	source := templateOwnershipEnv()
	lib := source.Runtime.Registry.DefinePackage("lib")
	user := source.Runtime.Package
	source.Runtime.Package = lib
	var fns []*LVal
	for n := range 6 {
		fns = append(fns, source.Lambda(Formals(), []*LVal{Int(n)}))
	}
	source.Runtime.Package = user
	fns = append(fns, source.Lambda(Formals(), []*LVal{Int(99)})) // not lib's
	names := make([]string, 30)
	for i := range names {
		names[i] = "n" + strconv.Itoa(i)
	}
	rng := rand.New(rand.NewSource(847)) //nolint:gosec // a fixed seed makes the oracle comparison reproducible; nothing here is security-sensitive
	bind := func(pkg *Package) {
		name := names[rng.Intn(len(names))]
		switch r := rng.Intn(10); {
		case r < 7:
			fn := fns[rng.Intn(len(fns))]
			if rng.Intn(2) == 0 {
				fn = fn.Copy() // a header copy is the same function
			}
			pkg.Put(Symbol(name), fn)
		default:
			pkg.Put(Symbol(name), Int(r))
		}
	}
	for range 60 {
		bind(lib)
		checkFIDIndex(t, source, lib, fns)
	}
	for range 100 {
		lib.Put(Symbol("pad"+strconv.Itoa(rng.Int())), String("x"))
	}
	checkFIDIndex(t, source, lib, fns)

	cases := []struct {
		name string
		opts []TemplateOption
	}{
		{"lazy", nil},
		{"lazy-frozen", []TemplateOption{TemplateWithFrozenPackages("lib")}},
		{"eager", []TemplateOption{TemplateWithEagerInstantiation()}},
		{"eager-frozen", []TemplateOption{TemplateWithEagerInstantiation(), TemplateWithFrozenPackages("lib")}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			tmpl, err := NewTemplate(source, tc.opts...)
			if err != nil {
				t.Fatal(err)
			}
			vm, err := tmpl.NewVM()
			if err != nil {
				t.Fatal(err)
			}
			pkg := vm.Runtime.Registry.Package("lib")
			// The VM's own function values: each binding of a source
			// function holds the VM's copy of it.
			vmFns := make([]*LVal, 0, len(fns))
			for _, fn := range fns {
				if name, ok := firstNameOfScan(lib, fn); ok {
					vmFns = append(vmFns, pkg.Get(Symbol(name)))
				}
			}
			checkFIDIndex(t, vm, pkg, vmFns)
			// Rebind existing names (slot writes while frozen), then a new
			// name (thaws), then more of both.
			for i := range 80 {
				if i == 40 {
					pkg.Put(Symbol("brand-new"), vmFns[0])
				}
				name := names[rng.Intn(len(names))]
				if rng.Intn(3) == 0 {
					pkg.Put(Symbol(name), Int(i))
				} else {
					pkg.Put(Symbol(name), vmFns[rng.Intn(len(vmFns))])
				}
				checkFIDIndex(t, vm, pkg, vmFns)
			}
			// An admitted snapshot keeps the index.
			reg := NewRegistry()
			reg.AddPackage(pkg)
			checkFIDIndex(t, vm, reg.Package("lib"), vmFns)
		})
	}
}

// TestFIDIndexReadDoesNotGrow pins that a lookup reads the names of one
// function, whatever the size of the package (luthersystems/elps#847).
func TestFIDIndexReadDoesNotGrow(t *testing.T) {
	for _, size := range []int{10, 1000, 10000} {
		env := templateOwnershipEnv()
		pkg := env.Runtime.Package
		fn := env.Lambda(Formals(), []*LVal{Int(1)})
		pkg.Put(Symbol("f"), fn)
		pkg.Put(Symbol("g"), fn)
		for i := range size {
			pkg.Put(Symbol("x"+strconv.Itoa(i)), env.Lambda(Formals(), []*LVal{Int(i)}))
		}
		if name, ok, read := pkg.funNameByFID(fn.FID()); name != "f" || !ok || read != 1 {
			t.Fatalf("size %d: FunNameByFID = %q %v read %d", size, name, ok, read)
		}
		if name, ok, read := pkg.FirstNameOf(fn); name != "f" || !ok || read != 1 {
			t.Fatalf("size %d: FirstNameOf = %q %v read %d", size, name, ok, read)
		}
		pkg.Put(Symbol("f"), Int(0))
		if name, ok, read := pkg.FirstNameOf(fn); name != "g" || !ok || read != 1 {
			t.Fatalf("size %d after rebinding: FirstNameOf = %q %v read %d", size, name, ok, read)
		}
	}
}
