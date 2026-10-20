// Copyright © 2026 The ELPS authors

package lisp

import (
	"math/rand"
	"strconv"
	"sync"
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

// TestFIDIndexInsertSharesSpare pins that a run of first names for new FIDs
// allocates once per spare block, not once per name, that blocks grow up
// to fidSpareBlock, and that no list taken from a block can reach another
// list by append (luthersystems/elps#855).
func TestFIDIndexInsertSharesSpare(t *testing.T) {
	// Blocks of 2, 4, 8, 16, 32 and 32 back 94 names.
	names := make([]string, 94)
	for i := range names {
		names[i] = "n" + strconv.Itoa(i)
	}
	allocs := testing.AllocsPerRun(100, func() {
		var idx fidIndex
		for _, name := range names {
			_ = idx.insert(nil, name)
		}
	})
	if allocs > 6 {
		t.Fatalf("%d first names allocated %v times, want at most 6", len(names), allocs)
	}
	var idx fidIndex
	lists := make([][]string, len(names))
	for i, name := range names {
		lists[i] = idx.insert(nil, name)
	}
	if cap(idx.spare) != fidSpareBlock {
		t.Fatalf("last block holds %d names, want %d", cap(idx.spare), fidSpareBlock)
	}
	for i, l := range lists {
		if len(l) != 1 || cap(l) != 1 || l[0] != names[i] {
			t.Fatalf("list %d = %q (cap %d), want [%s]", i, l, cap(l), names[i])
		}
	}
	a, b := lists[0], lists[1]
	if grown := append(a, "z"); grown[0] != "n0" || b[0] != "n1" {
		t.Fatalf("an append to one list changed another: %q %q", grown, b)
	}
	if got := idx.insert(a, "0"); len(got) != 2 || got[0] != "0" || got[1] != "n0" || a[0] != "n0" {
		t.Fatalf("insert into a one-name list = %q, list %q", got, a)
	}
}

// TestFIDIndexUsePackageAfterThaw pins that putName logs the 64 imports of
// a use-package into a thawed package and the first lookup applies them,
// taking the first names from the index's spare blocks
// (luthersystems/elps#855): blocks of 2, 4, 8, 16 and 32 and 2 names of the
// next one.  The index still matches a scan of the bindings.
func TestFIDIndexUsePackageAfterThaw(t *testing.T) {
	source := templateOwnershipEnv()
	user := source.Runtime.Package
	lib := source.Runtime.Registry.DefinePackage("funs")
	source.Runtime.Package = lib
	for i := range 64 {
		name := "funs" + strconv.Itoa(i)
		lib.Put(Symbol(name), source.Lambda(Formals(), []*LVal{Int(i)}))
		lib.Export(name)
	}
	source.Runtime.Package = user
	tmpl, err := NewTemplate(source, TemplateWithFrozenPackages("user", "funs"))
	if err != nil {
		t.Fatal(err)
	}
	vm, err := tmpl.NewVM()
	if err != nil {
		t.Fatal(err)
	}
	vm.UsePackage(Symbol("funs"))
	pkg := vm.Runtime.Registry.Package("user")
	if pkg.Frozen() || pkg.fids == nil {
		t.Fatal("use-package did not thaw the package and keep its index")
	}
	var fns []*LVal
	for i := range 64 {
		fns = append(fns, pkg.Get(Symbol("funs"+strconv.Itoa(i))))
	}
	if n := len(pkg.fids.pending); n != 64 {
		t.Fatalf("index holds %d pending writes after use-package, want 64", n)
	}
	checkFIDIndex(t, vm, pkg, fns)
	idx := pkg.loadFIDs()
	if n := len(idx.pending); n != 0 {
		t.Fatalf("index holds %d pending writes after a lookup, want 0", n)
	}
	if got := len(idx.spare); cap(idx.spare) != fidSpareBlock || got != 2 {
		t.Fatalf("spare block holds %d of %d names, want 2 of %d", got, cap(idx.spare), fidSpareBlock)
	}
}

// TestFIDIndexPendingRuns rebinds names of a package many times between
// lookups, on a cold package and on a thawed template VM, so the index
// applies runs of pending writes of every length, past fidPendingMax
// included, and checks it against a scan after each run
// (luthersystems/elps#855).
func TestFIDIndexPendingRuns(t *testing.T) {
	source := templateOwnershipEnv()
	lib := source.Runtime.Registry.DefinePackage("lib")
	user := source.Runtime.Package
	source.Runtime.Package = lib
	var fns []*LVal
	for n := range 6 {
		fns = append(fns, source.Lambda(Formals(), []*LVal{Int(n)}))
	}
	source.Runtime.Package = user
	names := make([]string, 40)
	for i := range names {
		names[i] = "n" + strconv.Itoa(i)
	}
	rng := rand.New(rand.NewSource(855)) //nolint:gosec // a fixed seed makes the oracle comparison reproducible; nothing here is security-sensitive
	run := func(t *testing.T, vm *LEnv, pkg *Package, fns []*LVal, n int) {
		t.Helper()
		for range n {
			name := names[rng.Intn(len(names))]
			if rng.Intn(4) == 0 {
				pkg.Put(Symbol(name), Int(1))
			} else {
				pkg.Put(Symbol(name), fns[rng.Intn(len(fns))])
			}
		}
		if idx := pkg.loadFIDs(); idx != nil && len(idx.pending) >= fidPendingMax {
			t.Fatalf("index holds %d pending writes, more than %d", len(idx.pending), fidPendingMax)
		}
		checkFIDIndex(t, vm, pkg, fns)
	}
	checkFIDIndex(t, source, lib, fns) // builds the cold index
	for _, n := range []int{1, 2, 7, 100, fidPendingMax - 1, fidPendingMax, fidPendingMax + 1, 3*fidPendingMax + 5} {
		run(t, source, lib, fns, n)
	}

	tmpl, err := NewTemplate(source, TemplateWithFrozenPackages("lib"))
	if err != nil {
		t.Fatal(err)
	}
	vm, err := tmpl.NewVM()
	if err != nil {
		t.Fatal(err)
	}
	pkg := vm.Runtime.Registry.Package("lib")
	pkg.Put(Symbol("thaw"), Int(0))
	if pkg.Frozen() {
		t.Fatal("a new name did not thaw the package")
	}
	vmFns := make([]*LVal, 0, len(fns))
	for _, fn := range fns {
		if name, ok := firstNameOfScan(lib, fn); ok {
			vmFns = append(vmFns, pkg.Get(Symbol(name)))
		}
	}
	for _, n := range []int{1, 50, fidPendingMax + 3} {
		run(t, vm, pkg, vmFns, n)
	}
}

// TestFIDIndexPendingConcurrentReaders looks up a thawed package from many
// goroutines while its index holds pending writes, so the readers race to
// apply them; run under -race it pins that no reader writes an index
// another can see (issue #397).
func TestFIDIndexPendingConcurrentReaders(t *testing.T) {
	source := templateOwnershipEnv()
	user := source.Runtime.Package
	lib := source.Runtime.Registry.DefinePackage("funs")
	source.Runtime.Package = lib
	for i := range 64 {
		name := "funs" + strconv.Itoa(i)
		lib.Put(Symbol(name), source.Lambda(Formals(), []*LVal{Int(i)}))
		lib.Export(name)
	}
	source.Runtime.Package = user
	tmpl, err := NewTemplate(source, TemplateWithFrozenPackages("user", "funs"))
	if err != nil {
		t.Fatal(err)
	}
	vm, err := tmpl.NewVM()
	if err != nil {
		t.Fatal(err)
	}
	vm.UsePackage(Symbol("funs"))
	pkg := vm.Runtime.Registry.Package("user")
	if pkg.fids == nil || len(pkg.fids.pending) == 0 {
		t.Fatal("use-package left no pending writes")
	}
	fn := pkg.Get(Symbol("funs7"))
	var wg sync.WaitGroup
	errs := make(chan string, 8)
	for range 8 {
		wg.Add(1)
		go func() {
			defer wg.Done()
			if name, ok, _ := pkg.FirstNameOf(fn); !ok || name != "funs7" {
				errs <- name
			}
		}()
	}
	wg.Wait()
	close(errs)
	for name := range errs {
		t.Fatalf("FirstNameOf = %q, want funs7", name)
	}
}
