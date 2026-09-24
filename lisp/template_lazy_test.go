// Copyright © 2026 The ELPS authors

package lisp

import (
	"runtime"
	"strconv"
	"testing"
	"weak"
)

// lazyFixture publishes a graph with sharing and a cycle:
//
//	user:shared  = (1 2)
//	user:a       = {"x": shared, "y": shared, "self": a, "inner": {"z": shared}}
//	user:b       = shared
//	lib:fn       = a closure; lib:v0..v199 scalars in lists
func lazyFixture(t *testing.T, opts ...TemplateOption) *Template {
	t.Helper()
	source := templateOwnershipEnv()
	user := source.Runtime.Package
	shared := QExpr([]*LVal{Int(1), Int(2)})
	a := SortedMap()
	inner := SortedMap()
	inner.MapSet("z", shared)
	a.MapSet("x", shared)
	a.MapSet("y", shared)
	a.MapSet("inner", inner)
	a.MapSet("self", a)
	user.Put(Symbol("shared"), shared)
	user.Put(Symbol("a"), a)
	user.Put(Symbol("b"), shared)
	lib := source.Runtime.Registry.DefinePackage("lib")
	lib.Put(Symbol("fn"), source.Lambda(Formals(), []*LVal{Int(1)}))
	for n := range 200 {
		lib.Put(Symbol("v"+strconv.Itoa(n)), QExpr([]*LVal{Int(n), String("s")}))
	}
	tmpl, err := NewTemplate(source, append([]TemplateOption{TemplateWithFrozenPackages("lib")}, opts...)...)
	if err != nil {
		t.Fatal(err)
	}
	return tmpl
}

func lazyInstanceOf(vm *LEnv) *lazyInstance {
	for _, pkg := range vm.Runtime.Registry.packages {
		if pkg.lazy != nil {
			return pkg.lazy.inst
		}
	}
	return nil
}

func TestTemplateLazyIdentityAndSharing(t *testing.T) {
	for _, mode := range []string{"lazy", "eager"} {
		t.Run(mode, func(t *testing.T) {
			var opts []TemplateOption
			if mode == "eager" {
				opts = append(opts, TemplateWithEagerInstantiation())
			}
			vm, err := lazyFixture(t, opts...).NewVM()
			if err != nil {
				t.Fatal(err)
			}
			user := vm.Runtime.Registry.Package("user")
			if user.Frozen() {
				t.Fatal("an unfrozen package reports Frozen")
			}
			// Reach the shared list through the map first, then the slots.
			a, _ := user.Symbol("a")
			x := a.MapGet("x")
			if a.MapGet("y") != x || a.MapGet("inner").MapGet("z") != x {
				t.Fatal("shared value materialized twice through one map")
			}
			if a.MapGet("self") != a {
				t.Fatal("cycle not closed")
			}
			b, _ := user.Symbol("b")
			shared, _ := user.Symbol("shared")
			if b != x || shared != x {
				t.Fatal("slot and map entry disagree on a shared value")
			}
			if again, _ := user.Symbol("a"); again != a {
				t.Fatal("slot identity unstable")
			}
			// Mutation is visible through every path, and only in this VM.
			x.Cells[0] = Int(99) //elps:mutates test mutates its own VM's private list
			other, err := lazyFixture(t, opts...).NewVM()
			if err != nil {
				t.Fatal(err)
			}
			if b.Cells[0].Int != 99 {
				t.Fatal("mutation not shared within the VM")
			}
			ob, _ := other.Runtime.Registry.Package("user").Symbol("b")
			if ob.Cells[0].Int != 1 {
				t.Fatal("mutation leaked across VMs")
			}
		})
	}
}

// TestTemplateLazyMaterializesOnlyWhatIsReached pins the point of laziness.
func TestTemplateLazyMaterializesOnlyWhatIsReached(t *testing.T) {
	vm, err := lazyFixture(t).NewVM()
	if err != nil {
		t.Fatal(err)
	}
	inst := lazyInstanceOf(vm)
	if inst == nil {
		t.Fatal("no lazy instance")
	}
	total := len(inst.p.values)
	if inst.count != 0 {
		t.Fatalf("NewVM materialized %d of %d values", inst.count, total)
	}
	lib := vm.Runtime.Registry.Package("lib")
	v7, _ := lib.Symbol("v7")
	if v7.Cells[0].Int != 7 {
		t.Fatalf("v7 = %v", v7)
	}
	if inst.count > 4 {
		t.Fatalf("one slot materialized %d of %d values", inst.count, total)
	}
	lib.symbolTable()
	if lib.lazy != nil {
		t.Fatal("a fully materialized package kept its lazy link")
	}
	eager, err := lazyFixture(t, TemplateWithEagerInstantiation()).NewVM()
	if err != nil {
		t.Fatal(err)
	}
	if lazyInstanceOf(eager) != nil {
		t.Fatal("TemplateWithEagerInstantiation built a lazy VM")
	}
}

// TestTemplateLazyThawKeepsPendingBindings: a write thaws a package without
// materializing its other bindings, and no reader sees the marker.
func TestTemplateLazyThawKeepsPendingBindings(t *testing.T) {
	vm, err := lazyFixture(t).NewVM()
	if err != nil {
		t.Fatal(err)
	}
	lib := vm.Runtime.Registry.Package("lib")
	inst := lib.lazy.inst
	lib.Put(Symbol("v3"), Int(-3)) // overwrite a pending binding
	lib.Put(Symbol("new"), Int(1))
	if lib.Frozen() || lib.base != nil {
		t.Fatal("write did not thaw")
	}
	if inst.count != 0 {
		t.Fatalf("thaw materialized %d values", inst.count)
	}
	if v, _ := lib.Symbol("v3"); v.Int != -3 {
		t.Fatalf("v3 = %v", v)
	}
	if v, _ := lib.Symbol("v4"); v.Type != LSExpr || v.Cells[0].Int != 4 {
		t.Fatalf("v4 = %v", v)
	}
	for name, v := range lib.symbolTable() {
		if v == lazyPending {
			t.Fatalf("%s leaked the pending marker", name)
		}
	}
	if lib.lazy != nil {
		t.Fatal("thawed package kept its lazy link after materializing everything")
	}
}

// TestTemplateLazyMapPendingAccounting: Set and Del of a pending entry, and a
// full sweep, leave no marker and drop the map's lazy link.
func TestTemplateLazyMapPendingAccounting(t *testing.T) {
	vm, err := lazyFixture(t).NewVM()
	if err != nil {
		t.Fatal(err)
	}
	a, _ := vm.Runtime.Registry.Package("user").Symbol("a")
	sm := a.Map().mapBacking.(sortedmap)
	if sm.lz == nil || sm.lz.pending == 0 {
		t.Fatal("map has nothing pending")
	}
	sm.Set(String("x"), Int(1))
	sm.Del(String("inner"))
	buf := make([]*LVal, sm.Len())
	sm.Entries(buf)
	for _, pair := range buf {
		if pair.Cells[1] == lazyPending {
			t.Fatal("Entries leaked the pending marker")
		}
	}
	if sm.lz.pending != 0 || sm.lz.inst != nil {
		t.Fatalf("map kept its lazy link: pending=%d", sm.lz.pending)
	}
}

// TestTemplateLazyDetachParity: detach walks a pending map exactly as it walks
// an eager one.
func TestTemplateLazyDetachParity(t *testing.T) {
	get := func(opts ...TemplateOption) string {
		vm, err := lazyFixture(t, opts...).NewVM()
		if err != nil {
			t.Fatal(err)
		}
		inner, _ := vm.Runtime.Registry.Package("user").Symbol("a")
		d, err := inner.MapGet("inner").detach()
		if err != nil {
			t.Fatal(err)
		}
		return d.String()
	}
	if lazy, eager := get(), get(TemplateWithEagerInstantiation()); lazy != eager {
		t.Fatalf("detach: lazy %s, eager %s", lazy, eager)
	}
}

// TestTemplateLazySlotWrite: rebinding an existing name writes the VM's slot
// (putSlot) without thawing or materializing, in frozen and unfrozen packages.
func TestTemplateLazySlotWrite(t *testing.T) {
	vm, err := lazyFixture(t).NewVM()
	if err != nil {
		t.Fatal(err)
	}
	for _, name := range []string{"lib", "user"} {
		pkg := vm.Runtime.Registry.Package(name)
		inst := pkg.lazy.inst
		before, pending := inst.count, pkg.lazy.pending
		sym := "v5"
		if name == "user" {
			sym = "b"
		}
		pkg.Put(Symbol(sym), Int(-5))
		if pkg.base == nil {
			t.Fatalf("%s: rebinding an existing name thawed", name)
		}
		if inst.count != before {
			t.Fatalf("%s: slot write materialized %d values", name, inst.count-before)
		}
		if pkg.lazy != nil && pkg.lazy.pending != pending-1 {
			t.Fatalf("%s: pending %d, want %d", name, pkg.lazy.pending, pending-1)
		}
		if v, _ := pkg.Symbol(sym); v.Int != -5 {
			t.Fatalf("%s: %s = %v", name, sym, v)
		}
	}
	// The overwritten slot's old value is still shared correctly elsewhere.
	a, _ := vm.Runtime.Registry.Package("user").Symbol("a")
	shared, _ := vm.Runtime.Registry.Package("user").Symbol("shared")
	if a.MapGet("x") != shared {
		t.Fatal("sharing broken after a slot write")
	}
}

// TestTemplateLazyPrewarm: a VM created with VMWithPrewarm builds, at NewVM,
// exactly the values earlier VMs used, with the same identities and sharing,
// and a VM created without it still builds nothing.
func TestTemplateLazyPrewarm(t *testing.T) {
	tmpl := lazyFixture(t)
	first, err := tmpl.NewVM(VMWithPrewarm()) // nothing is hot yet
	if err != nil {
		t.Fatal(err)
	}
	if n := lazyInstanceOf(first).count; n != 0 {
		t.Fatalf("prewarm with an empty hot set built %d values", n)
	}
	lib := first.Runtime.Registry.Package("lib")
	lib.Symbol("v7")
	a, _ := first.Runtime.Registry.Package("user").Symbol("a")
	a.MapGet("x")
	used := lazyInstanceOf(first).count

	warm, err := tmpl.NewVM(VMWithPrewarm())
	if err != nil {
		t.Fatal(err)
	}
	inst := lazyInstanceOf(warm)
	if inst.count != used {
		t.Fatalf("prewarm built %d values, earlier VM used %d", inst.count, used)
	}
	user := warm.Runtime.Registry.Package("user")
	wa, _ := user.Symbol("a")
	shared, _ := user.Symbol("shared")
	if wa.MapGet("x") != shared || wa.MapGet("self") != wa || wa.MapGet("inner").MapGet("z") != shared {
		t.Fatal("prewarm broke identity or sharing")
	}
	if v, _ := warm.Runtime.Registry.Package("lib").Symbol("v7"); v.Cells[0].Int != 7 {
		t.Fatalf("v7 = %v", v)
	}
	if fa, _ := first.Runtime.Registry.Package("user").Symbol("a"); fa == wa {
		t.Fatal("prewarmed VM shares a value with another VM")
	}
	cold, err := tmpl.NewVM()
	if err != nil {
		t.Fatal(err)
	}
	if n := lazyInstanceOf(cold).count; n != 0 {
		t.Fatalf("a VM without VMWithPrewarm built %d values", n)
	}
	eager, err := lazyFixture(t, TemplateWithEagerInstantiation()).NewVM(VMWithPrewarm())
	if err != nil || lazyInstanceOf(eager) != nil {
		t.Fatalf("VMWithPrewarm on an eager template: %v", err)
	}
}

// TestTemplatePackageSlotsDoNotRetainOtherPackages: retaining one package of
// a VM must not keep another package's values alive through a shared slot
// backing array.
func TestTemplatePackageSlotsDoNotRetainOtherPackages(t *testing.T) {
	for _, mode := range []string{"lazy", "lazy-frozen", "eager-frozen"} {
		t.Run(mode, func(t *testing.T) {
			source := templateOwnershipEnv()
			source.Runtime.Registry.DefinePackage("tiny").Put(Symbol("x"), Int(1))
			source.Runtime.Registry.DefinePackage("bulk").Put(Symbol("blob"), Bytes(make([]byte, 8<<20)))
			var opts []TemplateOption
			if mode != "lazy" {
				opts = append(opts, TemplateWithFrozenPackages("tiny", "bulk"))
			}
			if mode == "eager-frozen" {
				opts = append(opts, TemplateWithEagerInstantiation())
			}
			tmpl, err := NewTemplate(source, opts...)
			if err != nil {
				t.Fatal(err)
			}
			tiny, blob := lazySlotRetentionVM(t, tmpl)
			runtime.GC()
			runtime.GC()
			if blob.Value() != nil {
				t.Fatal("retaining one package kept another package's value alive")
			}
			if v, _ := tiny.Symbol("x"); v.Int != 1 {
				t.Fatalf("tiny:x = %v", v)
			}
		})
	}
}

//go:noinline
func lazySlotRetentionVM(t *testing.T, tmpl *Template) (*Package, weak.Pointer[LVal]) {
	vm, err := tmpl.NewVM()
	if err != nil {
		t.Fatal(err)
	}
	tiny := vm.Runtime.Registry.Package("tiny")
	tiny.Symbol("x")
	blob, _ := vm.Runtime.Registry.Package("bulk").Symbol("blob")
	if len(blob.Bytes()) != 8<<20 {
		t.Fatal("bulk not built")
	}
	return tiny, weak.Make(blob)
}
