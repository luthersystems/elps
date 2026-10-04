// Copyright © 2026 The ELPS authors

package lisp

import (
	"fmt"
	"testing"
)

func TestRegisteredBuiltinReplacementOrder(t *testing.T) {
	var r builtinRegistry
	var want []*LVal
	register := func(pkg, name string, shadows bool) {
		t.Helper()
		fn := registrationFunValue(pkg, name, name, LFunNone, Formals(),
			func(*LEnv, *LVal) *LVal { return Nil() }, "")
		for i, old := range want {
			if registrationKey(old) == registrationKey(fn) {
				want = append(want[:i], want[i+1:]...)
				break
			}
		}
		want = append(want, fn)
		r.register(fn, shadows)
	}
	check := func() {
		t.Helper()
		got := r.current()
		if len(got) != len(want) {
			t.Fatalf("got %d registrations, want %d", len(got), len(want))
		}
		for i, fn := range want {
			if got[i] != fn {
				t.Fatalf("registration %d is out of order", i)
			}
			key := registrationKey(fn)
			if r.lookup(key.pkg, key.name) != fn {
				t.Fatalf("lookup returned the wrong function for %v", key)
			}
		}
	}
	register("a", "same", false)
	register("b", "same", false)
	if r.slots != nil || r.ownIndex.Load() != nil {
		t.Fatal("ordinary registration built an index")
	}
	register("a", "same", true)
	check()
	for range 10 {
		register("a", "same", true)
		check()
	}
	indexed := len(r.slots.byKey)
	for i := range 10 * builtinChunkSize {
		register("a", fmt.Sprintf("new-%d", i), false)
	}
	if len(r.slots.byKey) != indexed {
		t.Fatal("ordinary registration updated the slot index")
	}
	register("a", "new-100", true)
	check()
	for range len(want) + 1 {
		register("b", "same", true)
	}
	check()
}

func TestRegisteredBuiltinReplacementSlotsBounded(t *testing.T) {
	for _, indexed := range []bool{false, true} {
		name := "before-first-lookup"
		if indexed {
			name = "after-first-lookup"
		}
		t.Run(name, func(t *testing.T) {
			env := newRegTestEnv(t)
			def := &regFormalsDef{name: "replaced", formals: Formals("x")}
			env.AddBuiltins(false, def)
			reg := env.Runtime.Registry
			pkg := env.Runtime.Package
			live := reg.builtins.numOwn()
			if indexed {
				reg.RegisteredBuiltin(pkg.Name, def.name)
			}
			older := make([]*LVal, 0, 10000)
			for range 10000 {
				old, ok := pkg.Symbol(def.name)
				if !ok {
					t.Fatal("missing binding")
				}
				older = append(older, old)
				if err := GoError(env.BindBuiltins(BindOpts{Shadow: true}, def)); err != nil {
					t.Fatal(err)
				}
				if slots := reg.builtins.numOwn(); slots > 2*live {
					t.Fatalf("registry holds %d slots for %d live entries", slots, live)
				}
			}
			newest, _ := pkg.Symbol(def.name)
			if got := reg.RegisteredBuiltin(pkg.Name, def.name); got != newest {
				t.Fatal("lookup did not return the newest function")
			}
			if _, _, ok := reg.RegisteredBuiltinName(newest); !ok {
				t.Fatal("newest function is not registered")
			}
			for i, old := range older {
				if _, _, ok := reg.RegisteredBuiltinName(old); ok {
					t.Fatalf("generation %d is still registered", i)
				}
			}
		})
	}
}

// Every VM a template mints shares the source's registration records, so a
// registered builtin is identified by the same record everywhere.  Each VM
// still has its own function value.
func TestRegistrationRecordSharedByForks(t *testing.T) {
	env := newRegTestEnv(t)
	env.AddBuiltins(true, &regFormalsDef{name: "shared-rec", formals: Formals("x")})
	reg := env.Runtime.Registry
	orig := reg.RegisteredBuiltin(env.Runtime.Package.Name, "shared-rec")
	if orig == nil || orig.funData().reg == nil {
		t.Fatal("registration made no record")
	}
	rec := orig.funData().reg
	// Rebind the name, so only the registry holds the builtin.
	env.Runtime.Package.Put(Symbol("shared-rec"), Int(1))
	approve := TemplateWithBuiltinPolicy(func(*LVal) bool { return true })
	for _, eager := range []bool{false, true} {
		opts := []TemplateOption{approve}
		if eager {
			opts = append(opts, TemplateWithEagerInstantiation())
		}
		tmpl, err := NewTemplate(env, opts...)
		if err != nil {
			t.Fatal(err)
		}
		for _, vmOpts := range [][]VMOption{nil, {VMWithPrewarm()}} {
			vm, err := tmpl.NewVM(vmOpts...)
			if err != nil {
				t.Fatal(err)
			}
			got := vm.Runtime.Registry.RegisteredBuiltin(env.Runtime.Package.Name, "shared-rec")
			if got == nil {
				t.Fatalf("eager=%v: the VM lost the registry entry", eager)
			}
			if got == orig || got.funData() == orig.funData() {
				t.Errorf("eager=%v: the VM shares the source's function value", eager)
			}
			if got.funData().reg != rec {
				t.Errorf("eager=%v: the VM's record is not the source's", eager)
			}
		}
	}
}
