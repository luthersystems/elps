// Copyright © 2026 The ELPS authors

package lisp

import "testing"

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
