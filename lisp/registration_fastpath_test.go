// Copyright © 2026 The ELPS authors

package lisp

import (
	"strings"
	"testing"
)

// Tests pinning the observable behavior of the registration fast path
// (registrationBound, registrationFunValue, Package.putName in env.go and
// package.go): the same function identities, documentation, duplicate
// detection and constant protection the pkg.Get / public-constructor /
// pkg.Put spelling provided, without the per-definition throwaway
// allocations.  These assertions existed nowhere else — the FID rendering
// and the duplicate panic were previously exercised only incidentally.

type regDocDef struct {
	regFormalsDef
	doc string
}

func (d *regDocDef) Docstring() string { return d.doc }

func newRegTestEnv(t *testing.T) *LEnv {
	t.Helper()
	env := NewEnv(nil)
	if rc := InitializeUserEnv(env); rc.Type == LError {
		t.Fatalf("InitializeUserEnv: %v", rc)
	}
	return env
}

// TestRegistrationFunValueIdentity: registration produces the same function
// identity surface as before — FID rendering per definition kind, package
// attribution, FunType, docstring in Cells[1], and the funNames bookkeeping
// pkg.Put performed.
func TestRegistrationFunValueIdentity(t *testing.T) {
	env := newRegTestEnv(t)
	pkg := env.Runtime.Package

	env.AddBuiltins(true, &regDocDef{regFormalsDef{name: "fastpath-fn", formals: Formals("x")}, "fn doc"})
	env.AddMacros(true, &regDocDef{regFormalsDef{name: "fastpath-mac", formals: Formals("x")}, "mac doc"})
	env.AddSpecialOps(true, &regDocDef{regFormalsDef{name: "fastpath-op", formals: Formals("x")}, "op doc"})

	check := func(name, wantFID string, wantType LFunType, wantDoc string) {
		t.Helper()
		v, ok := pkg.Symbol(name)
		if !ok || v.Type != LFun {
			t.Fatalf("%s did not register as a function: %v", name, v)
		}
		if v.FID() != wantFID {
			t.Errorf("%s FID = %q, want %q", name, v.FID(), wantFID)
		}
		if v.FunType != wantType {
			t.Errorf("%s FunType = %v, want %v", name, v.FunType, wantType)
		}
		if v.Package() != pkg.Name {
			t.Errorf("%s package = %q, want %q", name, v.Package(), pkg.Name)
		}
		if len(v.Cells) < 2 || v.Cells[1].Type != LString || v.Cells[1].Str != wantDoc {
			t.Errorf("%s docstring cell = %v, want %q", name, v.Cells[1], wantDoc)
		}
		if got := pkg.GetFunName(wantFID); got != name {
			t.Errorf("GetFunName(%q) = %q, want %q (putName must keep the funNames bookkeeping)", wantFID, got, name)
		}
	}
	check("fastpath-fn", "<builtin-function ``fastpath-fn''>", LFunNone, "fn doc")
	check("fastpath-mac", "<builtin-macro ``fastpath-mac''>", LFunMacro, "mac doc")
	check("fastpath-op", "<special-op ``fastpath-op''>", LFunSpecialOp, "op doc")
}

// TestRegistrationDuplicatePanics: binding one name twice still panics, with
// the same messages the pkg.Get probe produced.
func TestRegistrationDuplicatePanics(t *testing.T) {
	mustPanic := func(name, want string, f func()) {
		t.Helper()
		defer func() {
			r := recover()
			if r == nil {
				t.Fatalf("%s: expected panic", name)
			}
			msg, ok := r.(string)
			if !ok {
				t.Fatalf("%s: panic is %T, want string: %v", name, r, r)
			}
			if !strings.Contains(msg, want) {
				t.Fatalf("%s: panic %q does not contain %q", name, msg, want)
			}
		}()
		f()
	}

	env := newRegTestEnv(t)
	env.AddBuiltins(false, &regFormalsDef{name: "fastpath-dup", formals: Formals()})
	mustPanic("builtin duplicate", "symbol already defined: fastpath-dup", func() {
		env.AddBuiltins(false, &regFormalsDef{name: "fastpath-dup", formals: Formals()})
	})

	env2 := newRegTestEnv(t)
	env2.AddMacros(false, &regFormalsDef{name: "fastpath-dup-mac", formals: Formals()})
	mustPanic("macro duplicate", "macro already defined: fastpath-dup-mac", func() {
		env2.AddMacros(false, &regFormalsDef{name: "fastpath-dup-mac", formals: Formals()})
	})

	env3 := newRegTestEnv(t)
	env3.AddSpecialOps(false, &regFormalsDef{name: "fastpath-dup-op", formals: Formals()})
	mustPanic("special-op duplicate", "macro already defined: fastpath-dup-op", func() {
		env3.AddSpecialOps(false, &regFormalsDef{name: "fastpath-dup-op", formals: Formals()})
	})
}

// TestRegistrationConstantsProbeAsBound: the boolean constants are never in
// the package's symbol table — pkg.Get resolves them specially — so a raw
// table probe would report them unbound and a definition named "true" would
// silently fail to bind (Put rejects the constants, and registration does
// not check Put's error).  registrationBound makes them probe as bound, so
// the registration panics exactly as the pkg.Get spelling did.
func TestRegistrationConstantsProbeAsBound(t *testing.T) {
	for _, name := range []string{TrueSymbol, FalseSymbol} {
		v, bound := registrationBound(NewPackage("fastpath-test"), name)
		if !bound {
			t.Fatalf("%s must probe as bound", name)
		}
		if v.Type != LSymbol || v.Str != name {
			t.Fatalf("%s probes as %v, want the symbol itself", name, v)
		}
		env := newRegTestEnv(t)
		func() {
			defer func() {
				if recover() == nil {
					t.Fatalf("registering a builtin named %q must panic as a duplicate", name)
				}
			}()
			env.AddBuiltins(false, &regFormalsDef{name: name, formals: Formals()})
		}()
	}
}
