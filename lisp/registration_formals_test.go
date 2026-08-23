// Copyright © 2026 The ELPS authors

package lisp

import (
	"testing"
)

// Unit tests for registrationFormals (env.go), the switch point between the
// two mechanisms that make process-wide definition tables safe to register
// from (issues #363, #379, #513, #514):
//
//   - a SEALED template is aliased into the environment — sharing is safe
//     because sealed bytes never change (copy-on-write guards, the -race
//     seal watchdog, the checked-mode fingerprint verifier);
//   - an UNSEALED formals list gets a private deep copy via formalsCopier,
//     so no two environments ever share mutable formals.
//
// The elpscheck-tagged red-proof (registration_formals_elpscheck_test.go)
// and the cross-environment censuses (lisp/shared_formals_test.go,
// lisp/lisplib/formals_test.go) prove the composition end to end; these
// tests pin the switch itself, including the fallback edges.

type regFormalsDef struct {
	name    string
	formals *LVal
}

func (d *regFormalsDef) Name() string                     { return d.name }
func (d *regFormalsDef) Formals() *LVal                   { return d.formals }
func (d *regFormalsDef) Eval(env *LEnv, args *LVal) *LVal { return Nil() }
func (d *regFormalsDef) defs() []LBuiltinDef              { return []LBuiltinDef{d} }

// TestRegistrationFormalsAliasesSealed: a sealed template comes back as the
// SAME pointer — no copy, no block carve — and stays sealed.
func TestRegistrationFormalsAliasesSealed(t *testing.T) {
	formals := Formals("a", "b", OptArgSymbol, "opt")
	formals.SealAST()
	def := &regFormalsDef{name: "reg-formals-sealed", formals: formals}

	c := newFormalsCopier(def.defs())
	if len(c.vals) != 0 || len(c.ptrs) != 0 {
		t.Fatalf("copier block sized %d/%d for a sealed def; sealed formals must not reserve block storage",
			len(c.vals), len(c.ptrs))
	}
	got := registrationFormals(&c, def.Formals())
	if got != formals {
		t.Fatalf("sealed formals were not aliased: got %p want %p", got, formals)
	}
	if !got.IsSealed() {
		t.Fatal("aliased formals lost the seal")
	}
	for i, cell := range got.Cells {
		if !cell.IsSealed() {
			t.Fatalf("aliased formals cell %d is unsealed", i)
		}
	}
}

// TestRegistrationFormalsCopiesUnsealed: an unsealed formals list gets a
// private, structurally equal, UNSEALED copy — the pre-sharing #513 behavior.
func TestRegistrationFormalsCopiesUnsealed(t *testing.T) {
	formals := Formals("x", VarArgSymbol, "rest")
	def := &regFormalsDef{name: "reg-formals-unsealed", formals: formals}

	c := newFormalsCopier(def.defs())
	got := registrationFormals(&c, def.Formals())
	if got == formals {
		t.Fatalf("unsealed formals were aliased; two environments would share mutable storage (issue #363)")
	}
	if got.IsSealed() {
		t.Fatal("the private copy must be unsealed, ordinary mutable storage")
	}
	if got.String() != formals.String() {
		t.Fatalf("copy is not faithful: got %v want %v", got, formals)
	}
	for i := range formals.Cells {
		if got.Cells[i] == formals.Cells[i] {
			t.Fatalf("copy shares mutable formal cell %d with the template (issue #363)", i)
		}
	}
}

// TestSealedShareableFormalsRejectsPartialSeal: a header whose root carries
// the seal over UNSEALED cells must not be aliased — bind() reads the cells,
// so per-cell immutability is the actual safety condition.  The shape cannot
// be built through SealAST (it seals recursively); constructing it directly
// is exactly what the guard exists to be paranoid about.
func TestSealedShareableFormalsRejectsPartialSeal(t *testing.T) {
	formals := Formals("a", "b")
	formals.sealed = true // root only; cells deliberately left mutable

	if sealedShareableFormals(formals) {
		t.Fatal("a root-sealed list over unsealed cells must not be shareable")
	}
	def := &regFormalsDef{name: "reg-formals-partial", formals: formals}
	c := newFormalsCopier(def.defs())
	got := registrationFormals(&c, def.Formals())
	if got == formals {
		t.Fatal("partially sealed formals were aliased")
	}
	for i := range formals.Cells {
		if got.Cells[i] == formals.Cells[i] {
			t.Fatalf("copy shares mutable formal cell %d with the partially sealed template", i)
		}
	}
}

// TestSealedShareableFormalsEdges pins the non-list shapes: nil and
// non-LSExpr values are never shareable (the copier's own fallbacks own
// them), and an empty sealed list is shareable — there are no cells to
// mutate.
func TestSealedShareableFormalsEdges(t *testing.T) {
	if sealedShareableFormals(nil) {
		t.Fatal("nil must not be shareable")
	}
	s := String("not-formals")
	s.SealAST()
	if sealedShareableFormals(s) {
		t.Fatal("a sealed non-list must not be shareable")
	}
	empty := Formals()
	empty.SealAST()
	if !sealedShareableFormals(empty) {
		t.Fatal("an empty sealed formals list is safely shareable")
	}
}

// TestAddBuiltinsSharesSealedTemplate drives the full registration path: the
// same def registered into two environments lands as ONE sealed formals
// object when the template is sealed, and as two disjoint mutable objects
// when it is not.
func TestAddBuiltinsSharesSealedTemplate(t *testing.T) {
	sealedFormals := Formals("p", "q")
	sealedFormals.SealAST()
	unsealedFormals := Formals("r", "s")
	defs := []LBuiltinDef{
		&regFormalsDef{name: "reg-share-sealed", formals: sealedFormals},
		&regFormalsDef{name: "reg-share-unsealed", formals: unsealedFormals},
	}

	get := func(env *LEnv, name string) *LVal {
		v := env.Get(Symbol(name))
		if v.Type != LFun {
			t.Fatalf("%s did not register: %v", name, v)
		}
		return v.Cells[0]
	}

	newEnvWith := func() *LEnv {
		env := NewEnv(nil)
		rc := InitializeUserEnv(env)
		if rc.Type == LError {
			t.Fatalf("InitializeUserEnv: %v", rc)
		}
		env.AddBuiltins(true, defs...)
		return env
	}
	env1 := newEnvWith()
	env2 := newEnvWith()

	f1, f2 := get(env1, "reg-share-sealed"), get(env2, "reg-share-sealed")
	if f1 != f2 {
		t.Fatalf("sealed template was not shared: %p vs %p", f1, f2)
	}
	if f1 != sealedFormals {
		t.Fatalf("shared formals %p is not the sealed template %p", f1, sealedFormals)
	}

	u1, u2 := get(env1, "reg-share-unsealed"), get(env2, "reg-share-unsealed")
	if u1 == u2 {
		t.Fatalf("unsealed formals shared between environments (issue #363): %p", u1)
	}
	if u1 == unsealedFormals || u2 == unsealedFormals {
		t.Fatal("an environment aliases the unsealed template directly")
	}
	if u1.IsSealed() || u2.IsSealed() {
		t.Fatal("private copies of unsealed formals must stay unsealed")
	}
}
