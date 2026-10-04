// Copyright © 2026 The ELPS authors

package lisp

import (
	"cmp"
	"slices"
)

// The builtin registry (luthersystems/elps#800) records every function,
// macro and special operator as LEnv.AddBuiltins, LEnv.BindBuiltins,
// LEnv.AddMacros and LEnv.AddSpecialOps registered it, keyed by package and
// name.  A package binding can change later (set, set!, defun, a shadowing
// use-package or Put); the registry entry cannot.  Only a later
// registration of the same package and name replaces it.
//
// Identity is a registration record, not an *LVal.  registrationFunValue
// allocates one builtinRegistration per registration and stores it in the
// function's funData.  Every header of the function (FunRef, Copy) shares
// that funData, and a template copies funData headers by value, so the
// record pointer is the same in the source runtime and in every VM a
// template mints: cold, eager, lazy or prewarmed.  The record holds only two
// strings and is never written after construction, so VMs share it safely.

// builtinRegistration is the record of one registration.  It is immutable.
type builtinRegistration struct {
	pkg, name string
}

// builtinKey is a registry key.
type builtinKey struct {
	pkg, name string
}

// templateBuiltin is one registry entry in a template plan.  The plan is
// sorted by package, then name.
type templateBuiltin struct {
	rec   *builtinRegistration
	value templateRef
}

// builtinRegistry is a runtime's registry of builtins as registered.
//
//   - own holds the registrations made in this runtime: every one in a cold
//     environment, and those made after NewVM in a template VM.  It is nil
//     until the first registration.
//   - plan holds the entries a template VM inherited.  It belongs to the
//     template and is shared by every VM; nothing writes it.  An entry of own
//     with the same key replaces the plan entry.
//   - values holds this VM's value for each plan entry: all of them in an
//     eager VM, and in a lazy VM each one built on its first lookup through
//     lazy.
type builtinRegistry struct {
	own    map[builtinKey]*LVal
	lazy   *lazyInstance
	plan   []templateBuiltin
	values []*LVal
}

// register records fn, which registrationFunValue built, as the registered
// builtin of its package and name.
func (r *builtinRegistry) register(fn *LVal) {
	rec := fn.funData().reg
	if r.own == nil {
		r.own = make(map[builtinKey]*LVal)
	}
	r.own[builtinKey{rec.pkg, rec.name}] = fn
}

// planIndex returns the index of the plan entry for pkg and name.
func (r *builtinRegistry) planIndex(pkg, name string) (int, bool) {
	return slices.BinarySearchFunc(r.plan, builtinKey{pkg, name}, func(e templateBuiltin, k builtinKey) int {
		if c := cmp.Compare(e.rec.pkg, k.pkg); c != 0 {
			return c
		}
		return cmp.Compare(e.rec.name, k.name)
	})
}

// record returns the registration record for pkg and name, or nil.  It
// builds no value.
func (r *builtinRegistry) record(pkg, name string) *builtinRegistration {
	if fn, ok := r.own[builtinKey{pkg, name}]; ok {
		return fn.funData().reg
	}
	if i, ok := r.planIndex(pkg, name); ok {
		return r.plan[i].rec
	}
	return nil
}

// lookup returns this runtime's value of the registered builtin pkg:name,
// or nil.  In a lazy VM the first lookup of a plan entry builds it.
func (r *builtinRegistry) lookup(pkg, name string) *LVal {
	if fn, ok := r.own[builtinKey{pkg, name}]; ok {
		return fn
	}
	i, ok := r.planIndex(pkg, name)
	if !ok {
		return nil
	}
	return r.planValue(i)
}

// planValue returns this VM's value of plan entry i, building it in a lazy
// VM.
func (r *builtinRegistry) planValue(i int) *LVal {
	if r.values == nil {
		r.values = make([]*LVal, len(r.plan))
	}
	if v := r.values[i]; v != nil {
		return v
	}
	v := r.lazy.ref(r.plan[i].value)
	r.values[i] = v
	return v
}

// all returns every entry in key order, building each one.  Template
// publication uses it, so the registry of a template VM is published whole.
func (r *builtinRegistry) all() []*LVal {
	keys := make([]builtinKey, 0, len(r.own)+len(r.plan))
	for k := range r.own {
		keys = append(keys, k)
	}
	for _, e := range r.plan {
		if _, ok := r.own[builtinKey{e.rec.pkg, e.rec.name}]; !ok {
			keys = append(keys, builtinKey{e.rec.pkg, e.rec.name})
		}
	}
	slices.SortFunc(keys, func(a, b builtinKey) int {
		if c := cmp.Compare(a.pkg, b.pkg); c != 0 {
			return c
		}
		return cmp.Compare(a.name, b.name)
	})
	out := make([]*LVal, len(keys))
	for i, k := range keys {
		out[i] = r.lookup(k.pkg, k.name)
	}
	return out
}

// RegisteredBuiltin returns the function, macro or special operator
// registered as pkg:name, or nil when nothing is registered under that
// name.  Registration is LEnv.AddBuiltins, LEnv.BindBuiltins,
// LEnv.AddMacros or LEnv.AddSpecialOps.  The answer does not depend on what
// pkg binds name to now: set, set!, defun, Put and shadowing never change
// it.  A later registration of the same package and name replaces it.
//
// In a template VM the answer is the VM's own value of the builtin, the
// same value the package binds while the name is unchanged.  In a lazy VM
// the first call for a name builds the value, so the single-goroutine rule
// of Template.NewVM applies.
func (r *PackageRegistry) RegisteredBuiltin(pkg, name string) *LVal {
	if r == nil {
		return nil
	}
	return r.builtins.lookup(pkg, name)
}

// RegisteredBuiltinName returns the package and name under which fn was
// registered, and true, when fn is the function, macro or special
// operator that r's registration of that name created.  A header that
// shares fn's function data (a FunRef or Copy of it) is the same function
// and answers the same.
//
// The answer comes from the registration record elps stores with the
// function, never from its FID text.  It is false for every other value: a
// lambda, a builtin made by FunInPackage, a captured builtin (a libschema
// validator, for one), a native, a non-function, and a builtin that a later
// registration of the same name replaced.  Rebinding or shadowing the name
// does not change the answer.  The record is shared by every VM a template
// mints, so the answer is the same in the source and in each VM.
func (r *PackageRegistry) RegisteredBuiltinName(fn *LVal) (string, string, bool) {
	if r == nil || fn == nil || fn.Type != LFun {
		return "", "", false
	}
	fd, _ := fn.Native.(*funData)
	if fd == nil || fd.reg == nil {
		return "", "", false
	}
	if r.builtins.record(fd.reg.pkg, fd.reg.name) != fd.reg {
		return "", "", false
	}
	return fd.reg.pkg, fd.reg.name, true
}
