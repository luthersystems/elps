// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"fmt"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// This file pins the CONTRACT of lisp.InitializeTypedef, which issue #433 is
// about.
//
// #433 is a documentation defect, not a code defect.  InitializeTypedef's
// preconditions -- an environment whose Runtime.Package and Registry.Lang are
// already established -- are met by exactly one thing, InitializeUserEnv, and
// InitializeTypedef has exactly one caller in this tree, InitializeUserEnv
// itself.  What was wrong is that LEnv.New's doc comment offered
// InitializeTypedef as an ALTERNATIVE to InitializeUserEnv ("Generally that
// will be enabled by calling InitializeTypedef or InitializeUserEnv"), which
// it has never been: on a bare environment it panics before doing anything.
//
// The panics stay.  Issue #361 settled that policy for lisp/: "a panic in
// lisp means this is a bug in the interpreter, or in code that had no
// business calling this".  Downgrading these two to errors would blunt that
// signal to serve a call no correct embedder makes, and the doc comment was
// the only thing telling anyone to make it.
//
// Every test below is a GUARD -- all of them pass on main.  They exist so the
// documented contract cannot drift away from the code silently: if someone
// later adds the nil guards (a legitimate but larger change, per #433), these
// fail and point at the doc comments that must change with them.

// requirePanics runs fn and returns the recovered value, failing if fn did
// not panic.
func requirePanics(t *testing.T, what string, fn func()) (recovered any) {
	t.Helper()
	defer func() {
		recovered = recover()
		if recovered == nil {
			t.Errorf("%s: expected a panic, got none;"+
				" if the nil guards were added deliberately, InitializeTypedef's and"+
				" LEnv.New's doc comments must stop saying it panics (issue #433)", what)
		}
	}()
	fn()
	return nil
}

// TestInitializeTypedefRequiresRuntimePackage is a GUARD: it passes on main.
// It pins the first of the two unguarded dereferences #433 records -- the
// env.Runtime.Package.Name read in LEnv.builtin -- which is what a bare
// lisp.InitializeTypedef(lisp.NewEnv(nil)) hits.  StandardRuntime leaves
// Runtime.Package nil.
func TestInitializeTypedefRequiresRuntimePackage(t *testing.T) {
	env := lisp.NewEnv(nil)
	if env.Runtime.Package != nil {
		t.Fatalf("expected StandardRuntime to leave Runtime.Package nil, got %v;"+
			" this test can no longer observe the precondition it is about", env.Runtime.Package)
	}
	v := requirePanics(t, "InitializeTypedef on a bare environment", func() {
		lisp.InitializeTypedef(env)
	})
	if v != nil && !strings.Contains(strings.ToLower(fmt.Sprint(v)), "nil pointer") {
		t.Errorf("panicked with %v; expected the nil Runtime.Package dereference", v)
	}
}

// TestInitializeTypedefRequiresRegistryLang is a GUARD: it passes on main.
// It pins the SECOND unguarded dereference #433 records, the one the first
// hides.  Registry.Lang is "" until InitializeUserEnv sets it, and
// Packages[""] is a nil *Package -- the same shape as issue #425.  Giving the
// runtime a package (so LEnv.builtin succeeds) but no Lang exposes it: the
// panic then comes from (*Package).Put, not from LEnv.builtin.
func TestInitializeTypedefRequiresRegistryLang(t *testing.T) {
	env := lisp.NewEnv(nil)
	env.Runtime.Registry.DefinePackage(lisp.DefaultLangPackage)
	env.Runtime.Package = env.Runtime.Registry.Package(lisp.DefaultLangPackage)
	if env.Runtime.Registry.Lang != "" {
		t.Fatalf("expected Registry.Lang to be empty before InitializeUserEnv, got %q",
			env.Runtime.Registry.Lang)
	}
	if env.Runtime.Registry.Package(env.Runtime.Registry.Lang) != nil {
		t.Fatalf("expected Packages[%q] to be nil; this test can no longer observe"+
			" the precondition it is about", env.Runtime.Registry.Lang)
	}
	requirePanics(t, "InitializeTypedef with no Registry.Lang", func() {
		lisp.InitializeTypedef(env)
	})
}

// TestInitializeTypedefAfterInitializeUserEnv is the positive control for the
// two guards above: the precondition the doc comments now state is not only
// necessary but sufficient.  Without this, a change that made
// InitializeTypedef panic unconditionally would keep both guards passing.
func TestInitializeTypedefAfterInitializeUserEnv(t *testing.T) {
	env := lisp.NewEnv(nil)
	if rc := lisp.InitializeUserEnv(env); rc.Type == lisp.LError {
		t.Fatalf("initialize-user-env: %v", rc)
	}
	rc := lisp.InitializeTypedef(env)
	if rc.Type == lisp.LError {
		t.Fatalf("InitializeTypedef on an initialized environment: %v", rc)
	}
	if !rc.IsNil() {
		t.Errorf("expected nil, got %v", rc)
	}
}
