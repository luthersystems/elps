// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
)

// PackageRegistry.AddPackage is the sibling of #394's Program defect: another
// exported entry point that admits Go-built LVals into a Runtime without
// requiring or checking the seal.  #394 fixes the Program half and DOES NOT
// fix this half — see the AddPackage doc comment in lisp/package.go for why
// the same repair is impossible here and what is expected of the caller
// instead.
//
// These are CHARACTERIZATION tests: they pin the behaviour the documented
// contract describes, so the contract cannot drift silently and so a future
// change to it is a deliberate, visible edit rather than an accident.  The
// first test asserts sharing (the sharp edge the contract warns about); the
// second asserts that SealAST is a real mitigation and not folklore.

func sharedDataPackage(tb testing.TB, seal bool) (*lisp.Package, *lisp.LVal) {
	tb.Helper()
	pkg := lisp.NewPackage("shared")
	data := lisp.QExpr([]*lisp.LVal{lisp.Int(1), lisp.Int(2), lisp.Int(3)})
	if seal {
		data.SealAST()
	}
	pkg.Put(lisp.Symbol("data"), data)
	pkg.Export("data")
	return pkg, data
}

func sharedDataString(tb testing.TB, env *lisp.LEnv) string {
	tb.Helper()
	pkg := env.Runtime.Registry.Package("shared")
	if pkg == nil {
		tb.Fatal("shared package is not registered")
	}
	return pkg.Get(lisp.Symbol("data")).String()
}

// TestAddPackageSharesUnsealedValues records the contract: AddPackage stores
// the caller's *Package by pointer and copies nothing, so an UNSEALED value
// installed in a package registered with two registries is one piece of
// storage.  stable-sort in environment A rewrites it in place and environment
// B reads the result.  This is the reproduction named in #394, verified
// independently of the Program work on this branch.
func TestAddPackageSharesUnsealedValues(t *testing.T) {
	envA := leakEnv(t, parser.NewReader())
	envB := leakEnv(t, parser.NewReader())

	pkg, data := sharedDataPackage(t, false)
	if !envA.Runtime.Registry.AddPackage(pkg) {
		t.Fatal("envA AddPackage returned false")
	}
	if !envB.Runtime.Registry.AddPackage(pkg) {
		t.Fatal("envB AddPackage returned false")
	}
	if data.IsSealed() {
		t.Fatal("test setup: value should be unsealed")
	}

	if got := sharedDataString(t, envB); got != `'(1 2 3)` {
		t.Fatalf("envB before = %s, want '(1 2 3)", got)
	}
	if rc := envA.LoadString("a.lisp", `(stable-sort > shared:data)`); rc.Type == lisp.LError {
		t.Fatalf("envA sort: %v", rc)
	}
	// The contract, stated as an assertion.  If this ever starts failing,
	// AddPackage has grown a guard — update the doc in lisp/package.go and
	// this test together rather than deleting either.
	if got := sharedDataString(t, envB); got != `'(3 2 1)` {
		t.Errorf("envB after = %s, want '(3 2 1)"+
			": AddPackage's documented sharing contract appears to have changed", got)
	}
}

// TestAddPackageSealedValuesAreIsolated is the mitigation half: SealAST on
// the value before Put makes the registry's sharing harmless, because the
// kernel's mutation sites copy-on-write a sealed list (lisp/seal.go).  This
// is what the AddPackage doc tells an embedder to do, so it has to be true.
func TestAddPackageSealedValuesAreIsolated(t *testing.T) {
	envA := leakEnv(t, parser.NewReader())
	envB := leakEnv(t, parser.NewReader())

	pkg, data := sharedDataPackage(t, true)
	envA.Runtime.Registry.AddPackage(pkg)
	envB.Runtime.Registry.AddPackage(pkg)
	if !data.IsSealed() {
		t.Fatal("test setup: value should be sealed")
	}

	before := sharedDataString(t, envB)
	if rc := envA.LoadString("a.lisp", `(stable-sort > shared:data)`); rc.Type == lisp.LError {
		t.Fatalf("envA sort: %v", rc)
	}
	if after := sharedDataString(t, envB); after != before {
		t.Errorf("sealed shared value changed under envB: %s -> %s", before, after)
	}
	// And the sort still produces the sorted answer for envA — the seal costs
	// the caller nothing but the in-place effect, which was never useful.
	rc := envA.LoadString("a2.lisp", `(stable-sort > shared:data)`)
	if rc.Type == lisp.LError {
		t.Fatalf("envA sort: %v", rc)
	}
	if rc.String() != `'(3 2 1)` {
		t.Errorf("envA sort result = %s, want '(3 2 1)", rc.String())
	}
}
