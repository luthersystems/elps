// Copyright © 2025 The ELPS authors

package lisp

import "testing"

// TestGetFunNameReportsMostRecentlyBoundName pins the naming rule that the
// call-frame display name depends on: when one function value is bound under
// several names in a package, the name reported is the one it was most
// recently BOUND under, not the one it was most recently looked up under
// (issue #397 moved the bookkeeping to the write path; issue #271 is why the
// lookup must succeed at all rather than fall back and log).
//
// It is stated here against (*LEnv).GetFunName, the resolver every funCall,
// specialOpCall and macroCall goes through, rather than against the
// package-level table alone: any scheme that memoises the name to make that
// resolution cheaper has to keep the memo in step with a rebinding, and this
// test is where that claim is checked.
func TestGetFunNameReportsMostRecentlyBoundName(t *testing.T) {
	env := NewEnv(nil)
	pkg := env.Runtime.Registry.DefinePackage("naming")
	fn := FunInPackage("naming", "fid-two-names", Formals(), func(env *LEnv, args *LVal) *LVal {
		return Nil()
	})

	pkg.Put(Symbol("first"), fn)
	if got := env.GetFunName(fn); got != "first" {
		t.Fatalf("GetFunName after first binding = %q, want %q", got, "first")
	}
	if got := pkg.GetFunName(fn.FID()); got != "first" {
		t.Fatalf("Package.GetFunName after first binding = %q, want %q", got, "first")
	}

	// A lookup under the old name must not change the reported name...
	pkg.Put(Symbol("second"), fn)
	pkg.Get(Symbol("first"))
	if got := env.GetFunName(fn); got != "second" {
		t.Fatalf("GetFunName after rebinding = %q, want %q (the most recently "+
			"bound name, not the most recently looked-up one)", got, "second")
	}

	// ...and neither must reaching the value through the environment, which
	// hands back a FunRef header copy sharing the same funData.
	ref := env.GetGlobal(Symbol("naming:first"))
	if ref.Type != LFun {
		t.Fatalf("naming:first resolved to %s, want a function", ref.Type)
	}
	if got := env.GetFunName(ref); got != "second" {
		t.Fatalf("GetFunName through a FunRef copy = %q, want %q", got, "second")
	}

	// Rebinding again moves the name again.
	pkg.Put(Symbol("third"), fn)
	if got := env.GetFunName(fn); got != "third" {
		t.Fatalf("GetFunName after second rebinding = %q, want %q", got, "third")
	}
}
