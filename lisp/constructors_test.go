// Copyright © 2018 The ELPS authors

package lisp

import "testing"

// noopBuiltin is an LBuiltin used as a stand-in body for constructor
// tests — none of the cases below invoke it.
func noopBuiltin(_ *LEnv, _ *LVal) *LVal { return Nil() }

// TestFunInPackage_SetsPackage guards the FunInPackage / MacroInPackage
// / SpecialOpInPackage constructor invariant for issue #271: each must
// produce an LFun whose FunData.Package equals the pkg argument. A
// regression in this layer would re-enable the BUG: GetFunName log
// spam that the libschema fix and surrounding API safeguards
// eliminated.
func TestFunInPackage_SetsPackage(t *testing.T) {
	formals := QExpr([]*LVal{Symbol("x")})

	t.Run("FunInPackage", func(t *testing.T) {
		v := FunInPackage("my-pkg", "fid-1", formals, noopBuiltin)
		if v.Type != LFun {
			t.Fatalf("Type = %v, want LFun", v.Type)
		}
		if got := v.Package(); got != "my-pkg" {
			t.Errorf("Package() = %q, want %q", got, "my-pkg")
		}
		if got := v.FID(); got != "fid-1" {
			t.Errorf("FID() = %q, want %q", got, "fid-1")
		}
	})

	t.Run("MacroInPackage", func(t *testing.T) {
		v := MacroInPackage("my-pkg", "fid-2", formals, noopBuiltin)
		if v.Type != LFun {
			t.Fatalf("Type = %v, want LFun", v.Type)
		}
		if v.FunType != LFunMacro {
			t.Errorf("FunType = %v, want LFunMacro", v.FunType)
		}
		if got := v.Package(); got != "my-pkg" {
			t.Errorf("Package() = %q, want %q", got, "my-pkg")
		}
	})

	t.Run("SpecialOpInPackage", func(t *testing.T) {
		v := SpecialOpInPackage("my-pkg", "fid-3", formals, noopBuiltin)
		if v.Type != LFun {
			t.Fatalf("Type = %v, want LFun", v.Type)
		}
		if v.FunType != LFunSpecialOp {
			t.Errorf("FunType = %v, want LFunSpecialOp", v.FunType)
		}
		if got := v.Package(); got != "my-pkg" {
			t.Errorf("Package() = %q, want %q", got, "my-pkg")
		}
	})

	// Empty pkg is a valid input — it documents the (deprecated)
	// "caller will set Package later" path. Verify the constructor
	// doesn't reject or rewrite it.
	t.Run("FunInPackage empty pkg", func(t *testing.T) {
		v := FunInPackage("", "fid-4", formals, noopBuiltin)
		if got := v.Package(); got != "" {
			t.Errorf("Package() = %q, want empty", got)
		}
	})
}
