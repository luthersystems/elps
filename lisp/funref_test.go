// Copyright © 2018 The ELPS authors

package lisp

import "testing"

func TestFunRefDoesNotMutateOriginal(t *testing.T) {
	formals := QExpr([]*LVal{Symbol("x")})
	fun := Fun("test-fid", formals, nil)
	fun.Str = "original-name"

	alias := Symbol("alias-name")
	ref := FunRef(alias, fun)

	// FunRef must return a copy, not the original.
	if ref == fun {
		t.Error("FunRef returned the same pointer as the original function")
	}

	// The copy should have the alias name.
	if ref.Str != "alias-name" {
		t.Errorf("FunRef copy has Str = %q, want %q", ref.Str, "alias-name")
	}

	// The original must be unchanged.
	if fun.Str != "original-name" {
		t.Errorf("FunRef mutated original Str to %q, want %q", fun.Str, "original-name")
	}
}
