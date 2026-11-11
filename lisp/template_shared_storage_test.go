// Copyright © 2026 The ELPS authors

package lisp

import (
	"strings"
	"testing"
)

// Issue #631: exercise the public admission boundary in the core package,
// including capacity-only overlap and both non-overlapping ordering branches.
func TestTemplateSharedStorageAdmission(t *testing.T) {
	for _, tc := range []struct {
		name                    string
		sharedStart, sharedEnd  int
		start, length, capacity int
		reject                  bool
	}{
		{"mutable-before", 1, 3, 0, 2, 2, true},
		{"shared-before", 0, 2, 1, 2, 2, true},
		{"identical", 0, 2, 0, 2, 2, true},
		{"capacity-tail", 2, 3, 0, 1, 3, true},
		{"empty-positive-capacity", 2, 3, 1, 0, 2, true},
		{"adjacent-mutable-before", 1, 3, 0, 1, 1, false},
		{"adjacent-shared-before", 0, 2, 2, 1, 1, false},
		{"empty-zero-capacity", 0, 3, 1, 0, 0, false},
	} {
		t.Run(tc.name, func(t *testing.T) {
			env := templateOwnershipEnv()
			backing := []*LVal{Int(11), Int(22), Int(33)}
			shared := QExpr(backing[tc.sharedStart:tc.sharedEnd:tc.sharedEnd])
			shared.sealAST()
			mutable := QExpr(backing[tc.start : tc.start+tc.length : tc.start+tc.capacity])
			env.Runtime.Package.symbols["shared"] = shared
			env.Runtime.Package.symbols["mutable"] = mutable
			wantShared, wantMutable := shared.String(), mutable.String()
			plan, err := NewTemplate(env)
			if tc.reject {
				if plan != nil || err == nil || !strings.Contains(err.Error(), "mutable cells backing overlaps shared program storage") {
					t.Fatalf("overlap admitted or wrong rejection: plan-present=%t err=%v", plan != nil, err)
				}
			} else {
				if plan == nil || err != nil {
					t.Fatalf("non-overlapping storage rejected: %v", err)
				}
				vm, err := plan.NewVM()
				if err != nil {
					t.Fatal(err)
				}
				got := vm.Get(Symbol("mutable"))
				if got.String() != wantMutable || len(got.Cells) != tc.length || cap(got.Cells) != tc.capacity {
					t.Fatalf("admitted view changed: value=%v len=%d cap=%d", got, len(got.Cells), cap(got.Cells))
				}
				if len(got.Cells) != 0 {
					got.Cells[0] = Int(99)
				}
				if got := vm.Get(Symbol("shared")); got.String() != wantShared {
					t.Fatalf("private mutation changed shared code: %v", got)
				}
			}
			if shared.String() != wantShared || mutable.String() != wantMutable || len(mutable.Cells) != tc.length || cap(mutable.Cells) != tc.capacity {
				t.Fatal("publication or instance mutation changed source values or bounds")
			}
			for i, want := range []int{11, 22, 33} {
				if backing[i].Type != LInt || backing[i].Int != want {
					t.Fatal("source capacity backing changed")
				}
			}
			if !shared.IsSealed() || mutable.IsSealed() {
				t.Fatal("source sharing classification changed")
			}
		})
	}
}

func TestTemplateRejectsMutableFunctionCodeVector(t *testing.T) {
	env := templateOwnershipEnv()
	formals, result := Formals(), Int(17)
	formals.sealAST()
	result.sealAST()
	fn := env.Lambda(formals, []*LVal{result})
	view := QExpr(fn.Cells)
	env.Runtime.Package.symbols["fn"] = fn
	env.Runtime.Package.symbols["view"] = view
	if fn.Type != LFun || cap(fn.Cells) != len(fn.Cells) || view.IsSealed() {
		t.Fatal("fixture does not expose a shared function vector through a mutable header")
	}
	plan, err := NewTemplate(env)
	if plan != nil || err == nil || !strings.Contains(err.Error(), "mutable cells backing overlaps shared program storage") {
		t.Fatalf("function vector alias admitted: plan-present=%t err=%v", plan != nil, err)
	}
	if fn.Cells[0] != formals || fn.Cells[1] != result || &view.Cells[0] != &fn.Cells[0] {
		t.Fatal("rejection changed the source function or alias")
	}
	if got := env.FunCall(fn, SExpr(nil)); got.Type != LInt || got.Int != 17 {
		t.Fatalf("source function changed after rejection: %v", got)
	}
}
