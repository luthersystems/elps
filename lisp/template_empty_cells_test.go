// Copyright © 2026 The ELPS authors

package lisp

import (
	"reflect"
	"testing"
)

// Empty and nil cells behave alike for Lisp sequences, but an audited Go
// builtin can distinguish them through an explicit capture. Preserve that
// distinction without keeping a source backing array alive (#622).
func TestTemplatePreservesEmptyCellsInBuiltinCaptures(t *testing.T) {
	for _, nonnil := range []bool{false, true} {
		build := func() *LEnv {
			env := templateOwnershipEnv()
			var cells []*LVal
			if nonnil {
				cells = make([]*LVal, 0)
			}
			env.Runtime.Package.symbols["probe"] = newCapturedBuiltin(capturedBuiltin{
				FID: "probe", Package: "user", Formals: Formals(), Captures: QExpr(cells),
				Eval: func(_ *LEnv, _, captures *LVal) *LVal {
					if captures.Cells == nil {
						return Int(0)
					}
					return Int(1)
				},
			})
			return env
		}
		source := build()
		plan, err := NewTemplate(source)
		if err != nil {
			t.Fatal(err)
		}
		cold := build()
		want := 0
		if nonnil {
			want = 1
		}
		for range 2 {
			vm, err := plan.NewVM()
			if err != nil {
				t.Fatal(err)
			}
			for _, env := range []*LEnv{source, cold, vm} {
				got := env.FunCall(env.Runtime.Package.symbols["probe"], SExpr(nil))
				if got.Type != LInt || got.Int != want {
					t.Fatalf("nonnil=%t: got %v, want %d", nonnil, got, want)
				}
			}
		}
	}
}

func TestTemplateEmptyCellsDoNotRetainSourceBacking(t *testing.T) {
	source := templateOwnershipEnv()
	storage := []*LVal{Native(new(int)), Int(3)}
	empty := QExpr(storage[:0:0])
	source.Runtime.Package.symbols["empty"] = empty
	plan, err := NewTemplate(source)
	if err != nil {
		t.Fatal(err)
	}
	vm, err := plan.NewVM()
	if err != nil {
		t.Fatal(err)
	}
	got := vm.Runtime.Package.symbols["empty"]
	if got.Cells == nil || len(got.Cells) != 0 || cap(got.Cells) != 0 {
		t.Fatal("empty zero-capacity representation changed")
	}
	if reflect.ValueOf(got.Cells).Pointer() == reflect.ValueOf(storage).Pointer() {
		t.Fatal("empty cells retain the source backing and its unreachable native")
	}
	if empty.Cells == nil || len(empty.Cells) != 0 || cap(empty.Cells) != 0 {
		t.Fatal("publication changed the source view")
	}
}
