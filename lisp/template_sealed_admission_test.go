// Copyright © 2026 The ELPS authors

package lisp

import (
	"slices"
	"strings"
	"testing"
)

// Issues #621/#622: sealed syntax may be shared only when every reachable
// reference is immutable. Debugger argument metadata is a separate graph edge,
// and can hide a closure over the source environment outside Cells/Native.
func TestTemplateRejectsSealedMacroExpansionMetadata(t *testing.T) {
	source := templateOwnershipEnv()
	fn := source.Lambda(Formals(), []*LVal{Int(9)})
	metadata := &macroExpansionInfo{macroExpansionContext: &macroExpansionContext{Args: []*LVal{fn}, Name: "fixture-macro"}, ID: 3}
	node := Int(1)
	node.macroExpansion = metadata
	node.SealAST()
	source.Runtime.Package.symbols["node"] = node
	plan, err := NewTemplate(source)
	if plan != nil {
		// This is not just a harmless diagnostic field: the public accessor
		// exposes the original closure and therefore the original VM graph.
		vm, forkErr := plan.NewVM()
		if forkErr != nil {
			t.Fatal(forkErr)
		}
		got, ok := vm.Runtime.Package.symbols["node"].MacroExpansion()
		leaked := ok && len(got.Args) == 1 && got.Args[0] == fn && got.Args[0].Native.(*funData).env == source
		t.Fatalf("sealed debug graph admitted: public metadata exposes source closure=%t", leaked)
	}
	if err == nil || !strings.Contains(err.Error(), "sealed graph contains macro expansion metadata") {
		t.Fatalf("wrong rejection: %v", err)
	}
	got, ok := node.MacroExpansion()
	if !ok || got.Name != "fixture-macro" || got.ID != 3 || got.Args[0] != fn || node.macroExpansion != metadata || !node.IsSealed() {
		t.Fatal("rejection changed source metadata")
	}
}

func TestTemplateDropsUnsealedMacroExpansionMetadata(t *testing.T) {
	source := templateOwnershipEnv()
	node := Int(1)
	node.macroExpansion = &macroExpansionInfo{macroExpansionContext: &macroExpansionContext{Args: []*LVal{Native(new(int))}}, ID: 3}
	source.Runtime.Package.symbols["node"] = node
	plan, err := NewTemplate(source)
	if err != nil {
		t.Fatal(err)
	}
	vm, err := plan.NewVM()
	if err != nil {
		t.Fatal(err)
	}
	if got := vm.Runtime.Package.symbols["node"]; got.Int != 1 || got.macroExpansion != nil {
		t.Fatal("transient debug graph was retained")
	}
	if _, ok := node.MacroExpansion(); !ok {
		t.Fatal("publication changed source metadata")
	}
}

func TestTemplateValidatesSealedCellCapacity(t *testing.T) {
	for _, kind := range []string{"native", "function", "nil"} {
		t.Run(kind, func(t *testing.T) {
			source := templateOwnershipEnv()
			var hidden *LVal
			switch kind {
			case "native":
				hidden = Native(new(int))
			case "function":
				hidden = source.Lambda(Formals(), []*LVal{Int(9)})
			}
			first := Int(7)
			storage := []*LVal{first, hidden, nil}
			node := QExpr(storage[:1])
			node.SealAST()
			source.Runtime.Package.symbols["node"] = node
			plan, err := NewTemplate(source)
			if hidden == nil {
				if err != nil {
					t.Fatalf("nil capacity tail rejected: %v", err)
				}
				vm, err := plan.NewVM()
				if err != nil {
					t.Fatal(err)
				}
				got := vm.Runtime.Package.symbols["node"]
				if got.Cells[0].Int != 7 || cap(got.Cells) != 3 || got.Cells[:3][1] != nil || got.Cells[:3][2] != nil {
					t.Fatal("safe sealed capacity changed")
				}
			} else if plan != nil || err == nil || !strings.Contains(err.Error(), "sealed graph reaches mutable or opaque") {
				t.Fatalf("hidden %s admitted or wrong rejection: plan=%v err=%v", kind, plan, err)
			}
			if len(node.Cells) != 1 || cap(node.Cells) != 3 || node.Cells[0] != first || first.Int != 7 || !slices.Equal(storage, []*LVal{first, hidden, nil}) || !node.IsSealed() {
				t.Fatal("admission changed source cells")
			}
		})
	}
}
