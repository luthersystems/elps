// Copyright © 2026 The ELPS authors

package lisp

import (
	"runtime"
	"testing"
	"weak"

	"github.com/luthersystems/elps/parser/token"
)

// Issue #630: Go pointers can retain inaccessible parts of an allocation.
// Private mutable storage must not keep those source allocations alive. Shared
// code may do so, but retention is permitted, never required by the contract.
// Keep source construction in a separate frame so compiler liveness cannot
// accidentally turn the fixture's local variables into additional GC roots.
//
//go:noinline
func templateRetentionFixture(t *testing.T, shape string, sealed bool) (*Template, weak.Pointer[LEnv]) {
	t.Helper()
	source := templateOwnershipEnv()
	hidden := source.Lambda(Formals(), []*LVal{Int(1)})
	var root *LVal
	switch shape {
	case "zero-capacity":
		storage := []*LVal{hidden}
		root = QExpr(storage[:0:0])
	case "hidden-tail":
		storage := []*LVal{Int(7), hidden}
		root = QExpr(storage[:1:1])
	case "hidden-prefix":
		storage := []*LVal{hidden, Int(7)}
		root = QExpr(storage[1:2:2])
	case "header-array":
		storage := []LVal{*Int(7), *hidden}
		root = &storage[0]
	case "location-owner":
		owner := &struct {
			location token.Location
			source   *LEnv
		}{location: token.Location{File: "retention.lisp", Line: 1}, source: source}
		root = Int(7)
		root.SetSource(&owner.location)
	case "function-vector":
		root = source.Lambda(Formals(), []*LVal{Int(7)})
		count := len(root.Cells)
		storage := make([]*LVal, count+1)
		copy(storage, root.Cells)
		storage[count] = hidden
		for _, child := range storage[:count] {
			child.sealAST()
		}
		root.Cells = storage[:count:count]
	default:
		t.Fatalf("unknown retention fixture %q", shape)
	}
	if sealed {
		// Use the production marking operation, without the checked-build test
		// registry becoming an unrelated global owner of this synthetic graph.
		root.sealAST()
	}
	source.Runtime.Package.symbols["root"] = root
	plan, err := NewTemplate(source)
	if err != nil {
		t.Fatal(err)
	}
	return plan, weak.Make(source)
}

//go:noinline
func exerciseTemplateRetention(t *testing.T, shape string, sealed bool) weak.Pointer[LEnv] {
	t.Helper()
	plan, source := templateRetentionFixture(t, shape, sealed)
	for range 8 {
		runtime.GC()
	}
	shared := sealed || shape == "function-vector" || shape == "location-owner"
	if !shared && source.Value() != nil {
		t.Fatal("private mutable descriptors retained the source VM")
	}
	// Exercise two live siblings after GC; sharing must never lose accessible
	// content or expose the hidden closure, irrespective of allocation lifetime.
	var siblings []*LEnv
	for range 2 {
		vm, err := plan.NewVM()
		if err != nil {
			t.Fatal(err)
		}
		siblings = append(siblings, vm)
		value := vm.Runtime.Package.symbols["root"]
		if value == nil {
			t.Fatal("instance lost root")
		}
		switch shape {
		case "zero-capacity":
			if value.Type != LSExpr || value.Cells == nil || len(value.Cells) != 0 || cap(value.Cells) != 0 {
				t.Fatal("empty view shape changed")
			}
		case "hidden-tail", "hidden-prefix":
			if value.Type != LSExpr || len(value.Cells) != 1 || cap(value.Cells) != 1 || value.Cells[0].Type != LInt || value.Cells[0].Int != 7 {
				t.Fatalf("visible view changed: %v", value)
			}
		case "function-vector":
			if got := vm.FunCall(value, SExpr(nil)); got.Type != LInt || got.Int != 7 {
				t.Fatalf("function result changed: %v", got)
			}
		default:
			if value.Type != LInt || value.Int != 7 {
				t.Fatalf("scalar changed: %v", value)
			}
			if shape == "location-owner" {
				if got, ok := value.Source(); !ok || got.File != "retention.lisp" || got.Line != 1 {
					t.Fatalf("definition location changed: %v", got)
				}
			}
		}
	}
	runtime.KeepAlive(siblings)
	runtime.KeepAlive(plan)
	return source
}

func TestTemplateSourceAllocationLifetime(t *testing.T) {
	for _, shape := range []string{"zero-capacity", "hidden-tail", "hidden-prefix", "header-array", "location-owner", "function-vector"} {
		for _, mode := range []struct {
			name   string
			sealed bool
		}{{"mutable", false}, {"sealed", true}} {
			t.Run(shape+"/"+mode.name, func(t *testing.T) {
				source := exerciseTemplateRetention(t, shape, mode.sealed)
				// Both plan and instances have left the noinline frame. No
				// global compiler memo or diagnostic registry may retain them.
				for range 8 {
					runtime.GC()
				}
				if source.Value() != nil {
					t.Fatal("source retained after template and instances were dropped")
				}
			})
		}
	}
}
