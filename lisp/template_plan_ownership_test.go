// Copyright © 2026 The ELPS authors

package lisp

import (
	"reflect"
	"strings"
	"testing"
)

func templateOwnershipEnv() *LEnv {
	env := NewEnv(nil)
	env.Runtime.Package = env.Runtime.Registry.DefinePackage("user")
	return env
}

// Issues #622 and #629: publication owns metadata and stack-free saved errors.
// Retained diagnostic stacks are covered by categorical rejection tests.
func TestTemplatePlanOwnsMetadataAndStackFreeErrors(t *testing.T) {
	source := templateOwnershipEnv()
	pkg := source.Runtime.Package
	pkg.Doc = "original package"
	pkg.setSymbolDoc("failure", "original symbol")
	pkg.funNames["function-id"] = "original-function"
	pkg.Export("failure", "other")
	source.Runtime.MaxAlloc = 17
	source.Runtime.Stack.MaxHeightPhysical = 23
	source.Runtime.numenv = 41
	source.Runtime.numsym = 43
	errValue := Errorf("saved failure")
	pkg.symbols["failure"] = errValue
	alias := Errorf("another failure")
	alias.Cells = errValue.Cells
	pkg.symbols["alias"] = alias
	plan, err := NewTemplate(source)
	if err != nil {
		t.Fatal(err)
	}
	pkg.Doc = "changed package"
	pkg.symbolDocs["failure"] = "changed symbol"
	pkg.funNames["function-id"] = "changed-function"
	pkg.externals[0] = "changed-export"
	source.Runtime.MaxAlloc = 99
	source.Runtime.Stack.MaxHeightPhysical = 101
	source.Runtime.numenv = 103
	source.Runtime.numsym = 107
	errValue.Cells[0].Str = "changed source failure"
	var siblings []*LEnv
	for range 2 {
		vm, err := plan.NewVM()
		if err != nil {
			t.Fatal(err)
		}
		siblings = append(siblings, vm)
		got := vm.Runtime.Package
		if got == pkg || vm.Runtime == source.Runtime || got.Doc != "original package" || got.SymbolDoc("failure") != "original symbol" || got.GetFunName("function-id") != "original-function" || !reflect.DeepEqual(got.Externals(), []string{"failure", "other"}) {
			t.Fatalf("package metadata changed: %+v", got)
		}
		if vm.Runtime.MaxAlloc != 17 || vm.Runtime.Stack.MaxHeightPhysical != 23 || vm.Runtime.numenv != 41 || vm.Runtime.numsym != 43 {
			t.Fatal("runtime configuration or identifier counters changed")
		}
		recorded, other := got.symbols["failure"], got.symbols["alias"]
		if recorded == errValue || other == alias || recorded == other || recorded.Type != LError || other.Type != LError || recorded.Str != "error" || other.Str != "error" || recorded.Native != nil || other.Native != nil || recorded.Cells[0].Str != "saved failure" || recorded.Cells[0] != other.Cells[0] || &recorded.Cells[0] != &other.Cells[0] {
			t.Fatalf("stack-free error ownership or aliases changed: %v / %v", recorded, other)
		}
	}
	a := siblings[0].Runtime.Package.symbols["failure"]
	b := siblings[1].Runtime.Package.symbols["failure"]
	a.Cells[0] = String("sibling failure")
	if siblings[0].Runtime.Package.symbols["alias"].Cells[0].Str != "sibling failure" {
		t.Fatal("saved errors lost their shared cell slot")
	}
	siblings[0].Runtime.Package.setSymbolDoc("failure", "sibling changed")
	if b.Cells[0].Str != "saved failure" || siblings[1].Runtime.Package.SymbolDoc("failure") != "original symbol" || errValue.Cells[0].Str != "changed source failure" {
		t.Fatal("sibling metadata changed")
	}
	third, err := plan.NewVM()
	if err != nil {
		t.Fatal(err)
	}
	if got := third.Runtime.Package.symbols["failure"]; got.Native != nil || got.Cells[0].Str != "saved failure" {
		t.Fatal("VM mutated published stack-free error descriptors")
	}
}

func TestTemplatePlanStockMapBackingAliases(t *testing.T) {
	source := templateOwnershipEnv()
	a := SortedMap()
	copyWrapper := *a.Map()
	b := SortedMapFromData(&copyWrapper)
	if rc := a.Map().Set(Symbol("key"), Int(1)); rc.Type == LError {
		t.Fatal(rc)
	}
	if rc := a.Map().Set(String("self"), b); rc.Type == LError {
		t.Fatal(rc)
	}
	source.Runtime.Package.symbols["a"] = a
	source.Runtime.Package.symbols["b"] = b
	plan, err := NewTemplate(source)
	if err != nil {
		t.Fatal(err)
	}
	vm, err := plan.NewVM()
	if err != nil {
		t.Fatal(err)
	}
	fa, fb := vm.Runtime.Package.symbols["a"], vm.Runtime.Package.symbols["b"]
	if fa.Map() == fb.Map() || fa.Map() == a.Map() {
		t.Fatal("map wrapper identities collapsed")
	}
	if rc := fa.Map().Set(Symbol("key"), Int(9)); rc.Type == LError {
		t.Fatal(rc)
	}
	if got, _ := fb.Map().Get(Symbol("key")); got.Int != 9 {
		t.Fatal("stock map backing alias lost")
	}
	if got, _ := fa.Map().Get(String("self")); got != fb {
		t.Fatal("stock map cycle lost")
	}
	if got, _ := a.Map().Get(Symbol("key")); got.Int != 1 {
		t.Fatal("source map changed")
	}
	sibling, err := plan.NewVM()
	if err != nil {
		t.Fatal(err)
	}
	if got, _ := sibling.Runtime.Package.symbols["b"].Map().Get(Symbol("key")); got.Int != 1 {
		t.Fatal("sibling map changed")
	}
	if got := sortedMapEntries(fa.Map()); got.Type == LError || got.Cells[0].Cells[0].Type != LSymbol {
		t.Fatalf("map symbol-key policy changed: %v", got)
	}
}

func TestTemplatePlanRejectsMalformedJSONValues(t *testing.T) {
	env := templateOwnershipEnv()
	backing := jsonMap{"bad": 42}
	env.Runtime.Package.symbols["map"] = SortedMapFromData(NewMapData(backing))
	if plan, err := NewTemplate(env); plan != nil || err == nil || !strings.Contains(err.Error(), "JSON map entry \"bad\" is not an LVal: int") {
		t.Fatalf("malformed JSON payload admitted or wrong reason: plan=%v err=%v", plan, err)
	}
	if backing["bad"] != 42 {
		t.Fatal("rejection changed source")
	}
}
