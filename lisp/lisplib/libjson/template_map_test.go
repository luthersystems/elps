package libjson_test

import (
	"testing"

	"github.com/luthersystems/elps/internal/jsonraw"
	"github.com/luthersystems/elps/lisp"
)

// JSON maps use kernel-owned storage, not an external clone factory. Different
// wrappers preserve their backing alias inside a VM but never across VMs.
func TestJSONTemplateMapPreservesBehaviorAndIdentity(t *testing.T) {
	env, _ := decodedMapEnv(t, 2)
	source := map[string]any{"source": lisp.Int(1)}
	for _, name := range []string{"first", "alias"} {
		if result := env.PutGlobal(lisp.Symbol(name), jsonraw.Wrap(source)); result.Type == lisp.LError {
			t.Fatal(result)
		}
	}
	tmpl, err := lisp.NewTemplate(env, lisp.TemplateWithBuiltinPolicy(func(*lisp.LVal) bool { return true }))
	if err != nil {
		t.Fatal(err)
	}
	first, err := tmpl.NewVM()
	if err != nil {
		t.Fatal(err)
	}
	second, err := tmpl.NewVM()
	if err != nil {
		t.Fatal(err)
	}
	child, err := lisp.NewTemplate(first, lisp.TemplateWithBuiltinPolicy(func(*lisp.LVal) bool { return true }))
	if err != nil {
		t.Fatal(err)
	}
	grandchild, err := child.NewVM()
	if err != nil {
		t.Fatal(err)
	}
	for i, vm := range []*lisp.LEnv{first, second, grandchild} {
		m := vm.Get(lisp.Symbol("first")).Map()
		alias := vm.Get(lisp.Symbol("alias")).Map()
		if m == alias {
			t.Fatal("distinct wrappers collapsed")
		}
		if got := m.Set(lisp.String("value"), lisp.Int(i+2)); !got.IsNil() {
			t.Fatal(got)
		}
		if got, found := alias.Get(lisp.String("value")); !found || got.Int != i+2 {
			t.Fatalf("VM %d lost backing alias: %v", i, got)
		}
		for name, got := range map[string]*lisp.LVal{
			"symbol set":    m.Set(lisp.Symbol("value"), lisp.Int(99)),
			"symbol delete": m.Del(lisp.Symbol("value")),
		} {
			if got.Type != lisp.LError || got.Str != "error" {
				t.Fatalf("VM %d %s lost JSON key policy: %v", i, name, got)
			}
		}
		if got, found := m.Get(lisp.Symbol("value")); found || got.Type != lisp.LError {
			t.Fatalf("VM %d symbol read lost key policy: %v", i, got)
		}
		keys := m.Keys()
		if len(keys.Cells) != 2 || keys.Cells[0].Type != lisp.LString || keys.Cells[0].Str != "source" || keys.Cells[1].Str != "value" {
			t.Fatalf("VM %d keys changed: %v", i, keys)
		}
	}
	for i, vm := range []*lisp.LEnv{first, second, grandchild} {
		m := vm.Get(lisp.Symbol("first")).Map()
		if got, found := m.Get(lisp.String("value")); !found || got.Int != i+2 {
			t.Fatalf("VM %d saw sibling mutation: %v", i, got)
		}
	}
	if got, found := source["source"].(*lisp.LVal); !found || got.Int != 1 || len(source) != 1 {
		t.Fatal("source changed")
	}
}

func TestTemplateRejectsNilJSONMap(t *testing.T) {
	env, _ := decodedMapEnv(t, 2)
	env.PutGlobal(lisp.Symbol("subject"), jsonraw.Wrap(nil))
	if tmpl, err := lisp.NewTemplate(env, lisp.TemplateWithBuiltinPolicy(func(*lisp.LVal) bool { return true })); tmpl != nil || err == nil {
		t.Fatalf("nil non-writable JSON backing accepted: template=%v error=%v", tmpl, err)
	}
}
