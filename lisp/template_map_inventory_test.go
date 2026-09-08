// Copyright © 2026 The ELPS authors

package lisp

import (
	"reflect"
	"testing"
)

// Publication inventories source graph identities, not the temporary Lisp
// keys that Entries manufactures from Go strings (issue #622).
func TestTemplateInventoryVisitsOnlyMapGraphValues(t *testing.T) {
	for _, kind := range []string{"stock", "json"} {
		t.Run(kind, func(t *testing.T) {
			first, second := new(int), new(int)
			*first, *second = 17, 29
			a, z := Native(first), Native(second)
			var subject *LVal
			if kind == "stock" {
				subject = SortedMap()
			} else {
				subject = SortedMapFromData(NewMapData(make(jsonMap)))
			}
			for _, entry := range []struct {
				key   string
				value *LVal
			}{{"z", z}, {"self", subject}, {"a", a}, {"alias", a}} {
				if rc := subject.Map().Set(String(entry.key), entry.value); !rc.IsNil() {
					t.Fatal(rc)
				}
			}
			var visited []*int
			inventory := newTemplateInventory(templateConfig{nativePolicy: func(value any) bool {
				p, ok := value.(*int)
				if ok {
					visited = append(visited, p)
				}
				return ok
			}})
			if err := inventory.val(subject); err != nil {
				t.Fatal(err)
			}
			if !reflect.DeepEqual(visited, []*int{first, second}) {
				t.Fatalf("admission visited aliases twice or values out of key order: %v", visited)
			}
			if len(inventory.values) != 3 || inventory.values[subject] == 0 || inventory.values[a] == 0 || inventory.values[z] == 0 {
				t.Fatalf("inventory includes non-source identities: got %d, want map and two values", len(inventory.values))
			}
			// A value hidden under either map backing still requires admission;
			// ignoring the recursive error must never publish a partial graph.
			env := templateOwnershipEnv()
			env.Runtime.Package.symbols["subject"] = subject
			if tmpl, err := NewTemplate(env); tmpl != nil || err == nil || err.Error() != "template: user:subject: native *int has no template immutability declaration" {
				t.Fatalf("unapproved map value accepted or wrong error: template=%v error=%v", tmpl, err)
			}
			if self, ok := subject.Map().Get(String("self")); !ok || self != subject {
				t.Fatal("inventory changed the source cycle")
			}
			if *first != 17 || *second != 29 || subject.Map().Len() != 4 {
				t.Fatal("inventory changed source values")
			}
		})
	}
}

func TestTemplateInventoryJSONRawValidationPrecedesCallbacks(t *testing.T) {
	value := Native(new(int))
	backing := jsonMap{"a": value, "y": nil, "z": 42}
	calls := 0
	inventory := newTemplateInventory(templateConfig{nativePolicy: func(any) bool {
		calls++
		return true
	}})
	err := inventory.val(SortedMapFromData(NewMapData(backing)))
	if err == nil || err.Error() != `JSON map entry "y" is not an LVal: <nil>` {
		t.Fatalf("raw JSON validation changed: %v", err)
	}
	if calls != 0 {
		t.Fatalf("malformed JSON map invoked native admission %d times", calls)
	}
	if backing["a"] != value || backing["y"] != nil || backing["z"] != 42 || len(backing) != 3 {
		t.Fatal("rejection changed the source map")
	}

	// A typed nil is a valid graph edge, unlike a raw JSON null left undecoded.
	var nilValue *LVal
	valid := jsonMap{"nil": nilValue, "value": Int(17)}
	env := templateOwnershipEnv()
	env.Runtime.Package.symbols["subject"] = SortedMapFromData(NewMapData(valid))
	tmpl, err := NewTemplate(env)
	if err != nil {
		t.Fatal(err)
	}
	vm, err := tmpl.NewVM()
	if err != nil {
		t.Fatal(err)
	}
	got := vm.Runtime.Package.symbols["subject"].Map().mapBacking.(jsonMap)
	if nilEntry, ok := got["nil"]; !ok || nilEntry != nilValue {
		t.Fatalf("typed nil entry changed: %v, present=%t", nilEntry, ok)
	}
	if item, ok := got["value"].(*LVal); !ok || item.Type != LInt || item.Int != 17 {
		t.Fatalf("valid map value changed: %v", got["value"])
	}
}
