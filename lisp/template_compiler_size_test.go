// Copyright © 2026 The ELPS authors

package lisp

import (
	"fmt"
	"strconv"
	"testing"
)

// Issue #622: publication already knows the number of private graph identities.
// Aliases and shared syntax must not inflate its descriptor storage.
func TestTemplateCompilerSizesPrivateDescriptors(t *testing.T) {
	for _, count := range []int{0, 17, 513} {
		t.Run(strconv.Itoa(count), func(t *testing.T) {
			env := NewEnv(NewEnv(templateOwnershipEnv()))
			symbols := env.Runtime.Package.symbols
			for n := range count {
				v := Int(n)
				symbols[fmt.Sprintf("value-%d", n)] = v
				symbols[fmt.Sprintf("alias-%d", n)] = v
			}
			literal := QExpr([]*LVal{Int(17), Int(29)})
			literal.SealAST()
			symbols["literal"], symbols["nil"], symbols["true"] = literal, Nil(), Bool(true)
			bytes := Bytes([]byte("abc"))
			symbols["bytes"], symbols["bytes-alias"] = bytes, Quote(bytes)
			tmpl, err := NewTemplate(env)
			if err != nil {
				t.Fatal(err)
			}
			if len(tmpl.plan.values) != count+2 || cap(tmpl.plan.values) != count+2 {
				t.Fatalf("private value descriptors: length=%d capacity=%d, want %d", len(tmpl.plan.values), cap(tmpl.plan.values), count+2)
			}
			if len(tmpl.plan.envs) != 3 || cap(tmpl.plan.envs) != 3 {
				t.Fatalf("environment descriptors: length=%d capacity=%d, want 3", len(tmpl.plan.envs), cap(tmpl.plan.envs))
			}
			if len(tmpl.plan.bytes) != 1 || cap(tmpl.plan.bytes) != 1 {
				t.Fatalf("aliased byte descriptors: length=%d capacity=%d, want 1", len(tmpl.plan.bytes), cap(tmpl.plan.bytes))
			}
			if len(tmpl.plan.packages) != 1 || cap(tmpl.plan.packages) != 1 {
				t.Fatalf("package descriptors: length=%d capacity=%d, want 1", len(tmpl.plan.packages), cap(tmpl.plan.packages))
			}
			for range 2 {
				vm, err := tmpl.NewVM()
				if err != nil {
					t.Fatal(err)
				}
				got := vm.Runtime.Package.symbols
				for n := range count {
					key, alias := fmt.Sprintf("value-%d", n), fmt.Sprintf("alias-%d", n)
					if got[key] == symbols[key] || got[key] != got[alias] || got[key].Type != LInt || got[key].Int != n {
						t.Fatalf("value %d lost identity, content or isolation", n)
					}
					got[key].Int = -1
				}
				if got["literal"] != literal || got["nil"] != Nil() || got["true"] != Bool(true) {
					t.Fatal("shared values lost their original identities")
				}
				if got["bytes"].Native == bytes.Native || got["bytes"].Native != got["bytes-alias"].Native || string(got["bytes"].Bytes()) != "abc" {
					t.Fatal("byte payload lost its alias, content or isolation")
				}
				got["bytes"].Bytes()[0] = 'z'
				if string(bytes.Bytes()) != "abc" {
					t.Fatal("instance mutation changed source bytes")
				}
			}
		})
	}
}

// Map wrapper counts are already known after admission; entry and type counts
// are known directly from the interpreter-owned backing. Neither needs growth.
func TestTemplateCompilerSizesMapDescriptors(t *testing.T) {
	for _, count := range []int{0, 17, 513} {
		for _, json := range []bool{false, true} {
			t.Run(fmt.Sprintf("entries=%d/json=%t", count, json), func(t *testing.T) {
				env := templateOwnershipEnv()
				value := SortedMap()
				if json {
					value = SortedMapFromData(NewMapData(make(jsonMap)))
				}
				for n := range count {
					key := String(strconv.Itoa(n))
					if !json {
						key = Symbol(key.Str)
					}
					if result := value.Map().Set(key, Int(n)); !result.IsNil() {
						t.Fatal(result)
					}
				}
				env.Runtime.Package.symbols["value"] = value
				env.Runtime.Package.symbols["alias"] = Quote(value)
				for n := range count {
					env.Runtime.Package.symbols["payload-"+strconv.Itoa(n)] = SortedMap()
				}
				tmpl, err := NewTemplate(env)
				if err != nil {
					t.Fatal(err)
				}
				if len(tmpl.plan.maps) != count+1 || cap(tmpl.plan.maps) != count+1 || len(tmpl.plan.mapBackings) != count+1 {
					t.Fatalf("map payload identities: len=%d cap=%d backing count=%d", len(tmpl.plan.maps), cap(tmpl.plan.maps), len(tmpl.plan.mapBackings))
				}
				backing := tmpl.plan.mapBackings[0]
				if len(backing.entries) != count || cap(backing.entries) != count {
					t.Fatalf("map entries: len=%d cap=%d want=%d", len(backing.entries), cap(backing.entries), count)
				}
				if !json && (len(backing.types) != count || cap(backing.types) != count) {
					t.Fatalf("map key types: len=%d cap=%d want=%d", len(backing.types), cap(backing.types), count)
				}
				for range 2 {
					vm, err := tmpl.NewVM()
					if err != nil {
						t.Fatal(err)
					}
					got := vm.Runtime.Package.symbols["value"]
					if got.Map() == value.Map() || got.Map() != vm.Runtime.Package.symbols["alias"].Map() || got.Map().Len() != count {
						t.Fatal("map alias, isolation or entry count changed")
					}
					for _, key := range got.Map().Keys().Cells {
						wantType := LSymbol
						if json {
							wantType = LString
						}
						if key.Type != wantType {
							t.Fatalf("key %s has type %s, want %s", key.Str, key.Type, wantType)
						}
					}
					for n := range count {
						entry, ok := got.Map().Get(String(strconv.Itoa(n)))
						if !ok || entry.Type != LInt || entry.Int != n {
							t.Fatalf("entry %d: got %v present=%t", n, entry, ok)
						}
					}
					if result := got.Map().Set(String("new"), Int(42)); !result.IsNil() {
						t.Fatal(result)
					}
					if value.Map().Len() != count {
						t.Fatal("instance mutation changed source map")
					}
				}
			})
		}
	}
}
