// Copyright © 2026 The ELPS authors

package lisp

import (
	"strings"
	"testing"
)

// Issue #622: compilation must consume a closed admitted graph, not silently
// discover an edge admission omitted. Remove one identity after a successful
// scan to model such a walker regression, without changing the source graph.
func TestTemplateCompilerRejectsMissingAdmittedIdentity(t *testing.T) {
	for _, kind := range []string{"global", "native", "sealed", "capacity-tail", "capture", "stock-map", "json-map", "root-env", "closure-env"} {
		t.Run(kind, func(t *testing.T) {
			env := templateOwnershipEnv()
			leaf := Int(17)
			subject := leaf
			missingEnv := env
			switch kind {
			case "native":
				leaf = Native(17)
				subject = leaf
			case "sealed":
				leaf.SealAST()
			case "capacity-tail":
				subject = QExpr([]*LVal{leaf}[:0])
			case "capture":
				subject = newCapturedBuiltin(capturedBuiltin{
					Package: "user", FID: "capture", Formals: Formals(), Captures: leaf,
					Eval: func(_ *LEnv, _, captures *LVal) *LVal { return captures },
				})
			case "stock-map", "json-map":
				if kind == "stock-map" {
					subject = SortedMap()
				} else {
					subject = SortedMapFromData(NewMapData(make(jsonMap)))
				}
				for key, value := range map[string]*LVal{"leaf": leaf, "self": subject} {
					if rc := subject.Map().Set(String(key), value); !rc.IsNil() {
						t.Fatal(rc)
					}
				}
			case "closure-env":
				missingEnv = NewEnv(env)
				missingEnv.scope["leaf"] = leaf
				subject = missingEnv.Lambda(Formals(), []*LVal{Symbol("leaf")})
				missingEnv.scope["self"] = subject
			}
			env.Runtime.Package.symbols["subject"] = subject
			original := *leaf
			inventory := newTemplateInventory(templateConfig{})
			if err := inventory.scan(env); err != nil {
				t.Fatal(err)
			}
			want := "value missing admitted identity"
			switch kind {
			case "sealed":
				if !inventory.sealed[leaf] {
					t.Fatal("fixture did not admit the shared value")
				}
				delete(inventory.sealed, leaf)
			case "root-env", "closure-env":
				if _, ok := inventory.envs[missingEnv]; !ok {
					t.Fatal("fixture did not admit the environment")
				}
				delete(inventory.envs, missingEnv)
				want = "environment missing admitted identity"
			default:
				if _, ok := inventory.values[leaf]; !ok {
					t.Fatal("fixture did not admit the value")
				}
				delete(inventory.values, leaf)
			}
			plan, err := compileTemplate(env, inventory)
			if err == nil || !strings.Contains(err.Error(), want) || plan.root != 0 {
				t.Fatalf("missing identity did not reject the entire plan: root=%d err=%v", plan.root, err)
			}
			if env.Runtime.Package.symbols["subject"] != subject || leaf.Type != original.Type || leaf.Int != original.Int || leaf.Native != original.Native {
				t.Fatal("compiler rejection changed source state")
			}
		})
	}
}

func TestTemplateCompilerRejectsMissingStorageIdentity(t *testing.T) {
	for _, kind := range []string{"list-of-shared-values", "empty-positive-capacity", "bytes"} {
		t.Run(kind, func(t *testing.T) {
			env := templateOwnershipEnv()
			subject := QExpr([]*LVal{Bool(true)})
			switch kind {
			case "empty-positive-capacity":
				subject.Cells = subject.Cells[:0]
			case "bytes":
				subject = Bytes([]byte("abc"))
			}
			env.Runtime.Package.symbols["subject"] = subject
			inventory := newTemplateInventory(templateConfig{})
			if err := inventory.scan(env); err != nil {
				t.Fatal(err)
			}
			storage := inventory.storage()
			want := "template: cells missing admitted storage identity"
			if kind == "bytes" {
				payload := subject.Native.(*[]byte)
				if _, ok := storage.byteViews[payload]; !ok {
					t.Fatal("fixture did not index byte backing")
				}
				delete(storage.byteViews, payload)
				want = "template: bytes missing admitted storage identity"
			} else {
				if _, ok := storage.cellViews[subject]; !ok {
					t.Fatal("fixture did not index cell backing")
				}
				delete(storage.cellViews, subject)
			}
			compiler := templateCompiler{
				storage: storage, values: inventory.values, sealed: inventory.sealed,
				bytes: make(map[*[]byte]int), cellsReady: make([]bool, len(storage.cells)),
				plan: templatePlan{values: make([]templateValue, len(inventory.valueQueue))},
			}
			_, err := compiler.value(subject)
			if err == nil {
				err = compiler.err
			}
			if err == nil || err.Error() != want {
				t.Fatalf("unindexed storage was accepted: got %v, want %q", err, want)
			}
			if kind == "bytes" {
				if string(subject.Bytes()) != "abc" {
					t.Fatal("rejection changed source bytes")
				}
			} else if cap(subject.Cells) != 1 || subject.Cells[:1][0] != Bool(true) {
				t.Fatal("rejection changed source cells")
			}
		})
	}
}

func TestTemplateAdmissionIndicesPreserveClosedCycles(t *testing.T) {
	source := templateOwnershipEnv()
	lexical := NewEnv(source)
	leaf := Int(17)
	lexical.scope["leaf"] = leaf
	closure := lexical.Lambda(Formals(), []*LVal{Symbol("leaf")})
	lexical.scope["self"] = closure
	captures := QExpr([]*LVal{leaf, closure, nil})
	callback := newCapturedBuiltin(capturedBuiltin{
		Package: "user", FID: "cycle", Formals: Formals(), Captures: captures,
		Eval: func(_ *LEnv, _, values *LVal) *LVal { return values },
	})
	captures.Cells[2] = callback
	literal := QExpr([]*LVal{Nil(), Bool(true), Int(29)})
	literal.SealAST()
	source.Runtime.Package.symbols["callback"] = callback
	source.Runtime.Package.symbols["literal"] = literal
	source.scope["leaf"] = leaf
	inventory := newTemplateInventory(templateConfig{})
	if err := inventory.scan(source); err != nil {
		t.Fatal(err)
	}
	if index, seen := inventory.values[literal]; !seen || index != 0 || !inventory.sealed[literal] {
		t.Fatal("shared root did not retain its zero-index admission record")
	}
	for index, value := range inventory.valueQueue {
		if value.sealed || isSingleton(value) || inventory.values[value] != index+1 {
			t.Fatal("private value queue includes shared storage or disagrees with its index")
		}
	}
	if len(inventory.envQueue) != 2 || inventory.envs[source] != 1 || inventory.envs[lexical] != 2 {
		t.Fatal("captured environment cycle changed admission order or duplicated an environment")
	}
	plan, err := compileTemplate(source, inventory)
	if err != nil {
		t.Fatal(err)
	}
	tmpl := &Template{plan: plan}
	leaf.Int = 99
	for range 2 {
		vm, err := tmpl.NewVM()
		if err != nil {
			t.Fatal(err)
		}
		fn := vm.Runtime.Package.symbols["callback"]
		got := vm.FunCall(fn, Nil())
		if got.Type != LSExpr || len(got.Cells) != 3 || got.Cells[0] != vm.scope["leaf"] || got.Cells[0].Int != 17 || got.Cells[2] != fn {
			t.Fatalf("explicit capture cycle or snapshot changed: %v", got.Type)
		}
		copyClosure := got.Cells[1]
		copyEnv := copyClosure.Native.(*funData).env
		if copyEnv == lexical || copyEnv.parent != vm || copyEnv.scope["self"] != copyClosure || copyEnv.scope["leaf"] != got.Cells[0] {
			t.Fatal("lexical environment cycle or aliases changed")
		}
		if vm.Runtime.Package.symbols["literal"] != literal {
			t.Fatal("shared literal lost its source identity")
		}
		if value := vm.FunCall(copyClosure, Nil()); value.Type != LInt || value.Int != 17 {
			t.Fatalf("Lisp closure lost its captured binding: %v", value)
		}
		got.Cells[0].Int = 41
		if value := vm.FunCall(copyClosure, Nil()); value.Type != LInt || value.Int != 41 {
			t.Fatalf("Lisp closure lost the live capture alias: %v", value)
		}
		if leaf.Int != 99 {
			t.Fatal("instance mutation changed source state")
		}
	}
}
