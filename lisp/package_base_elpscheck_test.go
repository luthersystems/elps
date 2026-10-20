// Copyright © 2026 The ELPS authors

//go:build elpscheck

package lisp

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/internal/packagetable"
)

func TestFrozenPackageBaseFingerprint(t *testing.T) {
	for _, field := range []string{"index", "funNames", "fidNames", "symbolDocs", "externals", "bindings", "pending"} {
		for _, viaVM := range []bool{false, true} {
			t.Run(field+map[bool]string{false: "/helper", true: "/NewVM"}[viaVM], func(t *testing.T) {
				source := templateOwnershipEnv()
				p := source.Runtime.Registry.DefinePackage("frozen")
				p.Put(Symbol("value"), Int(1))
				p.Export("z", "value")
				tmpl, err := NewTemplate(source, TemplateWithFrozenPackages("frozen"))
				if err != nil {
					t.Fatal(err)
				}
				checkPackageBases(tmpl.plan.packages)
				var base *packageBase
				for _, pkg := range tmpl.plan.packages {
					if pkg.name == "frozen" {
						base = pkg.base
					}
				}
				// Deliberately bypass the write gate via private fields. The normal
				// elpsvet pass excludes tests; these writes are its negative control.
				switch field {
				case "index":
					base.index = packagetable.NewMap(map[string]int{"value": 9})
				case "funNames":
					base.funNames = packagetable.NewMap(map[string]string{"f": "corrupt"})
				case "fidNames":
					base.fidNames = packagetable.NewLists(map[string][]string{"f": {"corrupt"}})
				case "symbolDocs":
					base.symbolDocs = packagetable.NewMap(map[string]string{"value": "corrupt"})
				case "externals":
					base.externals = packagetable.NewStrings([]string{"value", "z"})
				case "bindings":
					base.bindings[0].value.shared = Int(2)
				case "pending":
					base.pending++
				}
				defer func() {
					msg, _ := recover().(string)
					if !strings.Contains(msg, "frozen package frozen: shared packageBase tables changed after publication") {
						t.Fatalf("want packageBase integrity panic, got %q", msg)
					}
				}()
				if viaVM {
					_, _ = tmpl.NewVM()
				} else {
					checkPackageBases(tmpl.plan.packages)
				}
			})
		}
	}
}

// TestFrozenPackageBaseFingerprintContents changes a table's contents but
// not its length, so the check must hash each entry, not only the counts.
func TestFrozenPackageBaseFingerprintContents(t *testing.T) {
	type tables struct{ before, after func(*packageBase) }
	cases := map[string]tables{
		"index": {
			func(b *packageBase) { b.index = packagetable.NewMap(map[string]int{"a": 1, "b": 2}) },
			func(b *packageBase) { b.index = packagetable.NewMap(map[string]int{"a": 2, "b": 1}) },
		},
		"funNames": {
			func(b *packageBase) { b.funNames = packagetable.NewMap(map[string]string{"f": "x", "g": "y"}) },
			func(b *packageBase) { b.funNames = packagetable.NewMap(map[string]string{"f": "y", "g": "x"}) },
		},
		"fidNames": {
			func(b *packageBase) { b.fidNames = packagetable.NewLists(map[string][]string{"f": {"a", "b"}}) },
			func(b *packageBase) { b.fidNames = packagetable.NewLists(map[string][]string{"f": {"a"}, "g": {"b"}}) },
		},
		"fidNames value": {
			func(b *packageBase) { b.fidNames = packagetable.NewLists(map[string][]string{"f": {"a"}}) },
			func(b *packageBase) { b.fidNames = packagetable.NewLists(map[string][]string{"f": {"b"}}) },
		},
		"symbolDocs": {
			func(b *packageBase) { b.symbolDocs = packagetable.NewMap(map[string]string{"f": "doc"}) },
			func(b *packageBase) { b.symbolDocs = packagetable.NewMap(map[string]string{"f": "dog"}) },
		},
	}
	for name, c := range cases {
		t.Run(name, func(t *testing.T) {
			b := &packageBase{}
			c.before(b)
			b.publish()
			checkPackageBases([]templatePackage{{name: "p", base: b}})
			c.after(b)
			defer func() {
				msg, _ := recover().(string)
				if !strings.Contains(msg, "frozen package p: shared packageBase tables changed after publication") {
					t.Fatalf("want packageBase integrity panic, got %q", msg)
				}
			}()
			checkPackageBases([]templatePackage{{name: "p", base: b}})
		})
	}
}
