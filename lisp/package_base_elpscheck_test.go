// Copyright © 2026 The ELPS authors

//go:build elpscheck

package lisp

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/internal/packagetable"
)

func TestFrozenPackageBaseFingerprint(t *testing.T) {
	for _, field := range []string{"index", "funNames", "symbolDocs", "externals"} {
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
				case "symbolDocs":
					base.symbolDocs = packagetable.NewMap(map[string]string{"value": "corrupt"})
				case "externals":
					base.externals = packagetable.NewStrings([]string{"value", "z"})
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
