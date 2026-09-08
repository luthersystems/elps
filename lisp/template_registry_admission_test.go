// Copyright © 2026 The ELPS authors

package lisp

import (
	"strings"
	"testing"
)

// Issue #622: every root must be in the admitted graph. Public Go fields can
// describe inconsistent registries; rejecting them must precede any traversal
// or host approval callback instead of silently dropping/renaming package data.
func TestTemplateRejectsInconsistentRegistryRoots(t *testing.T) {
	for _, tc := range []struct {
		name   string
		change func(*LEnv)
		want   string
	}{
		{"key-name", func(env *LEnv) { env.Runtime.Package.Name = "renamed" }, `registry key "user" does not match package name "renamed"`},
		{"unregistered", func(env *LEnv) { env.Runtime.Package = NewPackage("outside") }, `current package "outside" is not registered by identity`},
		{"different-pointer", func(env *LEnv) { env.Runtime.Package = NewPackage("user") }, `current package "user" is not registered by identity`},
	} {
		t.Run(tc.name, func(t *testing.T) {
			source := templateOwnershipEnv()
			original := source.Runtime.Registry.Package("user")
			opaque := new(int)
			*opaque = 7
			source.scope["opaque"] = Native(opaque)
			tc.change(source)
			current := source.Runtime.Package
			calls := 0
			plan, err := NewTemplate(source, TemplateWithNativePolicy(func(any) bool { calls++; return true }))
			if plan != nil || err == nil || !strings.Contains(err.Error(), tc.want) {
				t.Fatalf("malformed root admitted or wrong reason: plan=%v err=%v", plan, err)
			}
			if calls != 0 {
				t.Fatalf("root rejection happened after %d host approval callbacks", calls)
			}
			if source.Runtime.Package != current || source.Runtime.Registry.Package("user") != original || *opaque != 7 {
				t.Fatal("rejection changed source roots")
			}
		})
	}
}

func TestTemplatePreservesCurrentPackagePresence(t *testing.T) {
	for _, tc := range []struct {
		label, name string
		current     bool
	}{
		{"named", "user", true}, {"empty", "", true}, {"none", "", false},
	} {
		t.Run(tc.label, func(t *testing.T) {
			source := NewEnv(nil)
			if tc.current {
				source.Runtime.Package = source.Runtime.Registry.DefinePackage(tc.name)
				source.Runtime.Package.symbols["value"] = Int(7)
			}
			plan, err := NewTemplate(source)
			if err != nil {
				t.Fatal(err)
			}
			vm, err := plan.NewVM()
			if err != nil {
				t.Fatal(err)
			}
			if !tc.current {
				if vm.Runtime.Package != nil {
					t.Fatal("absent current package became present")
				}
				return
			}
			got := vm.Runtime.Package
			if got == nil || got == source.Runtime.Package || got != vm.Runtime.Registry.Package(tc.name) || got.Name != tc.name || got.symbols["value"].Int != 7 {
				t.Fatalf("current package presence or contents changed: %+v", got)
			}
		})
	}
}
