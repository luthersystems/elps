// Copyright © 2026 The ELPS authors

package lisp

import (
	"strconv"
	"testing"
)

// TestFunNamesByFID checks the first-name index on a cold package, a lazy
// template VM (frozen and not) and a thawed one: the same names, no binding
// materialized, and rebinding respected.
func TestFunNamesByFID(t *testing.T) {
	source := templateOwnershipEnv()
	lib := source.Runtime.Registry.DefinePackage("lib")
	user := source.Runtime.Package
	source.Runtime.Package = lib // the lambdas below are defined in lib
	fn := source.Lambda(Formals(), []*LVal{Int(1)})
	lib.Put(Symbol("zfn"), fn)
	lib.Put(Symbol("bfn"), fn) // an alias that sorts first
	lib.Put(Symbol("other"), source.Lambda(Formals(), []*LVal{Int(2)}))
	// A function of another package bound in lib is not lib's.
	source.Runtime.Package = user
	lib.Put(Symbol("foreign"), source.Lambda(Formals(), []*LVal{Int(3)}))
	for n := range 200 {
		lib.Put(Symbol("a"+strconv.Itoa(n)), QExpr([]*LVal{Int(n), String("s")}))
	}
	fid := fn.FID()
	want, read := lib.FunNamesByFID()
	if want[fid] != "bfn" || len(want) != 2 || read != 204 {
		t.Fatalf("cold index %v read %d", want, read)
	}
	for _, frozen := range []bool{false, true} {
		t.Run("frozen="+strconv.FormatBool(frozen), func(t *testing.T) {
			var opts []TemplateOption
			if frozen {
				opts = append(opts, TemplateWithFrozenPackages("lib"))
			}
			tmpl, err := NewTemplate(source, opts...)
			if err != nil {
				t.Fatal(err)
			}
			vm, err := tmpl.NewVM()
			if err != nil {
				t.Fatal(err)
			}
			pkg := vm.Runtime.Registry.Package("lib")
			before := lazyInstanceOf(vm).count
			got, _ := pkg.FunNamesByFID()
			if after := lazyInstanceOf(vm).count; after != before {
				t.Fatalf("FunNamesByFID materialized %d values", after-before)
			}
			if len(got) != 2 || got[fid] != "bfn" {
				t.Fatalf("lazy index %v", got)
			}
			// Rebinding the first name moves the function to the next.
			pkg.Put(Symbol("bfn"), Int(0))
			if rebound, _ := pkg.FunNamesByFID(); rebound[fid] != "zfn" {
				t.Fatalf("after rebinding: %v", rebound)
			}
			// A new, smaller alias is seen too (thaws a frozen package; the
			// thawed table keeps unbuilt bindings pending).
			pkg.Put(Symbol("aaa"), pkg.Get(Symbol("zfn")))
			if pkg.base != nil || pkg.lazy.inst == nil {
				t.Fatal("expected a thawed package with pending bindings")
			}
			before = lazyInstanceOf(vm).count
			got, _ = pkg.FunNamesByFID()
			if after := lazyInstanceOf(vm).count; after != before {
				t.Fatalf("FunNamesByFID materialized %d values after the thaw", after-before)
			}
			if got[fid] != "aaa" || got[pkg.Get(Symbol("other")).FID()] != "other" {
				t.Fatalf("after aliasing: %v", got)
			}
			if n := pkg.NumBindings(); n != 205 {
				t.Fatalf("NumBindings %d", n)
			}
		})
	}
}
