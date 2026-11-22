// Copyright © 2026 The ELPS authors

package lisp

import (
	"fmt"
	"runtime"
	"testing"
)

// measureAllocBytes runs fn once after a GC cycle and returns the bytes
// allocated during fn.
func measureAllocBytes(fn func()) uint64 {
	runtime.GC()
	var before, after runtime.MemStats
	runtime.ReadMemStats(&before)
	fn()
	runtime.ReadMemStats(&after)
	if after.TotalAlloc < before.TotalAlloc {
		return 0
	}
	return after.TotalAlloc - before.TotalAlloc
}

// TestFIDIndexSharedFIDScaling verifies that binding and unbinding many names
// under a single shared FID scales near-linearly rather than quadratically
// (luthersystems/elps#1010).
func TestFIDIndexSharedFIDScaling(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping scaling test under -short")
	}

	const (
		smallN   = 300
		largeN   = 10 * smallN
		maxRatio = 25.0
	)

	makeNames := func(n int) []string {
		names := make([]string, n)
		for i := range n {
			names[i] = fmt.Sprintf("rec-%06d", i)
		}
		return names
	}
	smallNames := makeNames(smallN)
	largeNames := makeNames(largeN)

	t.Run("live-index-binds", func(t *testing.T) {
		run := func(names []string) {
			env := templateOwnershipEnv()
			pkg := env.Runtime.Package
			fn := FunInPackage(pkg.Name, "record", Formals(), func(env *LEnv, args *LVal) *LVal {
				return Nil()
			})
			// Turn the FID index on before the bulk binds.
			pkg.FunNameByFID("record")
			for _, name := range names {
				pkg.Put(Symbol(name), fn)
			}
			if got, ok, _ := pkg.FirstNameOf(fn); !ok || got != names[0] {
				t.Fatalf("FirstNameOf = %q %v, want %q true", got, ok, names[0])
			}
		}
		// Warm up.
		run(smallNames[:10])
		smallBytes := measureAllocBytes(func() { run(smallNames) })
		largeBytes := measureAllocBytes(func() { run(largeNames) })
		ratio := float64(largeBytes) / float64(max(smallBytes, 1))
		t.Logf("live-index-binds: n=%d alloc=%d B, 10n=%d alloc=%d B, ratio=%.2fx",
			smallN, smallBytes, largeN, largeBytes, ratio)
		if ratio >= maxRatio {
			t.Fatalf("live-index-binds alloc ratio %.2fx >= %.1fx (n=%d: %d B, 10n=%d: %d B)",
				ratio, maxRatio, smallN, smallBytes, largeN, largeBytes)
		}
	})

	t.Run("template-vm-binds", func(t *testing.T) {
		source := templateOwnershipEnv()
		tmpl, err := NewTemplate(source, TemplateWithFrozenPackages(DefaultUserPackage))
		if err != nil {
			t.Fatal(err)
		}
		run := func(names []string) {
			vm, err := tmpl.NewVM()
			if err != nil {
				t.Fatal(err)
			}
			pkg := vm.Runtime.Package
			fn := FunInPackage(pkg.Name, "record", Formals(), func(env *LEnv, args *LVal) *LVal {
				return Nil()
			})
			for _, name := range names {
				pkg.Put(Symbol(name), fn)
			}
			if got, ok, _ := pkg.FirstNameOf(fn); !ok || got != names[0] {
				t.Fatalf("FirstNameOf = %q %v, want %q true", got, ok, names[0])
			}
		}
		run(smallNames[:10])
		smallBytes := measureAllocBytes(func() { run(smallNames) })
		largeBytes := measureAllocBytes(func() { run(largeNames) })
		ratio := float64(largeBytes) / float64(max(smallBytes, 1))
		t.Logf("template-vm-binds: n=%d alloc=%d B, 10n=%d alloc=%d B, ratio=%.2fx",
			smallN, smallBytes, largeN, largeBytes, ratio)
		if ratio >= maxRatio {
			t.Fatalf("template-vm-binds alloc ratio %.2fx >= %.1fx (n=%d: %d B, 10n=%d: %d B)",
				ratio, maxRatio, smallN, smallBytes, largeN, largeBytes)
		}
	})

	t.Run("frozen-slot-rebinds", func(t *testing.T) {
		buildTmpl := func(names []string) *Template {
			source := templateOwnershipEnv()
			pkg := source.Runtime.Package
			fn := source.Lambda(Formals(), []*LVal{Int(1)})
			for _, name := range names {
				pkg.Put(Symbol(name), fn)
			}
			tmpl, err := NewTemplate(source, TemplateWithFrozenPackages(DefaultUserPackage))
			if err != nil {
				t.Fatal(err)
			}
			return tmpl
		}
		smallTmpl := buildTmpl(smallNames)
		largeTmpl := buildTmpl(largeNames)
		run := func(tmpl *Template, names []string) {
			vm, err := tmpl.NewVM()
			if err != nil {
				t.Fatal(err)
			}
			pkg := vm.Runtime.Package
			fid := pkg.Get(Symbol(names[0])).FID()
			for _, name := range names {
				pkg.Put(Symbol(name), Int(1))
			}
			if !pkg.Frozen() {
				t.Fatal("rebinding existing slots thawed the frozen package")
			}
			if _, ok := pkg.FunNameByFID(fid); ok {
				t.Fatal("FunNameByFID found a name after unbinding all slots")
			}
		}
		smallBytes := measureAllocBytes(func() { run(smallTmpl, smallNames) })
		largeBytes := measureAllocBytes(func() { run(largeTmpl, largeNames) })
		ratio := float64(largeBytes) / float64(max(smallBytes, 1))
		t.Logf("frozen-slot-rebinds: n=%d alloc=%d B, 10n=%d alloc=%d B, ratio=%.2fx",
			smallN, smallBytes, largeN, largeBytes, ratio)
		if ratio >= maxRatio {
			t.Fatalf("frozen-slot-rebinds alloc ratio %.2fx >= %.1fx (n=%d: %d B, 10n=%d: %d B)",
				ratio, maxRatio, smallN, smallBytes, largeN, largeBytes)
		}
	})

	t.Run("delete-min-in-order", func(t *testing.T) {
		run := func(names []string) {
			env := templateOwnershipEnv()
			pkg := env.Runtime.Package
			fn := FunInPackage(pkg.Name, "record", Formals(), func(env *LEnv, args *LVal) *LVal {
				return Nil()
			})
			for _, name := range names {
				pkg.Put(Symbol(name), fn)
			}
			// Build the live index after binding, then delete the minimum name
			// repeatedly while querying FirstNameOf after each deletion.
			if got, ok, _ := pkg.FirstNameOf(fn); !ok || got != names[0] {
				t.Fatalf("initial FirstNameOf = %q %v, want %q true", got, ok, names[0])
			}
			for i, name := range names {
				pkg.Put(Symbol(name), Int(i))
				got, ok, _ := pkg.FirstNameOf(fn)
				if i+1 < len(names) {
					if !ok || got != names[i+1] {
						t.Fatalf("step %d: FirstNameOf = %q %v, want %q true", i, got, ok, names[i+1])
					}
				} else if ok {
					t.Fatalf("final FirstNameOf = %q true, want false", got)
				}
			}
		}
		smallBytes := measureAllocBytes(func() { run(smallNames) })
		largeBytes := measureAllocBytes(func() { run(largeNames) })
		allocRatio := float64(largeBytes) / float64(max(smallBytes, 1))
		// Only bytes are checked: a time ratio fails under CPU load (CI
		// runs the race and elpscheck suites at once), and the old
		// copy-per-write code fails the byte check as well.
		t.Logf("delete-min-in-order: n=%d alloc=%d B, 10n=%d alloc=%d B, ratio=%.2fx",
			smallN, smallBytes, largeN, largeBytes, allocRatio)
		if allocRatio >= maxRatio {
			t.Fatalf("delete-min-in-order alloc ratio %.2fx >= %.1fx", allocRatio, maxRatio)
		}
	})
}
