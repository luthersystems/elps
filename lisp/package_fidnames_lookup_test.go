// Copyright © 2026 The ELPS authors

package lisp

import (
	"fmt"
	"math/rand/v2"
	"slices"
	"testing"
)

// sharedFIDPackage returns a package that binds n names, "rec-000000" and
// up, each to its own function, all with the FID "record".  The first
// foreign names are bound to functions of another package, so FunNameByFID
// skips them.
func sharedFIDPackage(n, foreign int) (*Package, []*LVal) {
	env := NewEnv(nil)
	pkg := env.Runtime.Registry.DefinePackage("user")
	env.Runtime.Package = pkg
	fns := make([]*LVal, n)
	for i := range n {
		owner := pkg.Name
		if i < foreign {
			owner = "other"
		}
		fns[i] = FunInPackage(owner, "record", Formals(), func(*LEnv, *LVal) *LVal { return Nil() })
		pkg.Put(Symbol(fmt.Sprintf("rec-%06d", i)), fns[i])
	}
	return pkg, fns
}

// TestFIDNameSetFirstMatchParity checks that a set's sorted walk returns the
// same name, ok flag and read count as a scan of the sorted names, over
// seeded random sets and targets (luthersystems/elps#1010).
func TestFIDNameSetFirstMatchParity(t *testing.T) {
	for seed := uint64(1); seed <= 200; seed++ {
		rng := rand.New(rand.NewPCG(seed, 0x1010)) //nolint:gosec // G404: a fixed seed makes the test repeatable
		s := adoptSortedFIDNameSet(nil)
		pool := 1 + rng.IntN(400)
		for range 3 * pool {
			name := fmt.Sprintf("n%04d", rng.IntN(pool))
			if rng.IntN(4) == 0 {
				s.remove(name)
			} else {
				s.add(name)
			}
		}
		sorted := slices.Clone(s.heap)
		slices.Sort(sorted)
		heapBefore := slices.Clone(s.heap)
		for range 20 {
			// A target outside the set gives the no-match case.
			target := fmt.Sprintf("n%04d", rng.IntN(pool+pool/4+1))
			wantName, wantOK, wantRead := "", false, len(sorted)
			if i, found := slices.BinarySearch(sorted, target); found {
				wantName, wantOK, wantRead = target, true, i+1
			}
			m := s.firstMatch(func(name string) bool { return name == target })
			if m.name != wantName || m.ok != wantOK || m.read != wantRead {
				t.Fatalf("seed %d, target %q: firstMatch = %q %v %d, want %q %v %d",
					seed, target, m.name, m.ok, m.read, wantName, wantOK, wantRead)
			}
		}
		if !slices.Equal(s.heap, heapBefore) {
			t.Fatalf("seed %d: firstMatch changed the heap", seed)
		}
	}
}

// TestFIDLookupLowRankAllocates checks that FirstNameOf and FunNameByFID
// allocate nothing for a match at a low rank in a large FID set
// (luthersystems/elps#1010).
func TestFIDLookupLowRankAllocates(t *testing.T) {
	pkg, fns := sharedFIDPackage(2000, 1)
	if pkg.namesOfFID("record").set == nil {
		t.Fatal("the FID has no fidNameSet")
	}
	for _, rank := range []int{0, 1, 20} {
		allocs := testing.AllocsPerRun(100, func() {
			if _, ok, read := pkg.FirstNameOf(fns[rank]); !ok || read != rank+1 {
				t.Fatalf("FirstNameOf rank %d: ok %v, read %d", rank, ok, read)
			}
		})
		if allocs != 0 {
			t.Errorf("FirstNameOf rank %d: %v allocations, want 0", rank, allocs)
		}
	}
	allocs := testing.AllocsPerRun(100, func() {
		if name, ok, read := pkg.funNameByFID("record"); !ok || read != 2 || name != "rec-000001" {
			t.Fatalf("funNameByFID = %q %v %d, want rec-000001 true 2", name, ok, read)
		}
	})
	if allocs != 0 {
		t.Errorf("funNameByFID rank 1: %v allocations, want 0", allocs)
	}
}

// BenchmarkFirstNameOfSharedFID looks up functions ranked among the first
// 200 of 2000 functions that share one FID.
func BenchmarkFirstNameOfSharedFID(b *testing.B) {
	pkg, fns := sharedFIDPackage(2000, 0)
	pkg.FirstNameOf(fns[0])
	b.ReportAllocs()
	b.ResetTimer()
	for i := range b.N {
		if _, ok, _ := pkg.FirstNameOf(fns[i%200]); !ok {
			b.Fatal("FirstNameOf found no name")
		}
	}
}

// BenchmarkFunNameByFIDSharedFID looks up an FID whose first 100 of 2000
// names are bound to another package's functions.
func BenchmarkFunNameByFIDSharedFID(b *testing.B) {
	pkg, _ := sharedFIDPackage(2000, 100)
	b.ReportAllocs()
	b.ResetTimer()
	for range b.N {
		if _, ok := pkg.FunNameByFID("record"); !ok {
			b.Fatal("FunNameByFID found no name")
		}
	}
}

// BenchmarkBindSharedFID binds 3000 names to one function on a live FID
// index.
func BenchmarkBindSharedFID(b *testing.B) {
	names := make([]*LVal, 3000)
	for i := range names {
		names[i] = Symbol(fmt.Sprintf("rec-%06d", i))
	}
	b.ReportAllocs()
	for range b.N {
		env := NewEnv(nil)
		pkg := env.Runtime.Registry.DefinePackage("user")
		fn := FunInPackage(pkg.Name, "record", Formals(), func(*LEnv, *LVal) *LVal { return Nil() })
		pkg.FunNameByFID("record")
		for _, name := range names {
			pkg.Put(name, fn)
		}
	}
}
