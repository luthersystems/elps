// Copyright © 2026 The ELPS authors

package lisp

import (
	"fmt"
	"math/rand/v2"
	"slices"
	"strconv"
	"sync"
	"testing"

	"github.com/luthersystems/elps/internal/packagetable"
)

// refFIDIndex is a reference copy of the original sorted-slice fidIndex
// algorithm kept for differential testing against the hybrid implementation.
type refFIDIndex struct {
	over    map[string][]string
	base    packagetable.Lists
	pending []fidWrite
	live    bool
}

func (idx *refFIDIndex) record(w fidWrite) {
	if idx.live {
		idx.write(w)
		return
	}
	idx.pending = append(idx.pending, w)
	if len(idx.pending) >= fidPendingMax {
		idx.apply()
	}
}

func (idx *refFIDIndex) write(w fidWrite) {
	if w.add {
		idx.set(w.fid, refInsertSortedName(idx.list(w.fid), w.name))
	} else {
		idx.set(w.fid, refRemoveSortedName(idx.list(w.fid), w.name))
	}
}

func (idx *refFIDIndex) apply() {
	for _, w := range idx.pending {
		idx.write(w)
	}
	clear(idx.pending)
	idx.pending = idx.pending[:0]
}

func (idx *refFIDIndex) list(fid string) []string {
	if names, ok := idx.over[fid]; ok {
		return names
	}
	return slices.Collect(idx.base.List(fid))
}

func (idx *refFIDIndex) set(fid string, names []string) {
	if len(names) == 0 {
		names = nil
		if idx.base.ListLen(fid) == 0 {
			delete(idx.over, fid)
			return
		}
	}
	if idx.over == nil {
		idx.over = make(map[string][]string, 1)
	}
	idx.over[fid] = names
}

func (idx *refFIDIndex) pairs() []packagetable.Pair {
	var out []packagetable.Pair
	for p := range idx.base.All() {
		if _, ok := idx.over[p.Key]; !ok {
			out = append(out, p)
		}
	}
	for fid, names := range idx.over {
		for _, name := range names {
			out = append(out, packagetable.Pair{Key: fid, Value: name})
		}
	}
	return out
}

func refInsertSortedName(names []string, name string) []string {
	i, found := slices.BinarySearch(names, name)
	if found {
		return names
	}
	out := make([]string, 0, len(names)+1)
	out = append(out, names[:i]...)
	out = append(out, name)
	return append(out, names[i:]...)
}

func refRemoveSortedName(names []string, name string) []string {
	i, found := slices.BinarySearch(names, name)
	if !found {
		return names
	}
	if len(names) == 1 {
		return nil
	}
	out := make([]string, 0, len(names)-1)
	out = append(out, names[:i]...)
	return append(out, names[i+1:]...)
}

// assertFIDHeapValid checks the structural invariants of every fidNameSet in
// idx.over.
func assertFIDHeapValid(t *testing.T, idx *fidIndex) {
	t.Helper()
	for fid, entry := range idx.over {
		if entry.set == nil {
			if !slices.IsSorted(entry.list) {
				t.Fatalf("fid %q: small slice not sorted: %v", fid, entry.list)
			}
			if len(entry.list) > fidSetThreshold {
				t.Fatalf("fid %q: small slice length %d exceeds threshold %d", fid, len(entry.list), fidSetThreshold)
			}
			continue
		}
		s := entry.set
		if len(s.heap) == 0 {
			t.Fatalf("fid %q: empty fidNameSet retained in overlay", fid)
		}
		if len(s.pos) != len(s.heap) {
			t.Fatalf("fid %q: pos len %d != heap len %d", fid, len(s.pos), len(s.heap))
		}
		for i, name := range s.heap {
			if p, ok := s.pos[name]; !ok || p != i {
				t.Fatalf("fid %q: pos[%q] = %d %v, want %d", fid, name, p, ok, i)
			}
			if i > 0 {
				parent := (i - 1) / 2
				if s.heap[parent] >= name {
					t.Fatalf("fid %q: heap invariant broken at parent %d (%q) >= child %d (%q)",
						fid, parent, s.heap[parent], i, name)
				}
			}
		}
	}
}

// assertFIDIndexEqual checks that got matches ref for every FID in fids and
// in their serialized pairs tables.
func assertFIDIndexEqual(t *testing.T, got *fidIndex, ref *refFIDIndex, fids []string) {
	t.Helper()
	assertFIDHeapValid(t, got)
	for _, fid := range fids {
		want := ref.list(fid)
		v := got.view(fid)
		var gotNames []string
		if v.set != nil {
			if len(want) == 0 {
				t.Fatalf("fid %q: got non-nil set, want empty", fid)
			}
			if v.set.heap[0] != want[0] {
				t.Fatalf("fid %q: heap[0] = %q, want min %q", fid, v.set.heap[0], want[0])
			}
		}
		// A match that never succeeds makes firstMatch read every name, in
		// sorted order.
		m := v.firstMatch(func(name string) bool {
			gotNames = append(gotNames, name)
			return false
		})
		if m.ok || m.read != len(gotNames) || m.read != v.len() {
			t.Fatalf("fid %q: firstMatch = %v %d, read %d names, len %d", fid, m.ok, m.read, len(gotNames), v.len())
		}
		if !slices.Equal(gotNames, want) {
			t.Fatalf("fid %q: got %v, want %v", fid, gotNames, want)
		}
	}
	gotTable := packagetable.AdoptPairs(got.pairs())
	refTable := packagetable.AdoptPairs(ref.pairs())
	gotAll := slices.Collect(gotTable.All())
	refAll := slices.Collect(refTable.All())
	if !slices.Equal(gotAll, refAll) {
		t.Fatalf("pairs mismatch: got %d pairs, want %d pairs", len(gotAll), len(refAll))
	}
}

// TestFIDIndexParityWithReference compares fidIndex against a reference copy
// of the original sorted-slice algorithm across many seeded random sequences,
// covering base tables, threshold crossings, pending logs, duplicate adds,
// missing removes, empty-string keys and delete-min runs.
func TestFIDIndexParityWithReference(t *testing.T) {
	fids := []string{"", "f0", "f1", "f15", "f16", "f17", "record", "z"}
	namePool := make([]string, 0, 65)
	namePool = append(namePool, "")
	for i := range 64 {
		namePool = append(namePool, fmt.Sprintf("n%02d", i))
	}

	for seed := uint64(1); seed <= 64; seed++ {
		rng := rand.New(rand.NewPCG(seed, 0x1010)) //nolint:gosec // G404: deterministic seed makes the differential test repeatable

		// Build a random immutable base table spanning sizes below, at, and
		// above fidSetThreshold.
		var basePairs []packagetable.Pair
		if seed%4 != 0 {
			for _, fid := range fids {
				var count int
				switch fid {
				case "f1":
					count = 1
				case "f15":
					count = fidSetThreshold - 1
				case "f16":
					count = fidSetThreshold
				case "f17":
					count = fidSetThreshold + 1
				case "record":
					count = rng.IntN(40)
				default:
					count = rng.IntN(6)
				}
				perm := rng.Perm(len(namePool))
				for i := range count {
					basePairs = append(basePairs, packagetable.Pair{Key: fid, Value: namePool[perm[i]]})
				}
			}
		}
		base := packagetable.AdoptPairs(basePairs)
		live := seed%2 == 0
		got := &fidIndex{base: base, live: live}
		ref := &refFIDIndex{base: base, live: live}

		steps := 250
		if seed <= 2 {
			// Exercise pending log overflow past fidPendingMax.
			steps = fidPendingMax + 200
		}
		for step := range steps {
			fid := fids[rng.IntN(len(fids))]
			var w fidWrite
			switch rng.IntN(5) {
			case 0, 1, 2:
				// Add a random name (often pushing across fidSetThreshold).
				w = fidWrite{fid: fid, name: namePool[rng.IntN(len(namePool))], add: true}
			case 3:
				// Remove the current minimum when present, otherwise a random name.
				if cur := ref.list(fid); len(cur) > 0 && rng.IntN(2) == 0 {
					w = fidWrite{fid: fid, name: cur[0]}
				} else {
					w = fidWrite{fid: fid, name: namePool[rng.IntN(len(namePool))]}
				}
			case 4:
				// Clear every name of fid to exercise hiding base and set removal.
				for _, name := range ref.list(fid) {
					rw := fidWrite{fid: fid, name: name}
					got.record(rw)
					ref.record(rw)
				}
				continue
			}
			got.record(w)
			ref.record(w)
			if live && step%25 == 0 {
				assertFIDIndexEqual(t, got, ref, fids)
			}
		}
		if !live {
			got.apply()
			ref.apply()
		}
		assertFIDIndexEqual(t, got, ref, fids)
	}
}

// TestPackageFIDParityMultipleFunctionsSameFID verifies that FirstNameOf and
// FunNameByFID return the exact same (name, ok, read) triple as the original
// sorted-slice algorithm when multiple distinct functions (including foreign
// functions and macros) share one FID across more than fidSetThreshold
// bindings, in cold, eager, frozen and thawed packages.
func TestPackageFIDParityMultipleFunctionsSameFID(t *testing.T) {
	refFirstNameOf := func(pkg *Package, ref *refFIDIndex, fn *LVal) (string, bool, int) {
		fd, _ := fn.Native.(*funData)
		if fd == nil {
			return "", false, 0
		}
		names := ref.list(fd.fid)
		for i, name := range names {
			if pkg.bindingIsFun(name, fd) {
				return name, true, i + 1
			}
		}
		return "", false, len(names)
	}
	refFunNameByFID := func(pkg *Package, ref *refFIDIndex, fid string) (string, bool, int) {
		names := ref.list(fid)
		for i, name := range names {
			if pkg.bindingMatchesFID(name, fid) {
				return name, true, i + 1
			}
		}
		return "", false, len(names)
	}

	modes := []string{"cold", "eager", "frozen-slot", "lazy-thaw"}
	for _, mode := range modes {
		t.Run(mode, func(t *testing.T) {
			rng := rand.New(rand.NewPCG(1010, uint64(len(mode)))) //nolint:gosec // G404: deterministic seed makes the differential test repeatable

			source := templateOwnershipEnv()
			lib := source.Runtime.Registry.DefinePackage("lib")
			other := source.Runtime.Registry.DefinePackage("other")

			// Create distinct functions sharing the same FID "shared", plus a
			// foreign function and a macro with the same FID, and one unique FID.
			makeSharedFns := func(libPkg, otherPkg *Package) []*LVal {
				fns := make([]*LVal, 0, 7)
				for range 4 {
					fns = append(fns, FunInPackage(libPkg.Name, "shared", Formals(), func(env *LEnv, args *LVal) *LVal {
						return Nil()
					}))
				}
				fns = append(fns, FunInPackage(otherPkg.Name, "shared", Formals(), func(env *LEnv, args *LVal) *LVal {
					return Nil()
				}))
				macro := FunInPackage(libPkg.Name, "shared", Formals(), func(env *LEnv, args *LVal) *LVal {
					return Nil()
				})
				macro.FunType = LFunMacro
				fns = append(fns, macro)
				fns = append(fns, FunInPackage(libPkg.Name, "unique", Formals(), func(env *LEnv, args *LVal) *LVal {
					return Nil()
				}))
				return fns
			}

			// For template modes, seed the source with template-admissible
			// lambdas under 40 names, then rebind with shared-FID functions
			// inside the VM.
			const numNames = 40
			names := make([]string, numNames)
			for i := range numNames {
				names[i] = fmt.Sprintf("s%02d", i)
			}

			var vm *LEnv
			var pkg *Package
			var fns []*LVal
			ref := &refFIDIndex{live: true}

			if mode == "cold" {
				vm = source
				pkg = lib
				fns = makeSharedFns(lib, other)
				pkg.FunNameByFID("shared")
			} else {
				source.Runtime.Package = lib
				seedFn := source.Lambda(Formals(), []*LVal{Int(1)})
				for i := range numNames {
					lib.Put(Symbol(names[i]), seedFn)
				}
				var opts []TemplateOption
				switch mode {
				case "eager":
					opts = append(opts, TemplateWithEagerInstantiation())
				case "frozen-slot":
					opts = append(opts, TemplateWithFrozenPackages("lib"))
				case "lazy-thaw":
					opts = append(opts, TemplateWithFrozenPackages("lib"))
				}
				tmpl, err := NewTemplate(source, opts...)
				if err != nil {
					t.Fatal(err)
				}
				vm, err = tmpl.NewVM()
				if err != nil {
					t.Fatal(err)
				}
				pkg = vm.Runtime.Registry.Package("lib")
				otherPkg := vm.Runtime.Registry.Package("other")
				ref.base = pkg.fidNameLists()
				fns = makeSharedFns(pkg, otherPkg)
				if mode == "lazy-thaw" {
					// Bind one new name to thaw the package before the random run.
					pkg.Put(Symbol("thaw-trigger"), Int(0))
				}
			}

			// Track what ref expects for each name so ref mirrors pkg writes.
			boundFID := make(map[string]string)
			for _, name := range names {
				if id := pkg.bindingIdentity(name); id.ok && id.data != nil {
					boundFID[name] = id.data.fid
				}
			}

			checkAll := func(step int) {
				t.Helper()
				beforeLazy := 0
				if lazyInstanceOf(vm) != nil {
					beforeLazy = lazyInstanceOf(vm).count
				}
				for idx, fn := range fns {
					gName, gOK, gRead := pkg.FirstNameOf(fn)
					wName, wOK, wRead := refFirstNameOf(pkg, ref, fn)
					if gName != wName || gOK != wOK || gRead != wRead {
						t.Fatalf("step %d fn[%d]: FirstNameOf = (%q, %v, %d), want (%q, %v, %d)",
							step, idx, gName, gOK, gRead, wName, wOK, wRead)
					}
				}
				for _, fid := range []string{"shared", "unique", "missing"} {
					gName, gOK, gRead := pkg.funNameByFID(fid)
					wName, wOK, wRead := refFunNameByFID(pkg, ref, fid)
					if gName != wName || gOK != wOK || gRead != wRead {
						t.Fatalf("step %d fid %q: funNameByFID = (%q, %v, %d), want (%q, %v, %d)",
							step, fid, gName, gOK, gRead, wName, wOK, wRead)
					}
				}
				if lazyInstanceOf(vm) != nil {
					if after := lazyInstanceOf(vm).count; after != beforeLazy {
						t.Fatalf("step %d: lookups materialized %d lazy values", step, after-beforeLazy)
					}
				}
			}

			for step := range 200 {
				name := names[rng.IntN(len(names))]
				if prevFID, ok := boundFID[name]; ok {
					ref.write(fidWrite{fid: prevFID, name: name})
					delete(boundFID, name)
				}
				if rng.IntN(4) == 0 {
					pkg.Put(Symbol(name), Int(step))
				} else {
					fn := fns[rng.IntN(len(fns))]
					pkg.Put(Symbol(name), fn)
					boundFID[name] = fn.FID()
					ref.write(fidWrite{fid: fn.FID(), name: name, add: true})
				}
				if step%10 == 0 || step == 199 {
					checkAll(step)
				}
			}
			if mode == "frozen-slot" && !pkg.Frozen() {
				t.Fatal("frozen-slot mode thawed unexpectedly")
			}
		})
	}
}

// TestFIDIndexSettledConcurrentReadersWithSet verifies that concurrent readers
// calling settledFIDs on an unsettled index whose overlay already holds a
// fidNameSet (from frozen slot writes before thaw, or from a pending-log
// overflow) never race on the set's mutable map or heap.
func TestFIDIndexSettledConcurrentReadersWithSet(t *testing.T) {
	t.Run("thaw-with-slot-set-and-pending", func(t *testing.T) {
		source := templateOwnershipEnv()
		lib := source.Runtime.Registry.DefinePackage("lib")
		source.Runtime.Package = lib
		fn := source.Lambda(Formals(), []*LVal{Int(1)})
		for i := range 25 {
			lib.Put(Symbol(fmt.Sprintf("b%02d", i)), fn)
		}
		tmpl, err := NewTemplate(source, TemplateWithFrozenPackages("lib"))
		if err != nil {
			t.Fatal(err)
		}
		vm, err := tmpl.NewVM()
		if err != nil {
			t.Fatal(err)
		}
		pkg := vm.Runtime.Registry.Package("lib")
		vmFn := pkg.Get(Symbol("b00"))
		fid := vmFn.FID()

		// Rebind an existing slot while frozen so slotFidNames[fid] promotes to
		// a *fidNameSet (> fidSetThreshold names).
		pkg.Put(Symbol("b00"), Int(0))
		if !pkg.Frozen() || pkg.slotFidNames[fid].set == nil {
			t.Fatalf("expected frozen package with fidNameSet overlay, frozen=%v", pkg.Frozen())
		}
		// Thaw by adding a new binding; pkg.fids is not live and inherits the set.
		pkg.Put(Symbol("b99"), vmFn)
		// Add more pending writes touching the same FID (including removing min
		// and adding a new smallest name).
		pkg.Put(Symbol("b01"), Int(1))
		pkg.Put(Symbol("a00"), vmFn)
		if pkg.fids == nil || pkg.fids.live || len(pkg.fids.pending) == 0 || pkg.fids.over[fid].set == nil {
			t.Fatalf("expected unsettled index with set overlay and pending writes")
		}

		var wg sync.WaitGroup
		for range 16 {
			wg.Add(1)
			go func() {
				defer wg.Done()
				if got, ok, read := pkg.FirstNameOf(vmFn); !ok || got != "a00" || read != 1 {
					t.Errorf("FirstNameOf = (%q, %v, %d), want (%q, true, 1)", got, ok, read, "a00")
				}
				if got, ok := pkg.FunNameByFID(fid); !ok || got != "a00" {
					t.Errorf("FunNameByFID = (%q, %v), want (%q, true)", got, ok, "a00")
				}
			}()
		}
		wg.Wait()
	})

	t.Run("pending-overflow-then-concurrent-settle", func(t *testing.T) {
		source := templateOwnershipEnv()
		tmpl, err := NewTemplate(source, TemplateWithEagerInstantiation())
		if err != nil {
			t.Fatal(err)
		}
		vm, err := tmpl.NewVM()
		if err != nil {
			t.Fatal(err)
		}
		pkg := vm.Runtime.Package
		fn := FunInPackage(pkg.Name, "record", Formals(), func(env *LEnv, args *LVal) *LVal {
			return Nil()
		})
		// Overflow fidPendingMax so idx.apply() builds a fidNameSet in idx.over
		// while idx.live stays false, then queue extra pending writes on it.
		for i := range fidPendingMax + 30 {
			pkg.Put(Symbol("r"+strconv.Itoa(10000+i)), fn)
		}
		pkg.Put(Symbol("r10000"), Int(0))
		pkg.Put(Symbol("a-first"), fn)
		if pkg.fids == nil || pkg.fids.live || len(pkg.fids.pending) == 0 || pkg.fids.over["record"].set == nil {
			t.Fatalf("expected unsettled index with fidNameSet after pending overflow")
		}

		var wg sync.WaitGroup
		for range 16 {
			wg.Add(1)
			go func() {
				defer wg.Done()
				if got, ok, read := pkg.FirstNameOf(fn); !ok || got != "a-first" || read != 1 {
					t.Errorf("FirstNameOf = (%q, %v, %d), want (%q, true, 1)", got, ok, read, "a-first")
				}
				if got, ok := pkg.FunNameByFID("record"); !ok || got != "a-first" {
					t.Errorf("FunNameByFID = (%q, %v), want (%q, true)", got, ok, "a-first")
				}
			}()
		}
		wg.Wait()
	})
}
