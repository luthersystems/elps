// Copyright © 2026 The ELPS authors

package lisp

import (
	"runtime"
	"strconv"
	"sync"
	"testing"
	"weak"
)

func hotSetVM(t *testing.T, tmpl *Template, opts ...VMOption) *LEnv {
	t.Helper()
	vm, err := tmpl.NewVM(opts...)
	if err != nil {
		t.Fatal(err)
	}
	return vm
}

// readLib reads lib:name in vm as a program does, which builds it.
func readLib(t *testing.T, vm *LEnv, name string) *LVal {
	t.Helper()
	v, ok := vm.Runtime.Registry.Package("lib").Symbol(name)
	if !ok || v == nil {
		t.Fatalf("lib:%s is not bound", name)
	}
	return v
}

// libBuilt reports whether vm has built the value of lib:name, without
// reading the binding.
func libBuilt(t *testing.T, vm *LEnv, name string) bool {
	t.Helper()
	lib := vm.Runtime.Registry.Package("lib")
	i, ok := lib.base.index.Lookup(name)
	if !ok {
		t.Fatalf("lib:%s is not in the base", name)
	}
	return lazyInstanceOf(vm).values[lib.base.bindings[i].value.index-1] != nil
}

// TestTemplatePrewarmBuildsWhatMostVMsRead: VMWithPrewarm builds a root that
// at least half of the earlier counting VMs read, and not one that fewer
// read.
func TestTemplatePrewarmBuildsWhatMostVMsRead(t *testing.T) {
	tmpl := lazyFixture(t)
	for n := range 4 {
		vm := hotSetVM(t, tmpl)
		readLib(t, vm, "v1")
		if n == 0 {
			readLib(t, vm, "v2")
		}
	}
	vm := hotSetVM(t, tmpl, VMWithPrewarm())
	if !libBuilt(t, vm, "v1") {
		t.Error("prewarm did not build v1, which 4 of 4 VMs read")
	}
	if libBuilt(t, vm, "v2") {
		t.Error("prewarm built v2, which 1 of 4 VMs read")
	}
	if libBuilt(t, vm, "v3") {
		t.Error("prewarm built v3, which no VM read")
	}
	if got := readLib(t, vm, "v1").Cells[0].Int; got != 1 {
		t.Errorf("prewarmed v1 = %d", got)
	}
}

// TestTemplatePrewarmDoesNotTeachItself: a root that prewarm builds counts
// only when the VM reads it, so a root later VMs stop reading leaves the set.
func TestTemplatePrewarmDoesNotTeachItself(t *testing.T) {
	tmpl := lazyFixture(t)
	for range 4 {
		readLib(t, hotSetVM(t, tmpl), "v1")
	}
	// VM n is made after n-1 readers. The set is rebuilt before VMs 9 (4
	// reads of v1 by 8 readers) and 17 (4 of 16). The prewarmed VMs read v2
	// only.
	for n := 5; n <= 17; n++ {
		vm := hotSetVM(t, tmpl, VMWithPrewarm())
		if want := n < 17; libBuilt(t, vm, "v1") != want {
			t.Fatalf("VM %d: prewarm built v1 = %v, want %v", n, !want, want)
		}
		readLib(t, vm, "v2")
	}
}

// TestTemplatePrewarmIgnoresUnusedVMs: a VM that reads no root does not
// count, so VMs made ahead of demand and never used do not lower the share
// of a root that every used VM read.
func TestTemplatePrewarmIgnoresUnusedVMs(t *testing.T) {
	tmpl := lazyFixture(t)
	for n := range 7 {
		opts := []VMOption{}
		if n%2 == 0 {
			opts = append(opts, VMWithPrewarm())
		}
		hotSetVM(t, tmpl, opts...)
	}
	readLib(t, hotSetVM(t, tmpl, VMWithPrewarm()), "v1")
	for n := range 10 {
		vm := hotSetVM(t, tmpl, VMWithPrewarm())
		if !libBuilt(t, vm, "v1") {
			t.Fatalf("VM %d after the reader: prewarm did not build v1, which the one reader read", n)
		}
	}
}

// TestTemplateHotNextRebuild: bits are rebuilt at powers of two and then at
// every multiple of hotRebuildEvery readers.
func TestTemplateHotNextRebuild(t *testing.T) {
	for _, tc := range []struct{ at, want uint64 }{
		{0, 1}, {1, 2}, {2, 4}, {3, 4}, {4, 8}, {8, 16}, {15, 16}, {16, 32},
		{32, 48}, {33, 48}, {48, 64}, {64, 80}, {100, 112}, {1 << 20, 1<<20 + 16},
	} {
		if got := hotNextRebuild(tc.at); got != tc.want {
			t.Errorf("hotNextRebuild(%d) = %d, want %d", tc.at, got, tc.want)
		}
	}
}

// TestTemplatePrewarmKeepsWhatPrewarmedVMsRead: a prewarmed VM counts a read
// of a root that prewarm built for it, so a root every VM reads stays in the
// set.
func TestTemplatePrewarmKeepsWhatPrewarmedVMsRead(t *testing.T) {
	tmpl := lazyFixture(t)
	for range 4 {
		readLib(t, hotSetVM(t, tmpl), "v1")
	}
	// Rebuilds before VMs 5, 9, 17 and 33 see v1 read by every earlier VM.
	for n := 5; n <= 40; n++ {
		vm := hotSetVM(t, tmpl, VMWithPrewarm())
		if !libBuilt(t, vm, "v1") {
			t.Fatalf("VM %d: prewarm did not build v1, which every VM read", n)
		}
		readLib(t, vm, "v1")
	}
}

// TestTemplatePrewarmKeepsMapEntriesPrewarmedVMsRead: a counting VM counts a
// read of a sorted-map entry that prewarm built for it, as it counts a
// package binding: its map leaves the entry pending, so the read goes
// through lazyInstance.ref, and an entry every VM reads stays in the set.
func TestTemplatePrewarmKeepsMapEntriesPrewarmedVMsRead(t *testing.T) {
	source := templateOwnershipEnv()
	m := SortedMap()
	m.MapSetString("k", QExpr([]*LVal{Int(1), String("entry")}))
	source.Runtime.Package.Put(Symbol("m"), m)
	tmpl, err := NewTemplate(source)
	if err != nil {
		t.Fatal(err)
	}
	backings := tmpl.plan.mapBackings
	if len(backings) != 1 || len(backings[0].entries) != 1 || backings[0].entries[0].value.index == 0 {
		t.Fatalf("fixture: want one map with one per-VM entry, got %d map backings", len(backings))
	}
	entry := backings[0].entries[0].value.index - 1
	read := func(vm *LEnv) {
		t.Helper()
		mv, _ := vm.Runtime.Registry.Package("user").Symbol("m")
		if got := testMapGet(mv, "k"); got == nil || got.Cells[1].Str != "entry" {
			t.Fatalf("m[k] = %v", got)
		}
	}
	for range 4 {
		read(hotSetVM(t, tmpl))
	}
	// Rebuilds before VMs 5, 9, 17 and 33 see m[k] read by every earlier VM.
	for n := 5; n <= 40; n++ {
		vm := hotSetVM(t, tmpl, VMWithPrewarm())
		if lazyInstanceOf(vm).values[entry] == nil {
			t.Fatalf("VM %d: prewarm did not build m[k], which every VM read", n)
		}
		read(vm)
	}
}

// hotBit reports whether plan value i is in h's set.
func hotBit(h *templateHotSet, i int) bool {
	return h.bits[i/64].Load()&(1<<(uint(i)%64)) != 0
}

// TestTemplateHotSetFollowsAShift: the counts halve each time the readers
// reach 2*hotWindow, so the set follows the last hotWindow to 2*hotWindow
// readers, not every reader since the template was made.  After a long run
// in which every reader read root 0, readers that read only root 1 put root
// 1 in the set and take root 0 out within about hotWindow readers.
func TestTemplateHotSetFollowsAShift(t *testing.T) {
	h := newTemplateHotSet(2)
	read := func(i int) {
		h.readers.Add(1)
		h.reads[i].Add(1)
		h.maybeRebuild()
	}
	for range 50 * hotWindow {
		read(0)
	}
	if !hotBit(h, 0) || hotBit(h, 1) {
		t.Fatalf("after %d readers of root 0: root 0 hot = %v, root 1 hot = %v", 50*hotWindow, hotBit(h, 0), hotBit(h, 1))
	}
	if r := h.readers.Load(); r >= 2*hotWindow {
		t.Fatalf("after %d readers the count is %d, not below %d", 50*hotWindow, r, 2*hotWindow)
	}
	n := 0
	for hotBit(h, 0) || !hotBit(h, 1) {
		if n > hotWindow+2*hotRebuildEvery {
			t.Fatalf("after %d readers of root 1 only: root 0 hot = %v, root 1 hot = %v", n, hotBit(h, 0), hotBit(h, 1))
		}
		read(1)
		n++
	}
}

// TestTemplateHotSetSampling: the first hotCountAll VMs count their root
// reads, then the VMs that hotSample picks, for as long as the template makes
// VMs: decay keeps the counts small, so counting never stops.
func TestTemplateHotSetSampling(t *testing.T) {
	tmpl := lazyFixture(t)
	for made := uint64(1); made <= hotCountAll+8*hotCountEvery; made++ {
		vm := hotSetVM(t, tmpl)
		want := made <= hotCountAll || hotSample(made)
		if got := lazyInstanceOf(vm).seen != nil; got != want {
			t.Fatalf("VM %d counts = %v, want %v", made, got, want)
		}
	}
	const late = 1 << 40
	tmpl.plan.hot.made.Store(late - 1)
	counted := 0
	for made := uint64(late); made < late+8*hotCountEvery; made++ {
		want := hotSample(made)
		if got := lazyInstanceOf(hotSetVM(t, tmpl)).seen != nil; got != want {
			t.Fatalf("VM %d counts = %v, want %v", made, got, want)
		}
		if want {
			counted++
		}
	}
	if counted == 0 {
		t.Fatalf("none of the %d VMs made after VM %d counts", 8*hotCountEvery, late-1)
	}
}

// TestTemplateHotSampleCoversEveryPhase: hotSample picks about one VM in
// hotCountEvery in each phase of a workload that repeats every period VMs,
// so a pool whose consumers cycle through period kinds of work counts each
// kind in its share. made%hotCountEvery picks one phase only when period
// divides hotCountEvery: with period 4, every counting VM does the same work.
func TestTemplateHotSampleCoversEveryPhase(t *testing.T) {
	const perPhase = 1 << 14
	const want = perPhase / hotCountEvery
	for _, start := range []uint64{hotCountAll + 1, 1 << 40} {
		for _, period := range []uint64{1, 2, 3, 4, 5, 8, 16, 32, 48, 64} {
			counted := make([]int, period)
			for made := start; made < start+period*perPhase; made++ {
				if hotSample(made) {
					counted[made%period]++
				}
			}
			for phase, n := range counted {
				if n < want*3/4 || n > want*5/4 {
					t.Errorf("from VM %d, period %d, phase %d: %d of %d VMs count, want about %d", start, period, phase, n, perPhase, want)
				}
			}
		}
	}
}

// TestTemplatePrewarmFollowsPeriodicWork: VM n does work n%4, as in a pool
// whose consumers cycle through 4 endpoints. The VMs of works 1 to 3, three
// in four, read v11, and the VMs of work 0 read v10, so prewarm builds v11
// and not v10. If every counting VM after the first hotCountAll did the same
// work, the set would follow that work, not the VMs.
func TestTemplatePrewarmFollowsPeriodicWork(t *testing.T) {
	tmpl := lazyFixture(t)
	for n := 1; n <= 1000; n++ {
		vm := hotSetVM(t, tmpl, VMWithPrewarm())
		if n%4 == 0 {
			readLib(t, vm, "v10")
		} else {
			readLib(t, vm, "v11")
		}
	}
	vm := hotSetVM(t, tmpl, VMWithPrewarm())
	if !libBuilt(t, vm, "v11") {
		t.Error("prewarm did not build v11, which 3 in 4 VMs read")
	}
	if libBuilt(t, vm, "v10") {
		t.Error("prewarm built v10, which 1 in 4 VMs read")
	}
}

// TestTemplateHotSetOnlyForLazyPlans: only lazy VMs count root reads and
// prewarm, so an eager plan has no hot set, and VMWithPrewarm on it builds
// the VM as NewVM does.
func TestTemplateHotSetOnlyForLazyPlans(t *testing.T) {
	if lazyFixture(t).plan.hot == nil {
		t.Error("a lazy plan has no hot set")
	}
	tmpl := lazyFixture(t, TemplateWithEagerInstantiation())
	if tmpl.plan.hot != nil {
		t.Error("an eager plan has a hot set")
	}
	vm := hotSetVM(t, tmpl, VMWithPrewarm())
	if got := readLib(t, vm, "v7").Cells[0].Int; got != 7 {
		t.Errorf("lib:v7 = %d in a prewarmed eager VM", got)
	}
}

// retentionTemplate publishes user:m = {"k": (1)} and user:bulk, 1 MiB of
// bytes. Four VMs read m, m[k] and bulk, so prewarm builds all three.
func retentionTemplate(t *testing.T) *Template {
	t.Helper()
	source := templateOwnershipEnv()
	m := SortedMap()
	m.MapSetString("k", QExpr([]*LVal{Int(1)}))
	source.Runtime.Package.Put(Symbol("m"), m)
	source.Runtime.Package.Put(Symbol("bulk"), Bytes(make([]byte, 1<<20)))
	tmpl, err := NewTemplate(source)
	if err != nil {
		t.Fatal(err)
	}
	for range 4 {
		user := hotSetVM(t, tmpl).Runtime.Registry.Package("user")
		mv, _ := user.Symbol("m")
		testMapGet(mv, "k")
		user.Symbol("bulk")
	}
	return tmpl
}

// retainedMap makes a prewarmed VM, reads user:m but not m[k], and returns m
// and a weak pointer to the VM's bulk bytes, which prewarm built.
//
//go:noinline
func retainedMap(t *testing.T, tmpl *Template) (*LVal, weak.Pointer[[]byte]) {
	vm := hotSetVM(t, tmpl, VMWithPrewarm())
	user := vm.Runtime.Registry.Package("user")
	i, _ := user.base.index.Lookup("bulk")
	bulk := lazyInstanceOf(vm).values[user.base.bindings[i].value.index-1]
	if bulk == nil {
		t.Fatal("prewarm did not build bulk")
	}
	data, ok := bulk.Native.(*[]byte)
	if !ok {
		t.Fatalf("bulk holds %T", bulk.Native)
	}
	m, _ := user.Symbol("m")
	return m, weak.Make(data)
}

func collected(p weak.Pointer[[]byte]) bool {
	for range 3 {
		runtime.GC()
	}
	return p.Value() == nil
}

// TestTemplateRetainedMapReleasesVMWhenRead: a sorted map kept after its VM
// can keep the VM alive until each of its entries has been read (the
// Template.NewVM doc). In a VM that counts its root reads, an entry stays
// pending until it is read, even when prewarm built its value. A VM that
// does not count fills the prewarmed entry when it fills the map.
func TestTemplateRetainedMapReleasesVMWhenRead(t *testing.T) {
	t.Run("counting", func(t *testing.T) {
		tmpl := retentionTemplate(t)
		m, bulk := retainedMap(t, tmpl)
		if collected(bulk) {
			t.Fatal("m, with m[k] pending, did not keep the VM alive")
		}
		if got := testMapGet(m, "k"); got == nil || got.Cells[0].Int != 1 {
			t.Fatalf("m[k] = %v", got)
		}
		if !collected(bulk) {
			t.Error("m, read in full, keeps the VM alive")
		}
		runtime.KeepAlive(m)
	})
	t.Run("not counting", func(t *testing.T) {
		tmpl := retentionTemplate(t)
		made := uint64(hotCountAll)
		for hotSample(made + 1) {
			made++
		}
		tmpl.plan.hot.made.Store(made)
		m, bulk := retainedMap(t, tmpl)
		if !collected(bulk) {
			t.Error("m, whose one entry prewarm built, keeps the VM alive")
		}
		runtime.KeepAlive(m)
	})
}

// TestTemplatePrewarmConcurrent: VMs built and read on many goroutines at once
// learn and use one hot set, and each VM still owns its values.
func TestTemplatePrewarmConcurrent(t *testing.T) {
	tmpl := lazyFixture(t)
	var wg sync.WaitGroup
	errs := make(chan string, 8)
	for worker := range 8 {
		wg.Go(func() {
			for n := range 40 {
				vm, err := tmpl.NewVM(VMWithPrewarm())
				if err != nil {
					errs <- err.Error()
					return
				}
				name := "v" + strconv.Itoa((worker+n)%4)
				v, _ := vm.Runtime.Registry.Package("lib").Symbol(name)
				if v.Cells[0].Int != (worker+n)%4 {
					errs <- name + " = " + v.String()
					return
				}
				// A write to a prewarmed value must stay in this VM.
				v.Cells[0] = Int(-1)
			}
		})
	}
	wg.Wait()
	close(errs)
	for err := range errs {
		t.Error(err)
	}
	vm := hotSetVM(t, tmpl, VMWithPrewarm())
	for n := range 4 {
		if got := readLib(t, vm, "v"+strconv.Itoa(n)).Cells[0].Int; got != n {
			t.Errorf("v%d = %d after other VMs wrote their copies", n, got)
		}
	}
}
