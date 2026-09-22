// Copyright © 2025 The ELPS authors

package lisp

import (
	"math/rand"
	"reflect"
	"sort"
	"strconv"
	"testing"
)

// exportsOracle is the implementation Exports had before the sorted-insert
// fast path replaced it.  It is kept verbatim so the tests below compare the
// new implementation against the behaviour that shipped rather than against
// a restatement of it: the membership scan reads the PRE-CALL export list,
// so names repeated within one call are appended once each, and the whole
// list is re-sorted on every call, so an unsorted list left behind by Export
// comes back sorted.
func exportsOracle(pkg *Package, sym ...string) {
	sorted := make([]string, len(sym))
	copy(sorted, sym)
	sort.Strings(sorted)
	externs := pkg.externals
addloop:
	for _, symnew := range sorted {
		for _, s := range pkg.externals {
			if s == symnew {
				continue addloop
			}
		}
		externs = append(externs, symnew)
	}
	sort.Strings(externs)
	pkg.externals = externs
}

// exportScript is one sequence of operations on a package's export list.
// Each step is either an Exports call (possibly with several names, possibly
// with duplicates) or a raw Export append, which is what the AddBuiltins
// family in lisp/env.go does and what leaves the list unsorted.
type exportStep struct {
	names []string
	raw   bool
}

func runExportScript(t *testing.T, steps []exportStep, exports func(*Package, ...string)) []string {
	t.Helper()
	pkg := NewPackage("test")
	for _, step := range steps {
		if step.raw {
			pkg.Export(step.names...)
			continue
		}
		exports(pkg, step.names...)
	}
	return pkg.externals
}

// TestExportsMatchesPreviousImplementation pins Exports byte for byte
// against the implementation it replaced, over randomized scripts that mix
// single-name exports (the shape `(export 'a 'b)` reaches, one symbol per
// call), multi-name exports, repeated names within one call, re-exports of
// a name already exported, and raw Export appends that leave the list
// unsorted between calls.
func TestExportsMatchesPreviousImplementation(t *testing.T) {
	rng := rand.New(rand.NewSource(1))
	pool := []string{"a", "b", "c", "delta", "echo", "a", "zz", "m", "b", "n"}
	for trial := range 300 {
		steps := make([]exportStep, 0, 12)
		for range 1 + rng.Intn(10) {
			n := 1
			if rng.Intn(3) == 0 {
				n = rng.Intn(4) // may be 0: Exports() with no names re-sorts
			}
			names := make([]string, n)
			for i := range names {
				names[i] = pool[rng.Intn(len(pool))]
			}
			steps = append(steps, exportStep{names: names, raw: rng.Intn(5) == 0})
		}
		want := runExportScript(t, steps, exportsOracle)
		got := runExportScript(t, steps, (*Package).Exports)
		if !reflect.DeepEqual(got, want) {
			t.Fatalf("trial %d: script %v\n got %q\nwant %q", trial, steps, got, want)
		}
	}
}

// TestExportsDuplicateHandling states the two duplicate rules explicitly, so
// a change to either is a deliberate one rather than a randomized-test
// failure to interpret.
func TestExportsDuplicateHandling(t *testing.T) {
	pkg := NewPackage("test")
	// Repeats within ONE call are each appended: the membership test reads
	// the pre-call list, which does not contain them yet.
	pkg.Exports("a", "a", "b")
	if got, want := pkg.externals, []string{"a", "a", "b"}; !reflect.DeepEqual(got, want) {
		t.Errorf("externals = %q, want %q", got, want)
	}
	// A name already exported is not appended again, however many times.
	pkg.Exports("a")
	pkg.Exports("b")
	if got, want := pkg.externals, []string{"a", "a", "b"}; !reflect.DeepEqual(got, want) {
		t.Errorf("externals = %q, want %q", got, want)
	}
	// Export appends verbatim and does not deduplicate; the next Exports
	// re-sorts what it left behind.
	pkg.Export("z", "c")
	pkg.Exports("d")
	if got, want := pkg.externals, []string{"a", "a", "b", "c", "d", "z"}; !reflect.DeepEqual(got, want) {
		t.Errorf("externals = %q, want %q", got, want)
	}
}

func BenchmarkExportsOneAtATime(b *testing.B) {
	names := make([]string, 256)
	for i := range names {
		names[i] = "symbol-" + strconv.Itoa(i)
	}
	b.ReportAllocs()
	for b.Loop() {
		pkg := NewPackage("bench")
		for _, name := range names {
			pkg.Exports(name)
		}
	}
}

// BenchmarkExportsOneAtATimeOld measures the same loop through the
// pre-fast-path implementation kept above as the oracle, so the quadratic
// cost the fast path removes stays visible next to it.
func BenchmarkExportsOneAtATimeOld(b *testing.B) {
	names := make([]string, 256)
	for i := range names {
		names[i] = "symbol-" + strconv.Itoa(i)
	}
	b.ReportAllocs()
	for b.Loop() {
		pkg := NewPackage("bench")
		for _, name := range names {
			exportsOracle(pkg, name)
		}
	}
}
