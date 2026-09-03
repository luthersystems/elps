// Copyright © 2026 The ELPS authors

package lisp

import (
	"fmt"
	"strconv"
	"testing"
)

// TestForkSortedMapClone pins the structural clone forker.mapData takes for
// the stock sorted map against the contract the enumerate-and-reinsert path
// it replaced provided: same entries in the same order with the same key
// types, values remapped through the fork walk (so a mutable value is copied
// and an aliased map stays aliased), and no storage shared with the template
// in either direction.
func TestForkSortedMapClone(t *testing.T) {
	env := newForkTestEnv(t)

	m := SortedMap()
	inner := Array(nil, []*LVal{Int(1), Int(2)})
	for _, kv := range []struct {
		k, v *LVal
	}{
		{String("b-string"), inner},
		{Symbol("a-symbol"), String("sym-val")},
		{String("c-string"), Int(3)},
	} {
		if lerr := m.Map().Set(kv.k, kv.v); lerr.Type == LError {
			t.Fatalf("map set: %v", lerr)
		}
	}
	// The map is reachable under two names AND from inside one of its own
	// values: the clone must preserve both the alias and the cycle.
	if lerr := m.Map().Set(String("self"), Array(nil, []*LVal{m})); lerr.Type == LError {
		t.Fatalf("map set: %v", lerr)
	}
	env.PutGlobal(Symbol("cfg"), m)
	env.PutGlobal(Symbol("cfg-alias"), m)

	fork, err := env.Fork()
	if err != nil {
		t.Fatalf("fork: %v", err)
	}
	fm := fork.Runtime.Package.Get(Symbol("cfg"))
	if fm.Type != LSortMap {
		t.Fatalf("forked cfg: want sorted map, got %v", fm)
	}
	if fork.Runtime.Package.Get(Symbol("cfg-alias")).Native != fm.Native {
		t.Errorf("aliased binding was not remapped to the same clone")
	}
	if fm.Native == m.Native {
		t.Fatalf("forked map shares MapData with the template")
	}

	// Same entries, same order, same key types.
	oe, ne := sortedMapEntries(m.Map()), sortedMapEntries(fm.Map())
	if len(oe.Cells) != len(ne.Cells) {
		t.Fatalf("entry count differs: template %d, fork %d", len(oe.Cells), len(ne.Cells))
	}
	for i := range oe.Cells {
		ok, nk := oe.Cells[i].Cells[0], ne.Cells[i].Cells[0]
		if ok.Type != nk.Type || ok.Str != nk.Str {
			t.Errorf("entry %d key: template %v (%v), fork %v (%v)", i, ok, ok.Type, nk, nk.Type)
		}
	}

	// A mutable value is copied, not shared; the cycle points at the clone.
	fv, _ := fm.Map().Get(String("b-string"))
	if fv == inner {
		t.Errorf("mutable vector value shared with the template")
	}
	if eq := fv.Equal(inner); eq.Type != LSymbol || eq.Str != TrueSymbol {
		t.Errorf("vector value differs: %v vs %v", fv, inner)
	}
	self, _ := fm.Map().Get(String("self"))
	if got := self.Cells[1].Cells[0].Native; got != fm.Native { // Cells[1] holds an array's elements
		t.Errorf("self-reference points at %p, want the clone %p", got, fm.Native)
	}

	// Independence in both directions.
	if lerr := fm.Map().Set(String("fork-only"), Int(1)); lerr.Type == LError {
		t.Fatalf("fork map set: %v", lerr)
	}
	if _, found := m.Map().Get(String("fork-only")); found {
		t.Errorf("a key set in the fork appeared in the template")
	}
	if lerr := m.Map().Set(String("template-only"), Int(1)); lerr.Type == LError {
		t.Fatalf("template map set: %v", lerr)
	}
	if _, found := fm.Map().Get(String("template-only")); found {
		t.Errorf("a key set in the template appeared in the fork")
	}
	fv.Cells[1].Cells[0] = Int(99) //elps:mutates the fork's private copy, to prove the template's vector is untouched
	if inner.Cells[1].Cells[0].Int != 1 {
		t.Errorf("mutating the fork's vector value changed the template's")
	}

	// The key-type map is independent too.  Get and Len never consult it,
	// so a shared typemap is invisible to the probes above; it shows when
	// the fork stores a SYMBOL key and the template then stores a STRING
	// key of the same name: a shared typemap makes the template enumerate
	// its string key as a symbol (and a concurrent fork's Set would be a
	// concurrent write to the template's map).
	if lerr := fm.Map().Set(Symbol("k"), Int(1)); lerr.Type == LError {
		t.Fatalf("fork map set: %v", lerr)
	}
	if lerr := m.Map().Set(String("k"), Int(2)); lerr.Type == LError {
		t.Fatalf("template map set: %v", lerr)
	}
	for _, p := range sortedMapEntries(m.Map()).Cells {
		if p.Cells[0].Str == "k" && p.Cells[0].Type != LString {
			t.Errorf("template key %q enumerates as %v, want string: fork and template share a typemap", p.Cells[0].Str, p.Cells[0].Type)
		}
	}
}

// TestForkSortedMapClonePrunedMapIsRightSized pins that the clone is sized
// to the map's live entries, not to the template table's high-water mark.
// Go maps never shrink after deletes, and a maps.Clone-based clone copied
// the abandoned table: a map filled to 100k entries and pruned to 3 cost
// every fork 3.7MB and several times the wall time, instead of ~0.2MB.
func TestForkSortedMapClonePrunedMapIsRightSized(t *testing.T) {
	env := newForkTestEnv(t)
	m := SortedMap()
	const highWater = 100_000
	for i := range highWater {
		if lerr := m.Map().Set(String(strconv.Itoa(i)), Int(i)); lerr.Type == LError {
			t.Fatalf("map set: %v", lerr)
		}
	}
	for i := 3; i < highWater; i++ {
		m.Map().Del(String(strconv.Itoa(i)))
	}
	if m.Map().Len() != 3 {
		t.Fatalf("pruned map has %d entries, want 3", m.Map().Len())
	}
	env.PutGlobal(Symbol("staging"), m)

	res := testing.Benchmark(func(b *testing.B) {
		b.ReportAllocs()
		for b.Loop() {
			if _, err := env.Fork(); err != nil {
				b.Fatalf("fork: %v", err)
			}
		}
	})
	// The whole fork of this small env is ~0.25MB; the abandoned 100k-slot
	// table alone is ~3.5MB.
	const limit = 1 << 20
	if got := res.AllocedBytesPerOp(); got > limit {
		t.Errorf("fork of a pruned 3-entry map allocates %d B/op, want at most %d: the clone kept the template's %d-entry table", got, limit, highWater)
	}
}

// BenchmarkForkSortedMap measures Fork on a template whose mutable state is
// dominated by load-time sorted maps, the shape of a production phylum's
// rating tables and templates.  It exists so the benchmark gate sees the
// fork's map-cloning cost directly rather than diluted across an env-wide
// fork; the structural clone in forker.mapData is what it pins.
func BenchmarkForkSortedMap(b *testing.B) {
	env := newForkTestEnv(b)
	for i := range 20 {
		m := SortedMap()
		for j := range 200 {
			var v *LVal
			if j%2 == 0 {
				v = String(fmt.Sprintf("value-%d-%d", i, j))
			} else {
				v = Array(nil, []*LVal{Int(j), String("x")})
			}
			if lerr := m.Map().Set(String(fmt.Sprintf("key-%03d", j)), v); lerr.Type == LError {
				b.Fatalf("map set: %v", lerr)
			}
		}
		env.PutGlobal(Symbol(fmt.Sprintf("table-%d", i)), m)
	}
	b.ReportAllocs()
	for b.Loop() {
		if _, err := env.Fork(); err != nil {
			b.Fatalf("fork: %v", err)
		}
	}
}
