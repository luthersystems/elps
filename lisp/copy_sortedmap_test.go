// Copyright © 2026 The ELPS authors

package lisp

import (
	"fmt"
	"strconv"
	"testing"
)

// TestCopyMapDataStructuralClone pins LVal.copyMapData -- what assoc, dissoc
// and LVal.Copy build on every call -- against the contract of the
// enumerate-and-reinsert path it replaced for the stock sorted map: the same
// entries in the same order with the same key types, the value pointers
// SHARED (it copies the map's structure, not its values), and no storage
// shared with the source in either direction, the key-type map included.
func TestCopyMapDataStructuralClone(t *testing.T) {
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

	md, err := m.copyMapData()
	if err != nil {
		t.Fatalf("copyMapData: %v", err)
	}
	if md == m.Map() {
		t.Fatalf("copy returned the source MapData")
	}
	if _, ok := md.mapBacking.(sortedmap); !ok {
		t.Fatalf("copy of the stock map is backed by %T, want sortedmap", md.mapBacking)
	}
	cp := SortedMapFromData(md)

	// Same entries, same order, same key types, same value pointers.
	oe, ne := sortedMapEntries(m.Map()), sortedMapEntries(cp.Map())
	if len(oe.Cells) != len(ne.Cells) {
		t.Fatalf("entry count differs: source %d, copy %d", len(oe.Cells), len(ne.Cells))
	}
	for i := range oe.Cells {
		ok, nk := oe.Cells[i].Cells[0], ne.Cells[i].Cells[0]
		if ok.Type != nk.Type || ok.Str != nk.Str {
			t.Errorf("entry %d key: source %v (%v), copy %v (%v)", i, ok, ok.Type, nk, nk.Type)
		}
		if oe.Cells[i].Cells[1] != ne.Cells[i].Cells[1] {
			t.Errorf("entry %d value: copy holds %p, want the source's %p (values are shared, not copied)", i, ne.Cells[i].Cells[1], oe.Cells[i].Cells[1])
		}
	}

	// Independence in both directions.
	if lerr := cp.Map().Set(String("copy-only"), Int(1)); lerr.Type == LError {
		t.Fatalf("copy map set: %v", lerr)
	}
	if _, found := m.Map().Get(String("copy-only")); found {
		t.Errorf("a key set in the copy appeared in the source")
	}
	if lerr := m.Map().Set(String("source-only"), Int(1)); lerr.Type == LError {
		t.Fatalf("source map set: %v", lerr)
	}
	if _, found := cp.Map().Get(String("source-only")); found {
		t.Errorf("a key set in the source appeared in the copy")
	}
	if lerr := cp.Map().Del(String("c-string")); lerr.Type == LError {
		t.Fatalf("copy map del: %v", lerr)
	}
	if _, found := m.Map().Get(String("c-string")); !found {
		t.Errorf("a key deleted from the copy disappeared from the source")
	}

	// The key-type map is independent too (see TestForkSortedMapClone for
	// why this probe is the one that sees a shared typemap).
	if lerr := cp.Map().Set(Symbol("k"), Int(1)); lerr.Type == LError {
		t.Fatalf("copy map set: %v", lerr)
	}
	if lerr := m.Map().Set(String("k"), Int(2)); lerr.Type == LError {
		t.Fatalf("source map set: %v", lerr)
	}
	for _, p := range sortedMapEntries(m.Map()).Cells {
		if p.Cells[0].Str == "k" && p.Cells[0].Type != LString {
			t.Errorf("source key %q enumerates as %v, want string: copy and source share a typemap", p.Cells[0].Str, p.Cells[0].Type)
		}
	}
}

// TestCopyMapDataIsRightSized is the copyMapData twin of
// TestForkSortedMapClonePrunedMapIsRightSized: assoc on a map filled to 100k
// entries and pruned to 3 must not pay for the abandoned table.
func TestCopyMapDataIsRightSized(t *testing.T) {
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
	res := testing.Benchmark(func(b *testing.B) {
		b.ReportAllocs()
		for b.Loop() {
			if _, err := m.copyMapData(); err != nil {
				b.Fatal(err)
			}
		}
	})
	const limit = 64 << 10
	if got := res.AllocedBytesPerOp(); got > limit {
		t.Errorf("copy of a pruned 3-entry map allocates %d B/op, want at most %d: the copy kept the source's %d-entry table", got, limit, highWater)
	}
}

// TestCopyMapDataIsStructural is the assertion the contract tests above
// cannot make: they pass on the entries path too.  The entries path costs
// one pair list, its cells and a key per entry plus the sort, so a copy of a
// 1000-entry map made about 1030 allocations; the structural clone makes
// under ten.  A regression back to per-entry boxing fails this.
func TestCopyMapDataIsStructural(t *testing.T) {
	m := SortedMap()
	for i := range 1000 {
		if lerr := m.Map().Set(String(fmt.Sprintf("key-%04d", i)), Int(i)); lerr.Type == LError {
			t.Fatalf("map set: %v", lerr)
		}
	}
	res := testing.Benchmark(func(b *testing.B) {
		b.ReportAllocs()
		for b.Loop() {
			if _, err := m.copyMapData(); err != nil {
				b.Fatal(err)
			}
		}
	})
	const limit = 16
	if got := res.AllocsPerOp(); got > limit {
		t.Errorf("copy of a 1000-entry map makes %d allocations, want at most %d: the copy is boxing entries again", got, limit)
	}
}

// entriesOnlyMap is an embedder-style Map that offers nothing but the Map
// interface, so copyMapData has to take the entries path for it.
type entriesOnlyMap struct{ m map[string]*LVal }

func (e *entriesOnlyMap) Len() int { return len(e.m) }
func (e *entriesOnlyMap) Get(k *LVal) (*LVal, bool) {
	v, ok := e.m[k.Str]
	if !ok {
		return Nil(), false
	}
	return v, true
}
func (e *entriesOnlyMap) Set(k, v *LVal) *LVal { e.m[k.Str] = v; return Nil() }
func (e *entriesOnlyMap) Del(k *LVal) *LVal    { delete(e.m, k.Str); return Nil() }
func (e *entriesOnlyMap) Keys() *LVal          { return sortedMapEntries(e) }
func (e *entriesOnlyMap) Entries(buf []*LVal) *LVal {
	i := 0
	for k, v := range e.m {
		buf[i] = QExpr([]*LVal{String(k), v})
		i++
	}
	return Int(i)
}

// TestCopyMapDataEmbedderMapKeepsEntriesPath pins that a map with a custom
// backing is still copied through its entries into a stock sorted map, as
// before, with the same entries and shared values.
func TestCopyMapDataEmbedderMapKeepsEntriesPath(t *testing.T) {
	v1, v2 := Int(1), Array(nil, []*LVal{Int(2)})
	src := SortedMapFromData(NewMapData(&entriesOnlyMap{m: map[string]*LVal{"x": v1, "y": v2}}))
	md, err := src.copyMapData()
	if err != nil {
		t.Fatalf("copyMapData: %v", err)
	}
	if _, ok := md.mapBacking.(sortedmap); !ok {
		t.Fatalf("copy is backed by %T, want the stock sortedmap", md.mapBacking)
	}
	if md.Len() != 2 {
		t.Fatalf("copy has %d entries, want 2", md.Len())
	}
	if got, _ := md.Get(String("x")); got != v1 {
		t.Errorf("x: copy holds %p, want the source's %p", got, v1)
	}
	if got, _ := md.Get(String("y")); got != v2 {
		t.Errorf("y: copy holds %p, want the source's %p", got, v2)
	}
}

// largeStringKeyedMap binds m to a 1000-entry string-keyed sorted map.
func largeStringKeyedMap(b *testing.B, env *LEnv) {
	b.Helper()
	m := SortedMap()
	for i := range 1000 {
		if lerr := m.Map().Set(String(fmt.Sprintf("key-%04d", i)), Int(i)); lerr.Type == LError {
			b.Fatalf("map set: %v", lerr)
		}
	}
	env.PutGlobal(Symbol("m"), m)
}

// BenchmarkAssocLargeMap measures the non-mutating assoc on a 1000-entry
// map: its cost is the map copy, which is what copyMapData's structural
// clone pins.
func BenchmarkAssocLargeMap(b *testing.B) {
	env := newForkTestEnv(b)
	largeStringKeyedMap(b, env)
	b.ReportAllocs()
	for b.Loop() {
		v := env.Eval(SExpr([]*LVal{Symbol("assoc"), Symbol("m"), String("new-key"), Int(1)}))
		if v.Type == LError {
			b.Fatalf("assoc: %v", v)
		}
	}
}

// BenchmarkDissocLargeMap is the dissoc twin of BenchmarkAssocLargeMap.
func BenchmarkDissocLargeMap(b *testing.B) {
	env := newForkTestEnv(b)
	largeStringKeyedMap(b, env)
	b.ReportAllocs()
	for b.Loop() {
		v := env.Eval(SExpr([]*LVal{Symbol("dissoc"), Symbol("m"), String("key-0500")}))
		if v.Type == LError {
			b.Fatalf("dissoc: %v", v)
		}
	}
}

// BenchmarkSortedMapLiteral measures building a small string-keyed map,
// the shape of most map literals in a phylum; it pins the construction
// cost of the stock map (in particular that a string-keyed map allocates no
// key-type map).
func BenchmarkSortedMapLiteral(b *testing.B) {
	env := newForkTestEnv(b)
	expr := SExpr([]*LVal{Symbol("sorted-map"), String("a"), Int(1), String("b"), Int(2), String("c"), Int(3)})
	b.ReportAllocs()
	for b.Loop() {
		if v := env.Eval(expr); v.Type == LError {
			b.Fatalf("sorted-map: %v", v)
		}
	}
}
