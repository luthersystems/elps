// Copyright © 2026 The ELPS authors

package lisp

import (
	"fmt"
	"math/rand"
	"testing"
)

// oldSortedMapKeys is the pre-#670 sortedmap.Keys, kept verbatim as the
// oracle: it materialises every entry pair and keeps only the keys.
func oldSortedMapKeys(m sortedmap) *LVal {
	keys := sortedMapEntries(m)
	if keys.IsNil() || keys.Type == LError {
		return keys
	}
	for i := range keys.Cells {
		keys.Cells[i] = keys.Cells[i].Cells[0]
	}
	return keys
}

// oldJSONMapKeys is the pre-#670 jsonMap.Keys, kept verbatim as the oracle.
func oldJSONMapKeys(m jsonMap) (keys *LVal) {
	cells := make([]*LVal, len(m))
	keys = m.Entries(cells)
	if keys.Type == LError {
		return keys
	}
	keys = QExpr(cells)
	for i := range cells {
		cells[i] = cells[i].Cells[0]
	}
	return keys
}

// sameKeyList reports whether two key lists agree field for field on
// everything a caller can observe: list shape, order, type, spelling,
// quoting and nested cells.
func sameKeyList(t *testing.T, got, want *LVal) {
	t.Helper()
	if got.Type != want.Type || got.quoted != want.quoted || len(got.Cells) != len(want.Cells) {
		t.Fatalf("list shape: got %v %v %d, want %v %v %d",
			got.Type, got.quoted, len(got.Cells), want.Type, want.quoted, len(want.Cells))
	}
	if (got.Cells == nil) != (want.Cells == nil) {
		t.Fatalf("nil-ness of cells differs")
	}
	for i := range want.Cells {
		g, w := got.Cells[i], want.Cells[i]
		if g.Type != w.Type || g.Str != w.Str || g.quoted != w.quoted || len(g.Cells) != len(w.Cells) || g.String() != w.String() {
			t.Fatalf("key %d: got %#v, want %#v", i, g, w)
		}
	}
}

func randomKeysMaps(r *rand.Rand, n int) (sortedmap, jsonMap) {
	sm := newmap()
	jm := make(jsonMap, n)
	for i := 0; i < n; i++ {
		k := fmt.Sprintf("k%d", r.Intn(4*n+1))
		if r.Intn(3) == 0 {
			sm.Set(Symbol(k), Int(i))
		} else {
			sm.Set(String(k), Int(i))
		}
		jm[k] = Int(i)
	}
	return sm, jm
}

func TestMapKeysMatchesOldImplementation(t *testing.T) {
	r := rand.New(rand.NewSource(670))
	for iter := 0; iter < 300; iter++ {
		sm, jm := randomKeysMaps(r, r.Intn(40))
		sameKeyList(t, sm.Keys(), oldSortedMapKeys(sm))
		sameKeyList(t, jm.Keys(), oldJSONMapKeys(jm))

		// Fresh per call: no list, cell slice or key is shared between calls.
		for _, m := range []Map{sm, jm} {
			a, b := m.Keys(), m.Keys()
			if a == b || (len(a.Cells) > 0 && &a.Cells[0] == &b.Cells[0]) {
				t.Fatalf("Keys returned shared storage")
			}
			for i := range a.Cells {
				if a.Cells[i] == b.Cells[i] {
					t.Fatalf("Keys returned a shared key at %d", i)
				}
			}
		}
	}
}

func TestMapKeysAllocs(t *testing.T) {
	sm, jm := benchKeysMaps() // 100 entries, 10 of them symbol keys
	// cells + batched string keys + list header, plus Symbol and Quote per
	// symbol key.
	if got := testing.AllocsPerRun(50, func() { _ = sm.Keys() }); got != 3+2*10 {
		t.Errorf("sortedmap.Keys allocs = %v, want %d", got, 3+2*10)
	}
	if got := testing.AllocsPerRun(50, func() { _ = jm.Keys() }); got != 3 {
		t.Errorf("jsonMap.Keys allocs = %v, want 3", got)
	}
}
