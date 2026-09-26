// Copyright © 2026 The ELPS authors

package lisp

import (
	"fmt"
	"strconv"
	"testing"
)

// TestScopeTableMapSemantics drives a scopeTable and a map through the same
// puts, updates and lookups on both sides of scopeMapThreshold, so a table
// in either representation, and one that moved from the slice to the map,
// behaves exactly like the map it replaced.
func TestScopeTableMapSemantics(t *testing.T) {
	for _, n := range []int{0, 1, 4, scopeMapThreshold, scopeMapThreshold + 1, 3 * scopeMapThreshold} {
		t.Run(strconv.Itoa(n), func(t *testing.T) {
			var tbl scopeTable
			oracle := map[string]*LVal{}
			name := func(i int) string { return fmt.Sprintf("v%d", i) }
			check := func(stage string) {
				t.Helper()
				if tbl.len() != len(oracle) {
					t.Fatalf("%s: len %d, want %d", stage, tbl.len(), len(oracle))
				}
				for i := range n + 2 {
					got, ok := tbl.get(name(i))
					want, wok := oracle[name(i)]
					if ok != wok || got != want {
						t.Fatalf("%s: get(%s) = %v %t, want %v %t", stage, name(i), got, ok, want, wok)
					}
				}
				if (tbl.m != nil) != (tbl.len() > scopeMapThreshold) || (tbl.m != nil && tbl.bindings != nil) {
					t.Fatalf("%s: map %t slice %t with %d bindings", stage, tbl.m != nil, tbl.bindings != nil, tbl.len())
				}
			}
			check("empty")
			for i := range n {
				v := Int(i)
				tbl.put(name(i), v, 0)
				oracle[name(i)] = v
			}
			check("put")
			// Re-putting overwrites in place, as a map assignment would.
			for i := 0; i < n; i += 2 {
				v := Int(-i)
				tbl.put(name(i), v, 0)
				oracle[name(i)] = v
			}
			check("overwrite")
			for i := 1; i < n+2; i += 2 {
				v := Int(100 + i)
				_, bound := oracle[name(i)]
				if got := tbl.update(name(i), v); got != bound {
					t.Fatalf("update(%s) = %t, want %t", name(i), got, bound)
				}
				if bound {
					oracle[name(i)] = v
				}
			}
			check("update")
			seen := map[string]bool{}
			tbl.each(func(name string, v *LVal) bool {
				if seen[name] || oracle[name] != v {
					t.Fatalf("each: %s duplicate %t or wrong value", name, seen[name])
				}
				seen[name] = true
				return true
			})
			if len(seen) != len(oracle) {
				t.Fatalf("each yielded %d bindings, want %d", len(seen), len(oracle))
			}
		})
	}
}

// TestAllocEnvScopeCapacity pins that a co-allocated scope has capacity
// exactly n and that a scope outgrowing it keeps every binding.
func TestAllocEnvScopeCapacity(t *testing.T) {
	for n := range 7 {
		env := allocEnvScope(n)
		env.scopeHint = n
		switch {
		case n == 0 || n > 4:
			if env.scope.bindings != nil {
				t.Fatalf("n=%d: scope preallocated", n)
			}
		case cap(env.scope.bindings) != n || len(env.scope.bindings) != 0:
			t.Fatalf("n=%d: len %d cap %d", n, len(env.scope.bindings), cap(env.scope.bindings))
		}
		for i := range n + 3 {
			if rc := env.Put(Symbol(fmt.Sprintf("s%d", i)), Int(i)); rc.Type == LError {
				t.Fatal(rc)
			}
		}
		for i := range n + 3 {
			if got, ok := env.scope.get(fmt.Sprintf("s%d", i)); !ok || got.Int != i {
				t.Fatalf("n=%d: s%d = %v", n, i, got)
			}
		}
	}
}

// TestScopeTableAppendNew pins that the instantiation path, which appends
// unique names without put's duplicate check, builds the same table put does
// on both sides of scopeMapThreshold, starting from a slice-sized table.
func TestScopeTableAppendNew(t *testing.T) {
	for _, n := range []int{1, scopeMapThreshold, scopeMapThreshold + 1, 40} {
		var viaPut scopeTable
		viaAppend := newScopeTable(min(n, scopeMapThreshold))
		for i := range n {
			name, v := fmt.Sprintf("v%02d", i), Int(i)
			viaPut.put(name, v, 1)
			viaAppend.appendNew(name, v)
		}
		if viaAppend.len() != viaPut.len() || (viaAppend.m != nil) != (viaPut.m != nil) {
			t.Fatalf("n=%d: len %d/%d map %t/%t", n, viaAppend.len(), viaPut.len(), viaAppend.m != nil, viaPut.m != nil)
		}
		for i := range n + 1 {
			name := fmt.Sprintf("v%02d", i)
			a, aok := viaAppend.get(name)
			p, pok := viaPut.get(name)
			if a != p || aok != pok {
				t.Fatalf("n=%d: get(%s) = %v %t, put-built %v %t", n, name, a, aok, p, pok)
			}
		}
	}
}

// TestScopeTableLargeHintIsMap pins that a scope sized past scopeMapThreshold
// from the start (a wide let) is the map it always was, with no slice.
func TestScopeTableLargeHintIsMap(t *testing.T) {
	var tbl scopeTable
	tbl.put("a", Int(1), scopeMapThreshold+1)
	if tbl.m == nil || tbl.bindings != nil {
		t.Fatalf("map %t slice %t", tbl.m != nil, tbl.bindings != nil)
	}
	small := newScopeTable(scopeMapThreshold)
	if small.m != nil || cap(small.bindings) != scopeMapThreshold {
		t.Fatalf("threshold-sized scope: map %t cap %d", small.m != nil, cap(small.bindings))
	}
}
