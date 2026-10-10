// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// TestMapTypedKeys pins the typed map setters (elps#691) and Lookup.
func TestMapTypedKeys(t *testing.T) {
	m := lisp.SortedMap()
	if rc := m.MapSetString("a", lisp.Int(1)); rc.Type == lisp.LError {
		t.Fatal(rc)
	}
	if rc := m.MapSetLVal(lisp.Symbol("b"), lisp.Int(2)); rc.Type == lisp.LError {
		t.Fatal(rc)
	}
	mv, ok := lisp.AsMap(m)
	if !ok {
		t.Fatal("AsMap(SortedMap()) = false")
	}
	if got, ok := lisp.Lookup[int](mv, "a"); !ok || got != 1 {
		t.Fatalf("Lookup(a) = %v, %v", got, ok)
	}
	if got, ok := lisp.Lookup[int](mv, lisp.String("a")); !ok || got != 1 {
		t.Fatalf("Lookup(String(a)) = %v, %v", got, ok)
	}
	// Symbol and string keys are coerced alike, as MapSet documents.
	if got, ok := lisp.Lookup[int](mv, "b"); !ok || got != 2 {
		t.Fatalf("Lookup(b) = %v, %v", got, ok)
	}
	if got, ok := lisp.Lookup[*lisp.LVal](mv, "missing"); ok || got != nil {
		t.Fatalf("Lookup(missing) = %v, %v; want nil, false", got, ok)
	}
	if rc := lisp.Int(1).MapSetString("a", lisp.Nil()); rc.Type != lisp.LError {
		t.Fatalf("MapSetString on a non-map = %v, want an error", rc)
	}
}

// BenchmarkMapKeyAPI measures Lookup and compares the typed setters with
// the deprecated interface{} form; compare with benchstat -col /api.
func BenchmarkMapKeyAPI(b *testing.B) {
	m := lisp.SortedMap()
	for _, k := range []string{"alpha", "beta", "gamma", "delta", "epsilon"} {
		m.MapSetString(k, lisp.Int(len(k)))
	}
	lk := lisp.String("gamma")
	var sink *lisp.LVal
	mv, _ := lisp.AsMap(m)
	b.Run("op=get/key=lval/api=lookup", func(b *testing.B) {
		b.ReportAllocs()
		for range b.N {
			sink, _ = lisp.Lookup[*lisp.LVal](mv, lk)
		}
	})
	b.Run("op=get/key=string/api=lookup", func(b *testing.B) {
		b.ReportAllocs()
		for range b.N {
			sink, _ = lisp.Lookup[*lisp.LVal](mv, "gamma")
		}
	})
	v := lisp.Int(7)
	b.Run("op=set/key=lval/api=untyped", func(b *testing.B) {
		for range b.N {
			sink = m.MapSet(lk, v)
		}
	})
	b.Run("op=set/key=lval/api=typed", func(b *testing.B) {
		for range b.N {
			sink = m.MapSetLVal(lk, v)
		}
	})
	b.Run("op=set/key=string/api=untyped", func(b *testing.B) {
		b.ReportAllocs()
		for range b.N {
			sink = m.MapSet("gamma", v)
		}
	})
	b.Run("op=set/key=string/api=typed", func(b *testing.B) {
		b.ReportAllocs()
		for range b.N {
			sink = m.MapSetString("gamma", v)
		}
	})
	_ = sink
}
