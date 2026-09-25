// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// TestMapTypedKeys pins the typed map accessors (elps#691) to the
// deprecated interface{} forms they replace.
func TestMapTypedKeys(t *testing.T) {
	m := lisp.SortedMap()
	if rc := m.MapSetString("a", lisp.Int(1)); rc.Type == lisp.LError {
		t.Fatal(rc)
	}
	if rc := m.MapSetLVal(lisp.Symbol("b"), lisp.Int(2)); rc.Type == lisp.LError {
		t.Fatal(rc)
	}
	if got := m.MapGetString("a"); got.Int != 1 {
		t.Fatalf("MapGetString(a) = %v", got)
	}
	if got := m.MapGetLVal(lisp.String("a")); got.Int != 1 {
		t.Fatalf("MapGetLVal(a) = %v", got)
	}
	// Symbol and string keys are coerced alike, as MapSet documents.
	if got := m.MapGetString("b"); got.Int != 2 {
		t.Fatalf("MapGetString(b) = %v", got)
	}
	if a, b := m.MapGet("a"), m.MapGetString("a"); a != b {
		t.Fatalf("MapGet and MapGetString disagree: %v vs %v", a, b)
	}
	if a, b := m.MapGet("missing"), m.MapGetString("missing"); a.Type != b.Type {
		t.Fatalf("missing key: MapGet = %v, MapGetString = %v", a, b)
	}
	if rc := lisp.Int(1).MapSetString("a", lisp.Nil()); rc.Type != lisp.LError {
		t.Fatalf("MapSetString on a non-map = %v, want an error", rc)
	}
}

// BenchmarkMapKeyAPI compares the typed accessors with the deprecated
// interface{} forms in one binary; compare with benchstat -col /api.
func BenchmarkMapKeyAPI(b *testing.B) {
	m := lisp.SortedMap()
	for _, k := range []string{"alpha", "beta", "gamma", "delta", "epsilon"} {
		m.MapSetString(k, lisp.Int(len(k)))
	}
	lk := lisp.String("gamma")
	var sink *lisp.LVal
	b.Run("op=get/key=lval/api=untyped", func(b *testing.B) {
		for range b.N {
			sink = m.MapGet(lk)
		}
	})
	b.Run("op=get/key=lval/api=typed", func(b *testing.B) {
		for range b.N {
			sink = m.MapGetLVal(lk)
		}
	})
	b.Run("op=get/key=string/api=untyped", func(b *testing.B) {
		b.ReportAllocs()
		for range b.N {
			sink = m.MapGet("gamma")
		}
	})
	b.Run("op=get/key=string/api=typed", func(b *testing.B) {
		b.ReportAllocs()
		for range b.N {
			sink = m.MapGetString("gamma")
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
