package lisp

import "testing"

// TestMapDataGetString checks that GetString finds what Get finds for a
// string or symbol key, misses with a Go nil, and builds no key value.
func TestMapDataGetString(t *testing.T) {
	m := SortedMap()
	m.MapSet(String("a"), Int(1))
	m.MapSet(Symbol("b"), Int(2))
	m.MapSet(String("e"), Errorf("boom"))
	md, ok := m.Native.(*MapData)
	if !ok {
		t.Fatal("sorted-map has no *MapData")
	}
	for _, k := range []string{"a", "b", "e"} {
		want, wok := md.Get(String(k))
		got, gok := md.GetString(k)
		if gok != wok || got != want {
			t.Errorf("GetString(%q) = %v, %v; Get = %v, %v", k, got, gok, want, wok)
		}
	}
	if got, ok := md.GetString("missing"); ok || got != nil {
		t.Errorf("GetString(missing) = %v, %v; want nil, false", got, ok)
	}
	var none *MapData
	if got, ok := none.GetString("a"); ok || got != nil {
		t.Errorf("nil MapData: GetString = %v, %v; want nil, false", got, ok)
	}
	if n := testing.AllocsPerRun(100, func() { _, _ = md.GetString("a") }); n != 0 {
		t.Errorf("GetString allocates %v times per call, want 0", n)
	}
}
