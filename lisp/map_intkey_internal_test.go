// Copyright © 2026 The ELPS authors

package lisp

import (
	"strings"
	"testing"
)

// intKeyTestMap builds a stock map mixing int, string and symbol keys, with
// a mutable (vector) value under an int key so the walkers must copy it.
func intKeyTestMap(t testing.TB) *LVal {
	t.Helper()
	m := SortedMap()
	for _, kv := range [][2]*LVal{
		{String("b"), Int(1)},
		{Int(10), Array(nil, []*LVal{Int(1), Int(2)})},
		{Symbol("a"), Int(2)},
		{Int(-3), String("neg")},
		{String("1"), String("string-one")},
		{Int(1), String("int-one")},
	} {
		if lerr := m.Map().Set(kv[0], kv[1]); lerr.Type == LError {
			t.Fatalf("set %v: %v", kv[0], lerr)
		}
	}
	return m
}

const intKeyTestMapString = `(sorted-map -3 "neg" 1 "int-one" 10 (vector 1 2) "1" "string-one" 'a 2 "b" 1)`

// checkIntKeyTestMap asserts m holds exactly intKeyTestMap's entries, in
// order and with their key types, and that its int-keyed vector is not
// shared with src (nil: skip the check).
func checkIntKeyTestMap(t testing.TB, what string, m, src *LVal) {
	t.Helper()
	if m.Type != LSortMap {
		t.Fatalf("%s: want sorted-map, got %v", what, m)
	}
	if got := m.String(); got != intKeyTestMapString {
		t.Fatalf("%s: got %s, want %s", what, got, intKeyTestMapString)
	}
	keys := m.Map().Keys()
	wantTypes := []LType{LInt, LInt, LInt, LString, LSymbol, LString}
	for i, k := range keys.Cells {
		if k.Type != wantTypes[i] {
			t.Errorf("%s: key %d %v has type %v, want %v", what, i, k, k.Type, wantTypes[i])
		}
	}
	if v, ok := m.Map().Get(Int(1)); !ok || v.Str != "int-one" {
		t.Errorf("%s: get 1 = %v, %v", what, v, ok)
	}
	if v, ok := m.Map().Get(String("1")); !ok || v.Str != "string-one" {
		t.Errorf("%s: get \"1\" = %v, %v", what, v, ok)
	}
	if src != nil {
		a, _ := m.Map().Get(Int(10))
		b, _ := src.Map().Get(Int(10))
		if a == b {
			t.Errorf("%s: mutable int-keyed value shared with the source", what)
		}
	}
}

func TestIntKeyEntriesAndKeysOrder(t *testing.T) {
	m := intKeyTestMap(t)
	checkIntKeyTestMap(t, "stock", m, nil)
	if n := m.Map().Len(); n != 6 {
		t.Fatalf("len %d", n)
	}
	entries := sortedMapEntries(m.Map())
	var got []string
	for _, e := range entries.Cells {
		got = append(got, e.Cells[0].String())
	}
	if s := strings.Join(got, " "); s != `-3 1 10 "1" 'a "b"` {
		t.Errorf("entries order %s", s)
	}
	if _, ok := m.AppendSortedPairs(nil); ok {
		t.Errorf("AppendSortedPairs must decline a map with int keys: MapPair keys are strings")
	}
	// Deleting the last int key leaves a map indistinguishable from one
	// that never had any.
	for _, k := range []int{-3, 1, 10} {
		m.Map().Del(Int(k))
	}
	if pairs, ok := m.AppendSortedPairs(nil); !ok || len(pairs) != 3 {
		t.Errorf("AppendSortedPairs after int keys removed: %v %v", pairs, ok)
	}
}

func TestIntKeyCopyMapDataAndCopy(t *testing.T) {
	m := intKeyTestMap(t)
	md, err := m.copyMapData()
	if err != nil {
		t.Fatal(err)
	}
	cp := SortedMapFromData(md)
	checkIntKeyTestMap(t, "copyMapData", cp, nil)
	// Structural: a write to the copy's int table is not seen by the source.
	cp.Map().Set(Int(99), Int(1))
	cp.Map().Del(Int(1))
	checkIntKeyTestMap(t, "source after copy write", m, nil)

	c := m.Copy()
	checkIntKeyTestMap(t, "Copy", c, m)
	c.Map().Set(Int(98), Int(1))
	checkIntKeyTestMap(t, "source after Copy write", m, nil)
}

func TestIntKeyDetach(t *testing.T) {
	m := intKeyTestMap(t)
	d, err := m.detach()
	if err != nil {
		t.Fatal(err)
	}
	checkIntKeyTestMap(t, "detach", d, m)
}

func TestIntKeyGoValue(t *testing.T) {
	gm, ok := GoMap(intKeyTestMap(t))
	if !ok {
		t.Fatal("GoMap failed")
	}
	if gm[1] != "int-one" || gm["1"] != "string-one" || gm[-3] != "neg" {
		t.Errorf("GoMap: %v", gm)
	}
}

func TestIntKeyEqual(t *testing.T) {
	a, b := intKeyTestMap(t), intKeyTestMap(t)
	if eq := a.Equal(b); eq.Str != TrueSymbol {
		t.Errorf("equal maps compare %v", eq)
	}
	b.Map().Del(Int(1))
	b.Map().Set(String("x"), String("int-one"))
	if eq := a.Equal(b); eq.Str == TrueSymbol {
		t.Errorf("unequal maps compare equal")
	}
}

// TestIntKeyTemplates forks a map with int keys through every template
// instantiation mode: the fork holds the same entries, its mutable values
// are its own, and a write to a fork's int table reaches neither the
// template nor a sibling VM.
func TestIntKeyTemplates(t *testing.T) {
	policy := TemplateWithBuiltinPolicy(func(*LVal) bool { return true })
	for _, tc := range []struct {
		name string
		opts []TemplateOption
	}{
		{"lazy", []TemplateOption{policy}},
		{"eager", []TemplateOption{policy, TemplateWithEagerInstantiation()}},
		{"frozen", []TemplateOption{policy, TemplateWithFrozenPackages(DefaultUserPackage)}},
		{"frozen-eager", []TemplateOption{policy, TemplateWithEagerInstantiation(), TemplateWithFrozenPackages(DefaultUserPackage)}},
	} {
		t.Run(tc.name, func(t *testing.T) {
			env := newForkTestEnv(t)
			m := intKeyTestMap(t)
			env.PutGlobal(Symbol("cfg"), m)
			env.PutGlobal(Symbol("cfg-alias"), m)
			// A map whose only keys are ints, and an empty one that gains
			// its first int key only after the fork.
			only := SortedMap()
			only.Map().Set(Int(2), Array(nil, []*LVal{Int(0)}))
			env.PutGlobal(Symbol("only"), only)
			env.PutGlobal(Symbol("empty"), SortedMap())

			tmpl, err := NewTemplate(env, tc.opts...)
			if err != nil {
				t.Fatal(err)
			}
			vm1, err := tmpl.NewVM()
			if err != nil {
				t.Fatal(err)
			}
			vm2, err := tmpl.NewVM()
			if err != nil {
				t.Fatal(err)
			}
			f1 := vm1.Get(Symbol("cfg"))
			checkIntKeyTestMap(t, "vm1", f1, m)
			if vm1.Get(Symbol("cfg-alias")).Native != f1.Native {
				t.Errorf("alias not preserved")
			}
			if got := vm1.Get(Symbol("only")).String(); got != `(sorted-map 2 (vector 0))` {
				t.Errorf("only: %s", got)
			}
			f1.Map().Set(Int(10), String("rewritten"))
			f1.Map().Set(Int(77), String("new"))
			vm1.Get(Symbol("empty")).Map().Set(Int(5), Int(5))
			if v, _ := vm1.Get(Symbol("cfg-alias")).Map().Get(Int(77)); v.Str != "new" {
				t.Errorf("alias does not see write: %v", v)
			}
			checkIntKeyTestMap(t, "template source", m, nil)
			checkIntKeyTestMap(t, "vm2", vm2.Get(Symbol("cfg")), m)
			if got := vm2.Get(Symbol("empty")).String(); got != `(sorted-map)` {
				t.Errorf("sibling VM saw a write: %s", got)
			}
			// Republishing a VM carries its int keys forward.
			tmpl2, err := NewTemplate(vm1, tc.opts...)
			if err != nil {
				t.Fatal(err)
			}
			vm3, err := tmpl2.NewVM()
			if err != nil {
				t.Fatal(err)
			}
			if got := vm3.Get(Symbol("empty")).String(); got != `(sorted-map 5 5)` {
				t.Errorf("republished: %s", got)
			}
			if v, _ := vm3.Get(Symbol("cfg")).Map().Get(Int(77)); v.Str != "new" {
				t.Errorf("republished cfg lost int key: %v", v)
			}
		})
	}
}

// reverseIntMap is an embedder Map holding int keys whose Entries yields
// them in reverse order: the generic walker arms must still produce the
// documented order.
type reverseIntMap struct{ keys []*LVal }

func (m *reverseIntMap) Len() int                { return len(m.keys) }
func (m *reverseIntMap) Get(*LVal) (*LVal, bool) { return Nil(), false }
func (m *reverseIntMap) Set(k, v *LVal) *LVal    { return Errorf("read only") }
func (m *reverseIntMap) Del(*LVal) *LVal         { return Errorf("read only") }
func (m *reverseIntMap) Keys() *LVal             { return QExpr(nil) }
func (m *reverseIntMap) Entries(buf []*LVal) *LVal {
	for i := range m.keys {
		k := m.keys[len(m.keys)-1-i]
		buf[i] = QExpr([]*LVal{k, Int(i)})
	}
	return Int(len(m.keys))
}

func TestIntKeyEmbedderMapWalkers(t *testing.T) {
	src := SortedMapFromData(NewMapData(&reverseIntMap{keys: []*LVal{Int(-1), Int(2), Int(10), String("a"), String("10")}}))
	want := `(sorted-map -1 4 2 3 10 2 "10" 0 "a" 1)`
	if got := src.Copy(); got.String() != want {
		t.Errorf("copy: %v", got)
	}
	d, err := src.detach()
	if err != nil || d.String() != want {
		t.Errorf("detach: %v %v", d, err)
	}
	dup := SortedMapFromData(NewMapData(&reverseIntMap{keys: []*LVal{Int(3), Int(3)}}))
	if got := dup.Copy(); got.Type != LError || !strings.Contains(got.String(), "collide") {
		t.Errorf("duplicate int keys: %v", got)
	}
	bad := SortedMapFromData(NewMapData(&reverseIntMap{keys: []*LVal{Float(1)}}))
	if got := bad.Copy(); got.Type != LError || !strings.Contains(got.String(), "unhashable type: float") {
		t.Errorf("float key: %v", got)
	}
}
