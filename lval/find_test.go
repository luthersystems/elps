// Copyright © 2026 The ELPS authors

package lval

import (
	"fmt"
	"math/rand"
	"strings"
	"testing"
)

// findLinear is the reference for find: the first key whose keyID equals
// key's.
func findLinear(v Value, key Value) (int, bool) {
	id, ok := keyID(key)
	if !ok {
		return 0, false
	}
	for i, k := range v.keys {
		if kid, _ := keyID(k); kid == id {
			return i, true
		}
	}
	return 0, false
}

var findSpellings = []string{"a", "b", "~a", ":a", "::a", "true", "false", "~", ":", "", "~~a", "~$a", "z", "-1", "~i1"}

func randomFindKey(r *rand.Rand) Value {
	s := findSpellings[r.Intn(len(findSpellings))]
	switch r.Intn(6) {
	case 5:
		// No map key: find must never match it (its key text is "").
		return MakeFloat(float64(r.Intn(3)))
	case 0:
		return MakeString(s)
	case 1:
		return Value{kind: Symbol, str: s}
	case 2:
		return Value{kind: Keyword, str: s}
	case 3:
		return MakeBool(r.Intn(2) == 0)
	default:
		return MakeInt(int64(r.Intn(41)-20) * int64(r.Intn(3)*1000+1))
	}
}

// TestFindMatchesLinearScan holds the binary search in find to the linear
// keyID scan it replaced, over random maps whose keys alias each other
// (luthersystems/elps#979).
func TestFindMatchesLinearScan(t *testing.T) {
	r := rand.New(rand.NewSource(1)) //nolint:gosec // a repeatable test sequence
	for trial := range 50000 {
		var m Value
		for n := r.Intn(14); n > 0; n-- {
			m.Set(randomFindKey(r), MakeInt(int64(n)))
		}
		for i := 1; i < len(m.keys); i++ {
			a, _ := keyText(m.keys[i-1])
			b, _ := keyText(m.keys[i])
			c := strings.Compare(a, b)
			if c > 0 || (c == 0 && isKeyKind(m.keys[i-1].kind) && isKeyKind(m.keys[i].kind)) {
				t.Fatalf("trial %d: keys out of order: %q before %q", trial, a, b)
			}
		}
		for range 8 {
			k := randomFindKey(r)
			wantI, wantOK := findLinear(m, k)
			gotI, gotOK := m.find(k)
			if wantOK != gotOK || wantI != gotI {
				t.Fatalf("trial %d: map %v key %v (%s): find (%d,%v), linear scan (%d,%v)",
					trial, m, k, k.Kind(), gotI, gotOK, wantI, wantOK)
			}
		}
	}
}

// TestFindAllocatesNothing: a lookup of any key kind allocates nothing
// (luthersystems/elps#979).
func TestFindAllocatesNothing(t *testing.T) {
	m := MakeMap(MakeString("a"), MakeInt(1), MakeKeyword("k"), MakeInt(2), MakeInt(7), MakeInt(3),
		MakeBool(true), MakeInt(4), MakeSymbol("s"), MakeInt(5))
	for _, k := range []Value{MakeString("a"), MakeKeyword("k"), MakeInt(7), MakeBool(true), MakeSymbol("s"), MakeString(":k"), MakeString("~x")} {
		if n := testing.AllocsPerRun(100, func() { m.find(k) }); n != 0 {
			t.Errorf("find(%v %s) allocates %v times", k, k.Kind(), n)
		}
	}
}

// TestFindSkipsNonKeys: a float in a map shares the key text "" with the
// string "" but is no key, so Get("") must not find it and Set("") must
// not replace it.
func TestFindSkipsNonKeys(t *testing.T) {
	m := MakeMap(MakeFloat(1.5), MakeInt(7))
	if v, ok := m.Get(""); ok {
		t.Fatalf(`Get("") found %v in a map with only a float key`, v)
	}
	m.Set(MakeString(""), MakeInt(8))
	if len(m.Keys()) != 2 {
		t.Fatalf(`Set("") replaced the float key: keys %v`, m.Keys())
	}
	if v, ok := m.Get(""); !ok || v.String() != "8" {
		t.Fatalf(`Get("") = %v, %v after Set; want 8`, v, ok)
	}
}

func benchmarkGet(b *testing.B, n int) {
	kv := make([]Value, 0, 2*n)
	keys := make([]string, n)
	for i := range n {
		keys[i] = fmt.Sprintf("field_%04d", i)
		kv = append(kv, MakeString(keys[i]), MakeInt(int64(i)))
	}
	m := MakeMap(kv...)
	b.ReportAllocs()
	for b.Loop() {
		for _, k := range keys {
			if _, ok := m.Get(k); !ok {
				b.Fatal("missing key")
			}
		}
	}
}

func BenchmarkGet8(b *testing.B)   { benchmarkGet(b, 8) }
func BenchmarkGet500(b *testing.B) { benchmarkGet(b, 500) }
