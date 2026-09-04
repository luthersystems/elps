// Copyright © 2026 The ELPS authors

package libjson_test

import (
	"fmt"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
	"github.com/luthersystems/elps/parser"
)

// decodedMapEnv returns an environment with libjson loaded and the global m
// bound to a decoded JSON object of n string keys.  One value is an array,
// so a copy that must duplicate mutable values has something to duplicate.
func decodedMapEnv(t testing.TB, n int) (*lisp.LEnv, *lisp.LVal) {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	if rc := lisp.InitializeUserEnv(env); rc.Type == lisp.LError {
		t.Fatalf("InitializeUserEnv: %v", rc)
	}
	if rc := libjson.LoadPackage(env); rc.Type == lisp.LError {
		t.Fatalf("LoadPackage: %v", rc)
	}
	if rc := env.InPackage(lisp.String(lisp.DefaultUserPackage)); rc.Type == lisp.LError {
		t.Fatalf("InPackage: %v", rc)
	}
	var sb strings.Builder
	sb.WriteString(`(set 'm (json:load-string "{`)
	for i := range n {
		if i > 0 {
			sb.WriteString(",")
		}
		if i == 1 {
			fmt.Fprintf(&sb, `\"k%04d\": [1, 2]`, i)
		} else {
			fmt.Fprintf(&sb, `\"k%04d\": %d`, i, i)
		}
	}
	sb.WriteString(`}"))`)
	m := env.LoadString("decoded_map_test.lisp", sb.String())
	if m.Type != lisp.LSortMap {
		t.Fatalf("json:load-string returned %v, want a sorted map", m)
	}
	// The decoded map's backing is not reachable from outside package lisp;
	// its contract is: it rejects any key that is not a string.  That is
	// what tells a decoded map apart from the stock map its copies become.
	if lerr := m.Map().Set(lisp.Symbol("probe"), lisp.Int(0)); lerr.Type != lisp.LError {
		t.Fatalf("decoded map accepted a symbol key; json:load-string no longer returns libjson.SortedMap")
	}
	return env, m
}

// keysAndValues returns the map's keys in sorted order with the value bound
// to each, through the Map interface only.
func keysAndValues(t testing.TB, m *lisp.LVal) ([]string, []*lisp.LVal) {
	t.Helper()
	keys := m.Map().Keys()
	if keys.Type == lisp.LError {
		t.Fatalf("keys: %v", keys)
	}
	ks := make([]string, 0, len(keys.Cells))
	vs := make([]*lisp.LVal, 0, len(keys.Cells))
	for _, k := range keys.Cells {
		if k.Type != lisp.LString {
			t.Fatalf("key %v has type %v, want string", k, k.Type)
		}
		v, ok := m.Map().Get(k)
		if !ok {
			t.Fatalf("key %v enumerated but not found", k)
		}
		ks = append(ks, k.Str)
		vs = append(vs, v)
	}
	return ks, vs
}

// TestForkOfDecodedMapMatchesEntriesPath pins that Fork turns a decoded
// JSON map into what the entries path always produced for it -- a stock
// sorted map with the same string keys, mutable values copied, storage
// private to the fork -- now built through libjson.SortedMap's
// RangeStringKeys instead of a sorted pair list.
func TestForkOfDecodedMapMatchesEntriesPath(t *testing.T) {
	env, m := decodedMapEnv(t, 5)
	fork, err := env.Fork()
	if err != nil {
		t.Fatalf("fork: %v", err)
	}
	fm := fork.Runtime.Package.Get(lisp.Symbol("m"))
	if fm.Type != lisp.LSortMap {
		t.Fatalf("forked m: want sorted map, got %v", fm)
	}
	if fm.Native == m.Native {
		t.Fatalf("forked map shares MapData with the template")
	}
	// The fork's copy is the stock map, as before: unlike the decoded
	// original (see decodedMapEnv) it accepts a symbol key.
	if lerr := fm.Map().Set(lisp.Symbol("sym"), lisp.Int(0)); lerr.Type == lisp.LError {
		t.Errorf("forked copy rejected a symbol key: it is not the stock sorted map the entries path produced: %v", lerr)
	}
	if lerr := fm.Map().Del(lisp.Symbol("sym")); lerr.Type == lisp.LError {
		t.Fatalf("del: %v", lerr)
	}

	ok, ov := keysAndValues(t, m)
	nk, nv := keysAndValues(t, fm)
	if len(ok) != len(nk) {
		t.Fatalf("fork has %d keys, template %d", len(nk), len(ok))
	}
	for i := range ok {
		if ok[i] != nk[i] {
			t.Errorf("key %d: template %q, fork %q", i, ok[i], nk[i])
		}
		if eq := ov[i].Equal(nv[i]); eq.Type != lisp.LSymbol || eq.Str != lisp.TrueSymbol {
			t.Errorf("value %q differs: template %v, fork %v", ok[i], ov[i], nv[i])
		}
		if ov[i].Type == lisp.LArray && ov[i] == nv[i] {
			t.Errorf("mutable array value %q is shared between template and fork", ok[i])
		}
	}

	// Independence in both directions.
	if lerr := fm.Map().Set(lisp.String("fork-only"), lisp.Int(1)); lerr.Type == lisp.LError {
		t.Fatalf("fork set: %v", lerr)
	}
	if _, found := m.Map().Get(lisp.String("fork-only")); found {
		t.Errorf("a key set in the fork appeared in the template")
	}
	if lerr := m.Map().Set(lisp.String("template-only"), lisp.Int(1)); lerr.Type == lisp.LError {
		t.Fatalf("template set: %v", lerr)
	}
	if _, found := fm.Map().Get(lisp.String("template-only")); found {
		t.Errorf("a key set in the template appeared in the fork")
	}
}

// TestAssocOnDecodedMapMatchesEntriesPath pins the non-mutating assoc and
// dissoc on a decoded map: the result is a stock sorted map with the same
// keys, the same value POINTERS (these copy structure, not values), the
// requested change, and no effect on the original.
func TestAssocOnDecodedMapMatchesEntriesPath(t *testing.T) {
	env, m := decodedMapEnv(t, 5)
	ok, ov := keysAndValues(t, m)

	got := env.LoadString("assoc_test.lisp", `(assoc m "new" 42)`)
	if got.Type != lisp.LSortMap {
		t.Fatalf("assoc returned %v", got)
	}
	if got.Native == m.Native {
		t.Fatalf("assoc returned the original map")
	}
	if _, found := m.Map().Get(lisp.String("new")); found {
		t.Errorf("assoc mutated the original")
	}
	nk, nv := keysAndValues(t, got)
	if len(nk) != len(ok)+1 {
		t.Fatalf("assoc result has %d keys, want %d", len(nk), len(ok)+1)
	}
	for i := range ok {
		if nk[i] != ok[i] {
			t.Errorf("key %d: original %q, copy %q", i, ok[i], nk[i])
		}
		if nv[i] != ov[i] {
			t.Errorf("value %q: copy holds %p, want the original's %p (values are shared)", ok[i], nv[i], ov[i])
		}
	}
	if v, _ := got.Map().Get(lisp.String("new")); v.Type != lisp.LInt || v.Int != 42 {
		t.Errorf("new key: got %v", v)
	}
	// As before, the copy is the stock map and accepts a symbol key even
	// though the decoded original does not.
	if lerr := got.Map().Set(lisp.Symbol("sym"), lisp.Int(0)); lerr.Type == lisp.LError {
		t.Errorf("assoc result rejected a symbol key: it is not the stock sorted map the entries path produced: %v", lerr)
	}

	got = env.LoadString("dissoc_test.lisp", `(dissoc m "k0002")`)
	if got.Type != lisp.LSortMap {
		t.Fatalf("dissoc returned %v", got)
	}
	if _, found := got.Map().Get(lisp.String("k0002")); found {
		t.Errorf("dissoc result still holds the key")
	}
	if _, found := m.Map().Get(lisp.String("k0002")); !found {
		t.Errorf("dissoc mutated the original")
	}
	if got.Map().Len() != len(ok)-1 {
		t.Errorf("dissoc result has %d keys, want %d", got.Map().Len(), len(ok)-1)
	}
}

// BenchmarkForkDecodedMaps measures Fork on a template whose mutable state
// is 20 decoded JSON documents of 200 keys, the shape of a phylum that
// loads configuration or fixtures with json:load at load time.
func BenchmarkForkDecodedMaps(b *testing.B) {
	env, _ := decodedMapEnv(b, 200)
	for i := range 19 {
		if v := env.LoadString("bench.lisp", fmt.Sprintf(`(set 'm%d (json:load-string (json:dump-string m)))`, i)); v.Type == lisp.LError {
			b.Fatalf("copy: %v", v)
		}
	}
	b.ReportAllocs()
	for b.Loop() {
		if _, err := env.Fork(); err != nil {
			b.Fatalf("fork: %v", err)
		}
	}
}

// BenchmarkAssocDecodedMap measures the non-mutating assoc on a decoded
// 1000-key document: the request-handling shape of parse, then add a field.
func BenchmarkAssocDecodedMap(b *testing.B) {
	env, _ := decodedMapEnv(b, 1000)
	expr := lisp.SExpr([]*lisp.LVal{lisp.Symbol("assoc"), lisp.Symbol("m"), lisp.String("new-key"), lisp.Int(1)})
	b.ReportAllocs()
	for b.Loop() {
		if v := env.Eval(expr); v.Type == lisp.LError {
			b.Fatalf("assoc: %v", v)
		}
	}
}
