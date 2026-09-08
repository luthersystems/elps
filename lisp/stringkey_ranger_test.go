// Copyright © 2026 The ELPS authors

package lisp

import (
	"errors"
	"strings"
	"testing"
)

// rangerMap is a Map that implements StringKeyRanger and whose Entries
// panics: it proves that Fork and copyMapData take the ranger path, which
// the libjson-level tests cannot (they pass on the entries path too).
type rangerMap struct {
	m   map[string]*LVal
	err error
}

func (r *rangerMap) Len() int { return len(r.m) }
func (r *rangerMap) Get(k *LVal) (*LVal, bool) {
	v, ok := r.m[k.Str]
	if !ok {
		return Nil(), false
	}
	return v, true
}
func (r *rangerMap) Set(k, v *LVal) *LVal { r.m[k.Str] = v; return Nil() }
func (r *rangerMap) Del(k *LVal) *LVal    { delete(r.m, k.Str); return Nil() }
func (r *rangerMap) Keys() *LVal          { return sortedMapEntries(r) }
func (r *rangerMap) Entries([]*LVal) *LVal {
	panic("rangerMap.Entries: the entries path was taken for a StringKeyRanger")
}
func (r *rangerMap) RangeStringKeys(fn func(string, *LVal)) error {
	if r.err != nil {
		return r.err
	}
	for k, v := range r.m {
		fn(k, v)
	}
	return nil
}

// A foreign enumeration hook is useful for within-VM copy, but is not an
// ownership declaration. Template rejects it before its panic-bearing Entries
// method can execute; kernel JSON-map tests cover mutable values/cycles/aliases.
func TestTemplateRejectsForeignStringKeyRanger(t *testing.T) {
	env := newForkTestEnv(t)
	r := &rangerMap{m: map[string]*LVal{"n": Int(7)}}
	value := SortedMapFromData(NewMapData(r))
	env.PutGlobal(Symbol("doc"), value)
	vm, err := forkTestSnapshot(env)
	if vm != nil || err == nil || !strings.Contains(err.Error(), "not interpreter-owned") {
		t.Fatalf("foreign map accepted: vm=%v error=%v", vm, err)
	}
	if env.GetGlobal(Symbol("doc")) != value || r.m["n"].Int != 7 || len(r.m) != 1 {
		t.Fatal("rejection changed source map")
	}
}

// TestCopyMapDataTakesStringKeyRangerPath pins copyMapData's ranger path:
// a stock map with an empty key-type map, the same keys, and the value
// pointers SHARED.
func TestCopyMapDataTakesStringKeyRangerPath(t *testing.T) {
	v1, v2 := Int(1), Array(nil, []*LVal{Int(2)})
	src := SortedMapFromData(NewMapData(&rangerMap{m: map[string]*LVal{"x": v1, "y": v2}}))
	md, err := src.copyMapData()
	if err != nil {
		t.Fatalf("copyMapData: %v", err)
	}
	sm, ok := md.mapBacking.(sortedmap)
	if !ok {
		t.Fatalf("copy is backed by %T, want the stock sortedmap", md.mapBacking)
	}
	if len(sm.tm) != 0 {
		t.Errorf("copy has key types %v, want none", sm.tm)
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

// TestStringKeyRangerFailureIsNotSilent pins the error channel: a ranger
// that fails must not yield a partial copy.  copyMapData returns the error
// the entries path would have wrapped; Fork refuses the way it does when
// entries cannot be enumerated.
func TestStringKeyRangerFailureIsNotSilent(t *testing.T) {
	r := &rangerMap{m: map[string]*LVal{"x": Int(1)}, err: errors.New("enumeration failed")}
	src := SortedMapFromData(NewMapData(r))
	if md, err := src.copyMapData(); err == nil || !strings.Contains(err.Error(), "enumeration failed") {
		t.Errorf("copyMapData: got (%v, %v), want the ranger's error", md, err)
	}

	env := newForkTestEnv(t)
	env.PutGlobal(Symbol("doc"), src)
	refused := false
	func() {
		defer func() {
			if recover() != nil {
				refused = true
			}
		}()
		if _, err := forkTestSnapshot(env); err != nil {
			refused = true
		}
	}()
	if !refused {
		t.Errorf("Fork silently succeeded on a map whose enumeration fails")
	}
}
