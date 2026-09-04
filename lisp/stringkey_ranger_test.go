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

// TestForkTakesStringKeyRangerPath pins the fork's ranger path: the copy is
// the stock map with an empty key-type map and the same keys; the values
// went through the fork walk (a mutable value is copied, a value that
// reaches back to the map closes on the clone, a second binding maps to
// the same clone); and the template is untouched.
func TestForkTakesStringKeyRangerPath(t *testing.T) {
	env := newForkTestEnv(t)
	arr := Array(nil, []*LVal{Int(1), Int(2)})
	r := &rangerMap{m: map[string]*LVal{"n": Int(7), "arr": arr}}
	m := SortedMapFromData(NewMapData(r))
	r.m["self"] = Array(nil, []*LVal{m})
	env.PutGlobal(Symbol("doc"), m)
	env.PutGlobal(Symbol("doc-alias"), m)

	fork, err := env.Fork()
	if err != nil {
		t.Fatalf("fork: %v", err)
	}
	fm := fork.Runtime.Package.Get(Symbol("doc"))
	if fm.Type != LSortMap || fm.Native == m.Native {
		t.Fatalf("forked doc: %v (native %p, template %p)", fm, fm.Native, m.Native)
	}
	sm, ok := fm.Map().mapBacking.(sortedmap)
	if !ok {
		t.Fatalf("fork copy is backed by %T, want the stock sortedmap", fm.Map().mapBacking)
	}
	if len(sm.tm) != 0 {
		t.Errorf("fork copy has key types %v, want none for string keys", sm.tm)
	}
	if fork.Runtime.Package.Get(Symbol("doc-alias")).Native != fm.Native {
		t.Errorf("aliased binding was not remapped to the same clone")
	}
	if got := fm.Map().Len(); got != 3 {
		t.Fatalf("fork copy has %d entries, want 3", got)
	}
	if v, _ := fm.Map().Get(String("n")); v.Type != LInt || v.Int != 7 {
		t.Errorf("n: got %v", v)
	}
	fv, _ := fm.Map().Get(String("arr"))
	if fv == arr {
		t.Errorf("mutable array value is shared with the template")
	}
	if eq := fv.Equal(arr); eq.Type != LSymbol || eq.Str != TrueSymbol {
		t.Errorf("array value differs: %v vs %v", fv, arr)
	}
	self, _ := fm.Map().Get(String("self"))
	if got := self.Cells[1].Cells[0].Native; got != fm.Native { // Cells[1] holds an array's elements
		t.Errorf("self-reference points at %p, want the clone %p", got, fm.Native)
	}
	if lerr := fm.Map().Set(String("fork-only"), Int(1)); lerr.Type == LError {
		t.Fatalf("fork set: %v", lerr)
	}
	if _, found := r.m["fork-only"]; found {
		t.Errorf("a key set in the fork appeared in the template")
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
		if _, err := env.Fork(); err != nil {
			refused = true
		}
	}()
	if !refused {
		t.Errorf("Fork silently succeeded on a map whose enumeration fails")
	}
}
