// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"slices"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// The Map contract allows integer keys. Only the stock copy destination
// rejects them; alternating Entries must not choose a value hook to run first.
type copierRotatingIntMap struct {
	*intKeyedMap
	calls int
}

func (m *copierRotatingIntMap) Entries(buf []*lisp.LVal) *lisp.LVal {
	n := m.intKeyedMap.Entries(buf)
	m.calls++
	if m.calls%2 == 0 {
		slices.Reverse(buf[:n.Int])
	}
	return n
}

type copierObservedCloner struct {
	id      int
	observe func(int)
}

func (c copierObservedCloner) CloneNative() interface{} {
	c.observe(c.id)
	return c.id
}

// Regression for #643: Go evaluates c.copy(value) before calling Set, so
// relying on Set to reject the key invokes a hook for an uncopyable entry.
func TestCopyRejectsUnsupportedKeysBeforeCloningValues(t *testing.T) {
	t.Parallel()
	for _, wrapped := range []bool{false, true} {
		name := "direct"
		if wrapped {
			name = "inside_list"
		}
		t.Run(name, func(t *testing.T) {
			var calls []int
			value := func(id int) *lisp.LVal {
				return lisp.Native(copierObservedCloner{id, func(id int) { calls = append(calls, id) }})
			}
			m := &copierRotatingIntMap{intKeyedMap: &intKeyedMap{m: map[int]*lisp.LVal{
				1: value(1), 2: value(2),
			}}}
			src := lisp.SortedMapFromData(lisp.NewMapData(m))
			if wrapped {
				// The outer copy must fail too, without visiting its next value.
				src = lisp.QExpr([]*lisp.LVal{src, value(3)})
			}
			first, second := src.Copy(), src.Copy()
			for _, got := range []*lisp.LVal{first, second} {
				if got.Type != lisp.LError || !strings.Contains(got.String(), "unhashable type: int") {
					t.Fatalf("Copy = %v, want unsupported integer-key error", got)
				}
			}
			if first.String() != second.String() {
				t.Errorf("entry order changed rejection: %v versus %v", first, second)
			}
			if m.calls != 2 {
				t.Fatalf("Entries called %d times, want both permutations", m.calls)
			}
			if len(calls) != 0 {
				t.Errorf("rejected entries invoked clone hooks: %v", calls)
			}
		})
	}
}

func TestCopySupportedCustomMapStillClonesValues(t *testing.T) {
	t.Parallel()
	var calls []int
	value := func(id int) *lisp.LVal {
		return lisp.Native(copierObservedCloner{id, func(id int) { calls = append(calls, id) }})
	}
	src := lisp.SortedMapFromData(lisp.NewMapData(newCopierStringMap(map[string]*lisp.LVal{
		"b": value(2), "a": value(1),
	})))
	got := src.Copy()
	if got.Type != lisp.LSortMap || got.Map().Len() != 2 {
		t.Fatalf("Copy = %v, want two-entry map", got)
	}
	for key, want := range map[string]int{"a": 1, "b": 2} {
		v, ok := got.Map().Get(lisp.String(key))
		if !ok || v.Type != lisp.LNative || v.Native != want {
			t.Errorf("copied value at %q = %v, want cloned payload %d", key, v, want)
		}
	}
	if !slices.Equal(calls, []int{1, 2}) {
		t.Errorf("clone hook order = %v, want [1 2]", calls)
	}
}
