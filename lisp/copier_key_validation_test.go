// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"slices"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// The Map contract allows keys of any type. The stock copy destination takes
// int, string and symbol keys (#733) and rejects the rest; alternating
// Entries must not choose a value hook to run first.
type copierRotatingMap struct {
	*intKeyedMap
	calls int
	// float offers the keys as floats, which no stock map accepts.
	float bool
}

func (m *copierRotatingMap) Entries(buf []*lisp.LVal) *lisp.LVal {
	n := m.intKeyedMap.Entries(buf)
	if m.float {
		for _, pair := range buf[:n.Int] {
			pair.Cells[0] = lisp.Float(float64(pair.Cells[0].Int))
		}
	}
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

func (c copierObservedCloner) CloneNative() any {
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
			m := &copierRotatingMap{intKeyedMap: &intKeyedMap{m: map[int]*lisp.LVal{
				1: value(1), 2: value(2),
			}}, float: true}
			src := lisp.SortedMapFromData(lisp.NewMapData(m))
			if wrapped {
				// The outer copy must fail too, without visiting its next value.
				src = lisp.QExpr([]*lisp.LVal{src, value(3)})
			}
			first, second := src.Copy(), src.Copy()
			for _, got := range []*lisp.LVal{first, second} {
				if got.Type != lisp.LError || !strings.Contains(got.String(), "unhashable type: float") {
					t.Fatalf("Copy = %v, want unsupported float-key error", got)
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

// Int keys are supported (#733): an embedder map holding them copies into
// the stock map, and the clone hooks run in key order whatever order its
// Entries yields.
func TestCopyIntKeyedCustomMapClonesInKeyOrder(t *testing.T) {
	t.Parallel()
	var calls []int
	value := func(id int) *lisp.LVal {
		return lisp.Native(copierObservedCloner{id, func(id int) { calls = append(calls, id) }})
	}
	m := &copierRotatingMap{intKeyedMap: &intKeyedMap{m: map[int]*lisp.LVal{
		1: value(1), 2: value(2), -5: value(3),
	}}}
	src := lisp.SortedMapFromData(lisp.NewMapData(m))
	first, second := src.Copy(), src.Copy()
	for _, got := range []*lisp.LVal{first, second} {
		if got.Type != lisp.LSortMap {
			t.Fatalf("Copy = %v, want a sorted-map", got)
		}
		keys := got.Map().Keys()
		if s := keys.String(); s != "'(-5 1 2)" {
			t.Errorf("copied keys = %s", s)
		}
	}
	if !slices.Equal(calls, []int{3, 1, 2, 3, 1, 2}) {
		t.Errorf("clone hook order = %v, want key order on both copies", calls)
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
