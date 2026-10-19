// Copyright © 2026 The ELPS authors

// Package packagetable owns frozen package storage. Its private backing fields
// make writes through the lisp kernel's read accessors a compile-time error.
package packagetable

import (
	"iter"
	"maps"
	"slices"
	"strings"
)

// Scalar excludes pointers and other mutable values: a copy of a table entry
// must not provide a way to modify the table's storage.
type Scalar interface {
	~string | ~int
}

// Map is a read-only string-keyed table. Its zero value is an empty table.
type Map[V Scalar] struct {
	values map[string]V
}

// NewMap takes a private snapshot; the caller keeps ownership of values.
func NewMap[V Scalar](values map[string]V) Map[V] {
	return Map[V]{values: maps.Clone(values)}
}

// AdoptMap takes ownership of values without copying. The caller must hold no
// other reference to values; it is for a map the caller has just built.
func AdoptMap[V Scalar](values map[string]V) Map[V] {
	return Map[V]{values: values}
}

// Lookup returns an entry and whether it exists.
func (m Map[V]) Lookup(key string) (V, bool) {
	v, ok := m.values[key]
	return v, ok
}

// Len returns the number of entries.
func (m Map[V]) Len() int { return len(m.values) }

// Keys returns a fresh, sorted list of keys.
func (m Map[V]) Keys() []string {
	keys := make([]string, 0, len(m.values))
	for key := range m.values {
		keys = append(keys, key)
	}
	slices.Sort(keys)
	return keys
}

// All visits entries in key order.
func (m Map[V]) All() iter.Seq2[string, V] {
	return func(yield func(string, V) bool) {
		for _, key := range m.Keys() {
			if !yield(key, m.values[key]) {
				return
			}
		}
	}
}

// Unordered visits entries in no fixed order, without allocating.  A
// caller must compute a result that does not depend on the order, such as a
// minimum.
func (m Map[V]) Unordered() iter.Seq2[string, V] {
	return func(yield func(string, V) bool) {
		for key, v := range m.values {
			if !yield(key, v) {
				return
			}
		}
	}
}

// Copy returns a mutable copy, never the backing map.
func (m Map[V]) Copy() map[string]V { return maps.Clone(m.values) }

// Strings is a read-only list of export names in declaration order.
// Its zero value is an empty list.
type Strings struct {
	values []string
}

// NewStrings takes a private snapshot; the caller keeps ownership of values.
func NewStrings(values []string) Strings {
	return Strings{values: slices.Clone(values)}
}

// AdoptStrings takes ownership of values without copying. The caller must
// hold no other reference to the backing slice, as with AdoptMap.
func AdoptStrings(values []string) Strings { return Strings{values: values} }

// Len returns the number of names.
func (s Strings) Len() int { return len(s.values) }

// All visits names in declaration order, including duplicates.
func (s Strings) All() iter.Seq[string] {
	return func(yield func(string) bool) {
		for _, value := range s.values {
			if !yield(value) {
				return
			}
		}
	}
}

// Copy returns a mutable copy, never the backing slice.
func (s Strings) Copy() []string {
	out := make([]string, len(s.values))
	copy(out, s.values)
	return out
}

// Pair is one entry of a Lists table: Value is in the list of Key.
type Pair struct {
	Key, Value string
}

// Lists is a read-only string-keyed table of name lists.  Its zero value is
// an empty table.  A list is never handed out: callers read it by index.
// The table is one slice of pairs sorted by key, then value, so a table of
// any size is one allocation.
type Lists struct {
	pairs []Pair
}

// AdoptPairs takes ownership of pairs, sorts them by key, then value, and
// returns them as a table: each key's list is its pairs' values in order.
// The caller must not use pairs after the call.
func AdoptPairs(pairs []Pair) Lists {
	if len(pairs) == 0 {
		return Lists{}
	}
	slices.SortFunc(pairs, comparePairs)
	return Lists{pairs: pairs}
}

// NewLists takes a private snapshot of the table and of every list; the
// caller keeps ownership of values.  Each list must be sorted.
func NewLists(values map[string][]string) Lists {
	var pairs []Pair
	for key, list := range values {
		for _, value := range list {
			pairs = append(pairs, Pair{Key: key, Value: value})
		}
	}
	return AdoptPairs(pairs)
}

func comparePairs(a, b Pair) int {
	if c := strings.Compare(a.Key, b.Key); c != 0 {
		return c
	}
	return strings.Compare(a.Value, b.Value)
}

// span returns the bounds of key's pairs.
func (l Lists) span(key string) (int, int) {
	lo, _ := slices.BinarySearchFunc(l.pairs, key, func(p Pair, k string) int { return strings.Compare(p.Key, k) })
	hi := lo
	for hi < len(l.pairs) && l.pairs[hi].Key == key {
		hi++
	}
	return lo, hi
}

// Len returns the number of keys.
func (l Lists) Len() int {
	n := 0
	for i, p := range l.pairs {
		if i == 0 || p.Key != l.pairs[i-1].Key {
			n++
		}
	}
	return n
}

// ListLen returns the length of the list stored under key, 0 when none is.
func (l Lists) ListLen(key string) int {
	lo, hi := l.span(key)
	return hi - lo
}

// At returns element i of the list stored under key.
func (l Lists) At(key string, i int) string {
	lo, hi := l.span(key)
	if i < 0 || lo+i >= hi {
		panic("packagetable: list index out of range")
	}
	return l.pairs[lo+i].Value
}

// Span returns the bounds lo and hi of the list stored under key: its
// elements are ValueAt(lo) to ValueAt(hi-1).  lo == hi when no list is
// stored.
func (l Lists) Span(key string) (int, int) { return l.span(key) }

// ValueAt returns the value of pair i, for i in a range Span returned.
func (l Lists) ValueAt(i int) string { return l.pairs[i].Value }

// List visits the list stored under key in order.
func (l Lists) List(key string) iter.Seq[string] {
	return func(yield func(string) bool) {
		lo, hi := l.span(key)
		for _, p := range l.pairs[lo:hi] {
			if !yield(p.Value) {
				return
			}
		}
	}
}

// All visits every pair in order: by key, then value.
func (l Lists) All() iter.Seq[Pair] {
	return func(yield func(Pair) bool) {
		for _, p := range l.pairs {
			if !yield(p) {
				return
			}
		}
	}
}

// Keys returns a fresh, sorted list of keys.
func (l Lists) Keys() []string {
	var keys []string
	for i, p := range l.pairs {
		if i == 0 || p.Key != l.pairs[i-1].Key {
			keys = append(keys, p.Key)
		}
	}
	return keys
}

// Copy returns a mutable copy of the table and of every list, never the
// backing storage.
func (l Lists) Copy() map[string][]string {
	if len(l.pairs) == 0 {
		return nil
	}
	out := make(map[string][]string)
	for _, p := range l.pairs {
		out[p.Key] = append(out[p.Key], p.Value)
	}
	return out
}
