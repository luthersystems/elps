// Copyright © 2026 The ELPS authors

// Package packagetable owns frozen package storage. Its private backing fields
// make writes through the lisp kernel's read accessors a compile-time error.
package packagetable

import (
	"iter"
	"maps"
	"slices"
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
