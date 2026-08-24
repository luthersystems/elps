// Copyright © 2018 The ELPS authors

package lisp

import (
	"bytes"
	"sort"
)

type Map interface {
	Len() int
	// Get returns the value associated with the given key and a bool signaling
	// if the key was found in the map.  The first value returned by Get may be
	// an LError type if the implementation does not support the type of key
	// given.
	Get(key *LVal) (*LVal, bool)
	// Set associates key with val in the map.  Set may return an LError value
	// if the
	Set(key *LVal, val *LVal) *LVal
	// Del removes any association it has with key.  Del may return an LError
	// value if key was not a supported type or if the map does not support
	// dissociation.
	Del(key *LVal) *LVal
	// Keys returns a (sorted) list of keys with associated values in the map.
	Keys() *LVal
	// Entries copies its entries into the first Len() elements of buf.
	// Entries are represented as lists with two elements.  Entries returns the
	// number of elements written (i.e. Len) or an error if any was encountered.
	Entries(buf []*LVal) *LVal
}

// mapBacking aliases Map so MapData can embed the implementation (keeping
// the interface's method set promoted) without exporting a writable field.
// Swapping the backing of a sorted-map value in place — v.Map().Map = other
// — was an open aliasing/mutation channel on values that may be shared and
// sealed, the corruption class issue #382 closes; the backing is now fixed
// at construction (NewMapData, SortedMap, SortedMapFromData).
type mapBacking = Map

// MapData is a concrete type to store in an interface as to avoid expensive
// runtime interface type checking.  Construct it with NewMapData; the
// backing Map cannot be replaced after construction (issue #382).
type MapData struct {
	mapBacking
}

// NewMapData returns a MapData backed by m.  Together with
// SortedMapFromData it is the extension point for embedders that back a
// sorted-map with a custom Map implementation.
func NewMapData(m Map) *MapData {
	return &MapData{m}
}

// a sentinal type used to describe string-like keys in a sortedmap.
type keytype uint

const (
	stringkey keytype = iota
	symbolkey
)

type typemap map[interface{}]keytype

type sortedmap struct {
	m  map[interface{}]*LVal
	tm typemap
}

func newmap() sortedmap {
	return sortedmap{
		m:  make(map[interface{}]*LVal),
		tm: make(typemap),
	}
}

func (m sortedmap) typemap() typemap {
	return m.tm
}

func (m sortedmap) keytype(k interface{}) keytype {
	return m.typemap()[k]
}

func (m sortedmap) puttype(k interface{}, t keytype) {
	m.typemap()[k] = t
}

func (m sortedmap) deltype(k interface{}) {
	delete(m.typemap(), k)
}

func (m sortedmap) Len() int {
	return len(m.m)
}

func (m sortedmap) Get(key *LVal) (*LVal, bool) {
	switch key.Type {
	case LString, LSymbol:
		v := m.m[key.Str]
		if v != nil {
			return v, true
		}
		return Nil(), false
	default:
		return Errorf("unhashable type: %s", key.Type), false
	}
}

func (m sortedmap) Del(key *LVal) *LVal {
	switch key.Type {
	case LString, LSymbol:
		delete(m.m, key.Str)
		m.deltype(key.Str)
		return Nil()
	default:
		return Errorf("unhashable type: %s", key.Type)
	}
}

func (m sortedmap) Set(key, val *LVal) *LVal {
	switch key.Type {
	case LString:
		m.m[key.Str] = val
		return Nil()
	case LSymbol:
		m.m[key.Str] = val
		m.puttype(key.Str, symbolkey)
		return Nil()
	default:
		return Errorf("unhashable type: %s", key.Type)
	}
}

// Entries materialises the map as sorted two-element pair lists.
//
// The three objects a pair needs -- the pair LVal, its two-element Cells
// slice, and the key LVal -- are carved out of three arrays sized once from
// Len() rather than allocated per entry.  Nothing observable changes: each
// pair is still a distinct quoted LVal with its own Cells, and each key is
// still a distinct LVal, so a caller can hold, mutate or discard them exactly
// as before.  What changes is that a map of n entries costs three allocations
// instead of 3n.
//
// This is the dominant allocation site on the `json:dump` path (issue #379,
// item 6): the JSON encoder walks every map through Entries and throws every
// pair away as soon as it has written the two bytes of key and value it needs,
// so the per-entry boxing was pure garbage generated in proportion to the
// document.  Batching does not reduce the BYTES -- an LVal costs the same
// whether it sits in an array or alone -- it removes the allocator and GC
// traffic, which is what the profile said was expensive.
//
// The arrays are jointly retained: holding one pair keeps all of them alive.
// That is acceptable here because the caller supplied a buffer sized for the
// whole map and every in-tree caller (the encoder, Keys, sortedMapString,
// the sorted-map builtins) drops the entries as a unit.
func (m sortedmap) Entries(buf []*LVal) *LVal {
	n := len(m.m)
	if n == 0 {
		return Int(0)
	}
	if len(buf) < n {
		return Errorf("buffer has insufficient length")
	}
	pairs := make([]LVal, n)
	slots := make([]*LVal, 2*n)
	// The key array is made on first use rather than up front: a map keyed
	// entirely by symbols takes the other arm below and would otherwise pay
	// n LVals of dead storage for keys it never writes.
	var keys []LVal
	i := 0
	for k, v := range m.m {
		ks, ok := k.(string)
		if !ok {
			return Errorf("unexpected map key: %v", k)
		}
		cells := slots[2*i : 2*i+2 : 2*i+2]
		switch m.keytype(ks) {
		case stringkey:
			if keys == nil {
				keys = make([]LVal, n)
			}
			keys[i] = LVal{Type: LString, Str: ks}
			cells[0] = &keys[i]
		default:
			// A symbol key is quoted, and Quote wraps rather than
			// flagging, so this arm needs a second LVal that the string
			// arm does not.  Symbol keys are the rare case, so they keep
			// the straightforward construction.
			cells[0] = Quote(Symbol(ks))
		}
		cells[1] = v
		pairs[i] = LVal{Type: LSExpr, quoted: true, Cells: cells}
		buf[i] = &pairs[i]
		i++
	}
	sort.Sort(mapEntriesByKey(buf[:n]))
	return Int(n)
}

func (m sortedmap) Keys() *LVal {
	keys := sortedMapEntries(m)
	if keys.IsNil() || keys.Type == LError {
		return keys
	}
	for i := range keys.Cells {
		// Modifying lvals is shady in general but because they are generated
		// internally we know their structure.
		//elps:mutates keys and its pair cells are freshly built by sortedMapEntries above; rewriting the slots drops the values in place
		keys.Cells[i] = keys.Cells[i].Cells[0]
	}
	return keys
}

func sortedMapEntries(m Map) *LVal {
	cells := make([]*LVal, m.Len())
	lerr := m.Entries(cells)
	if lerr.Type == LError {
		return lerr
	}
	return QExpr(cells)
}

func sortedMapString(m *LVal, g cycleGuard) string {
	var buf bytes.Buffer
	buf.WriteString("(sorted-map")
	for _, pair := range sortedMapEntries(m.Map()).Cells {
		buf.WriteString(" ")
		buf.WriteString(pair.Cells[0].str(false, g))
		buf.WriteString(" ")
		buf.WriteString(pair.Cells[1].str(false, g))
	}
	buf.WriteString(")")
	return buf.String()
}

// mapEntriesByKey are internally known to be a list of pairs containing keys
// with valid types.
type mapEntriesByKey []*LVal

func (m mapEntriesByKey) Len() int {
	return len(m)
}

func (m mapEntriesByKey) Less(i, j int) bool {
	return m[i].Cells[0].Str < m[j].Cells[0].Str
}

func (m mapEntriesByKey) Swap(i, j int) {
	m[i], m[j] = m[j], m[i]
}

func mklist(v ...*LVal) *LVal {
	return QExpr(v)
}
