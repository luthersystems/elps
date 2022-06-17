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

// MapData is a concrete type to store in an interface as to avoid expensive
// runtime interface type checking
type MapData struct {
	Map
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

func (m sortedmap) Entries(buf []*LVal) *LVal {
	if len(m.m) == 0 {
		return Int(0)
	}
	if len(buf) < len(m.m) {
		return Errorf("buffer has insufficient length")
	}
	i := 0
	for k, v := range m.m {
		switch k := k.(type) {
		case string:
			switch m.keytype(k) {
			case stringkey:
				buf[i] = mklist(String(k), v)
			default:
				buf[i] = mklist(Quote(Symbol(k)), v)
			}
		default:
			return Errorf("unexpected map key: %v", k)
		}
		i++
	}
	sort.Sort(mapEntriesByKey(buf[:len(m.m)]))
	return Int(len(m.m))
}

func (m sortedmap) Keys() *LVal {
	keys := sortedMapEntries(m)
	if keys.IsNil() || keys.Type == LError {
		return keys
	}
	for i := range keys.Cells {
		// Modifying lvals is shady in general but because they are generated
		// internally we know their structure.
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

func sortedMapString(m *LVal) string {
	var buf bytes.Buffer
	buf.WriteString("(sorted-map")
	for _, pair := range sortedMapEntries(m.Map()).Cells {
		buf.WriteString(" ")
		buf.WriteString(pair.Cells[0].String())
		buf.WriteString(" ")
		buf.WriteString(pair.Cells[1].String())
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
