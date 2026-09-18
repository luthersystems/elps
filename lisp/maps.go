// Copyright © 2018 The ELPS authors

package lisp

import (
	"cmp"
	"slices"
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

// A MapData need not have a backing at all: NewMapData(nil), reached through
// SortedMapFromData, is the documented extension point and nothing in it
// requires an implementation.  The methods below answer for that degenerate
// value as the empty, unwritable map it is, so that every walk over one --
// rendering, equality, depth checking, the sorted-map builtins, the JSON
// encoder -- reads it as empty instead of calling a method on a nil interface
// and dying with a nil dereference the evaluator can only report as an
// internal panic.  They shadow the promoted methods; a caller holding the
// backing itself is unaffected.
//
// The three REBUILDING walkers (copier.mapData, detachMapData, GoValue) keep
// the explicit arms they already have: each has to construct a fresh
// degenerate map rather than merely read one.

// Len reports the number of entries, or zero for a map with no backing.
func (md *MapData) Len() int {
	if md == nil || md.mapBacking == nil {
		return 0
	}
	return md.mapBacking.Len()
}

// Get reads an entry.  A map with no backing holds none.
func (md *MapData) Get(key *LVal) (*LVal, bool) {
	if md == nil || md.mapBacking == nil {
		return Nil(), false
	}
	return md.mapBacking.Get(key)
}

// Set associates key with val.  A map with no backing has nowhere to put it,
// and reporting that is the only honest answer: silently dropping the write
// would let a program believe an entry exists.
func (md *MapData) Set(key, val *LVal) *LVal {
	if md == nil || md.mapBacking == nil {
		return Errorf("sorted-map has no backing implementation")
	}
	return md.mapBacking.Set(key, val)
}

// Del removes an entry.  A map with no backing holds none to remove.
func (md *MapData) Del(key *LVal) *LVal {
	if md == nil || md.mapBacking == nil {
		return Nil()
	}
	return md.mapBacking.Del(key)
}

// Keys lists the keys, none for a map with no backing.
func (md *MapData) Keys() *LVal {
	if md == nil || md.mapBacking == nil {
		return QExpr(nil)
	}
	return md.mapBacking.Keys()
}

// Entries writes the entries into buf, none for a map with no backing.
func (md *MapData) Entries(buf []*LVal) *LVal {
	if md == nil || md.mapBacking == nil {
		return Int(0)
	}
	return md.mapBacking.Entries(buf)
}

// a sentinal type used to describe string-like keys in a sortedmap.
type keytype uint

const (
	stringkey keytype = iota
	symbolkey
)

// Every key a sortedmap stores is a Go string: Set keys on LVal.Str for both
// LString and LSymbol and rejects every other type, so the Go maps are keyed
// on string rather than interface{}.  Keying on interface{} boxed each
// string into a heap-allocated interface value on insert (one allocation per
// entry, on top of the map growth) and had every read wrap the key in an
// interface before hashing; a string key hashes and compares directly.
type typemap map[string]keytype

type sortedmap struct {
	m  map[string]*LVal
	tm typemap
}

func newmap() sortedmap {
	return sortedmap{
		m:  make(map[string]*LVal),
		tm: make(typemap),
	}
}

// newmapSized is newmap with the entry table sized for n keys of either
// type.  The key-type map stays at its zero size, as newmap leaves it: Set
// writes it only for symbol keys, which are the rare case, and a map that
// holds none never touches it.
func newmapSized(n int) sortedmap {
	return sortedmap{
		m:  make(map[string]*LVal, n),
		tm: make(typemap),
	}
}

func (m sortedmap) typemap() typemap {
	return m.tm
}

func (m sortedmap) keytype(k string) keytype {
	return m.typemap()[k]
}

func (m sortedmap) puttype(k string, t keytype) {
	m.typemap()[k] = t
}

func (m sortedmap) deltype(k string) {
	delete(m.typemap(), k)
}

// emptyLike returns an empty sortedmap sized to receive a copy of m: both
// Go maps are made with m's CURRENT lengths.  Sizing to the current length
// rather than cloning the table matters: Go maps never shrink after
// deletes, so a map filled to 100k entries and pruned to 3 would otherwise
// cost every copy the high-water-mark table
// (TestForkSortedMapClonePrunedMapIsRightSized).
func (m sortedmap) emptyLike() sortedmap {
	return sortedmap{
		m:  make(map[string]*LVal, len(m.m)),
		tm: make(typemap, len(m.tm)),
	}
}

// copyInto copies m's entries and its key-type map into cp, an emptyLike
// of m, passing each value through val (nil shares the value pointer).  The
// entries are what Set stores -- the value under its key string -- and the
// key-type map is copied verbatim, which is what Entries reads when it
// decides whether a key comes back as a string or a symbol according to
// its most recent write.  The result is therefore
// indistinguishable from enumerating the entries in sorted order and
// re-inserting them, minus the sort, the per-entry pair cells and the
// incremental map growth.
//
// It is split from emptyLike so a caller that memoises copies by identity
// (the fork walker, issue #576) can publish cp before the values are
// walked: an entry may reach back to the map being copied, and the Go maps
// inside cp are references, so entries written here are visible through a
// *MapData built around cp earlier.  val runs in Go map order, which is
// unspecified; a caller that must see entries in a fixed order (detach,
// which reports the first failing key) stays on the Entries path.
func (m sortedmap) copyInto(cp sortedmap, val func(*LVal) *LVal) {
	if val == nil {
		for k, v := range m.m {
			cp.m[k] = v
		}
	} else {
		for k, v := range m.m {
			cp.m[k] = val(v)
		}
	}
	for k, t := range m.tm {
		cp.tm[k] = t
	}
}

// clone is emptyLike followed by copyInto.
func (m sortedmap) clone(val func(*LVal) *LVal) sortedmap {
	cp := m.emptyLike()
	m.copyInto(cp, val)
	return cp
}

// StringKeyRanger is an optional interface a Map implementation can
// provide when its Entries emits every key as an unquoted LString, never a
// symbol.  RangeStringKeys calls fn exactly once per entry, in unspecified
// order, with the key string Entries would emit and the value exactly as
// Entries would emit it, and must not mutate the map while doing so.  It
// returns nil, or the failure Entries would have reported; every caller
// discards a partial walk on error.  Len is used only as a size hint.
//
// A map whose Entries can emit a symbol key must not implement this: the
// callers store every key as a string, which is what Set does with an
// LString key, so a symbol flag the entries path would have recorded
// through Set(LSymbol) would be lost.
//
// Fork and the copy behind assoc, dissoc and LVal.Copy use it to build the
// stock sorted-map copy of such a map without first boxing every entry
// into a pair list: the copy is the same stock map the Entries path
// produces for it, so nothing observable changes -- only the sort, the
// pair cells and the incremental growth go. Decoded JSON maps, which
// json:load returns, implement it.
type StringKeyRanger interface {
	RangeStringKeys(fn func(key string, val *LVal)) error
}

// emptyForStringKeys returns an empty stock sortedmap sized for n string
// keys.  Set with an LString key writes no key type, so tm stays at its
// zero size, as newmap leaves it.
func emptyForStringKeys(n int) sortedmap {
	return newmapSized(n)
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
		m.deltype(key.Str)
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
	for ks, v := range m.m {
		cells := slots[2*i : 2*i+2 : 2*i+2]
		switch m.keytype(ks) {
		case stringkey:
			if keys == nil {
				keys = make([]LVal, n)
			}
			keys[i] = LVal{Type: LString, Str: ks}
			cells[0] = &keys[i]
		default:
			// A symbol key is quoted, and Quote copies a not-yet-quoted
			// value to flag it, so this arm allocates a second LVal that
			// the string arm does not.  Symbol keys are the rare case, so
			// they keep the straightforward construction.
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

// sortMapEntriesByKey orders a pair list sortedMapEntries produced, so a
// walk over a map's entries -- and with it every host CloneNative call the
// walk makes -- runs in an order that depends only on the map's contents
// rather than on the order the backing Map's Entries happened to yield.
// The Map interface above documents Keys as returning a sorted list and
// says nothing whatever about the order of Entries, so an embedder's
// implementation over a Go map yields whatever permutation it gets.
//
// By (Str, Type) rather than Str alone, so the order is total over the key
// kinds Str does not separate: an LString and an LSymbol that spell the
// same thing sort the same way on every walk.  Stable, so any pair the
// comparison still cannot separate keeps the order Entries gave it rather
// than moving under the sort.
//
// Both value walkers that reach a host clone hook through sortedMapEntries
// call this -- copier.mapData's generic arm and detachMapData -- so one
// embedder's map is walked in ONE order by both, rather than each walker
// having its own comparison to drift.
//
//elps:mutates reorders a cells slice the caller owns outright: every caller passes the slice sortedMapEntries allocated for that call, held only by a local, so nothing outside the call can observe the permutation
func sortMapEntriesByKey(entries []*LVal) {
	slices.SortStableFunc(entries, func(a, b *LVal) int {
		if r := cmp.Compare(a.Cells[0].Str, b.Cells[0].Str); r != 0 {
			return r
		}
		return cmp.Compare(a.Cells[0].Type, b.Cells[0].Type)
	})
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
