// Copyright © 2026 The ELPS authors

package lisp

import (
	"cmp"
	"slices"
	"sync"
)

// MapKey is a sorted-map key presented by value, so MapRange needs no LVal
// per entry.  Type is LInt, LString or LSymbol; Int holds an LInt key and Str
// the spelling of a string or symbol key.
type MapKey struct {
	Str  string
	Int  int
	Type LType
}

// LVal returns the key as a fresh LVal, the same value MapKeys would list for
// it: an int, a string, or a quoted symbol.
func (k MapKey) LVal() *LVal {
	switch k.Type {
	case LInt:
		return Int(k.Int)
	case LSymbol:
		return Quote(Symbol(k.Str))
	default:
		return String(k.Str)
	}
}

type mapRangeEntry struct {
	val *LVal
	key MapKey
}

// mapRangePool recycles MapRange's ordering buffers.  A buffer holds nothing
// between uses (it is cleared before it is returned), so no value or key
// outlives the call that sorted it and nothing is shared between runtimes.
var mapRangePool = sync.Pool{New: func() any { return new([]mapRangeEntry) }}

// MapRange calls fn for each entry of the sorted-map v in the map's
// documented order -- int keys first by value, then string and symbol keys by
// spelling, the order MapKeys and MapEntries use -- until fn returns false
// (luthersystems/elps#745).  Keys arrive by value (MapKey) and values are the
// map's own, as MapEntries hands them out.
//
// For the interpreter's own map backings MapRange allocates nothing in the
// steady state: it orders the entries in a recycled buffer rather than
// building the key or pair lists MapKeys and MapEntries return.  A custom
// backing (NewMapData) is read through its Entries method, which allocates.
//
// The entries are captured before the first call to fn, so fn may read the
// map but must not rely on seeing its own writes to it.  MapRange charges no
// evaluation step; a builtin iterating a large map charges what its Lisp
// counterpart did (LEnv.Step per element, for example).  It panics if v is
// not a sorted-map, like MapKeys.
func (v *LVal) MapRange(fn func(key MapKey, val *LVal) bool) {
	md := v.Map()
	if md == nil || md.mapBacking == nil {
		return
	}
	bufp := mapRangePool.Get().(*[]mapRangeEntry)
	buf := (*bufp)[:0]
	defer func() {
		clear(buf)
		*bufp = buf[:0]
		mapRangePool.Put(bufp)
	}()
	switch b := md.mapBacking.(type) {
	case sortedmap:
		b.forceAll()
		for k, val := range b.ints() {
			buf = append(buf, mapRangeEntry{key: MapKey{Type: LInt, Int: k}, val: val})
		}
		for ks, val := range b.m {
			t := LString
			if b.keytype(ks) != stringkey {
				t = LSymbol
			}
			buf = append(buf, mapRangeEntry{key: MapKey{Type: t, Str: ks}, val: val})
		}
	case jsonMap:
		for ks, x := range b {
			buf = append(buf, mapRangeEntry{key: MapKey{Type: LString, Str: ks}, val: jsonMapLVal(x)})
		}
	default:
		n := md.Len()
		entries := make([]*LVal, n)
		if r := md.Entries(entries); r.Type == LError {
			return
		}
		for _, e := range entries[:n] {
			if e == nil || len(e.Cells) < 2 {
				continue
			}
			k := e.Cells[0]
			key := MapKey{Type: k.Type, Str: k.Str}
			if k.Type == LInt {
				key = MapKey{Type: LInt, Int: k.Int}
			}
			buf = append(buf, mapRangeEntry{key: key, val: e.Cells[1]})
		}
	}
	slices.SortFunc(buf, compareMapRangeEntries)
	for i := range buf {
		if !fn(buf[i].key, buf[i].val) {
			return
		}
	}
}

func compareMapRangeEntries(a, b mapRangeEntry) int {
	ai, bi := a.key.Type == LInt, b.key.Type == LInt
	switch {
	case ai && bi:
		return cmp.Compare(a.key.Int, b.key.Int)
	case ai:
		return -1
	case bi:
		return 1
	}
	if c := cmp.Compare(a.key.Str, b.key.Str); c != 0 {
		return c
	}
	return cmp.Compare(a.key.Type, b.key.Type)
}
