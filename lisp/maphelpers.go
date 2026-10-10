// Copyright © 2026 The ELPS authors

package lisp

import (
	"cmp"
	"iter"
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

// Name returns the spelling of a string or symbol key and true, and "" and
// false for an int key.
func (k MapKey) Name() (string, bool) {
	if k.Type == LString || k.Type == LSymbol {
		return k.Str, true
	}
	return "", false
}

type mapRangeEntry struct {
	val *LVal
	key MapKey
}

// mapRangePool recycles MapRange's ordering buffers.  A buffer holds nothing
// between uses (it is cleared before it is returned), so no value or key
// outlives the call that sorted it and nothing is shared between runtimes.
var mapRangePool = sync.Pool{New: func() any { return new([]mapRangeEntry) }}

// MapRange calls fn for each entry of the sorted-map m in the map's
// documented order -- int keys first by value, then string and symbol keys by
// spelling, the order MapKeys and MapEntries use -- until fn returns false
// (luthersystems/elps#745).  Keys arrive by value (MapKey) and values are the
// map's own, as MapEntries hands them out.
//
// When (keys m) would fail, MapRange returns that error, with keys' own
// message, and calls fn for nothing: m not a sorted-map ("first argument is
// not a map: <type>") or m larger than the runtime's MaxAlloc.  Otherwise it
// returns Nil(), or the error a custom backing's Entries method raises.
//
// For the interpreter's own map backings MapRange allocates nothing in the
// steady state: it orders the entries in a recycled buffer rather than
// building the key or pair lists MapKeys and MapEntries return.  A custom
// backing (NewMapData) is read through its Entries method, which allocates.
//
// The entries are captured before the first call to fn, so fn may read the
// map but must not rely on seeing its own writes to it.  MapRange charges no
// evaluation step and makes no context check; a builtin iterating a large map
// charges what its Lisp counterpart did (LEnv.Step per element, for example).
func (env *LEnv) MapRange(m *LVal, fn func(key MapKey, val *LVal) bool) *LVal {
	if m.Type != LSortMap {
		return env.Errorf("first argument is not a map: %s", m.Type)
	}
	if lerr := env.CheckAlloc(m.Len()); lerr.IsError() {
		return lerr
	}
	return m.mapRange(fn)
}

// mapRange is MapRange's walk over a sorted-map v.
func (v *LVal) mapRange(fn func(key MapKey, val *LVal) bool) *LVal {
	md := v.Map()
	if md == nil || md.mapBacking == nil {
		return Nil()
	}
	bufp, ok := mapRangePool.Get().(*[]mapRangeEntry)
	if !ok {
		bufp = new([]mapRangeEntry)
	}
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
		if r := md.Entries(entries); r.IsError() {
			return r
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
			break
		}
	}
	return Nil()
}

func compareMapRangeEntries(a, b mapRangeEntry) int {
	return compareMapKeyValues(a.key, b.key)
}

// compareMapKeyValues orders keys as MapKeys does: int keys first by value, then
// string and symbol keys by spelling.
func compareMapKeyValues(a, b MapKey) int {
	ai, bi := a.Type == LInt, b.Type == LInt
	switch {
	case ai && bi:
		return cmp.Compare(a.Int, b.Int)
	case ai:
		return -1
	case bi:
		return 1
	}
	if c := cmp.Compare(a.Str, b.Str); c != 0 {
		return c
	}
	return cmp.Compare(a.Type, b.Type)
}

// All returns an iterator over the entries of the sorted-map v, in the order
// of MapRange: int keys first by value, then string and symbol keys by
// spelling.  For any other value it yields nothing.  Breaking out of the
// loop early is safe.
//
//	for k, val := range m.All() { ... }
//
// All makes no check and charges no step.  Use it after a check that v is a
// map, or where the Lisp made none; where the Lisp called keys, use
// LEnv.MapRange, which makes keys' checks.  It shares MapRange's recycled
// buffer, and it loads every lazy value of a map a template built.  A
// custom backing (NewMapData) that fails ends the loop.
func (v *LVal) All() iter.Seq2[MapKey, *LVal] {
	return func(yield func(MapKey, *LVal) bool) {
		if v == nil || v.Type != LSortMap {
			return
		}
		v.mapRange(yield)
	}
}

// Keys returns an iterator over the keys of the sorted-map v, in the order of
// All.  For any other value it yields nothing, where MapKeys panics.
//
//	for k := range m.Keys() {
//		if name, ok := k.Name(); ok && strings.HasPrefix(name, "$") { ... }
//	}
//
// Keys reads keys only: it loads no lazy value and builds no list.  It
// makes no check and charges no step.  Breaking out of the loop early is
// safe.  A custom backing (NewMapData) that fails ends the loop.
func (v *LVal) Keys() iter.Seq[MapKey] {
	return func(yield func(MapKey) bool) {
		if v == nil || v.Type != LSortMap {
			return
		}
		v.mapKeyRange(yield)
	}
}

// mapKeyPool recycles mapKeyRange's ordering buffers.  A buffer holds
// nothing between uses.
var mapKeyPool = sync.Pool{New: func() any { return new([]MapKey) }}

// mapKeyRange is Keys's walk over a sorted-map v.  It reads the keys of the
// interpreter's backings without their values.
func (v *LVal) mapKeyRange(fn func(key MapKey) bool) {
	md := v.Map()
	if md == nil || md.mapBacking == nil {
		return
	}
	bufp, ok := mapKeyPool.Get().(*[]MapKey)
	if !ok {
		bufp = new([]MapKey)
	}
	buf := (*bufp)[:0]
	defer func() {
		clear(buf)
		*bufp = buf[:0]
		mapKeyPool.Put(bufp)
	}()
	switch b := md.mapBacking.(type) {
	case sortedmap:
		for k := range b.ints() {
			buf = append(buf, MapKey{Type: LInt, Int: k})
		}
		for ks := range b.m {
			t := LString
			if b.keytype(ks) != stringkey {
				t = LSymbol
			}
			buf = append(buf, MapKey{Type: t, Str: ks})
		}
	case jsonMap:
		for ks := range b {
			buf = append(buf, MapKey{Type: LString, Str: ks})
		}
	default:
		n := md.Len()
		entries := make([]*LVal, n)
		if r := md.Entries(entries); r.IsError() {
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
			buf = append(buf, key)
		}
	}
	slices.SortFunc(buf, compareMapKeyValues)
	for _, k := range buf {
		if !fn(k) {
			break
		}
	}
}
