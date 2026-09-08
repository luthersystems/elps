// Copyright © 2026 The ELPS authors

package lisp

import "sort"

// jsonMap is the interpreter-owned string-only map used for decoded JSON.
// Values must be *LVal. It reuses encoding/json's map storage without conversion.
// Like other VM values it is mutable, and must not be shared between runtimes.
type jsonMap map[string]interface{}

var _ Map = jsonMap(nil)
var _ StringKeyRanger = jsonMap(nil)

// RangeStringKeys implements StringKeyRanger: fn sees each key as the
// string Entries emits for it and each value exactly as Entries emits it
// (an error value for a non-LVal, as jsonMapLVal does there).  Enumerating a Go
// map cannot fail, so it always returns nil. Copy operations can enumerate a
// decoded document without first materialising every entry as a pair list.
func (m jsonMap) RangeStringKeys(fn func(key string, val *LVal)) error {
	for k, x := range m {
		fn(k, jsonMapLVal(x))
	}
	return nil
}

func (m jsonMap) Len() int {
	return len(m)
}

func (m jsonMap) Get(k *LVal) (*LVal, bool) {
	if k.Type != LString {
		return Errorf("sorted-map decoded from json cannot hold key with type %s", GetType(k)), false
	}
	x, ok := m[k.Str]
	if !ok {
		return Nil(), false
	}
	return jsonMapLVal(x), true
}

func (m jsonMap) Del(k *LVal) *LVal {
	if k.Type != LString {
		return Errorf("sorted-map decoded from json cannot hold key with type %s", GetType(k))
	}
	delete(m, k.Str)
	return Nil()
}

func (m jsonMap) Set(k *LVal, v *LVal) *LVal {
	if k.Type != LString {
		return Errorf("sorted-map decoded from json cannot hold key with type %s", GetType(k))
	}
	m[k.Str] = v
	return Nil()
}

// Entries materialises the map as sorted two-element pair lists.
//
// A pair used to cost three allocations -- the two-element Cells slice, the
// key LVal, and the pair LVal itself -- so a map of n entries cost 3n.  Two of
// the three are now carved out of arrays sized once from len(m): all the Cells
// slices share one backing array, and all the key LVals share another.  Only
// the pair LVal is still allocated per entry. n entries therefore cost n+2
// allocations. Keeping this layout preserves the existing encoder cost.
//
// Nothing observable changes.  Each pair is still a distinct quoted LVal, each
// Cells slice is capped to its own two slots so an append cannot reach the
// next pair's, and each key is still a distinct LVal a caller may hold or
// discard independently.  What changes is allocator and GC traffic, which is
// what the issue #379 item-6 profile identified: the JSON encoder walks every
// map through Entries and discards every pair as soon as it has written the
// key and value, so this boxing was garbage generated in proportion to the
// document -- 40% of all objects allocated by the libjson benchmark suite.
//
// The arrays are jointly retained: holding one pair or one key keeps all of
// them alive. The encoder drops entries as a unit; callers retaining entries
// should be aware that one entry retains the batch's key and slot arrays.
func (m jsonMap) Entries(cells []*LVal) *LVal {
	n := len(m)
	if n == 0 {
		return Int(0)
	}
	if len(cells) < n {
		return Errorf("buffer has insufficient length")
	}
	slots := make([]*LVal, 2*n)
	keys := make([]LVal, n)
	i := 0
	for k, x := range m {
		// The literal below must stay identical to what String
		// builds; TestBatchStringMatchesConstructor pins that.
		keys[i] = LVal{Type: LString, Str: k}
		pair := slots[2*i : 2*i+2 : 2*i+2]
		pair[0] = &keys[i]
		pair[1] = jsonMapLVal(x)
		cells[i] = QExpr(pair)
		i++
	}
	sort.Sort(jsonMapEntriesByKey(cells[:i]))
	return Int(len(cells))
}

func (m jsonMap) Keys() (keys *LVal) {
	cells := make([]*LVal, len(m))
	keys = m.Entries(cells) // save stack space :\
	if keys.Type == LError {
		return keys
	}
	keys = QExpr(cells)
	for i := range cells {
		cells[i] = cells[i].Cells[0]
	}
	return keys
}

// JSON keys are all strings; their order is independent of the stock map's
// symbol-key bookkeeping.
type jsonMapEntriesByKey []*LVal

func (m jsonMapEntriesByKey) Len() int           { return len(m) }
func (m jsonMapEntriesByKey) Less(i, j int) bool { return m[i].Cells[0].Str < m[j].Cells[0].Str }
func (m jsonMapEntriesByKey) Swap(i, j int)      { m[i], m[j] = m[j], m[i] }

func jsonMapLVal(x interface{}) (v *LVal) {
	var ok bool
	if v, ok = x.(*LVal); ok {
		return v
	}
	return Errorf("value is not an LVal: %T", x)
}
