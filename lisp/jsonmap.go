// Copyright © 2026 The ELPS authors

package lisp

import "slices"

// jsonMap is the interpreter-owned string-keyed map used for decoded JSON.
// Symbol keys use their names for access; keys are always emitted as strings.
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
	if k.Type != LString && k.Type != LSymbol {
		return Errorf("sorted-map decoded from json cannot hold key with type %s", GetType(k)), false
	}
	x, ok := m[k.Str]
	if !ok {
		return Nil(), false
	}
	return jsonMapLVal(x), true
}

func (m jsonMap) Del(k *LVal) *LVal {
	if k.Type != LString && k.Type != LSymbol {
		return Errorf("sorted-map decoded from json cannot hold key with type %s", GetType(k))
	}
	delete(m, k.Str)
	return Nil()
}

func (m jsonMap) Set(k *LVal, v *LVal) *LVal {
	if k.Type != LString && k.Type != LSymbol {
		return Errorf("sorted-map decoded from json cannot hold key with type %s", GetType(k))
	}
	m[k.Str] = v
	return Nil()
}

// Entries materialises the map as sorted two-element pair lists.
//
// A pair used to cost three allocations -- the two-element Cells slice, the
// key LVal, and the pair LVal itself -- so a map of n entries cost 3n.  All
// three are now carved out of arrays sized once from len(m): all the Cells
// slices share one backing array, all the key LVals share another, and all
// the pair headers a third, as the stock sortedmap.Entries does.  n entries
// therefore cost three allocations.
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
// should be aware that one entry retains the batch's pair, key and slot arrays.
func (m jsonMap) Entries(cells []*LVal) *LVal {
	n := len(m)
	if n == 0 {
		return Int(0)
	}
	if len(cells) < n {
		return Errorf("buffer has insufficient length")
	}
	pairs := make([]LVal, n)
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
		// The same header QExpr(pair) builds, carved from one array.
		pairs[i] = LVal{Type: LSExpr, quoted: true, Cells: pair}
		cells[i] = &pairs[i]
		i++
	}
	slices.SortFunc(cells[:i], comparePairKeyStr)
	return Int(len(cells))
}

// Keys builds the sorted key list directly rather than through Entries: one
// batch of fresh LString keys and the list's cells, nothing else.
func (m jsonMap) Keys() *LVal {
	n := len(m)
	cells := make([]*LVal, n)
	if n == 0 {
		return QExpr(cells)
	}
	keys := make([]LVal, n)
	i := 0
	for k := range m {
		keys[i] = LVal{Type: LString, Str: k}
		cells[i] = &keys[i]
		i++
	}
	slices.SortFunc(cells, compareKeyStr)
	return QExpr(cells)
}

func jsonMapLVal(x interface{}) (v *LVal) {
	var ok bool
	if v, ok = x.(*LVal); ok {
		return v
	}
	return Errorf("value is not an LVal: %T", x)
}
