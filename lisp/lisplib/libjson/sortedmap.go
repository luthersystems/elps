package libjson

import (
	"sort"

	"github.com/luthersystems/elps/lisp"
)

// SortedMap implements lisp.Map and only supports string keys.  Values must be
// lisp.LVal.
type SortedMap map[string]interface{}

var _ lisp.Map = SortedMap(nil)

func (m SortedMap) Len() int {
	return len(m)
}

func (m SortedMap) Get(k *lisp.LVal) (*lisp.LVal, bool) {
	if k.Type != lisp.LString {
		return lisp.Errorf("sorted-map decoded from json cannot hold key with type %s", lisp.GetType(k)), false
	}
	x, ok := m[k.Str]
	if !ok {
		return lisp.Nil(), false
	}
	return mapLVal(x), true
}

func (m SortedMap) Del(k *lisp.LVal) *lisp.LVal {
	if k.Type != lisp.LString {
		return lisp.Errorf("sorted-map decoded from json cannot hold key with type %s", lisp.GetType(k))
	}
	delete(m, k.Str)
	return lisp.Nil()
}

func (m SortedMap) Set(k *lisp.LVal, v *lisp.LVal) *lisp.LVal {
	if k.Type != lisp.LString {
		return lisp.Errorf("sorted-map decoded from json cannot hold key with type %s", lisp.GetType(k))
	}
	m[k.Str] = v
	return lisp.Nil()
}

// Entries materialises the map as sorted two-element pair lists.
//
// A pair used to cost three allocations -- the two-element Cells slice, the
// key LVal, and the pair LVal itself -- so a map of n entries cost 3n.  Two of
// the three are now carved out of arrays sized once from len(m): all the Cells
// slices share one backing array, and all the key LVals share another.  Only
// the pair LVal is still allocated per entry, because lisp.QExpr is the only
// way to set the quoted flag from outside package lisp.  n entries therefore
// cost n+2 allocations.
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
// them alive.  Every caller here obtains entries for a whole map and drops
// them as a unit (the encoder) or keeps all the keys (Keys, below), so there
// is no case where one survivor pins an otherwise dead map.
func (m SortedMap) Entries(cells []*lisp.LVal) *lisp.LVal {
	n := len(m)
	if n == 0 {
		return lisp.Int(0)
	}
	if len(cells) < n {
		return lisp.Errorf("buffer has insufficient length")
	}
	slots := make([]*lisp.LVal, 2*n)
	keys := make([]lisp.LVal, n)
	i := 0
	for k, x := range m {
		// The literal below must stay identical to what lisp.String
		// builds; TestBatchStringMatchesConstructor pins that.
		keys[i] = lisp.LVal{Type: lisp.LString, Str: k}
		pair := slots[2*i : 2*i+2 : 2*i+2]
		pair[0] = &keys[i]
		pair[1] = mapLVal(x)
		cells[i] = lisp.QExpr(pair)
		i++
	}
	sort.Sort(mapEntriesByKey(cells[:i]))
	return lisp.Int(len(cells))
}

func (m SortedMap) Keys() (keys *lisp.LVal) {
	cells := make([]*lisp.LVal, len(m))
	keys = m.Entries(cells) // save stack space :\
	if keys.Type == lisp.LError {
		return keys
	}
	keys = lisp.QExpr(cells)
	for i := range cells {
		cells[i] = cells[i].Cells[0]
	}
	return keys
}

// mapEntriesByKey is duplicated from the lisp package but probably deserves to
// be because that may have to deal other types of keys where we are focused
// only on strings.
type mapEntriesByKey []*lisp.LVal

func (m mapEntriesByKey) Len() int           { return len(m) }
func (m mapEntriesByKey) Less(i, j int) bool { return m[i].Cells[0].Str < m[j].Cells[0].Str }
func (m mapEntriesByKey) Swap(i, j int)      { m[i], m[j] = m[j], m[i] }

func mapLVal(x interface{}) (v *lisp.LVal) {
	var ok bool
	if v, ok = x.(*lisp.LVal); ok {
		return v
	}
	return lisp.Errorf("value is not an LVal: %T", x)
}
