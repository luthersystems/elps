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

func (m SortedMap) Entries(cells []*lisp.LVal) *lisp.LVal {
	if len(m) == 0 {
		return lisp.Int(0)
	}
	if len(cells) < len(m) {
		return lisp.Errorf("buffer has insufficient length")
	}
	i := 0
	for k, x := range m {
		cells[i] = lisp.QExpr([]*lisp.LVal{
			lisp.String(k),
			mapLVal(x),
		})
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
