// Copyright © 2018 The ELPS authors

package lisp

import (
	"bytes"
	"fmt"
	"sort"
)

func sortedMapString(m *LVal) string {
	var buf bytes.Buffer
	buf.WriteString("(sorted-map")
	keys := sortedMapKeys(m)
	for _, key := range keys {
		buf.WriteString(" ")
		buf.WriteString(key.String())
		buf.WriteString(" ")
		buf.WriteString(mapGet(m, key, nil).String())
	}
	buf.WriteString(")")
	return buf.String()
}

func sortedMapKeys(m *LVal) []*LVal {
	var ks mapKeys
	for k := range m.Map() {
		key := sortedMapKey(k)
		if key == nil {
			panic("unable to serialize hashmap key")
		}
		ks = append(ks, key)
	}
	sort.Sort(ks)
	return ks
}

func mapHasKey(m *LVal, key *LVal) *LVal {
	k := toSortedMapKey(key)
	if k == nil {
		return Errorf("unhashable type: %v", key.Type)
	}
	mmap := m.Map()
	switch k := k.(type) {
	case string:
		v := mmap[k]
		if v != nil {
			return Bool(true)
		}
		v = mmap[mapSymbol(k)]
		if v != nil {
			return Bool(true)
		}
	case mapSymbol:
		v := mmap[k]
		if v != nil {
			return Bool(true)
		}
		v = mmap[string(k)]
		if v != nil {
			return Bool(true)
		}
	}
	return Bool(false)
}

func mapGet(m, key, def *LVal) *LVal {
	k := toSortedMapKey(key)
	if k == nil {
		return Errorf("unhashable type: %s", key.Type)
	}
	mmap := m.Map()
	switch k := k.(type) {
	case string:
		v := mmap[k]
		if v != nil {
			return v
		}
		v = mmap[mapSymbol(k)]
		if v != nil {
			return v
		}
	case mapSymbol:
		v := mmap[k]
		if v != nil {
			return v
		}
		v = mmap[string(k)]
		if v != nil {
			return v
		}
	}
	if def == nil {
		return Nil()
	}
	return def
}

func mapSet(m *LVal, key *LVal, val *LVal, coerce bool) *LVal {
	k := toSortedMapKey(key)
	if k == nil {
		return Errorf("unhashable type: %s", key.Type)
	}
	mmap := m.Map()
	switch _k := k.(type) {
	case string:
		_, ok := mmap[mapSymbol(_k)]
		if ok {
			if coerce {
				k = mapSymbol(_k)
			} else {
				return Errorf("map contains both symbol and string key: %s", k)
			}
		}
	case mapSymbol:
		_, ok := mmap[string(_k)]
		if ok {
			if coerce {
				k = string(_k)
			} else {
				return Errorf("map contains both symbol and string key: %s", k)
			}
		}
	}
	mmap[k] = val
	return Nil()
}

func mapDel(m *LVal, key *LVal, coerce bool) *LVal {
	k := toSortedMapKey(key)
	if k == nil {
		return Errorf("unhashable type: %s", key.Type)
	}
	mmap := m.Map()
	switch _k := k.(type) {
	case string:
		_, ok := mmap[mapSymbol(_k)]
		if ok {
			if coerce {
				k = mapSymbol(_k)
			} else {
				return Errorf("map contains both symbol and string key: %s", k)
			}
		}
	case mapSymbol:
		_, ok := mmap[string(_k)]
		if ok {
			if coerce {
				k = string(_k)
			} else {
				return Errorf("map contains both symbol and string key: %s", k)
			}
		}
	}
	delete(mmap, k)
	return Nil()
}

// BUG:  Numbers cannot be used as map keys because there is no simple way to
// retain their input type while also checking for equality using a builtin
// (golang) map.  This is not considered a limitation for now because it
// doesn't hinder JSON serialization.
func toSortedMapKey(v *LVal) interface{} {
	switch v.Type {
	//case LInt:
	//	return v.Int
	//case LFloat:
	//	return v.Float
	case LString:
		return v.Str
	case LSymbol:
		return mapSymbol(v.Str)
	}
	return nil
}

func sortedMapKey(k interface{}) *LVal {
	switch v := k.(type) {
	//case int:
	//	return Int(v)
	//case float64:
	//	return Float(v)
	case string:
		return String(v)
	case mapSymbol:
		return Quote(Symbol(string(v)))
	}
	return Error(fmt.Errorf("invalid key type: %T", k))
}

type mapSymbol string

type mapKeys []*LVal

func (ks mapKeys) Len() int { return len(ks) }

func (ks mapKeys) Swap(i, j int) {
	ks[i], ks[j] = ks[j], ks[i]
}

func (ks mapKeys) Less(i, j int) bool {
	if ks[i].IsNumeric() && ks[j].IsNumeric() {
		// TODO: Fall back to numeric sort here
	}
	if !ks.compatible(i, j) {
		return ks[i].Type < ks[j].Type
	}
	switch ks[i].Type {
	//case LInt:
	//	return ks[i].Int < ks[j].Int
	//case LFloat:
	//	return ks[i].Float < ks[j].Float
	case LString, LSymbol:
		return ks[i].Str < ks[j].Str
	}
	// should not be reachable
	return false
}

func (ks mapKeys) compatible(i, j int) bool {
	switch ks[i].Type {
	case LString, LSymbol:
		return ks[j].Type == LString || ks[j].Type == LSymbol
	}
	return false
}
