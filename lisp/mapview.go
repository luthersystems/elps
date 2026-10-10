// Copyright © 2026 The ELPS authors

package lisp

import "iter"

// MapView is a sorted-map whose type has been checked.  AsMap makes the one
// check; a function that takes a MapView needs none, because the compiler
// guarantees it holds a sorted-map.  MapView holds one pointer, so passing
// it costs what passing the *LVal does.
//
//	m, ok := lisp.AsMap(v)
//	if !ok {
//		return env.Errorf("argument is not a sorted-map: %v", v.Type)
//	}
//	name, _ := lisp.Lookup[string](m, "name")
//	inner, ok := lisp.Lookup[lisp.MapView](m, "response")
//
// The zero MapView holds no map: Lookup finds nothing in it, Keys and All
// yield nothing, Len is 0 and LVal is nil.
type MapView struct {
	v *LVal
}

// AsMap returns v as a MapView and true when v is a sorted-map, and the
// zero MapView and false otherwise (nil included).  It makes no check
// beyond the type and charges no step.
func AsMap(v *LVal) (MapView, bool) {
	if v == nil || v.Type != LSortMap {
		return MapView{}, false
	}
	return MapView{v: v}, true
}

// LVal returns the sorted-map value m views, or nil for the zero MapView.
// The value is shared, not copied.
func (m MapView) LVal() *LVal {
	return m.v
}

// Len returns the number of entries in m.
func (m MapView) Len() int {
	if m.v == nil {
		return 0
	}
	return m.v.Map().Len()
}

// Keys returns an iterator over the keys of m.  See LVal.Keys.
func (m MapView) Keys() iter.Seq[MapKey] {
	return m.v.Keys()
}

// All returns an iterator over the entries of m.  See LVal.All.
func (m MapView) All() iter.Seq2[MapKey, *LVal] {
	return m.v.All()
}

// Key is a type Lookup accepts as a sorted-map key.  A string key also
// finds a symbol key with the same spelling, because a sorted-map treats
// the two alike.  An int finds an int key.  A MapKey (from Keys or All)
// and an *LVal find the key they hold.  A named string type such as
// `type Field string` is not a Key; convert it with string(f).
type Key interface {
	string | int | MapKey | *LVal
}

// Lookup reads the value of key in m as a T, with the conversions of
// ResultAs: string, int, float64, bool, []byte, []*LVal, Cells, Text,
// Name, *LVal, MapView, or the payload of a native value.  It returns the
// value and true, or T's zero value and false when m has no such key, when
// the value is an error, or when the value does not convert to T.
//
//	status, ok := lisp.Lookup[string](m, "status")
//	n, _ := lisp.Lookup[int](m, 3)
//	inner, ok := lisp.Lookup[lisp.MapView](m, "response")
//	raw, ok := lisp.Lookup[*lisp.LVal](m, k)
//
// Lookup makes no check and charges no step.  A string or int key
// allocates nothing on the interpreter's own maps.
func Lookup[T any, K Key](m MapView, key K) (T, bool) {
	var zero T
	if m.v == nil {
		return zero, false
	}
	v, ok := mapLookup(m.v.Map(), key)
	if !ok || v == nil || v.IsError() {
		return zero, false
	}
	return valueAs[T](v)
}

// mapLookup finds key in md.  The switch is on a pointer to the key, so a
// string or int key is not boxed.
func mapLookup[K Key](md *MapData, key K) (*LVal, bool) {
	switch k := any(&key).(type) {
	case *string:
		return md.getString(*k)
	case *int:
		return md.getInt(*k)
	case *MapKey:
		if k.Type == LInt {
			return md.getInt(k.Int)
		}
		return md.getString(k.Str)
	case **LVal:
		if *k == nil {
			return nil, false
		}
		return md.Get(*k)
	}
	return nil, false
}
