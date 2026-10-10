// Copyright © 2026 The ELPS authors

package lisp

import "fmt"

// mapOfStack is the number of converted keys and values MapOf holds in a
// fixed array on the stack before it allocates a slice.
const mapOfStack = 16

// MapOf returns a new sorted map built from kv, which holds alternating Go
// keys and values.  It converts each one and calls SortedMapOf, so it
// returns what the sorted-map builtin returns: the same map, or the same
// error from the same check (the context check, "uneven number of
// arguments: N" and the allocation check before each new key).
//
//	return lisp.Result(env.MapOf(
//		"id", id,
//		"type", typ,
//		"description", desc))
//
// A key is a string or an *LVal.  A value is one of the Func*E result types:
// *LVal (a nil *LVal becomes ()), string, int, float64, bool, []byte (bytes),
// []*LVal or Cells (a list that uses the slice as its storage).  Any other
// key or value type is a bug in the caller, and MapOf panics, as fmt does
// for a bad verb; elpsidiom reports it at build time.  MapOf never turns a
// value into a native: use NativeOf for one.
//
// MapOf uses a type switch, not reflection.  A string key or value becomes a
// new string value, as lisp.String does.  The converted keys and values sit
// in a fixed array on the stack for up to 16 of them, else in one allocated
// slice.  MapOf charges no step.
func (env *LEnv) MapOf(kv ...any) *LVal {
	var buf [mapOfStack]*LVal
	var cells []*LVal
	if len(kv) <= mapOfStack {
		cells = buf[:len(kv)]
	} else {
		cells = make([]*LVal, len(kv))
	}
	for i, x := range kv {
		if i%2 == 0 {
			cells[i] = mapOfKey(x)
		} else {
			cells[i] = mapOfValue(x)
		}
	}
	return env.SortedMapOf(cells...)
}

// mapOfKey converts a MapOf key.
func mapOfKey(x any) *LVal {
	switch k := x.(type) {
	case string:
		return String(k)
	case *LVal:
		if k == nil {
			panic("lisp.MapOf: nil *LVal key")
		}
		return k
	}
	panic(fmt.Sprintf("lisp.MapOf: key of type %T; a key is a string or an *LVal", x))
}

// mapOfValue converts a MapOf value.
func mapOfValue(x any) *LVal {
	switch v := x.(type) {
	case *LVal:
		if v == nil {
			return Nil()
		}
		return v
	case string:
		return String(v)
	case int:
		return Int(v)
	case float64:
		return Float(v)
	case bool:
		return Bool(v)
	case []byte:
		return Bytes(v)
	case []*LVal:
		return QExpr(v)
	case Cells:
		return QExpr(v)
	}
	panic(fmt.Sprintf("lisp.MapOf: value of type %T; a value is *LVal, string, int, float64, bool, []byte, []*LVal or Cells", x))
}
