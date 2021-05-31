// Copyright © 2018 The ELPS authors

package lisp

import "reflect"

// True interprets v as a boolean and returns the result.
//
// NOTE:  I don't like this name, really.  But I can't think of a better one.
func True(v *LVal) bool {
	if v.IsNil() {
		return false
	}
	if v.Type != LSymbol {
		return true
	}
	return v.Str != FalseSymbol
}

// Not interprets v as a boolean value and returns its negation.
func Not(v *LVal) bool {
	return !True(v)
}

// GoValue converts v to its natural representation in Go.  Quotes are ignored
// and all lists are turned into slices.  Symbols are converted to strings.
// The value Nil() is converted to nil.  Functions are returned as is.
//
// NOTE:  These semantics may change.  It's unclear what the exact need is in
// corner cases.
func GoValue(v *LVal) interface{} {
	if v.IsNil() {
		return nil
	}
	switch v.Type {
	case LError:
		return (error)((*ErrorVal)(v))
	case LSymbol, LString:
		return v.Str
	case LBytes:
		return v.Bytes
	case LInt:
		return v.Int
	case LFloat:
		return v.Float
	case LQuote:
		return GoValue(v.Cells[0])
	case LSExpr:
		s, _ := GoSlice(v)
		return s
	case LSortMap:
		m, _ := GoMap(v)
		return m
	case LArray:
		dims, storage := v.Cells[0], v.Cells[1]
		s, _ := GoSlice(SExpr(storage.Cells))
		switch dims.Len() {
		case 0:
			return s[0]
		case 1:
			return s
		default:
			// TODO:  Slice up the backing storage s to create a multidimensional
			// slice (e.g.  [][]interface{}).
			return v
		}
	case LNative:
		return v.Native
	}
	return v
}

// GoError returns an error that represents v.  If v is not LError then nil is
// returned.
func GoError(v *LVal) error {
	if v.Type != LError {
		return nil
	}
	return (*ErrorVal)(v)
}

// GoString returns the string that v represents and the value true.  If v does
// not represent a string GoString returns a false second argument
func GoString(v *LVal) (string, bool) {
	if v.Type != LString {
		return "", false
	}
	return v.Str, true
}

// SymbolName returns the name of the symbol that v represents and the value
// true.  If v does not represent a symbol SymbolName returns a false second
// argument
func SymbolName(v *LVal) (string, bool) {
	if v.Type != LSymbol {
		return "", false
	}
	return v.Str, true
}

// GoInt converts the numeric value that v represents to and int and returns it
// with the value true.  If v does not represent a number GoInt returns a
// false second argument
func GoInt(v *LVal) (int, bool) {
	if !v.IsNumeric() {
		return 0, false
	}
	if v.Type == LFloat {
		return int(v.Float), true
	}
	return v.Int, true
}

// GoFloat64 converts the numeric value that v represents to a float64 and
// returns it with the value true.  If v does not represent a number GoFloat64
// returns a false second argument
func GoFloat64(v *LVal) (float64, bool) {
	if !v.IsNumeric() {
		return 0, false
	}
	if v.Type == LFloat {
		return v.Float, true
	}
	return float64(v.Int), true
}

// GoSlice returns the string that v represents and the value true.  If v does
// not represent a string GoSlice returns a false second argument
func GoSlice(v *LVal) ([]interface{}, bool) {
	if v.Type != LSExpr {
		return nil, false
	}
	vs := make([]interface{}, len(v.Cells))
	for i := range vs {
		vs[i] = GoValue(v.Cells[i])
	}
	return vs, true
}

// GoMap converts an LSortMap to its Go equivalent and returns it with a true
// second argument.  If v does not represent a map GoMap returns a false second
// argument.  Application's using custom Map implementations which allow
// arbitrary keys may not be able to construct a native Go map, in which case
// GoMap returns (nil, true).
func GoMap(v *LVal) (map[interface{}]interface{}, bool) {
	if v.Type != LSortMap {
		return nil, false
	}
	data := v.Map()
	m := make(gomap, data.Len())
	for _, pair := range sortedMapEntries(data).Cells {
		if !checkGoMapInsert(m, pair.Cells[0], pair.Cells[1]) {
			return nil, true
		}
	}
	return m, true
}

func checkGoMapInsert(m gomap, lk, lv *LVal) (ok bool) {
	// k is definitely assignable to m's key type (interface{}) but map keys
	// must also be comparable which is not known without reflection on k's
	// type (or through recovering a failed map assignment).
	k := GoValue(lk)
	if reflect.TypeOf(k).Comparable() {
		m[k] = GoValue(lv)
		return true
	}
	return false
}

type gomap = map[interface{}]interface{}
