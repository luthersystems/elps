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
// A bytes value is returned as a []byte that COPIES the lisp value's storage,
// so writing to it cannot be observed through the original (issue #548).  The
// cost is proportional to the length; see the LBytes arm of goValueNode for
// why the copy is not optional.  A native value is the opposite case and is
// returned BY REFERENCE: the payload is the embedder's own, so GoValue hands
// back what the caller already owns.
//
// Excessive nesting returns an ordinary *ErrorVal implementing error, using
// MaxValueDepth. No partial converted container is returned. Cycles discovered
// within the cap retain the historical return of the original *LVal.
//
// NOTE:  These semantics may change.  It's unclear what the exact need is in
// corner cases.
func GoValue(v *LVal) interface{} {
	out, ok := convertValue(v)
	if !ok {
		return v
	}
	return out
}

func convertValue(root *LVal) (interface{}, bool) {
	type frame struct {
		v      *LVal
		dst    *interface{}
		finish func()
		depth  int
		leave  bool
	}
	var out interface{}
	pending := []frame{{v: root, dst: &out}}
	var path map[*LVal]bool
	for len(pending) > 0 {
		f := pending[len(pending)-1]
		pending = pending[:len(pending)-1]
		if f.finish != nil {
			f.finish()
			continue
		}
		if f.leave {
			delete(path, f.v)
			continue
		}
		v := f.v
		if f.depth >= MaxValueDepth {
			return (*ErrorVal)(Error(ValueDepthError(MaxValueDepth))), true
		}
		if v.IsNil() {
			*f.dst = nil
			continue
		}
		var children []*LVal
		switch v.Type {
		case LQuote:
			children = v.Cells[:1]
		case LSExpr:
			children = v.Cells
		case LArray:
			if v.Cells[0].Len() > 1 {
				*f.dst = v
				continue
			}
			children = v.Cells[1].Cells
		case LSortMap:
		default:
			*f.dst = conversionLeaf(v)
			continue
		}
		if f.depth >= 64 {
			if path == nil {
				path = make(map[*LVal]bool)
			}
			if path[v] {
				return nil, false
			}
			path[v] = true
			pending = append(pending, frame{v: v, leave: true})
		}
		if v.Type == LSortMap {
			entries := sortedMapEntries(v.Map())
			if entries.Type == LError {
				return (*ErrorVal)(entries), true
			}
			m := make(map[interface{}]interface{}, len(entries.Cells))
			*f.dst = m
			for i := len(entries.Cells) - 1; i >= 0; i-- {
				pair := entries.Cells[i]
				if len(pair.Cells) != 2 {
					return nil, false
				}
				kv := make([]interface{}, 2)
				pending = append(pending, frame{finish: func() {
					if kv[0] != nil && reflect.ValueOf(kv[0]).Comparable() {
						m[kv[0]] = kv[1]
					} else {
						*f.dst = map[interface{}]interface{}(nil)
					}
				}}, frame{v: pair.Cells[1], dst: &kv[1], depth: f.depth + 1}, frame{v: pair.Cells[0], dst: &kv[0], depth: f.depth + 1})
			}
			continue
		}
		if v.Type == LQuote || (v.Type == LArray && v.Cells[0].Len() == 0) {
			pending = append(pending, frame{v: children[0], dst: f.dst, depth: f.depth + 1})
			continue
		}
		values := make([]interface{}, len(children))
		*f.dst = values
		for i := len(children) - 1; i >= 0; i-- {
			pending = append(pending, frame{v: children[i], dst: &values[i], depth: f.depth + 1})
		}
	}
	return out, true
}

func conversionLeaf(v *LVal) interface{} {
	switch v.Type {
	case LError:
		return (*ErrorVal)(v)
	case LSymbol, LString:
		return v.Str
	case LBytes:
		b := v.Bytes()
		out := make([]byte, len(b))
		copy(out, b)
		return out
	case LInt:
		return v.Int
	case LFloat:
		return v.Float
	case LNative:
		return v.Native
	default:
		return v
	}
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

// GoSlice converts a list to a Go slice. Non-lists, cycles and walks exceeding
// MaxValueDepth return (nil, false).
func GoSlice(v *LVal) ([]interface{}, bool) {
	if v.Type != LSExpr {
		return nil, false
	}
	out, ok := convertValue(v)
	if !ok {
		return nil, false
	}
	if v.IsNil() {
		return []interface{}{}, true
	}
	values, ok := out.([]interface{})
	return values, ok
}

// GoMap converts an LSortMap to its Go equivalent and returns it with a true
// second argument.  If v does not represent a map GoMap returns a false second
// argument.  Application's using custom Map implementations which allow
// arbitrary keys may not be able to construct a native Go map, in which case
// GoMap returns (nil, true). Cycles and excessive nesting return (nil, false).
func GoMap(v *LVal) (map[interface{}]interface{}, bool) {
	if v.Type != LSortMap {
		return nil, false
	}
	out, ok := convertValue(v)
	if !ok {
		return nil, false
	}
	values, ok := out.(map[interface{}]interface{})
	return values, ok
}
