// Copyright © 2026 The ELPS authors

package lisp

import "reflect"

// ResultAs is Result followed by a conversion of the value to the Go type T.
// When v is an LError it returns v as the error.  When v converts to T it
// returns the Go value and a nil error.  Otherwise it returns an error with
// condition "error", such as "value is not a string: int".
//
//	keys, err := lisp.ResultAs[lisp.Cells](env.CallBuiltin(coreKeys, m))
//	msg, err := lisp.ResultAs[string](env.FormatString("unknown type: {}", k))
//
// T selects the conversion:
//
//	string        a string only (use SymbolName for a symbol)
//	int           an integer only; a float is not truncated
//	float64       any number; an integer is converted
//	bool          any value, by truthiness (see True); it never fails
//	[]byte        bytes
//	[]*LVal       the cells of a list
//	Cells         the cells of a list
//	Text          a string (copied) or bytes
//	Name          a string or a symbol, its text
//	*LVal         any value, as is
//	MapView       a sorted-map
//	any other T   the payload of a native value, through NativeValue[T]
//
// A named type such as `type Status string` is "any other T": it reads a
// native, not a string.  A []byte, []*LVal or Cells result shares the
// value's storage, so treat it as read-only.
//
// Use ResultAs only where the value always has type T, such as the list that
// keys returns or the string that format-string returns.  A mismatch is then
// a bug in the Go code.  ResultAs uses no reflection and allocates nothing
// unless the conversion fails.
func ResultAs[T any](v *LVal) (T, error) {
	var zero T
	if v.IsError() {
		return zero, (*ErrorVal)(v)
	}
	out, ok := valueAs[T](v)
	if !ok {
		return zero, (*ErrorVal)(Errorf("value is not %s: %v", typeNoun[T](), v.Type))
	}
	return out, nil
}

// valueAs converts v to a T for ResultAs, Lookup, SeqOf, the Func*E
// arguments and CellReader.  The switch is on
// a pointer to the result, so each call is one type compare and no
// reflection.
func valueAs[T any](v *LVal) (T, bool) {
	var out T
	switch p := any(&out).(type) {
	case *string:
		if v.Type != LString {
			return out, false
		}
		*p = v.Str
	case *int:
		if v.Type != LInt {
			return out, false
		}
		*p = v.Int
	case *float64:
		switch v.Type {
		case LInt:
			*p = float64(v.Int)
		case LFloat:
			*p = v.Float
		default:
			return out, false
		}
	case *bool:
		*p = True(v)
	case *[]byte:
		if v.Type != LBytes {
			return out, false
		}
		*p = v.Bytes()
	case *[]*LVal:
		if v.Type != LSExpr {
			return out, false
		}
		*p = v.Cells
	case *Cells:
		if v.Type != LSExpr {
			return out, false
		}
		*p = v.Cells
	case *Name:
		if v.Type != LString && v.Type != LSymbol {
			return out, false
		}
		*p = Name(v.Str)
	case *Text:
		switch v.Type {
		case LString:
			*p = Text(v.Str)
		case LBytes:
			*p = v.Bytes()
		default:
			return out, false
		}
	case **LVal:
		*p = v
	case *MapView:
		if v.Type != LSortMap {
			return out, false
		}
		*p = MapView{v: v}
	default:
		return NativeValue[T](v)
	}
	return out, true
}

// typeNoun names T in an error message: "a string", "an integer", or "a
// native *pkg.Type" for a native payload type.
func typeNoun[T any]() string {
	var out T
	switch any(&out).(type) {
	case *string:
		return "a string"
	case *int:
		return "an integer"
	case *float64:
		return "a number"
	case *bool:
		return "a bool"
	case *[]byte:
		return "bytes"
	case *[]*LVal, *Cells:
		return "a list"
	case **LVal:
		return "a value"
	case *MapView:
		return "a sorted-map"
	case *Text:
		return textNoun
	case *Name:
		return nameNoun
	}
	return "a native " + reflect.TypeFor[T]().String()
}

// SeqOf converts a list or a one-dimensional vector to a Go slice of T, with
// the conversions of ResultAs for each element.  It returns ok=false when v
// is not such a sequence and when any element does not convert to T; there
// is no partial result.
//
//	names, ok := lisp.SeqOf[string](methodArgs)
//
// SeqOf allocates the slice it returns and nothing else.  It makes no check
// and charges no step.  It reads one level only: GoSliceOf converts nested
// values and reads lists only.
func SeqOf[T any](v *LVal) ([]T, bool) {
	cells, ok := v.SeqCells()
	if !ok {
		return nil, false
	}
	out := make([]T, len(cells))
	for i, c := range cells {
		x, ok := valueAs[T](c)
		if !ok {
			return nil, false
		}
		out[i] = x
	}
	return out, true
}
