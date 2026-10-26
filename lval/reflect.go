// Copyright © 2026 The ELPS authors

package lval

import (
	"errors"
	"math"
	"reflect"

	"github.com/luthersystems/elps/elpstag"
)

var valueType = reflect.TypeFor[Value]()

// Decode stores v in the Go value dst points to:
//
//   - a Value takes v as it is;
//   - an int or uint type takes an int (or a whole float), a float type any
//     number, a string a string, symbol or keyword, a bool a boolean, a
//     []byte bytes;
//   - a slice or array takes a list or vector, nil as an empty slice;
//   - a map takes a sorted map, and a struct a sorted map whose keys name
//     its fields by the rules of package elpstag, which lisp.Decode
//     follows too: the field's elps tag (`elps:"name"`), or its name,
//     in any case, as a string, symbol or keyword key; when several keys
//     match a field, the one spelled exactly as its name wins;
//   - an any takes a natural Go value: int64, float64, string, bool,
//     []byte, []any, map[string]any, or a Value for anything else.
func Decode(v Value, dst any) error {
	// The common scalar types skip reflection, which costs far more fuel
	// than the store.
	switch d := dst.(type) {
	case *Value:
		*d = v
		return nil
	case *int:
		n, ok := wholeNumber(v)
		if !ok || int64(int(n)) != n {
			return decodeErr("", v, "int")
		}
		*d = int(n)
		return nil
	case *int64:
		n, ok := wholeNumber(v)
		if !ok {
			return decodeErr("", v, "int64")
		}
		*d = n
		return nil
	case *float64:
		f, ok := v.Float()
		if !ok {
			return decodeErr("", v, "float64")
		}
		*d = f
		return nil
	case *string:
		str, ok := v.Str()
		if !ok {
			return decodeErr("", v, "string")
		}
		*d = str
		return nil
	case *bool:
		b, ok := v.Bool()
		if !ok {
			return decodeErr("", v, "bool")
		}
		*d = b
		return nil
	default:
	}
	rv := reflect.ValueOf(dst)
	if rv.Kind() != reflect.Pointer || rv.IsNil() {
		return errors.New("lval: Decode needs a non-nil pointer")
	}
	return decode(v, rv.Elem(), "")
}

func decodeErr(path string, v Value, want string) error {
	if path == "" {
		path = "value"
	}
	return errors.New("lval: " + path + ": cannot store " + v.kind.String() + " in " + want)
}

func decode(v Value, dst reflect.Value, path string) error {
	if dst.Type() == valueType {
		dst.Set(reflect.ValueOf(v))
		return nil
	}
	switch dst.Kind() {
	case reflect.Pointer:
		if v.kind == Nil {
			dst.SetZero()
			return nil
		}
		if dst.IsNil() {
			dst.Set(reflect.New(dst.Type().Elem()))
		}
		return decode(v, dst.Elem(), path)
	case reflect.Interface:
		if dst.NumMethod() != 0 {
			return decodeErr(path, v, dst.Type().String())
		}
		if v.kind == Nil {
			dst.SetZero()
			return nil
		}
		dst.Set(reflect.ValueOf(natural(v)))
		return nil
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
		n, ok := wholeNumber(v)
		if !ok || dst.OverflowInt(n) {
			return decodeErr(path, v, dst.Type().String())
		}
		dst.SetInt(n)
	case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64:
		n, ok := wholeNumber(v)
		if !ok || n < 0 || dst.OverflowUint(uint64(n)) {
			return decodeErr(path, v, dst.Type().String())
		}
		dst.SetUint(uint64(n))
	case reflect.Float32, reflect.Float64:
		f, ok := v.Float()
		if !ok {
			return decodeErr(path, v, dst.Type().String())
		}
		dst.SetFloat(f)
	case reflect.String:
		s, ok := v.Str()
		if !ok {
			return decodeErr(path, v, "string")
		}
		dst.SetString(s)
	case reflect.Bool:
		b, ok := v.Bool()
		if !ok {
			return decodeErr(path, v, "bool")
		}
		dst.SetBool(b)
	case reflect.Slice:
		if dst.Type().Elem().Kind() == reflect.Uint8 {
			if b, ok := v.Bytes(); ok {
				dst.SetBytes(b)
				return nil
			}
		}
		if v.kind != Nil && v.kind != List && v.kind != Vector {
			return decodeErr(path, v, dst.Type().String())
		}
		s := reflect.MakeSlice(dst.Type(), len(v.items), len(v.items))
		for i, it := range v.items {
			if err := decode(it, s.Index(i), path+"["+itoa(i)+"]"); err != nil {
				return err
			}
		}
		dst.Set(s)
	case reflect.Array:
		if (v.kind != List && v.kind != Vector && v.kind != Nil) || len(v.items) != dst.Len() {
			return decodeErr(path, v, dst.Type().String())
		}
		for i, it := range v.items {
			if err := decode(it, dst.Index(i), path+"["+itoa(i)+"]"); err != nil {
				return err
			}
		}
	case reflect.Map:
		if v.kind != Map {
			return decodeErr(path, v, dst.Type().String())
		}
		m := reflect.MakeMapWithSize(dst.Type(), len(v.keys))
		for i, k := range v.keys {
			kv := reflect.New(dst.Type().Key()).Elem()
			if err := decode(k, kv, path+" key"); err != nil {
				return err
			}
			ev := reflect.New(dst.Type().Elem()).Elem()
			if err := decode(v.items[i], ev, path+"."+k.String()); err != nil {
				return err
			}
			m.SetMapIndex(kv, ev)
		}
		dst.Set(m)
	case reflect.Struct:
		if v.kind != Map {
			return decodeErr(path, v, dst.Type().String())
		}
		fields := elpstag.Fields(dst.Type())
		// The keys are in sorted order, so a field keeps its first
		// matching key unless a later one is spelled exactly as its name.
		match := make([]int, len(fields))
		exact := make([]bool, len(fields))
		for i := range match {
			match[i] = -1
		}
		for j, k := range v.keys {
			ks, ok := k.Str()
			if !ok {
				continue
			}
			i, e := elpstag.Find(fields, ks)
			if i < 0 || match[i] >= 0 && (exact[i] || !e) {
				continue
			}
			match[i], exact[i] = j, e
		}
		for i, f := range fields {
			if match[i] < 0 {
				continue
			}
			if err := decode(v.items[match[i]], dst.Field(f.Index), joinPath(path, f.Name)); err != nil {
				return err
			}
		}
	default:
		return decodeErr(path, v, dst.Type().String())
	}
	return nil
}

func joinPath(path, name string) string {
	if path == "" {
		return name
	}
	return path + "." + name
}

func wholeNumber(v Value) (int64, bool) {
	switch v.kind {
	case Int:
		return v.i, true
	case Float:
		if v.f == math.Trunc(v.f) && math.Abs(v.f) < 1<<63 {
			return int64(v.f), true
		}
	default:
	}
	return 0, false
}

// natural is v as a plain Go value.
func natural(v Value) any {
	switch v.kind {
	case Nil:
		return nil
	case Int:
		return v.i
	case Float:
		return v.f
	case String, Symbol, Keyword:
		if v.kind == String {
			return v.str
		}
		return v
	case Bool:
		return v.i == 1
	case Bytes:
		return v.bytes
	case List, Vector:
		out := make([]any, len(v.items))
		for i, it := range v.items {
			out[i] = natural(it)
		}
		return out
	case Map:
		out := make(map[string]any, len(v.keys))
		for i, k := range v.keys {
			s, ok := k.Str()
			if !ok {
				return v
			}
			out[s] = natural(v.items[i])
		}
		return out
	default:
	}
	return v
}

// Encode returns the Value of a Go value: the reverse of Decode. A struct
// becomes a sorted map with string keys (its fields' elps tags or names; a
// tag with ",omitempty" leaves out a zero field), a Go map a sorted map, a
// slice a vector, a nil pointer, slice or map nil.
func Encode(x any) (Value, error) {
	switch v := x.(type) {
	case Value:
		return v, nil
	case int:
		return MakeInt(int64(v)), nil
	case int64:
		return MakeInt(v), nil
	case float64:
		return MakeFloat(v), nil
	case string:
		return MakeString(v), nil
	case bool:
		return MakeBool(v), nil
	default:
	}
	return encode(reflect.ValueOf(x))
}

func encode(rv reflect.Value) (Value, error) {
	if !rv.IsValid() {
		return Value{}, nil
	}
	if rv.Type() == valueType {
		return rv.Interface().(Value), nil //nolint:forcetypeassert // checked above
	}
	switch rv.Kind() {
	case reflect.Pointer, reflect.Interface:
		if rv.IsNil() {
			return Value{}, nil
		}
		return encode(rv.Elem())
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
		return MakeInt(rv.Int()), nil
	case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64:
		u := rv.Uint()
		if u > math.MaxInt64 {
			return Value{}, errors.New("lval: uint too large for an ELPS int")
		}
		return MakeInt(int64(u)), nil
	case reflect.Float32, reflect.Float64:
		return MakeFloat(rv.Float()), nil
	case reflect.String:
		return MakeString(rv.String()), nil
	case reflect.Bool:
		return MakeBool(rv.Bool()), nil
	case reflect.Slice, reflect.Array:
		if rv.Kind() == reflect.Slice && rv.Type().Elem().Kind() == reflect.Uint8 {
			return MakeBytes(rv.Bytes()), nil
		}
		if rv.Kind() == reflect.Slice && rv.IsNil() {
			return Value{}, nil
		}
		items := make([]Value, rv.Len())
		for i := range items {
			var err error
			if items[i], err = encode(rv.Index(i)); err != nil {
				return Value{}, err
			}
		}
		return MakeVector(items...), nil
	case reflect.Map:
		if rv.IsNil() {
			return Value{}, nil
		}
		m := Value{kind: Map}
		it := rv.MapRange()
		for it.Next() {
			k, err := encode(it.Key())
			if err != nil {
				return Value{}, err
			}
			ev, err := encode(it.Value())
			if err != nil {
				return Value{}, err
			}
			m.Set(k, ev)
		}
		return m, nil
	case reflect.Struct:
		m := Value{kind: Map}
		for _, f := range elpstag.Fields(rv.Type()) {
			fv := rv.Field(f.Index)
			if f.OmitEmpty && fv.IsZero() {
				continue
			}
			ev, err := encode(fv)
			if err != nil {
				return Value{}, err
			}
			m.Set(MakeString(f.Name), ev)
		}
		return m, nil
	default:
	}
	return Value{}, errors.New("lval: cannot encode a " + rv.Type().String())
}

func itoa(i int) string {
	const digits = "0123456789"
	if i < 10 {
		return digits[i : i+1]
	}
	return itoa(i/10) + digits[i%10:i%10+1]
}
