// Copyright © 2026 The ELPS authors

package lval

import (
	"errors"
	"math"
	"reflect"
	"slices"
	"strconv"
	"strings"

	"github.com/luthersystems/elps/elpstag"
)

var valueType = reflect.TypeFor[Value]()

// maxCodecDepth bounds the nesting Decode and Encode walk, as lisp.Decode
// and lisp.Encode bound theirs. They recurse on the Go stack, so a Go value
// that holds itself must fail here, not overflow the stack.
const maxCodecDepth = 10_000

func errTooDeep(path string) error {
	return errors.New("lval: " + orValue(path) + ": nested more than " + strconv.Itoa(maxCodecDepth) + " levels")
}

// Decode stores v in the Go value dst points to:
//
//   - a Value takes v as it is;
//   - an int or uint type takes an int (or a whole float), a float type any
//     number, a string a string, symbol or keyword, a bool a boolean or nil
//     (false), a []byte bytes;
//   - a slice or array takes a list or vector, nil as an empty slice;
//   - a map takes a sorted map, and a struct a sorted map whose keys name
//     its fields by the rules of package elpstag, which lisp.Decode
//     follows too: the field's elps tag (`elps:"name"`), or its name,
//     in any case, as a string, symbol or keyword key; when several keys
//     match a field, the one spelled exactly as its name wins;
//   - an any takes a natural Go value: int64, float64, string, bool,
//     []byte, []any, map[string]any, or a Value for anything else.
//
// These are lisp.DecodeInto's rules, and a map of keys that convert to one
// Go key (such as "a" and :a for a map[string]T) fails as it does there.
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
		b, ok := decodeBool(v)
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
	return decode(v, rv.Elem(), "", 0)
}

func decodeBool(v Value) (bool, bool) {
	if v.kind == Nil {
		return false, true
	}
	return v.Bool()
}

func decodeErr(path string, v Value, want string) error {
	if path == "" {
		path = "value"
	}
	return errors.New("lval: " + path + ": cannot store " + v.kind.String() + " in " + want)
}

func decode(v Value, dst reflect.Value, path string, depth int) error {
	if depth > maxCodecDepth {
		return errTooDeep(path)
	}
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
		return decode(v, dst.Elem(), path, depth+1)
	case reflect.Interface:
		if dst.NumMethod() != 0 {
			return decodeErr(path, v, dst.Type().String())
		}
		if v.kind == Nil {
			dst.SetZero()
			return nil
		}
		x, err := natural(v, path, depth)
		if err != nil {
			return err
		}
		dst.Set(reflect.ValueOf(x))
		return nil
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
		n, ok := wholeNumber(v)
		if !ok || dst.OverflowInt(n) {
			return decodeErr(path, v, dst.Type().String())
		}
		dst.SetInt(n)
	case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64, reflect.Uintptr:
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
		b, ok := decodeBool(v)
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
			if err := decode(it, s.Index(i), path+"["+strconv.Itoa(i)+"]", depth+1); err != nil {
				return err
			}
		}
		dst.Set(s)
	case reflect.Array:
		if (v.kind != List && v.kind != Vector && v.kind != Nil) || len(v.items) != dst.Len() {
			return decodeErr(path, v, dst.Type().String())
		}
		for i, it := range v.items {
			if err := decode(it, dst.Index(i), path+"["+strconv.Itoa(i)+"]", depth+1); err != nil {
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
			if err := decode(k, kv, path+" key", depth+1); err != nil {
				return err
			}
			if m.MapIndex(kv).IsValid() {
				return errors.New("lval: " + orValue(path) + ": two keys convert to one " + dst.Type().Key().String() + " key")
			}
			ev := reflect.New(dst.Type().Elem()).Elem()
			if err := decode(v.items[i], ev, joinPath(path, k.String()), depth+1); err != nil {
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
		// A field keeps its first matching key in the order lisp.DecodeInto
		// reads them, by name, unless a later one is spelled exactly as
		// its name.
		match := make([]int, len(fields))
		exact := make([]bool, len(fields))
		for i := range match {
			match[i] = -1
		}
		for _, j := range namedKeys(v) {
			i, e := elpstag.Find(fields, keyName(v.keys[j]))
			if i < 0 || match[i] >= 0 && (exact[i] || !e) {
				continue
			}
			match[i], exact[i] = j, e
		}
		for i, f := range fields {
			if match[i] < 0 {
				continue
			}
			if err := decode(v.items[match[i]], dst.Field(f.Index), joinPath(path, f.Name), depth+1); err != nil {
				return err
			}
		}
	default:
		return decodeErr(path, v, dst.Type().String())
	}
	return nil
}

// keyName is a key's spelling as a lisp map key's name: a keyword with
// its colon, and true and false as their symbols. It is "" for an int.
func keyName(k Value) string {
	switch k.kind {
	case String, Symbol:
		return k.str
	case Keyword:
		return ":" + k.str
	case Bool:
		if k.i == 1 {
			return "true"
		}
		return "false"
	default:
	}
	return ""
}

// namedKeys returns the positions of m's keys that have a name (all but
// int keys), in the order lisp reads a map's keys: by their names.
func namedKeys(m Value) []int {
	out := make([]int, 0, len(m.keys))
	for j, k := range m.keys {
		if k.kind != Int {
			out = append(out, j)
		}
	}
	slices.SortStableFunc(out, func(a, b int) int { return strings.Compare(keyName(m.keys[a]), keyName(m.keys[b])) })
	return out
}

func orValue(path string) string {
	if path == "" {
		return "value"
	}
	return path
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
		if v.f == math.Trunc(v.f) && v.f >= math.MinInt64 && v.f < math.MaxInt64 {
			return int64(v.f), true
		}
	default:
	}
	return 0, false
}

// natural is v as a plain Go value, as lisp.DecodeInto stores it in an
// any.
func natural(v Value, path string, depth int) (any, error) {
	if depth > maxCodecDepth {
		return nil, errTooDeep(path)
	}
	switch v.kind {
	case Nil:
		return nil, nil
	case Int:
		return v.i, nil
	case Float:
		return v.f, nil
	case String:
		return v.str, nil
	case Bool:
		return v.i == 1, nil
	case Bytes:
		return v.bytes, nil
	case List, Vector:
		out := make([]any, len(v.items))
		for i, it := range v.items {
			x, err := natural(it, path+"["+strconv.Itoa(i)+"]", depth+1)
			if err != nil {
				return nil, err
			}
			out[i] = x
		}
		return out, nil
	case Map:
		keys := namedKeys(v)
		if len(keys) != len(v.keys) {
			return v, nil // an int key: no string map holds it
		}
		out := make(map[string]any, len(v.keys))
		for _, j := range keys {
			name := keyName(v.keys[j])
			if v.keys[j].kind == Keyword && len(name) > 1 {
				name = name[1:] // as lisp's keywordName
			}
			if _, dup := out[name]; dup {
				return nil, errors.New("lval: " + orValue(path) + ": two keys convert to one string key")
			}
			x, err := natural(v.items[j], joinPath(path, name), depth+1)
			if err != nil {
				return nil, err
			}
			out[name] = x
		}
		return out, nil
	default:
	}
	return v, nil
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
	return encode(reflect.ValueOf(x), "", 0)
}

func encode(rv reflect.Value, path string, depth int) (Value, error) {
	if depth > maxCodecDepth {
		return Value{}, errTooDeep(path)
	}
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
		return encode(rv.Elem(), path, depth+1)
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
		return MakeInt(rv.Int()), nil
	case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64, reflect.Uintptr:
		u := rv.Uint()
		if u > math.MaxInt64 {
			return Value{}, errors.New("lval: " + orValue(path) + ": uint too large for an ELPS int")
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
			if items[i], err = encode(rv.Index(i), path+"["+strconv.Itoa(i)+"]", depth+1); err != nil {
				return Value{}, err
			}
		}
		return MakeVector(items...), nil
	case reflect.Map:
		if rv.IsNil() {
			return Value{}, nil
		}
		var keys, vals []Value
		it := rv.MapRange()
		for it.Next() {
			k, err := encode(it.Key(), path+" key", depth+1)
			if err != nil {
				return Value{}, err
			}
			if _, ok := keyID(k); !ok {
				return Value{}, errors.New("lval: " + orValue(path) + ": cannot use " + k.kind.String() + " as a map key")
			}
			ev, err := encode(it.Value(), joinPath(path, k.String()), depth+1)
			if err != nil {
				return Value{}, err
			}
			keys, vals = append(keys, k), append(vals, ev)
		}
		return buildMap(keys, vals), nil
	case reflect.Struct:
		fields := elpstag.Fields(rv.Type())
		if len(fields) == 0 && rv.NumField() > 0 {
			return Value{}, errors.New("lval: " + orValue(path) + ": cannot encode a " + rv.Type().String() + ": it has no exported fields")
		}
		var keys, vals []Value
		for _, f := range fields {
			fv := rv.Field(f.Index)
			if f.OmitEmpty && fv.IsZero() {
				continue
			}
			ev, err := encode(fv, joinPath(path, f.Name), depth+1)
			if err != nil {
				return Value{}, err
			}
			keys, vals = append(keys, MakeString(f.Name)), append(vals, ev)
		}
		return buildMap(keys, vals), nil
	default:
	}
	return Value{}, errors.New("lval: " + orValue(path) + ": cannot encode a " + rv.Type().String())
}
