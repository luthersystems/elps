// Copyright © 2026 The ELPS authors

package lisp

import (
	"fmt"
	"math"
	"reflect"
	"strconv"
	"sync"

	"github.com/luthersystems/elps/elpstag"
)

// maxCodecDepth bounds the nesting Decode and Encode walk.  They recurse on
// the Go stack, so the limit is fixed and far below MaxValueDepth.
const maxCodecDepth = 10_000

var (
	lvalPtrType = reflect.TypeFor[*LVal]()
	codecFields sync.Map // reflect.Type -> []elpstag.Field
)

func structFields(t reflect.Type) []elpstag.Field {
	fs, ok := codecFields.Load(t)
	if !ok {
		fs, _ = codecFields.LoadOrStore(t, elpstag.Fields(t))
	}
	return fs.([]elpstag.Field) //nolint:forcetypeassert // the cache holds only elpstag.Fields results
}

// Decode converts v to the Go type T.  It is DecodeInto on a new T: see
// DecodeInto for the conversions.  When v is an LError it returns v as the
// error.
//
//	type openArgs struct {
//		Prefix    string `elps:"prefix"`
//		HDK       int    `elps:"hdk"`
//		Autoflush bool   `elps:"autoflush"`
//	}
//	o, err := lisp.Decode[openArgs](m)
func Decode[T any](v *LVal) (T, error) {
	var out T
	err := DecodeInto(v, &out)
	return out, err
}

// DecodeInto stores v in the Go value dst points to.  A struct field or
// pointer target that v does not name keeps its value, so dst can hold
// defaults.  The conversions, by the type of the target:
//
//	*LVal         any value, as is
//	int types     an int, or a float with no fraction, in range
//	uint types    the same, not negative
//	float types   any number
//	string        a string, or a symbol other than true and false (a
//	              keyword without its colon)
//	bool          the symbol true or false, or () for false
//	[]byte        bytes (copied), or a list or vector of ints
//	slice         a list or a vector; () is an empty slice
//	array         a list or a vector of the array's length
//	map           a sorted map; its keys convert to the key type, and two
//	              keys that convert to one Go key are an error
//	struct        a sorted map, whose keys name fields by the rules of
//	              package elpstag; an unnamed field keeps its value
//	pointer       () is a nil pointer; anything else fills the pointee,
//	              allocated when nil
//	interface     () is nil; a value with methods takes only a native
//	any           a natural Go value: () is nil, an int is int64, a float
//	              float64, a string string, true and false bool, bytes
//	              []byte, a list or vector []any, a map with string or
//	              symbol keys map[string]any (a keyword key without its
//	              colon), a native its payload, and any other value the
//	              *LVal
//
// A native value converts to a target its payload is assignable to, before
// the rules above.  A map that holds a native, function or other value with
// no Go equivalent decodes into an *LVal or any field.
//
// A container v reaches along several paths decodes once per target type,
// and the result is shared at each place: two slices, maps or new pointers
// of the result can be the same.  So a value with shared structure decodes
// in time linear in its distinct containers.  Treat a result as read-only,
// or copy it before writing to it.
//
// The error names the path of the value that failed, such as "hdk" or
// "items[2].name", and the types; it never renders a value.  DecodeInto
// makes no check and charges no step.  It panics when dst is not a non-nil
// pointer.
func DecodeInto(v *LVal, dst any) error {
	if v.IsError() {
		return (*ErrorVal)(v)
	}
	if v == nil {
		v = Nil()
	}
	if done, err := decodeFast(v, dst); done {
		return err
	}
	rv := reflect.ValueOf(dst)
	if rv.Kind() != reflect.Pointer || rv.IsNil() {
		panic(fmt.Sprintf("lisp.DecodeInto: destination %T is not a non-nil pointer", dst))
	}
	var d decoder
	return d.value(v, rv.Elem(), "", 0)
}

// decodeFast stores v in the common scalar targets without reflection.  It
// returns false for any other target, and for a native, whose payload the
// reflect path tries first.
func decodeFast(v *LVal, dst any) (bool, error) {
	if d, ok := dst.(**LVal); ok {
		*d = v
		return true, nil
	}
	if v.Type == LNative {
		return false, nil
	}
	switch d := dst.(type) {
	case *string:
		s, ok := decodeString(v)
		if !ok {
			return true, decodeErr("", v, "string")
		}
		*d = s
	case *int:
		n, ok := wholeInt(v)
		if !ok {
			return true, decodeErr("", v, "int")
		}
		*d = n
	case *bool:
		b, ok := decodeBool(v)
		if !ok {
			return true, decodeErr("", v, "bool")
		}
		*d = b
	default:
		return false, nil
	}
	return true, nil
}

// CodecError is the error of Decode, DecodeInto and Encode for a value they
// cannot convert.
type CodecError struct {
	// Path is the place of the value, such as "items[2].name"; "" is the
	// value itself.
	Path string
	// Msg says what failed.
	Msg string
}

func (e *CodecError) Error() string {
	if e.Path == "" {
		return "lisp: " + e.Msg
	}
	return "lisp: " + e.Path + ": " + e.Msg
}

func decodeErr(path string, v *LVal, want string) error {
	return &CodecError{Path: path, Msg: "cannot store " + v.Type.String() + " in " + want}
}

func errTooDeep(path string) error {
	return &CodecError{Path: path, Msg: "nested more than " + strconv.Itoa(maxCodecDepth) + " levels"}
}

func joinField(path, name string) string {
	if path == "" {
		return name
	}
	return path + "." + name
}

func joinIndex(path string, i int) string {
	return path + "[" + strconv.Itoa(i) + "]"
}

// decodeMemoKey is a container and the Go type it decoded to; a nil type is
// the natural value of an any target.
type decodeMemoKey struct {
	v *LVal
	t reflect.Type
}

// decoder holds the results of the containers one DecodeInto decoded, so a
// container reached along several paths decodes once per type.
type decoder struct {
	memo map[decodeMemoKey]reflect.Value
}

func (d *decoder) lookup(v *LVal, t reflect.Type) (reflect.Value, bool) {
	r, ok := d.memo[decodeMemoKey{v, t}]
	return r, ok
}

func (d *decoder) store(v *LVal, t reflect.Type, r reflect.Value) {
	if d.memo == nil {
		d.memo = make(map[decodeMemoKey]reflect.Value)
	}
	d.memo[decodeMemoKey{v, t}] = r
}

func (d *decoder) value(v *LVal, dst reflect.Value, path string, depth int) error {
	if depth > maxCodecDepth {
		return errTooDeep(path)
	}
	t := dst.Type()
	if t == lvalPtrType {
		dst.Set(reflect.ValueOf(v))
		return nil
	}
	if v.Type == LNative && v.Native != nil && reflect.TypeOf(v.Native).AssignableTo(t) {
		dst.Set(reflect.ValueOf(v.Native))
		return nil
	}
	switch t.Kind() {
	case reflect.Pointer:
		if v.IsNil() {
			dst.SetZero()
			return nil
		}
		if !dst.IsNil() {
			return d.value(v, dst.Elem(), path, depth+1)
		}
		if r, ok := d.lookup(v, t); ok {
			dst.Set(r)
			return nil
		}
		p := reflect.New(t.Elem())
		if err := d.value(v, p.Elem(), path, depth+1); err != nil {
			return err
		}
		d.store(v, t, p)
		dst.Set(p)
		return nil
	case reflect.Interface:
		if v.IsNil() {
			dst.SetZero()
			return nil
		}
		if t.NumMethod() != 0 {
			return decodeErr(path, v, t.String())
		}
		x, err := d.natural(v, path, depth)
		if err != nil {
			return err
		}
		dst.Set(reflect.ValueOf(x))
		return nil
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
		n, ok := wholeInt64(v)
		if !ok || dst.OverflowInt(n) {
			return decodeErr(path, v, t.String())
		}
		dst.SetInt(n)
	case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64, reflect.Uintptr:
		n, ok := wholeInt64(v)
		if !ok || n < 0 || dst.OverflowUint(uint64(n)) {
			return decodeErr(path, v, t.String())
		}
		dst.SetUint(uint64(n))
	case reflect.Float32, reflect.Float64:
		f, ok := number(v)
		if !ok {
			return decodeErr(path, v, t.String())
		}
		dst.SetFloat(f)
	case reflect.String:
		s, ok := decodeString(v)
		if !ok {
			return decodeErr(path, v, t.String())
		}
		dst.SetString(s)
	case reflect.Bool:
		b, ok := decodeBool(v)
		if !ok {
			return decodeErr(path, v, t.String())
		}
		dst.SetBool(b)
	case reflect.Slice:
		return d.slice(v, dst, path, depth)
	case reflect.Array:
		items, ok := v.SeqCells()
		if !ok || len(items) != dst.Len() {
			return decodeErr(path, v, t.String())
		}
		for i, it := range items {
			if err := d.value(it, dst.Index(i), joinIndex(path, i), depth+1); err != nil {
				return err
			}
		}
	case reflect.Map:
		return d.goMap(v, dst, path, depth)
	case reflect.Struct:
		if v.Type != LSortMap {
			return decodeErr(path, v, t.String())
		}
		return d.structFields(v, dst, path, depth)
	default:
		return decodeErr(path, v, t.String())
	}
	return nil
}

func (d *decoder) slice(v *LVal, dst reflect.Value, path string, depth int) error {
	t := dst.Type()
	if v.Type == LBytes && t.Elem().Kind() == reflect.Uint8 {
		b := v.Bytes()
		s := reflect.MakeSlice(t, len(b), len(b))
		for i, c := range b {
			s.Index(i).SetUint(uint64(c))
		}
		dst.Set(s)
		return nil
	}
	items, ok := v.SeqCells()
	if !ok {
		return decodeErr(path, v, t.String())
	}
	if r, ok := d.lookup(v, t); ok {
		dst.Set(r)
		return nil
	}
	s := reflect.MakeSlice(t, len(items), len(items))
	for i, it := range items {
		if err := d.value(it, s.Index(i), joinIndex(path, i), depth+1); err != nil {
			return err
		}
	}
	if len(items) > 0 {
		d.store(v, t, s)
	}
	dst.Set(s)
	return nil
}

func (d *decoder) goMap(v *LVal, dst reflect.Value, path string, depth int) error {
	t := dst.Type()
	if v.Type != LSortMap {
		return decodeErr(path, v, t.String())
	}
	if r, ok := d.lookup(v, t); ok {
		dst.Set(r)
		return nil
	}
	m := reflect.MakeMapWithSize(t, v.Map().Len())
	for k, ev := range v.All() {
		kv := reflect.New(t.Key()).Elem()
		if err := d.value(mapKeyValue(k), kv, path+" key", depth+1); err != nil {
			return err
		}
		if m.MapIndex(kv).IsValid() {
			return &CodecError{Path: path, Msg: "two keys convert to one " + t.Key().String() + " key"}
		}
		e := reflect.New(t.Elem()).Elem()
		if err := d.value(ev, e, joinField(path, mapKeyText(k)), depth+1); err != nil {
			return err
		}
		m.SetMapIndex(kv, e)
	}
	d.store(v, t, m)
	dst.Set(m)
	return nil
}

// structFields fills the fields of dst that the keys of the sorted map v
// name.  It reads the keys in sorted order and keeps a field's first
// matching key, unless a later one is exact and the first was not: the
// elpstag rule on several matching keys.
func (d *decoder) structFields(v *LVal, dst reflect.Value, path string, depth int) error {
	fields := structFields(dst.Type())
	if len(fields) == 0 {
		return nil
	}
	var valBuf [16]*LVal
	var exactBuf [16]bool
	vals, exacts := valBuf[:], exactBuf[:]
	if len(fields) > len(valBuf) {
		vals, exacts = make([]*LVal, len(fields)), make([]bool, len(fields))
	}
	for k, ev := range v.All() {
		name, ok := k.Name()
		if !ok {
			continue
		}
		i, e := elpstag.Find(fields, name)
		if i < 0 || (vals[i] != nil && (exacts[i] || !e)) {
			continue
		}
		vals[i], exacts[i] = ev, e
	}
	for i, f := range fields {
		if vals[i] == nil {
			continue
		}
		if err := d.value(vals[i], dst.Field(f.Index), joinField(path, f.Name), depth+1); err != nil {
			return err
		}
	}
	return nil
}

func mapKeyValue(k MapKey) *LVal {
	switch k.Type {
	case LInt:
		return Int(k.Int)
	case LSymbol:
		return Symbol(k.Str)
	default:
		return String(k.Str)
	}
}

func mapKeyText(k MapKey) string {
	if k.Type == LInt {
		return strconv.Itoa(k.Int)
	}
	return k.Str
}

func decodeString(v *LVal) (string, bool) {
	switch v.Type {
	case LString:
		return v.Str, true
	case LSymbol:
		if v.Str == TrueSymbol || v.Str == FalseSymbol {
			return "", false
		}
		return keywordName(v.Str), true
	default:
		return "", false
	}
}

// keywordName is s without the colon of a keyword.
func keywordName(s string) string {
	if len(s) > 1 && s[0] == ':' {
		return s[1:]
	}
	return s
}

func decodeBool(v *LVal) (bool, bool) {
	switch {
	case v.IsSymbol(TrueSymbol):
		return true, true
	case v.IsSymbol(FalseSymbol), v.IsNil():
		return false, true
	default:
		return false, false
	}
}

func number(v *LVal) (float64, bool) {
	switch v.Type {
	case LInt:
		return float64(v.Int), true
	case LFloat:
		return v.Float, true
	default:
		return 0, false
	}
}

func wholeInt64(v *LVal) (int64, bool) {
	switch v.Type {
	case LInt:
		return int64(v.Int), true
	case LFloat:
		if v.Float == math.Trunc(v.Float) && v.Float >= math.MinInt64 && v.Float < math.MaxInt64 {
			return int64(v.Float), true
		}
	default:
	}
	return 0, false
}

func wholeInt(v *LVal) (int, bool) {
	n, ok := wholeInt64(v)
	if !ok || int64(int(n)) != n {
		return 0, false
	}
	return int(n), true
}

// natural is v as a plain Go value, for an any target.
func (d *decoder) natural(v *LVal, path string, depth int) (any, error) {
	if depth > maxCodecDepth {
		return nil, errTooDeep(path)
	}
	if v.IsNil() {
		return nil, nil
	}
	switch v.Type {
	case LInt:
		return int64(v.Int), nil
	case LFloat:
		return v.Float, nil
	case LString:
		return v.Str, nil
	case LSymbol:
		if b, ok := decodeBool(v); ok {
			return b, nil
		}
		return v, nil
	case LBytes:
		return append([]byte(nil), v.Bytes()...), nil
	case LNative:
		return v.Native, nil
	case LSExpr, LArray, LSortMap:
		if r, ok := d.lookup(v, nil); ok {
			return r.Interface(), nil
		}
		x, err := d.naturalContainer(v, path, depth)
		if err != nil {
			return nil, err
		}
		d.store(v, nil, reflect.ValueOf(x))
		return x, nil
	default:
		return v, nil
	}
}

func (d *decoder) naturalContainer(v *LVal, path string, depth int) (any, error) {
	if v.Type != LSortMap {
		items, ok := v.SeqCells()
		if !ok {
			return v, nil
		}
		out := make([]any, len(items))
		for i, it := range items {
			x, err := d.natural(it, joinIndex(path, i), depth+1)
			if err != nil {
				return nil, err
			}
			out[i] = x
		}
		return out, nil
	}
	out := make(map[string]any, v.Map().Len())
	for k, ev := range v.All() {
		name, ok := k.Name()
		if !ok {
			return v, nil
		}
		name = keywordName(name)
		if _, dup := out[name]; dup {
			return nil, &CodecError{Path: path, Msg: "two keys convert to one string key"}
		}
		x, err := d.natural(ev, joinField(path, name), depth+1)
		if err != nil {
			return nil, err
		}
		out[name] = x
	}
	return out, nil
}

// Encode returns x as an ELPS value, the reverse of DecodeInto:
//
//	*LVal               as is; a nil *LVal is ()
//	int and uint types  an int; a value out of int's range is an error
//	float types         a float
//	string              a string
//	bool                the symbol true or false
//	[]byte              bytes (copied); a byte array is a vector
//	slice, array        a vector; a nil slice is ()
//	map                 a sorted map; its keys must encode as strings,
//	                    symbols or ints
//	struct              a sorted map with string keys, by the rules of
//	                    package elpstag (omitempty leaves a zero field out)
//	pointer, interface  the value it holds; nil is ()
//
// Any other kind (chan, func, complex) is an error, and so is a struct
// with fields but none that maps to a key, such as time.Time, whose state
// would be lost.  A struct is never a native: wrap a value that should stay
// native in an *LVal field with NativeOf.  Encode keeps no memo: a Go
// value that holds one pointer, slice or map along several paths encodes
// it once per path, and a cycle fails at the depth limit.  Encode makes no check and charges no step, as the package-level
// MapOf; the values it builds are new, except an *LVal it is given.
func Encode(x any) (*LVal, error) {
	switch v := x.(type) {
	case nil:
		return Nil(), nil
	case *LVal:
		if v == nil {
			return Nil(), nil
		}
		return v, nil
	case string:
		return String(v), nil
	case int:
		return Int(v), nil
	case float64:
		return Float(v), nil
	case bool:
		return Bool(v), nil
	}
	return encodeValue(reflect.ValueOf(x), "", 0)
}

func encodeErr(path string, msg string) error {
	return &CodecError{Path: path, Msg: msg}
}

func encodeValue(rv reflect.Value, path string, depth int) (*LVal, error) {
	if depth > maxCodecDepth {
		return nil, errTooDeep(path)
	}
	if !rv.IsValid() {
		return Nil(), nil
	}
	t := rv.Type()
	if t == lvalPtrType {
		v, _ := rv.Interface().(*LVal)
		if v == nil {
			return Nil(), nil
		}
		return v, nil
	}
	switch t.Kind() {
	case reflect.Pointer, reflect.Interface:
		if rv.IsNil() {
			return Nil(), nil
		}
		return encodeValue(rv.Elem(), path, depth+1)
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
		n := rv.Int()
		if int64(int(n)) != n {
			return nil, encodeErr(path, "cannot encode "+t.String()+" "+strconv.FormatInt(n, 10)+" as an int")
		}
		return Int(int(n)), nil
	case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64, reflect.Uintptr:
		u := rv.Uint()
		if u > math.MaxInt {
			return nil, encodeErr(path, "cannot encode "+t.String()+" "+strconv.FormatUint(u, 10)+" as an int")
		}
		return Int(int(u)), nil
	case reflect.Float32, reflect.Float64:
		return Float(rv.Float()), nil
	case reflect.String:
		return String(rv.String()), nil
	case reflect.Bool:
		return Bool(rv.Bool()), nil
	case reflect.Slice, reflect.Array:
		if t.Kind() == reflect.Slice && t.Elem().Kind() == reflect.Uint8 {
			b := make([]byte, rv.Len())
			for i := range b {
				b[i] = byte(rv.Index(i).Uint()) //nolint:gosec // the element's kind is uint8
			}
			return Bytes(b), nil
		}
		if t.Kind() == reflect.Slice && rv.IsNil() {
			return Nil(), nil
		}
		cells := make([]*LVal, rv.Len())
		for i := range cells {
			c, err := encodeValue(rv.Index(i), joinIndex(path, i), depth+1)
			if err != nil {
				return nil, err
			}
			cells[i] = c
		}
		return Vector(cells), nil
	case reflect.Map:
		if rv.IsNil() {
			return Nil(), nil
		}
		m := SortedMap()
		it := rv.MapRange()
		for it.Next() {
			k, err := encodeValue(it.Key(), path+" key", depth+1)
			if err != nil {
				return nil, err
			}
			if k.Type != LString && k.Type != LSymbol && k.Type != LInt {
				return nil, encodeErr(path, "cannot use "+k.Type.String()+" as a map key")
			}
			ev, err := encodeValue(it.Value(), joinField(path, keyText(k)), depth+1)
			if err != nil {
				return nil, err
			}
			if r := m.MapSetLVal(k, ev); r.IsError() {
				return nil, encodeErr(path, "cannot set a map key")
			}
		}
		return m, nil
	case reflect.Struct:
		fields := structFields(t)
		if len(fields) == 0 && t.NumField() > 0 {
			return nil, encodeErr(path, "cannot encode a "+t.String()+": it has no exported fields")
		}
		m := SortedMap()
		for _, f := range fields {
			fv := rv.Field(f.Index)
			if f.OmitEmpty && fv.IsZero() {
				continue
			}
			ev, err := encodeValue(fv, joinField(path, f.Name), depth+1)
			if err != nil {
				return nil, err
			}
			m.MapSetString(f.Name, ev)
		}
		return m, nil
	default:
		return nil, encodeErr(path, "cannot encode a "+t.String())
	}
}

func keyText(k *LVal) string {
	if k.Type == LInt {
		return strconv.Itoa(k.Int)
	}
	return k.Str
}
