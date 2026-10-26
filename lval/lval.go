// Copyright © 2026 The ELPS authors

// Package lval holds ELPS values in Go without the interpreter, and reads
// and writes them as elps typed JSON (json:dump-bytes :typed true,
// libjson.DumpTyped). A Value holds one value: an int, a float, a string, a
// symbol, a keyword, a boolean, bytes, a list, a vector, a sorted map, a
// tagged value or an array.
//
// It is for Go code that exchanges values with an ELPS program but cannot
// link package lisp, such as a WebAssembly guest. Typed JSON keeps every
// type: 1 and 1.0 stay apart, and so do a string, a symbol and a keyword of
// one spelling. Marshal writes the bytes libjson.DumpTyped writes for the
// same value, and Unmarshal reads what libjson.LoadTyped reads.
//
// Decode and Encode convert between a Value and ordinary Go values. A
// struct maps to a sorted map by the rules of package elpstag, which
// lisp.Decode and lisp.Encode follow too:
//
//	var req struct {
//		Item     string `elps:"item"`
//		Quantity int    `elps:"quantity"`
//	}
//	if err := lval.Decode(v, &req); err != nil { ... }
//
// The package imports neither package lisp nor encoding/json.
package lval

import (
	"slices"
	"strconv"
	"strings"
)

// Kind is the ELPS type of a Value.
type Kind uint8

// The kinds of Value.
const (
	Nil Kind = iota // the empty list, ()
	Int
	Float
	String
	Symbol
	Keyword
	Bool
	Bytes
	List
	Vector
	Map
	Tagged
	Array
)

var kindNames = [...]string{"nil", "int", "float", "string", "symbol", "keyword", "bool", "bytes", "list", "vector", "sorted-map", "tagged", "array"}

func (k Kind) String() string {
	if int(k) < len(kindNames) {
		return kindNames[k]
	}
	return "kind(" + strconv.Itoa(int(k)) + ")"
}

// Value is one ELPS value. The zero Value is nil, the empty list.
type Value struct {
	str   string  // string, symbol and keyword (no colon) text; tagged type name
	items []Value // list, vector, tagged (one item), array cells; map values
	keys  []Value // map keys, in items order
	dims  []int   // array dimensions
	bytes []byte
	f     float64
	i     int64 // int; bool (0 or 1)
	kind  Kind
}

// MakeInt returns an int.
func MakeInt(n int64) Value { return Value{kind: Int, i: n} }

// MakeFloat returns a float.
func MakeFloat(f float64) Value { return Value{kind: Float, f: f} }

// MakeString returns a string.
func MakeString(s string) Value { return Value{kind: String, str: s} }

// MakeSymbol returns the symbol name. As in ELPS, "true" and "false" are
// booleans, and a name that starts with a colon is a keyword.
func MakeSymbol(name string) Value {
	switch name {
	case "true":
		return MakeBool(true)
	case "false":
		return MakeBool(false)
	}
	if strings.HasPrefix(name, ":") {
		return Value{kind: Keyword, str: name[1:]}
	}
	return Value{kind: Symbol, str: name}
}

// MakeKeyword returns the keyword :name. A leading colon is optional.
func MakeKeyword(name string) Value {
	return Value{kind: Keyword, str: strings.TrimPrefix(name, ":")}
}

// MakeBool returns the symbol true or false.
func MakeBool(b bool) Value {
	v := Value{kind: Bool}
	if b {
		v.i = 1
	}
	return v
}

// MakeBytes returns bytes.
func MakeBytes(b []byte) Value { return Value{kind: Bytes, bytes: b} }

// MakeList returns a list of vs; with none it returns nil.
func MakeList(vs ...Value) Value {
	if len(vs) == 0 {
		return Value{}
	}
	return Value{kind: List, items: vs}
}

// MakeVector returns a vector of vs.
func MakeVector(vs ...Value) Value {
	if vs == nil {
		vs = []Value{}
	}
	return Value{kind: Vector, items: vs}
}

// MakeTagged returns a tagged value of type typ.
func MakeTagged(typ string, data Value) Value {
	return Value{kind: Tagged, str: typ, items: []Value{data}}
}

// MakeArray returns an array of dims with cells in row-major order.
func MakeArray(dims []int, cells ...Value) Value {
	return Value{kind: Array, dims: dims, items: cells}
}

// MakeMap returns a sorted map of alternating keys and values. A key must
// be a string, a symbol, a keyword, an int or a boolean; a later value of
// the same key replaces an earlier one.
func MakeMap(kv ...Value) Value {
	n := len(kv) / 2
	keys, vals := make([]Value, n), make([]Value, n)
	for i := range n {
		keys[i], vals[i] = kv[2*i], kv[2*i+1]
	}
	return buildMap(keys, vals)
}

// StrMap returns a sorted map with string keys.
func StrMap(kv map[string]Value) Value {
	keys, vals := make([]Value, 0, len(kv)), make([]Value, 0, len(kv))
	for k, v := range kv {
		keys = append(keys, MakeString(k))
		vals = append(vals, v)
	}
	return buildMap(keys, vals)
}

// buildMap returns the map that Set of each key and value in turn builds,
// in O(n log n) where n Sets take O(n^2).
func buildMap(keys, vals []Value) Value {
	type entry struct {
		text     string
		key, val Value
	}
	seen := make(map[string]int, len(keys))
	entries := make([]entry, 0, len(keys))
	for i, k := range keys {
		if id, ok := keyID(k); ok {
			if j, dup := seen[id]; dup {
				// As Set: the later key and value replace the earlier.
				entries[j].text, _ = keyText(k)
				entries[j].key, entries[j].val = k, vals[i]
				continue
			}
			seen[id] = len(entries)
		}
		t, _ := keyText(k)
		entries = append(entries, entry{text: t, key: k, val: vals[i]})
	}
	slices.SortStableFunc(entries, func(a, b entry) int { return strings.Compare(a.text, b.text) })
	m := Value{kind: Map, keys: make([]Value, len(entries)), items: make([]Value, len(entries))}
	for i, e := range entries {
		m.keys[i], m.items[i] = e.key, e.val
	}
	return m
}

// Kind returns v's ELPS type.
func (v Value) Kind() Kind { return v.kind }

// IsNil reports whether v is the empty list.
func (v Value) IsNil() bool { return v.kind == Nil }

// Int returns v's int, and whether v is an int.
func (v Value) Int() (int64, bool) { return v.i, v.kind == Int }

// Float returns v as a float (an int converted), and whether it is a number.
func (v Value) Float() (float64, bool) {
	switch v.kind {
	case Float:
		return v.f, true
	case Int:
		return float64(v.i), true
	default:
	}
	return 0, false
}

// Str returns the text of a string, a symbol or a keyword (without its
// colon), and whether v is one of them.
func (v Value) Str() (string, bool) {
	switch v.kind {
	case String, Symbol, Keyword:
		return v.str, true
	default:
	}
	return "", false
}

// Bool returns v's boolean, and whether v is true or false.
func (v Value) Bool() (bool, bool) { return v.i == 1, v.kind == Bool }

// Bytes returns v's bytes, and whether v is bytes.
func (v Value) Bytes() ([]byte, bool) { return v.bytes, v.kind == Bytes }

// Items returns the elements of a list, a vector or an array (row-major),
// or a map's values in key order. It returns nil for anything else.
func (v Value) Items() []Value {
	switch v.kind {
	case List, Vector, Array, Map:
		return v.items
	default:
	}
	return nil
}

// Len is the number of elements of a list, vector, array or map.
func (v Value) Len() int { return len(v.Items()) }

// Index returns element i of a list, vector or array.
func (v Value) Index(i int) Value {
	if items := v.Items(); i >= 0 && i < len(items) && v.kind != Map {
		return items[i]
	}
	return Value{}
}

// Keys returns a map's keys, in order.
func (v Value) Keys() []Value {
	if v.kind != Map {
		return nil
	}
	return v.keys
}

// Get returns the value of key in a map, and whether it is there. A Go
// string key finds a string key; use a Value to find a symbol or keyword.
func (v Value) Get(key any) (Value, bool) {
	if v.kind != Map {
		return Value{}, false
	}
	k := keyOf(key)
	i, ok := v.find(k)
	if !ok {
		return Value{}, false
	}
	return v.items[i], true
}

func keyOf(key any) Value {
	switch k := key.(type) {
	case Value:
		return k
	case string:
		return MakeString(k)
	case int:
		return MakeInt(int64(k))
	case int64:
		return MakeInt(k)
	case bool:
		return MakeBool(k)
	}
	return Value{kind: List} // matches nothing: a list is not a key
}

// Set sets key to val in the map v, which must be a map (the zero Value
// becomes one). As in an ELPS map, a string and a symbol of one spelling
// are one key: setting one replaces the other.
func (v *Value) Set(key, val Value) {
	if v.kind == Nil {
		v.kind = Map
	}
	if i, ok := v.find(key); ok {
		if v.keys[i].kind == key.kind {
			v.items[i] = val
			return
		}
		v.keys = slices.Delete(v.keys, i, i+1)
		v.items = slices.Delete(v.items, i, i+1)
	}
	kt, _ := keyText(key)
	i, _ := slices.BinarySearchFunc(v.keys, kt, func(k Value, t string) int {
		s, _ := keyText(k)
		return strings.Compare(s, t)
	})
	v.keys = slices.Insert(v.keys, i, key)
	v.items = slices.Insert(v.items, i, val)
}

// find returns the position of the map's key that is the same ELPS key as
// key.
func (v Value) find(key Value) (int, bool) {
	id, ok := keyID(key)
	if !ok {
		return 0, false
	}
	for i, k := range v.keys {
		if kid, _ := keyID(k); kid == id {
			return i, true
		}
	}
	return 0, false
}

// keyID is what makes two map keys the same ELPS key.
func keyID(k Value) (string, bool) {
	switch k.kind {
	case String, Symbol:
		return "s" + k.str, true
	case Keyword:
		return "s:" + k.str, true
	case Bool:
		if k.i == 1 {
			return "strue", true
		}
		return "sfalse", true
	case Int:
		return "i" + strconv.FormatInt(k.i, 10), true
	default:
	}
	return "", false
}

// keyText is a key's member text before escaping: maps order their keys by
// it, as typed JSON writes them.
func keyText(k Value) (string, bool) {
	switch k.kind {
	case String:
		if strings.HasPrefix(k.str, "~") {
			return "~" + k.str, true
		}
		return k.str, true
	case Symbol:
		return "~$" + k.str, true
	case Keyword:
		return "~:" + k.str, true
	case Int:
		return "~i" + strconv.FormatInt(k.i, 10), true
	case Bool:
		if k.i == 1 {
			return "~?t", true
		}
		return "~?f", true
	default:
	}
	return "", false
}

// Tag returns a tagged value's type name, and whether v is tagged.
func (v Value) Tag() (string, bool) {
	return v.str, v.kind == Tagged && len(v.items) == 1
}

// TagData returns a tagged value's data (nil for anything else).
func (v Value) TagData() Value {
	if v.kind != Tagged || len(v.items) != 1 {
		return Value{}
	}
	return v.items[0]
}

// Dims returns an array's dimensions.
func (v Value) Dims() []int {
	switch v.kind {
	case Array:
		return v.dims
	case Vector:
		return []int{len(v.items)}
	default:
	}
	return nil
}

// String renders v in ELPS syntax, for messages and logs.
func (v Value) String() string {
	var b strings.Builder
	v.render(&b)
	return b.String()
}

func (v Value) render(b *strings.Builder) {
	switch v.kind {
	case Nil:
		b.WriteString("()")
	case Int:
		b.WriteString(strconv.FormatInt(v.i, 10))
	case Float:
		b.WriteString(strconv.FormatFloat(v.f, 'g', -1, 64))
	case String:
		b.WriteString(strconv.Quote(v.str))
	case Symbol:
		b.WriteString(v.str)
	case Keyword:
		b.WriteString(":" + v.str)
	case Bool:
		b.WriteString(strconv.FormatBool(v.i == 1))
	case Bytes:
		b.WriteString("#<bytes " + strconv.Itoa(len(v.bytes)) + ">")
	case List, Vector:
		if v.kind == Vector {
			b.WriteString("(vector")
		} else {
			b.WriteString("'(")
		}
		for i, it := range v.items {
			if i > 0 || v.kind == Vector {
				b.WriteByte(' ')
			}
			it.render(b)
		}
		b.WriteByte(')')
	case Map:
		b.WriteString("(sorted-map")
		for i, k := range v.keys {
			b.WriteByte(' ')
			k.render(b)
			b.WriteByte(' ')
			v.items[i].render(b)
		}
		b.WriteByte(')')
	case Tagged:
		b.WriteString("#<" + v.str + " ")
		if len(v.items) == 1 {
			v.items[0].render(b)
		}
		b.WriteByte('>')
	case Array:
		b.WriteString("#<array>")
	}
}
