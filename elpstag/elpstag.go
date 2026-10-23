// Copyright © 2026 The ELPS authors

// Package elpstag holds the rules that map the fields of a Go struct to the
// keys of an ELPS sorted map.  lisp.Decode and lisp.Encode use them, and so
// can code that cannot link the interpreter, such as a WASM guest, so both
// sides of a boundary match struct fields the same way.
//
// The rules:
//
//   - An exported field maps to the key named in its elps tag
//     (`elps:"name"`), or to its Go name when the tag names none.  A field
//     tagged `elps:"-"` and an unexported field map to no key.
//   - An embedded field is a field like any other, named by its type.  Its
//     fields are not promoted.
//   - The tag option omitempty (`elps:"name,omitempty"`) leaves a field with
//     its zero value out when a struct is encoded.  Other options are
//     ignored.
//   - A field whose name equals an earlier field's, ignoring case, maps to
//     no key.
//   - A map key matches a field when, without the leading colon of a
//     keyword, it equals the field's name ignoring case.  When several keys
//     of a map match one field, the key spelled exactly as the name wins,
//     and then the first in the map's sorted key order.
//
// The package imports only reflect and strings.
package elpstag

import (
	"reflect"
	"strings"
)

// TagKey is the struct tag key the package reads.
const TagKey = "elps"

// Field is a struct field that maps to a map key.
type Field struct {
	// Name is the map key.
	Name string
	// Index is the field's index in the struct, for reflect.Value.Field.
	Index int
	// OmitEmpty is true when the tag has the omitempty option.
	OmitEmpty bool
}

// Parse returns the Field f maps to, and whether f maps to a key at all.
// The Field's Index is the last element of f.Index, which is f's index in
// its struct.  Parse does not apply the rule on duplicate names, which needs
// the whole struct: use Fields for that.
func Parse(f reflect.StructField) (Field, bool) {
	if !f.IsExported() {
		return Field{}, false
	}
	tag := f.Tag.Get(TagKey)
	if tag == "-" {
		return Field{}, false
	}
	name, opts, _ := strings.Cut(tag, ",")
	out := Field{Name: name}
	if len(f.Index) > 0 {
		out.Index = f.Index[len(f.Index)-1]
	}
	for opts != "" {
		var opt string
		opt, opts, _ = strings.Cut(opts, ",")
		if opt == "omitempty" {
			out.OmitEmpty = true
		}
	}
	if out.Name == "" {
		out.Name = f.Name
	}
	return out, true
}

// Fields returns the fields of the struct type t that map to keys, in
// declaration order.  It panics when t is not a struct type.
func Fields(t reflect.Type) []Field {
	n := t.NumField()
	out := make([]Field, 0, n)
	for i := range n {
		f, ok := Parse(t.Field(i))
		if !ok || duplicate(out, f.Name) {
			continue
		}
		out = append(out, f)
	}
	return out
}

func duplicate(fields []Field, name string) bool {
	for _, f := range fields {
		if strings.EqualFold(f.Name, name) {
			return true
		}
	}
	return false
}

// Match returns whether the map key key matches the field name name, and
// then whether it is spelled exactly as name.  key is the text of a string,
// symbol or keyword key; a keyword's leading colon is removed first.
func Match(key, name string) (bool, bool) {
	key = strings.TrimPrefix(key, ":")
	if key == name {
		return true, true
	}
	return strings.EqualFold(key, name), false
}

// Find returns the index in fields of the field key matches, and whether
// the match is exact (see Match).  It returns -1 when key matches no field.
// A decoder that reads a map's keys in sorted order applies the rule on
// several matching keys by keeping a field's first match, unless a later
// one is exact and the first was not.
func Find(fields []Field, key string) (int, bool) {
	for i, f := range fields {
		if m, e := Match(key, f.Name); m {
			return i, e
		}
	}
	return -1, false
}
