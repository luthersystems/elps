// Copyright © 2026 The ELPS authors
//
// typeFields, dominantField, isValidTag and parseTag are adapted from
// encoding/json (Go 1.26), Copyright 2010 The Go Authors, under the BSD-style
// license in the Go source distribution.

package libjson

import (
	"cmp"
	"encoding/json"
	"fmt"
	"reflect"
	"slices"
	"strings"
	"sync"
	"unicode"
)

// nativeField is one struct field encoding/json writes, as nativeWalker needs
// it: where the value is, its member name as written, and the tag
// options that change whether and how it is written.
type nativeField struct {
	typ       reflect.Type
	isZero    func(reflect.Value) bool
	name      string
	nameJSON  string // `"` + HTML-escaped name + `":`, as encoding/json writes it
	index     []int
	tag       bool
	omitEmpty bool
	omitZero  bool
	quoted    bool
}

// nativeFieldCache maps a struct type to its []nativeField. Entries depend
// only on the type, so a value computed by any goroutine is valid for all.
var nativeFieldCache sync.Map

// nativeStructInfo is a nativeFieldCache entry.
type nativeStructInfo struct {
	badTag string
	fields []nativeField
}

// cachedNativeFields returns the fields encoding/json writes for struct type
// t, and why the walk refuses the type when one of its json tags is one the
// two JSON engines read differently.
func cachedNativeFields(t reflect.Type) ([]nativeField, string) {
	f, ok := nativeFieldCache.Load(t)
	if !ok {
		fields, badTag := nativeTypeFields(t)
		f, _ = nativeFieldCache.LoadOrStore(t, &nativeStructInfo{fields: fields, badTag: badTag})
	}
	info, _ := f.(*nativeStructInfo)
	return info.fields, info.badTag
}

type isZeroer interface{ IsZero() bool }

var isZeroerType = reflect.TypeFor[isZeroer]()

// nativeTypeFields returns the fields encoding/json encodes for struct type t,
// in the order it writes them. It is encoding/json's typeFields with the
// decoding indexes and encoders left out, so a field this walk visits is a
// field encoding/json writes, and no other.
//
// The second result is not empty when a json tag of t, or of a struct it
// embeds, is one the two JSON engines of Go 1.27 read differently
// (luthersystems/elps#928).  It names the first such field.
//
//   - A name the v1 engine does not accept, such as one with a quote, a
//     backslash, a control character or invalid UTF-8.  The v1 engine uses
//     the Go field name in its place.  The v2 engine uses the tag's name,
//     or a part of it.
//   - A format option.  The v1 engine ignores it and the v2 engine refuses
//     the type.
//
// The test is wider than the difference: it reads the tag as the v1 engine
// does, and does not follow the v2 engine's quoting.  A tag that only one
// reading calls wrong is refused too, which keeps the two verdicts equal.
func nativeTypeFields(t reflect.Type) ([]nativeField, string) {
	var badTag string
	current := []nativeField{}
	next := []nativeField{{typ: t}}
	var count, nextCount map[reflect.Type]int
	visited := map[reflect.Type]bool{}
	var fields []nativeField

	for len(next) > 0 {
		current, next = next, current[:0]
		count, nextCount = nextCount, map[reflect.Type]int{}

		for _, f := range current {
			if visited[f.typ] {
				continue
			}
			visited[f.typ] = true
			for i := range f.typ.NumField() {
				sf := f.typ.Field(i)
				if sf.Anonymous {
					t := sf.Type
					if t.Kind() == reflect.Pointer {
						t = t.Elem()
					}
					if !sf.IsExported() && t.Kind() != reflect.Struct {
						continue
					}
				} else if !sf.IsExported() {
					continue
				}
				tag := sf.Tag.Get("json")
				if tag == "-" {
					continue
				}
				name, opts := parseNativeTag(tag)
				if !isValidNativeTag(name) {
					if name != "" && badTag == "" {
						badTag = fmt.Sprintf("struct field %s.%s has the json tag name %q, which encoding/json does not accept", f.typ, sf.Name, name)
					}
					name = ""
				}
				if badTag == "" && nativeTagHasFormat(opts) {
					badTag = fmt.Sprintf("struct field %s.%s has a json tag with a format option", f.typ, sf.Name)
				}
				index := make([]int, len(f.index)+1)
				copy(index, f.index)
				index[len(f.index)] = i

				ft := sf.Type
				if ft.Name() == "" && ft.Kind() == reflect.Pointer {
					ft = ft.Elem()
				}
				quoted := false
				if nativeTagHas(opts, "string") {
					switch ft.Kind() {
					case reflect.Bool,
						reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64,
						reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64, reflect.Uintptr,
						reflect.Float32, reflect.Float64,
						reflect.String:
						quoted = true
					default:
					}
				}
				if name != "" || !sf.Anonymous || ft.Kind() != reflect.Struct {
					tagged := name != ""
					if name == "" {
						name = sf.Name
					}
					field := nativeField{
						name:      name,
						tag:       tagged,
						index:     index,
						typ:       ft,
						nameJSON:  string(appendJSONString(nil, name)) + ":",
						omitEmpty: nativeTagHas(opts, "omitempty"),
						omitZero:  nativeTagHas(opts, "omitzero"),
						quoted:    quoted,
					}
					if field.omitZero {
						field.isZero = nativeIsZeroFunc(sf.Type)
					}
					fields = append(fields, field)
					if count[f.typ] > 1 {
						fields = append(fields, fields[len(fields)-1])
					}
					continue
				}
				nextCount[ft]++
				if nextCount[ft] == 1 {
					next = append(next, nativeField{name: ft.Name(), index: index, typ: ft})
				}
			}
		}
	}

	slices.SortFunc(fields, func(a, b nativeField) int {
		if c := strings.Compare(a.name, b.name); c != 0 {
			return c
		}
		if c := cmp.Compare(len(a.index), len(b.index)); c != 0 {
			return c
		}
		if a.tag != b.tag {
			if a.tag {
				return -1
			}
			return +1
		}
		return slices.Compare(a.index, b.index)
	})

	out := fields[:0]
	var advance int
	for i := 0; i < len(fields); i += advance {
		fi := fields[i]
		for advance = 1; i+advance < len(fields); advance++ {
			if fields[i+advance].name != fi.name {
				break
			}
		}
		if advance == 1 {
			out = append(out, fi)
			continue
		}
		if dominant, ok := dominantNativeField(fields[i : i+advance]); ok {
			out = append(out, dominant)
		}
	}
	fields = out
	slices.SortFunc(fields, func(a, b nativeField) int {
		return slices.Compare(a.index, b.index)
	})
	return fields, badTag
}

// nativeIsZeroFunc is the IsZero test encoding/json uses for an omitzero
// field of type t, or nil when it uses reflect.Value.IsZero.
func nativeIsZeroFunc(t reflect.Type) func(reflect.Value) bool {
	switch {
	case t.Kind() == reflect.Interface && t.Implements(isZeroerType):
		return func(v reflect.Value) bool {
			if v.IsNil() || (v.Elem().Kind() == reflect.Pointer && v.Elem().IsNil()) {
				return true
			}
			z, _ := v.Interface().(isZeroer)
			return z.IsZero()
		}
	case t.Kind() == reflect.Pointer && t.Implements(isZeroerType):
		return func(v reflect.Value) bool {
			if v.IsNil() {
				return true
			}
			z, _ := v.Interface().(isZeroer)
			return z.IsZero()
		}
	case t.Implements(isZeroerType):
		return func(v reflect.Value) bool {
			z, _ := v.Interface().(isZeroer)
			return z.IsZero()
		}
	case reflect.PointerTo(t).Implements(isZeroerType):
		return func(v reflect.Value) bool {
			if !v.CanAddr() {
				v2 := reflect.New(v.Type()).Elem()
				v2.Set(v)
				v = v2
			}
			z, _ := v.Addr().Interface().(isZeroer)
			return z.IsZero()
		}
	}
	return nil
}

func dominantNativeField(fields []nativeField) (nativeField, bool) {
	if len(fields) > 1 && len(fields[0].index) == len(fields[1].index) && fields[0].tag == fields[1].tag {
		return nativeField{}, false
	}
	return fields[0], true
}

func parseNativeTag(tag string) (string, string) {
	name, opts, _ := strings.Cut(tag, ",")
	return name, opts
}

func nativeTagHas(opts, name string) bool {
	for opts != "" {
		var o string
		o, opts, _ = strings.Cut(opts, ",")
		if o == name {
			return true
		}
	}
	return false
}

// nativeTagHasFormat reports whether opts has a format option, as in
// `json:",format:RFC3339"`.
func nativeTagHasFormat(opts string) bool {
	for opts != "" {
		var o string
		o, opts, _ = strings.Cut(opts, ",")
		if strings.HasPrefix(o, "format:") {
			return true
		}
	}
	return false
}

func isValidNativeTag(s string) bool {
	if s == "" {
		return false
	}
	for _, c := range s {
		switch {
		case strings.ContainsRune("!#$%&()*+-./:;<=>?@[]^_{|}~ ", c):
		case !unicode.IsLetter(c) && !unicode.IsDigit(c):
			return false
		}
	}
	return true
}

// isNativeEmptyValue is encoding/json's isEmptyValue, the omitempty test.
func isNativeEmptyValue(v reflect.Value) bool {
	switch v.Kind() {
	case reflect.Array, reflect.Map, reflect.Slice, reflect.String:
		return v.Len() == 0
	case reflect.Bool,
		reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64,
		reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64, reflect.Uintptr,
		reflect.Float32, reflect.Float64,
		reflect.Interface, reflect.Pointer:
		return v.IsZero()
	default:
		return false
	}
}

var (
	jsonMarshalerType = reflect.TypeFor[json.Marshaler]()
	jsonNumberType    = reflect.TypeFor[json.Number]()
)
