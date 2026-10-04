// Copyright © 2026 The ELPS authors
//
// typeFields, dominantField, isValidTag and parseTag are adapted from
// encoding/json (Go 1.26), Copyright 2010 The Go Authors, under the BSD-style
// license in the Go source distribution.

package libjson

import (
	"cmp"
	"encoding/json"
	"reflect"
	"slices"
	"strings"
	"sync"
	"unicode"
)

// nativeField is one struct field encoding/json writes, as nativeWalker needs
// it: where the value is, how many bytes its member name costs, and the tag
// options that change whether and how it is written.
type nativeField struct {
	typ       reflect.Type
	isZero    func(reflect.Value) bool
	name      string
	index     []int
	nameLen   int // len(`"` + HTML-escaped name + `":`)
	tag       bool
	omitEmpty bool
	omitZero  bool
	quoted    bool
}

// nativeFieldCache maps a struct type to its []nativeField. Entries depend
// only on the type, so a value computed by any goroutine is valid for all.
var nativeFieldCache sync.Map

func cachedNativeFields(t reflect.Type) []nativeField {
	if f, ok := nativeFieldCache.Load(t); ok {
		fields, _ := f.([]nativeField)
		return fields
	}
	f, _ := nativeFieldCache.LoadOrStore(t, nativeTypeFields(t))
	fields, _ := f.([]nativeField)
	return fields
}

type isZeroer interface{ IsZero() bool }

var isZeroerType = reflect.TypeFor[isZeroer]()

// nativeTypeFields returns the fields encoding/json encodes for struct type t,
// in the order it writes them. It is encoding/json's typeFields with the
// decoding indexes and encoders left out, so a field this walk visits is a
// field encoding/json writes, and no other.
func nativeTypeFields(t reflect.Type) []nativeField {
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
					name = ""
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
						nameLen:   len(appendJSONString(nil, name)) + 1,
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
	return fields
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
