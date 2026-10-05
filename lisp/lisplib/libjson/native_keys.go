// Copyright © 2026 The ELPS authors

package libjson

import (
	"cmp"
	"reflect"
)

// compareNativeKeys orders comparable Go keys without calling user methods.
// It orders TextMarshaler calls before their JSON names are available.
// Pointers compare by address, so their order is stable for the same input.
func compareNativeKeys(a, b reflect.Value) int {
	if a.Type() != b.Type() {
		if c := cmp.Compare(a.Type().String(), b.Type().String()); c != 0 {
			return c
		}
		return cmp.Compare(reflect.ValueOf(a.Type()).Pointer(), reflect.ValueOf(b.Type()).Pointer())
	}
	switch a.Kind() {
	case reflect.String:
		return cmp.Compare(a.String(), b.String())
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
		return cmp.Compare(a.Int(), b.Int())
	case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64, reflect.Uintptr:
		return cmp.Compare(a.Uint(), b.Uint())
	case reflect.Float32, reflect.Float64:
		return cmp.Compare(a.Float(), b.Float())
	case reflect.Complex64, reflect.Complex128:
		if c := cmp.Compare(real(a.Complex()), real(b.Complex())); c != 0 {
			return c
		}
		return cmp.Compare(imag(a.Complex()), imag(b.Complex()))
	case reflect.Bool:
		if a.Bool() == b.Bool() {
			return 0
		}
		if a.Bool() {
			return 1
		}
		return -1
	case reflect.Pointer, reflect.Chan, reflect.UnsafePointer:
		return cmp.Compare(a.Pointer(), b.Pointer())
	case reflect.Struct:
		for i := range a.NumField() {
			if c := compareNativeKeys(a.Field(i), b.Field(i)); c != 0 {
				return c
			}
		}
		return 0
	case reflect.Array:
		for i := range a.Len() {
			if c := compareNativeKeys(a.Index(i), b.Index(i)); c != 0 {
				return c
			}
		}
		return 0
	case reflect.Interface:
		if a.IsNil() {
			if b.IsNil() {
				return 0
			}
			return -1
		}
		if b.IsNil() {
			return 1
		}
		return compareNativeKeys(a.Elem(), b.Elem())
	default:
		panic("noncomparable native map key")
	}
}
