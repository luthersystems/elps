// Copyright © 2026 The ELPS authors

package elpstest

import (
	"reflect"

	"github.com/luthersystems/elps/lisp"
)

// MapData is a wrapper, not storage identity. Inspect only the two finite
// interpreter-owned implementations admitted by Template, independently of
// its compiler/inventory. Reflection reads map addresses, never dereferences
// unsafe pointers or mutates backing fields. Unknown host Map implementations
// still need behavioral mutation probes; Template does not admit them.
// This is non-_test.go because the exported RunForkCheck test-support API uses
// it in importing packages. Keep this independent of the template compiler's
// own storage inventory; TestForkOracleStockMapLayouts pins the private layout.
func oracleMapBackingIDs(data *lisp.MapData) []nativePayloadIdentity {
	if data == nil {
		return nil
	}
	field := reflect.ValueOf(data).Elem().FieldByName("mapBacking")
	if field.Kind() != reflect.Interface {
		panic("fork oracle: MapData layout changed; update its independent storage census")
	}
	backing := field.Elem()
	if !backing.IsValid() {
		return nil
	}
	if backing.Kind() == reflect.Map {
		if backing.IsNil() {
			return nil
		}
		return []nativePayloadIdentity{{reflect.Map, backing.Pointer()}}
	}
	if backing.Kind() != reflect.Struct || backing.Type().PkgPath() != "github.com/luthersystems/elps/lisp" || backing.Type().Name() != "sortedmap" {
		return nil
	}
	var ids []nativePayloadIdentity
	for _, name := range []string{"m", "tm"} {
		field := backing.FieldByName(name)
		if field.Kind() != reflect.Map {
			panic("fork oracle: stock map layout changed")
		}
		if !field.IsNil() {
			ids = append(ids, nativePayloadIdentity{reflect.Map, field.Pointer()})
		}
	}
	// lz, a lazily instantiated map's link to its VM's lazy instance, is
	// not program-visible storage: it is allocated per VM beside m, is nil
	// in a source map and becomes inert once no entry is pending, so it is
	// not an identity the oracle compares with the source.
	if backing.FieldByName("lz").Kind() != reflect.Pointer {
		panic("fork oracle: stock map layout changed")
	}
	return ids
}
