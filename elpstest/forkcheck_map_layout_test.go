// Copyright © 2026 The ELPS authors

package elpstest

import (
	"reflect"
	"slices"
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// This test intentionally pins private interpreter layout. The oracle used by
// downstream RunForkCheck callers must not silently overlook newly owned map
// storage, or start panicking only when a downstream test exercises a map.
func TestForkOracleStockMapLayouts(t *testing.T) {
	wrapper := reflect.TypeFor[lisp.MapData]()
	field, ok := wrapper.FieldByName("mapBacking")
	if !ok || wrapper.NumField() != 1 || field.Type != reflect.TypeFor[lisp.Map]() {
		t.Fatal("MapData layout changed: update the independent oracle map census")
	}
	stock := lisp.SortedMap().Map()
	backing := reflect.ValueOf(stock).Elem().FieldByIndex(field.Index).Elem()
	if backing.Kind() != reflect.Struct || backing.Type().Name() != "sortedmap" || backing.Type().PkgPath() != "github.com/luthersystems/elps/lisp" || backing.NumField() != 3 {
		t.Fatalf("stock map layout changed: update the independent oracle map census: %s", backing.Type())
	}
	if m, ok := backing.Type().FieldByName("m"); !ok || m.Type.Kind() != reflect.Map || m.Type.Key().Kind() != reflect.String {
		t.Fatal("stock map field m changed: update the independent oracle map census")
	}
	kt, ok := backing.Type().FieldByName("kt")
	if !ok || kt.Type.Kind() != reflect.Pointer || kt.Type.Elem().Kind() != reflect.Struct || kt.Type.Elem().NumField() != 2 {
		t.Fatal("stock map field kt changed: update the independent oracle map census")
	}
	for name, key := range map[string]reflect.Kind{"types": reflect.String, "ints": reflect.Int} {
		field, ok := kt.Type.Elem().FieldByName(name)
		if !ok || field.Type.Kind() != reflect.Map || field.Type.Key().Kind() != key {
			t.Fatalf("stock map side table %q changed: update the independent oracle map census", name)
		}
	}
	if lz, ok := backing.Type().FieldByName("lz"); !ok || lz.Type.Kind() != reflect.Pointer {
		t.Fatal("stock map field lz changed: update the independent oracle map census")
	}
	for _, tc := range []struct {
		name string
		make func() *lisp.MapData
		want int
	}{
		{"stock", func() *lisp.MapData { return lisp.SortedMap().Map() }, 2},
		{"stock-symbol-keys", func() *lisp.MapData {
			m := lisp.SortedMap().Map()
			m.Set(lisp.Symbol("a"), lisp.Int(1))
			return m
		}, 3},
		{"stock-int-keys", func() *lisp.MapData {
			m := lisp.SortedMap().Map()
			m.Set(lisp.Int(1), lisp.Int(1))
			return m
		}, 3},
		{"json", func() *lisp.MapData { return oracleJSONValue(make(map[string]any)).Map() }, 1},
	} {
		t.Run(tc.name, func(t *testing.T) {
			first, independent := tc.make(), tc.make()
			wrapped := *first
			ids := oracleMapBackingIDs(first)
			if len(ids) != tc.want || !slices.Equal(ids, oracleMapBackingIDs(&wrapped)) {
				t.Fatalf("copied wrapper lost %d physical backing identities: %v", tc.want, ids)
			}
			for _, id := range oracleMapBackingIDs(independent) {
				if slices.Contains(ids, id) {
					t.Fatal("independent maps reported shared backing")
				}
			}
		})
	}
	if len(oracleMapBackingIDs(nil)) != 0 || len(oracleMapBackingIDs(new(lisp.MapData))) != 0 {
		t.Fatal("absent backing reported physical storage")
	}
}
