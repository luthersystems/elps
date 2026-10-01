package lisp

import (
	"fmt"
	"testing"
)

func TestMapKeyDefaultTypes(t *testing.T) {
	types := []LType{
		LInvalid, LInt, LFloat, LError, LSymbol, LSExpr, LFun, LQuote,
		LString, LBytes, LSortMap, LArray, LNative, LTaggedVal,
		LMarkTerminal, LMarkTailRec, LMarkMacExpand,
		LTypeMax, LTypeMax + 1, ^LType(0),
	}
	for _, typ := range types {
		t.Run(fmt.Sprintf("%d", typ), func(t *testing.T) {
			m := SortedMap().Map()
			key := &LVal{Type: typ, Str: "key", Int: 7}
			val := Int(42)
			admitted := typ == LString || typ == LSymbol || typ == LInt
			set := m.Set(key, val)
			got, ok := m.Get(key)
			del := m.Del(key)
			if admitted {
				if !set.IsNil() || !ok || got != val || !del.IsNil() || m.Len() != 0 {
					t.Fatal("map rejected an admitted key")
				}
				return
			}
			want := fmt.Sprintf("unhashable type: %s", typ)
			for _, result := range []*LVal{set, got, del} {
				if result.Type != LError || (*ErrorVal)(result).ErrorMessage() != want {
					t.Fatalf("map result = %v, want error %q", result, want)
				}
			}
			if ok || m.Len() != 0 {
				t.Fatal("map accepted an unsupported key")
			}
		})
	}
}
