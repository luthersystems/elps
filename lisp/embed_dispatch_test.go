package lisp

import (
	"fmt"
	"reflect"
	"testing"
)

func TestConversionDefaultTypes(t *testing.T) {
	for _, tc := range []struct {
		v    *LVal
		want any
	}{
		{Int(7), 7},
		{Float(1.5), 1.5},
		{Symbol("name"), "name"},
		{String("text"), "text"},
		{Bytes([]byte("x")), []byte("x")},
		{Native("native"), "native"},
	} {
		if got, ok := convertValue(tc.v, MaxValueDepth); !ok || !reflect.DeepEqual(got, tc.want) {
			t.Fatalf("convertValue(type=%d) = %#v, %v, want %#v, true", tc.v.Type, got, ok, tc.want)
		}
		if got := GoValue(QExpr([]*LVal{tc.v})); !reflect.DeepEqual(got, []any{tc.want}) {
			t.Fatalf("container conversion(type=%d) = %#v", tc.v.Type, got)
		}
	}
	err := Errorf("failure")
	if got, ok := convertValue(err, MaxValueDepth); !ok || got != (*ErrorVal)(err) {
		t.Fatal("error conversion changed the error identity")
	}
	if got := GoValue(QExpr([]*LVal{err})); !reflect.DeepEqual(got, []any{(*ErrorVal)(err)}) {
		t.Fatal("container conversion changed the error identity")
	}
	for _, typ := range []LType{
		LInvalid, LFun, LTaggedVal, LMarkTerminal, LMarkTailRec,
		LMarkMacExpand, LTypeMax, LTypeMax + 1, ^LType(0),
	} {
		t.Run(fmt.Sprintf("%d", typ), func(t *testing.T) {
			v := &LVal{Type: typ, Cells: []*LVal{Int(7)}}
			if got := conversionLeaf(v); got != v {
				t.Fatal("conversionLeaf changed the value identity")
			}
			if got, ok := convertValue(v, MaxValueDepth); !ok || got != v {
				t.Fatal("convertValue changed the value identity")
			}
			if got := GoValue(QExpr([]*LVal{v})); !reflect.DeepEqual(got, []any{v}) {
				t.Fatal("container conversion changed the value identity")
			}
		})
	}
	for _, typ := range []LType{LQuote, LSExpr, LArray, LSortMap} {
		v := &LVal{Type: typ}
		if got := conversionLeaf(v); got != v {
			t.Fatalf("conversionLeaf changed container type %d", typ)
		}
	}
}
