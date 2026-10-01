package libjson

import (
	"fmt"
	"reflect"
	"testing"

	"github.com/luthersystems/elps/lisp"
)

func TestSerializerConversionDefaultTypes(t *testing.T) {
	s := DefaultSerializer()
	for _, tc := range []struct {
		v          *lisp.LVal
		plain      any
		stringNums any
	}{
		{lisp.Int(7), 7, "7"},
		{lisp.Float(1.5), 1.5, "1.5"},
		{lisp.Symbol("name"), "name", "name"},
		{lisp.Symbol(lisp.TrueSymbol), true, true},
		{lisp.Symbol(lisp.FalseSymbol), false, false},
		{s.Null, nil, nil},
		{lisp.String("text"), "text", "text"},
		{lisp.Bytes([]byte("x")), []byte("x"), []byte("x")},
		{lisp.Native("native"), "native", "native"},
	} {
		for _, stringNums := range []bool{false, true} {
			want := tc.plain
			if stringNums {
				want = tc.stringNums
			}
			if got, ok := s.convertValue(tc.v, stringNums); !ok || !reflect.DeepEqual(got, want) {
				t.Fatalf("convertValue(type=%d, stringNums=%v) = %#v, %v, want %#v, true", tc.v.Type, stringNums, got, ok, want)
			}
			if got := s.GoValue(lisp.QExpr([]*lisp.LVal{tc.v}), stringNums); !reflect.DeepEqual(got, []any{want}) {
				t.Fatalf("container conversion(type=%d, stringNums=%v) = %#v", tc.v.Type, stringNums, got)
			}
		}
	}
	err := lisp.Errorf("failure")
	if got, ok := s.convertValue(err, false); !ok || got != (*lisp.ErrorVal)(err) {
		t.Fatal("error conversion changed the error identity")
	}
	if got := s.GoValue(lisp.QExpr([]*lisp.LVal{err}), false); !reflect.DeepEqual(got, []any{(*lisp.ErrorVal)(err)}) {
		t.Fatal("container conversion changed the error identity")
	}
	for _, typ := range []lisp.LType{
		lisp.LInvalid, lisp.LFun, lisp.LTaggedVal,
		lisp.LMarkTerminal, lisp.LMarkTailRec, lisp.LMarkMacExpand,
		lisp.LTypeMax, lisp.LTypeMax + 1, ^lisp.LType(0),
	} {
		t.Run(fmt.Sprintf("%d", typ), func(t *testing.T) {
			v := &lisp.LVal{Type: typ, Cells: []*lisp.LVal{lisp.Int(7)}}
			for _, stringNums := range []bool{false, true} {
				if got := s.conversionLeaf(v, stringNums); got != v {
					t.Fatal("conversionLeaf changed the value identity")
				}
				if got, ok := s.convertValue(v, stringNums); !ok || got != v {
					t.Fatal("convertValue changed the value identity")
				}
				if got := s.GoValue(lisp.QExpr([]*lisp.LVal{v}), stringNums); !reflect.DeepEqual(got, []any{v}) {
					t.Fatal("container conversion changed the value identity")
				}
			}
		})
	}
	for _, typ := range []lisp.LType{lisp.LQuote, lisp.LSExpr, lisp.LArray, lisp.LSortMap} {
		v := &lisp.LVal{Type: typ}
		if got := s.conversionLeaf(v, false); got != v {
			t.Fatalf("conversionLeaf changed container type %d", typ)
		}
	}
}
