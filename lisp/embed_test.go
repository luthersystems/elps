package lisp

import (
	"reflect"
	"testing"
)

func TestVectorGoValue(t *testing.T) {
	tests := []struct {
		lval *LVal
		gval interface{}
	}{
		{
			Array(QExpr([]*LVal{Int(0)}), []*LVal{}),
			[]interface{}{},
		},
		{
			Array(nil, []*LVal{}),
			[]interface{}{},
		},
		{
			Array(QExpr([]*LVal{Int(1)}), []*LVal{Int(1)}),
			[]interface{}{int(1)},
		},
		{
			Array(nil, []*LVal{Int(1)}),
			[]interface{}{int(1)},
		},
	}
	for i, test := range tests {
		gval := GoValue(test.lval)
		if !reflect.DeepEqual(gval, test.gval) {
			t.Errorf("test %d:  lisp value %v with unexpected go value %#v (expected %#v)", i, test.lval, gval, test.gval)
		}
	}
}
