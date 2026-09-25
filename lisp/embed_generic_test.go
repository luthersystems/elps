// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"fmt"
	"reflect"
	"testing"

	"github.com/luthersystems/elps/internal/fuzzval"
	"github.com/luthersystems/elps/lisp"
)

func TestGoSliceOf(t *testing.T) {
	strs, ok := lisp.GoSliceOf[string](lisp.QExpr([]*lisp.LVal{lisp.String("a"), lisp.Symbol("b")}))
	if !ok || !reflect.DeepEqual(strs, []string{"a", "b"}) {
		t.Fatalf("GoSliceOf[string] = %v, %v", strs, ok)
	}
	ints, ok := lisp.GoSliceOf[int](lisp.QExpr([]*lisp.LVal{lisp.Int(1), lisp.Int(2)}))
	if !ok || !reflect.DeepEqual(ints, []int{1, 2}) {
		t.Fatalf("GoSliceOf[int] = %v, %v", ints, ok)
	}
	floats, ok := lisp.GoSliceOf[float64](lisp.QExpr([]*lisp.LVal{lisp.Float(1.5)}))
	if !ok || !reflect.DeepEqual(floats, []float64{1.5}) {
		t.Fatalf("GoSliceOf[float64] = %v, %v", floats, ok)
	}
	nested, ok := lisp.GoSliceOf[[]any](lisp.QExpr([]*lisp.LVal{lisp.QExpr([]*lisp.LVal{lisp.Int(1)})}))
	if !ok || !reflect.DeepEqual(nested, [][]any{{1}}) {
		t.Fatalf("GoSliceOf[[]any] = %v, %v", nested, ok)
	}
	empty, ok := lisp.GoSliceOf[string](lisp.Nil())
	if !ok || empty == nil || len(empty) != 0 {
		t.Fatalf("GoSliceOf of nil = %#v, %v; want an empty non-nil slice", empty, ok)
	}
	for name, v := range map[string]*lisp.LVal{
		"mismatch":  lisp.QExpr([]*lisp.LVal{lisp.String("a"), lisp.Int(1)}),
		"nil elem":  lisp.QExpr([]*lisp.LVal{lisp.Nil()}),
		"not list":  lisp.String("a"),
		"float/int": lisp.QExpr([]*lisp.LVal{lisp.Float(1)}),
	} {
		if got, ok := lisp.GoSliceOf[string](v); ok || got != nil {
			if name == "float/int" {
				continue
			}
			t.Errorf("%s: GoSliceOf[string] = %v, %v; want nil, false", name, got, ok)
		}
	}
	if got, ok := lisp.GoSliceOf[int](lisp.QExpr([]*lisp.LVal{lisp.Float(1)})); ok {
		t.Errorf("GoSliceOf[int] of a float = %v; want false", got)
	}
	anys, ok := lisp.GoSliceOf[any](lisp.QExpr([]*lisp.LVal{lisp.Nil(), lisp.Int(1)}))
	if !ok || !reflect.DeepEqual(anys, []any{nil, 1}) {
		t.Fatalf("GoSliceOf[any] = %v, %v", anys, ok)
	}
}

func TestGoSliceOfCycle(t *testing.T) {
	l := lisp.QExpr([]*lisp.LVal{lisp.Int(1)})
	l.Cells = append(l.Cells, l)
	if got, ok := lisp.GoSliceOf[any](l); ok {
		t.Fatalf("GoSliceOf of a cycle = %v, true; want false", got)
	}
}

func TestGoMapOf(t *testing.T) {
	m := lisp.SortedMap()
	m.MapSetString("a", lisp.Int(1))
	m.MapSetString("b", lisp.Int(2))
	got, ok := lisp.GoMapOf[string, int](m)
	if !ok || !reflect.DeepEqual(got, map[string]int{"a": 1, "b": 2}) {
		t.Fatalf("GoMapOf[string,int] = %v, %v", got, ok)
	}
	if got, ok := lisp.GoMapOf[string, string](m); ok || got != nil {
		t.Fatalf("GoMapOf value mismatch = %v, %v; want nil, false", got, ok)
	}
	if got, ok := lisp.GoMapOf[int, int](m); ok || got != nil {
		t.Fatalf("GoMapOf key mismatch = %v, %v; want nil, false", got, ok)
	}
	if got, ok := lisp.GoMapOf[string, int](lisp.Int(1)); ok || got != nil {
		t.Fatalf("GoMapOf of a non-map = %v, %v; want nil, false", got, ok)
	}
	c := lisp.SortedMap()
	c.MapSetString("self", c)
	if got, ok := lisp.GoMapOf[string, any](c); ok {
		t.Fatalf("GoMapOf of a cycle = %v, true; want false", got)
	}
	d := lisp.SortedMap()
	if got, ok := lisp.GoMapOf[string, int](d); !ok || len(got) != 0 {
		t.Fatalf("GoMapOf of an empty map = %v, %v", got, ok)
	}
}

// Values are compared by their %#v rendering rather than reflect.DeepEqual,
// because a NaN element is not DeepEqual to itself.
//
// FuzzGoSliceMapOf is the differential the issue asks for: with the widest
// type parameters, GoSliceOf/GoMapOf agree with GoSlice/GoMap exactly, and a
// narrower parameter succeeds only when every element has that type.
func FuzzGoSliceMapOf(f *testing.F) {
	for _, s := range fuzzval.Seeds() {
		f.Add(s)
	}
	f.Fuzz(func(t *testing.T, data []byte) {
		v := fuzzval.New(data, nil).Value()
		if v == nil {
			return
		}
		ws, wok := lisp.GoSlice(v)
		gs, gok := lisp.GoSliceOf[any](v)
		if wok != gok || (wok && fmt.Sprintf("%#v", ws) != fmt.Sprintf("%#v", gs)) {
			t.Fatalf("GoSliceOf[any] = %v, %v; GoSlice = %v, %v", gs, gok, ws, wok)
		}
		ss, sok := lisp.GoSliceOf[string](v)
		if sok {
			if !wok || len(ss) != len(ws) {
				t.Fatalf("GoSliceOf[string] ok where GoSlice = %v, %v", ws, wok)
			}
			for i := range ws {
				if ws[i].(string) != ss[i] {
					t.Fatalf("element %d: %v vs %v", i, ws[i], ss[i])
				}
			}
		} else if wok {
			allStrings := true
			for _, x := range ws {
				if _, ok := x.(string); !ok {
					allStrings = false
				}
			}
			if allStrings {
				t.Fatalf("GoSliceOf[string] refused an all-string slice %v", ws)
			}
		}
		wm, wmok := lisp.GoMap(v)
		gm, gmok := lisp.GoMapOf[any, any](v)
		if wmok != gmok || (wmok && wm != nil && fmt.Sprintf("%#v", wm) != fmt.Sprintf("%#v", gm)) {
			t.Fatalf("GoMapOf[any,any] = %v, %v; GoMap = %v, %v", gm, gmok, wm, wmok)
		}
	})
}
