package libjson_test

import (
	"reflect"
	"testing"

	"github.com/luthersystems/elps/internal/jsonraw"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
)

// #627: the deprecated conversion returns ordinary Go values, not the private
// decoder storage whose values must be *LVal.
func TestGoMapReturnsOrdinaryGoValues(t *testing.T) {
	serializer := libjson.DefaultSerializer()
	nested := jsonraw.Wrap(map[string]any{"text": lisp.String("value")})
	source := jsonraw.Wrap(map[string]any{"number": lisp.Int(7), "nested": nested})
	for _, stringNumbers := range []bool{false, true} {
		var number any = 7
		if stringNumbers {
			number = "7"
		}
		want := map[string]any{"number": number, "nested": map[string]any{"text": "value"}}
		got, ok := serializer.GoMap(source, stringNumbers)
		if !ok || !reflect.DeepEqual(got, want) {
			t.Fatalf("stringNumbers=%t: got %#v, %t; want %#v, true", stringNumbers, got, ok, want)
		}
		if generic := serializer.GoValue(source, stringNumbers); !reflect.DeepEqual(generic, want) {
			t.Fatalf("generic conversion changed: got %#v, want %#v", generic, want)
		}
		got["number"] = "changed"
		got["nested"].(map[string]any)["text"] = "changed"
		if source.MapGetString("number").Int != 7 || nested.MapGetString("text").Str != "value" {
			t.Fatal("Go map writes changed Lisp source values")
		}
	}
	if got, ok := serializer.GoMap(lisp.Int(7), false); got != nil || ok {
		t.Fatalf("non-map converted: got %#v, %t", got, ok)
	}
}

func TestGoMapRejectsBooleanSymbolKey(t *testing.T) {
	source := lisp.SortedMap()
	if err := source.Map().Set(lisp.Symbol("true"), lisp.Int(7)); err.Type == lisp.LError {
		t.Fatal(err)
	}
	for _, stringNumbers := range []bool{false, true} {
		got, ok := libjson.DefaultSerializer().GoMap(source, stringNumbers)
		if got != nil || ok {
			t.Errorf("stringNumbers=%t: got %#v, %t; want nil, false", stringNumbers, got, ok)
		}
	}
}

func TestGoValuePreservesInvalidMapConversion(t *testing.T) {
	source := lisp.SortedMap()
	if err := source.Map().Set(lisp.Symbol("true"), lisp.Int(7)); err.Type == lisp.LError {
		t.Fatal(err)
	}
	serializer := libjson.DefaultSerializer()
	for _, stringNumbers := range []bool{false, true} {
		// GoValue historically ignores GoMap's validity flag and returns
		// its typed nil map, including inside another container.
		got := serializer.GoValue(source, stringNumbers)
		m, ok := got.(map[string]any)
		if !ok || m != nil {
			t.Fatalf("GoValue: got %#v (%T); want typed nil map", got, got)
		}
		outer := lisp.SortedMap()
		if err := outer.Map().Set(lisp.String("nested"), source); err.Type == lisp.LError {
			t.Fatal(err)
		}
		want := map[string]any{"nested": map[string]any(nil)}
		if got := serializer.GoValue(outer, stringNumbers); !reflect.DeepEqual(got, want) {
			t.Fatalf("nested GoValue: got %#v; want %#v", got, want)
		}
		if got, ok := serializer.GoMap(outer, stringNumbers); !ok || !reflect.DeepEqual(got, want) {
			t.Fatalf("nested GoMap: got %#v, %t; want %#v, true", got, ok, want)
		}
	}
}
