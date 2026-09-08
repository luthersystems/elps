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
		if source.MapGet("number").Int != 7 || nested.MapGet("text").Str != "value" {
			t.Fatal("Go map writes changed Lisp source values")
		}
	}
	if got, ok := serializer.GoMap(lisp.Int(7), false); got != nil || ok {
		t.Fatalf("non-map converted: got %#v, %t", got, ok)
	}
}
