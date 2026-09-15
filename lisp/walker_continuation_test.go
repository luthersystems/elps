// Copyright © 2026 The ELPS authors

package lisp

import (
	"errors"
	"reflect"
	"testing"
)

func TestGoConversionContinuationSiblings(t *testing.T) {
	shared := SortedMap()
	shared.MapSet("x", Vector([]*LVal{Int(7), String("value")}))
	v := QExpr([]*LVal{shared, shared, Vector(nil)})
	wantMap := map[interface{}]interface{}{"x": []interface{}{7, "value"}}
	want := []interface{}{wantMap, wantMap, []interface{}{}}
	// Force stack growth and cycle tracking before the shared siblings.
	for range 80 {
		v = &LVal{Type: LQuote, Cells: []*LVal{v}}
	}
	if got := GoValue(v); !reflect.DeepEqual(got, want) {
		t.Fatalf("converted siblings = %#v, want %#v", got, want)
	}
	shared.MapSet("cycle", shared)
	if got := GoValue(v); got != v {
		t.Fatal("cycle must return the original root")
	}
}

// Scalar map values skip frames, but still count toward the depth cap and
// preserve the typed error an embedder can inspect with errors.As.
func TestCopyScalarMapDepthBoundary(t *testing.T) {
	v := SortedMap()
	v.MapSet("leaf", Int(7))
	for range 1023 {
		v = QExpr([]*LVal{v})
	}
	cp, failure := v.copyWithRuntime(&Runtime{MaxValueDepth: 1024})
	var depthErr ValueDepthError
	if cp != failure || !errors.As(GoError(cp), &depthErr) || depthErr != 1024 {
		t.Fatalf("expected typed depth error, got %v", cp)
	}
	cp, failure = v.copyWithRuntime(&Runtime{MaxValueDepth: 1025})
	if failure != nil {
		t.Fatalf("valid boundary copy failed: %v", failure)
	}
	for range 1023 {
		cp = cp.Cells[0]
	}
	if cp.MapGet("leaf").Int != 7 {
		t.Fatal("copy lost leaf at valid depth")
	}
}
