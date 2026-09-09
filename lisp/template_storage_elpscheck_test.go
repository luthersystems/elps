// Copyright © 2026 The ELPS authors

//go:build elpscheck

package lisp

import "testing"

// Issue #639: storage relies on admission sorting mutable spans. A caller
// that skips that step must fail at the invariant, not during materialization.
func TestTemplateStorageRejectsUnsortedSpans(t *testing.T) {
	backing := []*LVal{Int(1), Int(2)}
	low := QExpr(backing[:1:1])
	high := QExpr(backing[1:2:2])
	inventory := &templateInventory{cells: []templateCellSpan{
		newTemplateCellSpan(high), newTemplateCellSpan(low),
	}}
	defer func() {
		if got := recover(); got != "template: storage requires sorted mutable cell spans" {
			t.Fatalf("expected storage ordering assertion, got %v", got)
		}
	}()
	inventory.storage()
}

func TestTemplateStorageAcceptsSortedSpans(t *testing.T) {
	backing := []*LVal{Int(1), Int(2)}
	whole := QExpr(backing)
	prefix := QExpr(backing[:1:1])
	suffix := QExpr(backing[1:2:2])
	for _, tc := range []struct {
		name   string
		values []*LVal
	}{
		{"empty", nil},
		{"single", []*LVal{whole}},
		{"adjacent", []*LVal{prefix, suffix}},
		{"equal_start_long_first", []*LVal{whole, prefix}},
		{"equal_start_short_first", []*LVal{prefix, whole}},
	} {
		t.Run(tc.name, func(t *testing.T) {
			inventory := &templateInventory{}
			for _, value := range tc.values {
				inventory.cells = append(inventory.cells, newTemplateCellSpan(value))
			}
			storage := inventory.storage()
			if len(storage.cellViews) != len(tc.values) {
				t.Fatalf("got %d views, want %d", len(storage.cellViews), len(tc.values))
			}
			for _, value := range tc.values {
				view, ok := storage.cellViews[value]
				if !ok || view.length != len(value.Cells) || view.capacity != cap(value.Cells) {
					t.Fatalf("incorrect view for source: %+v (present=%t)", view, ok)
				}
				for i, want := range value.Cells {
					if got := storage.cells[view.storage][view.offset+i]; got != want {
						t.Fatalf("slot %d: got %p, want source value %p", i, got, want)
					}
				}
			}
		})
	}
}
