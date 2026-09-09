// Copyright © 2026 The ELPS authors

package lisp

import "testing"

// Issue #639: overlap admission may change its index, but not its half-open
// interval semantics. These synthetic numeric spans are never dereferenced;
// the model deliberately uses no sorting, binary search or prefix maxima.
func TestTemplateOverlapIndexMatchesPairwiseModel(t *testing.T) {
	var intervals []templateCellSpan
	for start := uintptr(0); start < 4; start++ {
		for end := start + 1; end <= 4; end++ {
			intervals = append(intervals, templateCellSpan{start: start, end: end})
		}
	}
	// Ten nonempty intervals produce 1 + 10 + 10*10 ordered collections.
	// Ordered pairs include reversed inputs, duplicates, nested spans and
	// adjacency. Empty collections cover both sides of the early return.
	collections := [][]templateCellSpan{nil}
	for _, first := range intervals {
		collections = append(collections, []templateCellSpan{first})
		for _, second := range intervals {
			collections = append(collections, []templateCellSpan{first, second})
		}
	}
	cases := 0
	for _, mutable := range collections {
		for _, shared := range collections {
			wantOverlap := false
			for _, m := range mutable {
				for _, s := range shared {
					if m.start < s.end && s.start < m.end {
						wantOverlap = true
					}
				}
			}
			inventory := templateInventory{
				cells:       append([]templateCellSpan(nil), mutable...),
				sharedCells: append([]templateCellSpan(nil), shared...),
			}
			err := inventory.checkSharedStorage()
			if (err != nil) != wantOverlap {
				t.Fatalf("mutable=%+v shared=%+v: overlap=%t, got error %v", mutable, shared, wantOverlap, err)
			}
			if err != nil && err.Error() != "template: mutable cells backing overlaps shared program storage" {
				t.Fatalf("mutable=%+v shared=%+v: wrong rejection: %v", mutable, shared, err)
			}
			// storage() consumes this order after admission. In particular,
			// a successful no-shared-spans shortcut must not skip the sort.
			for i := 1; i < len(inventory.cells); i++ {
				if inventory.cells[i-1].start > inventory.cells[i].start {
					t.Fatalf("mutable=%+v shared=%+v: admission left storage input unsorted: %+v", mutable, shared, inventory.cells)
				}
			}
			cases++
		}
	}
	if cases != 12321 { // (1 + 10 + 10*10)^2, not sampled inputs.
		t.Fatalf("exhaustive interval coverage changed: got %d cases, want 12321", cases)
	}
}

func TestTemplateOverlapIndexKeepsEarlierContainingSpan(t *testing.T) {
	// The last mutable span starting before the shared end does not overlap.
	// Its earlier containing span does: an index must retain the prefix's
	// maximum end, not merely the end of its last span.
	inventory := templateInventory{
		cells: []templateCellSpan{
			{start: 2, end: 3},
			{start: 0, end: 10},
		},
		sharedCells: []templateCellSpan{{start: 5, end: 6}},
	}
	if err := inventory.checkSharedStorage(); err == nil || err.Error() != "template: mutable cells backing overlaps shared program storage" {
		t.Fatalf("earlier containing interval was missed: %v", err)
	}
}
