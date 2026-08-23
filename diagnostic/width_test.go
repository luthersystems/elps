// Copyright © 2026 The ELPS authors

package diagnostic

import "testing"

// TestWidthTablesAreSortedAndDisjoint pins the invariant inRanges' binary
// search depends on: each table is sorted by lo, every range is well-formed,
// and consecutive ranges neither overlap nor touch (adjacent ranges would be
// harmless but mean the generator misbehaved).  If a hand edit ever breaks
// this, lookups silently misclassify runes and the only other symptom is a
// misplaced caret.
func TestWidthTablesAreSortedAndDisjoint(t *testing.T) {
	t.Parallel()
	for _, table := range []struct {
		name   string
		ranges []runeRange
	}{
		{"zeroWidth", zeroWidth[:]},
		{"wide", wide[:]},
	} {
		for i, rr := range table.ranges {
			if rr.lo > rr.hi {
				t.Errorf("%s[%d]: lo 0x%04X > hi 0x%04X", table.name, i, rr.lo, rr.hi)
			}
			if rr.hi > 0x10FFFF {
				t.Errorf("%s[%d]: hi 0x%04X beyond the last codepoint", table.name, i, rr.hi)
			}
			if i > 0 && rr.lo <= table.ranges[i-1].hi+1 {
				t.Errorf("%s[%d]: lo 0x%04X overlaps or touches previous range ending 0x%04X",
					table.name, i, rr.lo, table.ranges[i-1].hi)
			}
		}
	}
	// The two tables must also be disjoint from each other: a rune cannot be
	// both zero-width and wide, and which table wins would otherwise be an
	// accident of runeCellWidth's case order.
	for _, zr := range zeroWidth {
		for r := zr.lo; r <= zr.hi; r++ {
			if inRanges(r, wide[:]) {
				t.Fatalf("U+%04X is in both zeroWidth and wide", r)
			}
		}
	}
}

// TestRuneCellWidth covers the classification edges that displayWidth's
// string-level tests do not reach directly: the fast paths below U+0300, the
// boundaries of the halfwidth/fullwidth forms block, and the out-of-range
// guard.
func TestRuneCellWidth(t *testing.T) {
	t.Parallel()
	for _, tc := range []struct {
		name string
		r    rune
		want int
	}{
		{"NUL", 0x00, 0},
		{"bell", 0x07, 0},
		{"escape", 0x1B, 0},
		{"space", ' ', 1},
		{"tilde", '~', 1},
		{"DEL", 0x7F, 0},
		{"C1 control", 0x9B, 0},
		{"NBSP", 0xA0, 1},
		{"soft hyphen", 0xAD, 0},
		{"latin-1 e-acute", 0xE9, 1},
		{"first combining mark", 0x0300, 0},
		{"last of combining block", 0x036F, 0},
		{"greek alpha", 0x03B1, 1},
		{"zero width space", 0x200B, 0},
		{"zero width joiner", 0x200D, 0},
		{"first hangul jamo lead", 0x1100, 2},
		{"CJK ideograph", 0x52A0, 2},
		{"hiragana a", 0x3042, 2},
		{"fullwidth A", 0xFF21, 2},
		{"last fullwidth cell", 0xFF60, 2},
		{"halfwidth ideographic full stop", 0xFF61, 1},
		{"halfwidth katakana a", 0xFF71, 1},
		{"fullwidth won sign", 0xFFE6, 2},
		{"replacement char", 0xFFFD, 1},
		{"astral math alpha", 0x1D6FC, 1},
		{"emoji grinning face", 0x1F600, 2},
		{"CJK extension B", 0x20000, 2},
		{"last plane-3 wide", 0x3FFFD, 2},
		// go-runewidth v0.0.27 (the reference classification) counts the
		// plane-14 tag characters as narrow, not zero; keep parity.
		{"plane-14 tag", 0xE0001, 1},
		{"private use", 0xF0000, 1},
		{"negative rune", -1, 0},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			if got := runeCellWidth(tc.r); got != tc.want {
				t.Errorf("runeCellWidth(U+%04X) = %d, want %d", tc.r, got, tc.want)
			}
		})
	}
}
