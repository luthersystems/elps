// Copyright © 2026 The ELPS authors

package lisp

import (
	"math"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// TestMakeSequenceCapIsAlwaysAllocatable is the assertion that matters about
// the pre-sized capacity: whatever make-sequence computes, make must accept
// it.  The test hands the computed number to make itself, so it pins the
// failure mode directly -- "makeslice: cap out of range" -- without running
// the sequence loop, which for these inputs does not terminate in any useful
// time and is not something a test should provoke.
//
// The limits below include the ones an embedder can set.  MaxAlloc is a
// number an embedder chooses and is under no obligation to make a count of
// elements that can exist; with it raised, a capacity clamped only to it
// asked for more than a slice can address and make rejected it, turning a
// huge forward range from a slow exhaustion into an immediate
// internal-panic.
func TestMakeSequenceCapIsAlwaysAllocatable(t *testing.T) {
	limits := []int{1, 4096, DefaultMaxAlloc, 1 << 40, 1 << 50, math.MaxInt64}
	starts := []int{math.MinInt64, -1 << 40, -1, 0, 1, 1 << 40, math.MaxInt64 - 1}
	stops := []int{math.MinInt64, -1 << 40, -1, 0, 1, 1 << 40, math.MaxInt64}
	steps := []int{1, 2, 7, 1 << 20, math.MaxInt64}

	for _, limit := range limits {
		for _, start := range starts {
			for _, stop := range stops {
				for _, step := range steps {
					n := makeSequenceCap(start, stop, step, limit)
					require.GreaterOrEqual(t, n, 0,
						"negative capacity for (%d %d %d) limit %d", start, stop, step, limit)
					require.LessOrEqual(t, n, maxSequencePresize,
						"capacity above the absolute bound for (%d %d %d) limit %d", start, stop, step, limit)
					// The assertion: make accepts it.  A capacity that
					// makeslice rejects panics here and fails the test.
					require.NotPanics(t, func() { _ = make([]*LVal, 0, n) },
						"make rejected the capacity for (%d %d %d) limit %d", start, stop, step, limit)
				}
			}
		}
	}
}

// TestMakeSequenceCapValues pins what the capacity actually is, so that
// bounding it cannot quietly turn into not sizing at all.  A sequence short
// enough to size gets its exact length; everything that must not be sized
// gets zero.
func TestMakeSequenceCapValues(t *testing.T) {
	const limit = DefaultMaxAlloc
	tests := []struct {
		name                    string
		start, stop, step, want int
	}{
		// Ordinary forward ranges get their exact element count.
		{"unit step", 0, 10, 1, 10},
		{"step divides the span", 0, 10, 2, 5},
		{"step does not divide the span", 0, 10, 3, 4},
		{"negative start", -5, 5, 1, 10},
		{"step past the span", 0, 10, 100, 1},

		// Nothing to size.
		{"equal endpoints", 7, 7, 1, 0},
		{"backwards by one", 1, 0, 1, 0},
		{"backwards to the smallest int", 1, math.MinInt64, 1, 0},
		{"backwards from zero", 0, math.MinInt64, 3, 0},
		{"backwards across the whole range", math.MaxInt64, math.MinInt64, 1, 0},

		// Forward but longer than either bound.
		{"clamped to the allocation limit", 0, math.MaxInt64, 1, maxSequencePresize},
		{"clamped to the absolute bound", 0, 1 << 40, 1, maxSequencePresize},
		{"just above the absolute bound", 0, maxSequencePresize + 1, 1, maxSequencePresize},
		{"exactly the absolute bound", 0, maxSequencePresize, 1, maxSequencePresize},
		{"just below the absolute bound", 0, maxSequencePresize - 1, 1, maxSequencePresize - 1},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			assert.Equal(t, test.want, makeSequenceCap(test.start, test.stop, test.step, limit))
		})
	}
}

// TestMakeSequenceCapRespectsATightLimit keeps the allocation-limit clamp
// honest: an embedder that lowers MaxAlloc gets a capacity no larger than
// what the loop will be allowed to build.
func TestMakeSequenceCapRespectsATightLimit(t *testing.T) {
	assert.Equal(t, 32, makeSequenceCap(0, 1000, 1, 32))
	assert.Equal(t, 100, makeSequenceCap(0, 100, 1, 4096))
}
