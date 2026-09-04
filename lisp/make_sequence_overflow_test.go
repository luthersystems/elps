// Copyright © 2018 The ELPS authors

package lisp_test

import (
	"runtime"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// minInt64Literal is the smallest int64, written out because the reader
// parses it as one literal.
const minInt64Literal = `-9223372036854775808`

// TestMakeSequenceBackwardsRangeDoesNotSize pins the direction check in
// make-sequence's integer fast path.
//
// The sequence is empty whenever stop <= start, and the length the fast path
// computes is stop-start.  That subtraction only wraps NEGATIVE -- the signal
// the fast path relies on to bail out -- when stop is genuinely greater than
// start.  Run backwards it wraps POSITIVE: (make-sequence 1 MinInt64) yields a
// difference of MaxInt64, so a fast path that sizes before checking the
// direction reserves the whole allocation limit for a sequence with no
// elements at all.
//
// At the default limit that is a silent 80MB reservation for a call that
// returns ().  Under a limit an embedder has raised it is worse: the
// capacity is past what a slice can hold, so make panics and the call fails
// where it used to succeed.
func TestMakeSequenceBackwardsRangeDoesNotSize(t *testing.T) {
	// Well past the default limit, and past what any capacity could back:
	// the point is that a backwards range never consults the limit at all.
	env := newLimitTestEnv(t, lisp.WithMaxAlloc(1<<50))

	backwards := []string{
		`(make-sequence 1 ` + minInt64Literal + `)`,
		`(make-sequence 5 -9223372036854775805)`,
		`(make-sequence 0 ` + minInt64Literal + ` 3)`,
		// Equal endpoints: also empty, also nothing to size.
		`(make-sequence 7 7)`,
	}
	for _, expr := range backwards {
		t.Run(expr, func(t *testing.T) {
			res := env.LoadString("test", expr)
			require.NotEqual(t, lisp.LError, res.Type,
				"a backwards range must return the empty sequence, not fail: %v", res)
			assert.Equal(t, `'()`, res.String())
		})
	}
}

// TestMakeSequenceBackwardsRangeAllocatesNothing is the other half: the
// default-limit call returns () on both trees, so only the memory it moves
// distinguishes them.  Sizing from a wrapped difference reserves one pointer
// per element of the whole allocation limit -- about 80MB -- for a result
// with no elements.
//
// The bound below is three orders of magnitude under that reservation and
// three above what the call actually costs, so it discriminates without
// pinning an exact count.
func TestMakeSequenceBackwardsRangeAllocatesNothing(t *testing.T) {
	env := newLimitTestEnv(t)
	expr := `(make-sequence 1 ` + minInt64Literal + `)`

	// Warm the reader and any lazily built environment state, and confirm
	// the shape under test before measuring it.
	require.Equal(t, `'()`, env.LoadString("test", expr).String())

	const runs = 20
	var before, after runtime.MemStats
	runtime.GC()
	runtime.ReadMemStats(&before)
	for range runs {
		if res := env.LoadString("test", expr); res.Type == lisp.LError {
			t.Fatalf("unexpected error: %v", res)
		}
	}
	runtime.ReadMemStats(&after)

	perCall := (after.TotalAlloc - before.TotalAlloc) / runs
	t.Logf("bytes allocated per call: %d", perCall)
	assert.Less(t, perCall, uint64(1<<20),
		"a backwards range must not reserve capacity for the allocation limit")
}
