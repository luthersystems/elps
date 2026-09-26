// Copyright © 2026 The ELPS authors

package libjson

import (
	"context"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// doublingDAG returns a value whose JSON is 2^n copies of leaf, built from n
// shared lists -- the shape that makes an uncapped dump exponential.
func doublingDAG(leaf *lisp.LVal, n int) *lisp.LVal {
	v := leaf
	for range n {
		v = lisp.QExpr([]*lisp.LVal{v, v})
	}
	return v
}

// wrapLists nests v inside depth single-element lists, which pushes the
// encoder past encodeGuardDepth and into its second pass.
func wrapLists(v *lisp.LVal, depth int) *lisp.LVal {
	for range depth {
		v = lisp.QExpr([]*lisp.LVal{v})
	}
	return v
}

// The budget has to stop the encode EARLY, not just reject the finished
// document: a check only at the end would still build the whole exponential
// output first.  The documents here are about 6 MB uncapped, so an encoder
// that lost the per-value check fails the length assertion instead of
// exhausting memory.
func TestEncodeBudgetStopsEarly(t *testing.T) {
	const maxBytes = 1024
	// Room for the one value that may be in flight when the cap is noticed.
	const slack = 64
	// Int leaves: strings and bytes also reserve their own size, which
	// would stop the encode early by itself and hide a lost per-value check.
	dag := doublingDAG(lisp.Int(12345), 20)
	for _, tc := range []struct {
		name string
		v    *lisp.LVal
		deep bool
	}{
		{"counting pass", dag, false},
		{"second pass", wrapLists(dag, 2*encodeGuardDepth), true},
	} {
		t.Run(tc.name, func(t *testing.T) {
			enc := getEncoder(false)
			defer putEncoder(enc)
			err := enc.encodeLimit(tc.v, lisp.MaxValueDepth, encodeBudget{maxBytes: maxBytes})
			var size encodeSizeError
			require.ErrorAs(t, err, &size)
			assert.Equal(t, encodeSizeError(maxBytes), size)
			assert.Equal(t, tc.deep, enc.nestedDeep, "the document did not take the intended pass")
			assert.LessOrEqual(t, enc.buf.Len(), maxBytes+slack,
				"the encoder wrote far past the cap before stopping")
		})
	}
}

// A cancelled context stops the encode within encodeContextInterval values,
// in both passes, even when the byte cap is nowhere near.
func TestEncodeBudgetObservesCancelledContext(t *testing.T) {
	ctx, cancel := context.WithCancel(context.Background())
	cancel()
	dag := doublingDAG(lisp.Int(1), 20)
	for _, v := range []*lisp.LVal{dag, wrapLists(dag, 2*encodeGuardDepth)} {
		enc := getEncoder(false)
		err := enc.encodeLimit(v, lisp.MaxValueDepth, encodeBudget{maxBytes: 1 << 40, ctx: ctx})
		var cancelled encodeCancelledError
		require.ErrorAs(t, err, &cancelled)
		require.ErrorIs(t, err, context.Canceled)
		// Each value writes at most a few bytes ("1," or "[").
		assert.LessOrEqual(t, enc.buf.Len(), 4*encodeContextInterval+2*encodeGuardDepth)
		putEncoder(enc)
	}
}

// A zero budget is the Go-level Dump: no cap, no context.
func TestEncodeZeroBudgetIsUnbounded(t *testing.T) {
	enc := getEncoder(false)
	defer putEncoder(enc)
	require.NoError(t, enc.encodeLimit(doublingDAG(lisp.Int(1), 12), lisp.MaxValueDepth, encodeBudget{}))
	assert.Equal(t, 2*(1<<12)-1+2*((1<<12)-1), enc.buf.Len())
}
