// Copyright © 2026 The ELPS authors

package lisp

import (
	"strconv"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// branchingRingMaps builds a ring of n sorted maps where every node holds the
// next node under two keys. The fan-out is what makes an undetected cycle
// expand exponentially rather than merely deeply, and the period is what puts
// the repeat out of reach of the lazy cycle guard once it exceeds
// maxRenderDepth - cycleGuardDepth.
func branchingRingMaps(t *testing.T, n int) *LVal {
	t.Helper()
	nodes := make([]*LVal, n)
	for i := range nodes {
		nodes[i] = SortedMap()
	}
	for i, node := range nodes {
		next := nodes[(i+1)%n]
		require.True(t, node.MapSetLVal(String("a"), next).IsNil())
		require.True(t, node.MapSetLVal(String("b"), next).IsNil())
	}
	return nodes[0]
}

// branchingRingCells is the same shape built from ordinary s-expressions, so
// the guarantee is about the renderer rather than about sorted maps.
func branchingRingCells(t *testing.T, n int) *LVal {
	t.Helper()
	nodes := make([]*LVal, n)
	for i := range nodes {
		nodes[i] = SExpr(nil)
	}
	for i, node := range nodes {
		next := nodes[(i+1)%n]
		node.Cells = []*LVal{next, next}
	}
	return nodes[0]
}

// A cyclic value renders whatever its period is. The lazy cycle guard starts
// recording a path at cycleGuardDepth and rendering stops at maxRenderDepth,
// so a ring longer than the difference is never observed by the lazy walk; it
// must still not degrade to #<truncated>, and the bounded renderer must agree
// with String at the exact byte limit the way every other value does.
func TestRenderLongCyclePeriodsStillRender(t *testing.T) {
	for _, build := range []struct {
		name string
		fn   func(*testing.T, int) *LVal
	}{
		{"sorted-map", branchingRingMaps},
		{"list", branchingRingCells},
	} {
		for _, n := range []int{61, 962, 1200, 4000} {
			t.Run(build.name+"/"+strconv.Itoa(n), func(t *testing.T) {
				ring := build.fn(t, n)
				start := time.Now()
				got := ring.String()
				elapsed := time.Since(start)
				assert.NotContains(t, got, renderTruncatedMark, "a cyclic value must render")
				assert.Contains(t, got, cycleMark)
				assert.Less(t, elapsed, 10*time.Second, "rendering a ring must not unroll it")
				// The oracle every other value obeys: the exact limit
				// accepts the complete rendering, one byte less rejects it.
				bounded, ok := ring.boundedString(len(got))
				require.True(t, ok, "exact limit rejected the complete rendering")
				assert.Equal(t, got, bounded)
				bounded, ok = ring.boundedString(len(got) - 1)
				assert.False(t, ok)
				assert.Empty(t, bounded)
			})
		}
	}
}

// The analysis that decides whether the strict rendering may stand walks
// where the rendering does not: past the depth cap and past the byte limit.
// A malformed header it is therefore the first to reach must not turn a value
// the renderer was describing into a dead process.
func TestRenderLongCycleWithMalformedNode(t *testing.T) {
	ring := branchingRingCells(t, 1200)
	// Past maxRenderDepth: every rendering pass stops before this node, so
	// the cycle search is the only walk that reaches it.
	deep := ring
	for range maxRenderDepth + 64 {
		deep = deep.Cells[0]
	}
	deep.Cells = append(deep.Cells, &LVal{Type: LSortMap})
	var got string
	assert.NotPanics(t, func() { got = ring.String() })
	assert.NotEmpty(t, got)
}

// The lisp-visible half of the same regression: format-string reported a
// misleading allocation error for a value whose rendering is tens of
// kilobytes, because the work budget the lazy pass burned unrolling a cycle
// it could not observe was charged to the strict retry as well.
func TestFormatStringRendersLongCycle(t *testing.T) {
	env := initSafetyTestEnv(t)
	env.Put(Symbol("ring"), branchingRingMaps(t, 1200))
	out := env.Eval(SExpr([]*LVal{Symbol("format-string"), String("{}"), Symbol("ring")}))
	require.NotEqual(t, LError, out.Type, "format-string failed: %v", out)
	assert.Greater(t, len(out.Str), 1000)
	assert.Contains(t, out.Str, cycleMark)
}
