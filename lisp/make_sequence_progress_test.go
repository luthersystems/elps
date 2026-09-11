// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"math"
	"strconv"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// These ranges have at most four elements. The tiny allocation cap also
// bounds the broken implementation, which wraps or repeats until it errors.
func TestMakeSequenceProgress(t *testing.T) {
	maxInt := int(^uint(0) >> 1)
	minInt := -maxInt - 1
	spacing := 2
	if strconv.IntSize == 64 {
		spacing = 2048
	}
	boundary := math.Ldexp(1, strconv.IntSize-1)
	for _, tc := range []struct {
		name              string
		start, stop, step *lisp.LVal
		want              []*lisp.LVal
	}{
		{"exact integer stop", lisp.Int(maxInt - 1), lisp.Int(maxInt), lisp.Int(1), []*lisp.LVal{lisp.Int(maxInt - 1)}},
		{"integer step crosses maximum", lisp.Int(maxInt - 1), lisp.Int(maxInt), lisp.Int(2), []*lisp.LVal{lisp.Int(maxInt - 1)}},
		{"two elements before overflow", lisp.Int(maxInt - 3), lisp.Int(maxInt), lisp.Int(2), []*lisp.LVal{lisp.Int(maxInt - 3), lisp.Int(maxInt - 1)}},
		{"maximum step crosses stop", lisp.Int(1), lisp.Int(maxInt), lisp.Int(maxInt), []*lisp.LVal{lisp.Int(1)}},
		{"range spans integer domain", lisp.Int(minInt), lisp.Int(maxInt), lisp.Int(maxInt), []*lisp.LVal{lisp.Int(minInt), lisp.Int(-1), lisp.Int(maxInt - 1)}},
		{"float stop beyond integer maximum", lisp.Int(maxInt - spacing + 1), lisp.Float(boundary + float64(2*spacing)), lisp.Int(spacing), []*lisp.LVal{lisp.Int(maxInt - spacing + 1), lisp.Float(boundary), lisp.Float(boundary + float64(spacing))}},
		{"mixed stop preserves integer elements", lisp.Int(1), lisp.Float(3.5), lisp.Int(1), []*lisp.LVal{lisp.Int(1), lisp.Int(2), lisp.Int(3)}},
		{"mixed step preserves initial element", lisp.Int(0), lisp.Int(2), lisp.Float(0.5), []*lisp.LVal{lisp.Int(0), lisp.Float(0.5), lisp.Float(1), lisp.Float(1.5)}},
		{"infinite successor passes finite stop", lisp.Float(1), lisp.Float(2), lisp.Float(math.Inf(1)), []*lisp.LVal{lisp.Float(1)}},
		{"infinite successor reaches infinite stop", lisp.Float(1), lisp.Float(math.Inf(1)), lisp.Float(math.Inf(1)), []*lisp.LVal{lisp.Float(1)}},
		{"exact allocation boundary", lisp.Int(0), lisp.Int(4), lisp.Int(1), []*lisp.LVal{lisp.Int(0), lisp.Int(1), lisp.Int(2), lisp.Int(3)}},
		{"empty reverse range", lisp.Int(maxInt), lisp.Int(minInt), lisp.Int(2), nil},
		{"empty equal float range", lisp.Float(1e20), lisp.Float(1e20), lisp.Float(1), nil},
	} {
		t.Run(tc.name, func(t *testing.T) {
			env := newLimitTestEnv(t, lisp.WithMaxAlloc(4))
			before := []string{tc.start.String(), tc.stop.String(), tc.step.String()}
			for name, value := range map[string]*lisp.LVal{"start": tc.start, "stop": tc.stop, "step": tc.step} {
				require.NoError(t, lisp.GoError(env.PutGlobal(lisp.Symbol(name), value)))
			}
			got := env.LoadString("progress.lisp", `(make-sequence start stop step)`)
			assert.Equal(t, before, []string{tc.start.String(), tc.stop.String(), tc.step.String()}, "range arguments changed")
			require.False(t, lisp.IsInternalPanic(got), "%v", got)
			require.Equal(t, lisp.LSExpr, got.Type, "%v", got)
			require.Len(t, got.Cells, len(tc.want))
			for i, want := range tc.want {
				assert.Equal(t, want.Type, got.Cells[i].Type, "element %d type", i)
				assert.Equal(t, want.Int, got.Cells[i].Int, "element %d integer", i)
				assert.Equal(t, want.Float, got.Cells[i].Float, "element %d float", i)
			}
		})
	}
}

func TestMakeSequenceIneffectiveFloatStep(t *testing.T) {
	for _, tc := range []struct {
		name, source string
	}{
		{"positive magnitude", `(make-sequence 1e20 1.0000000000000002e20 1.0)`},
		{"negative magnitude", `(make-sequence -1e20 -9.999999999999998e19 1.0)`},
		{"mixed integer start", `(make-sequence 1 2 1e-300)`},
	} {
		t.Run(tc.name, func(t *testing.T) {
			env := newLimitTestEnv(t, lisp.WithMaxAlloc(4))
			got := env.LoadString("progress.lisp", tc.source)
			require.Equal(t, lisp.LError, got.Type, "%v", got)
			assert.False(t, lisp.IsInternalPanic(got))
			assert.Contains(t, got.String(), "step does not advance")
			assert.NotContains(t, got.String(), "allocation size")
		})
	}
	// A real five-element result must still be stopped by the allocation cap.
	env := newLimitTestEnv(t, lisp.WithMaxAlloc(4))
	got := env.LoadString("progress.lisp", `(make-sequence 0 5)`)
	require.Equal(t, lisp.LError, got.Type, "%v", got)
	assert.False(t, lisp.IsInternalPanic(got))
	assert.Contains(t, got.String(), "allocation size 5 exceeds maximum (4)")
}
