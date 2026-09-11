// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"fmt"
	"math"
	"strconv"
	"testing"

	"github.com/luthersystems/elps/elpstest"
	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/require"
)

func TestMultiplyPromotesBeforeIntegerOverflow(t *testing.T) {
	// All factors are powers of two, so the expected float is exact. The
	// integer prefix overflows on either supported int width.
	large := int(1) << (strconv.IntSize - 2)
	want := math.Ldexp(1, strconv.IntSize-1)
	for _, expr := range []string{
		fmt.Sprintf("(* %d 4 0.5)", large),
		fmt.Sprintf("(* %d 0.5 4)", large),
		fmt.Sprintf("(* 0.5 %d 4)", large),
		fmt.Sprintf("(* %d -4 0.5)", -large),
	} {
		t.Run(expr, func(t *testing.T) {
			env, err := (&elpstest.Runner{}).NewEnv(t)
			require.NoError(t, err)
			got := env.LoadString("multiply", expr)
			require.Equal(t, lisp.LFloat, got.Type, "%v", got)
			require.InDelta(t, want, got.Float, 0)
		})
	}
}

func TestMultiplyRetainsIntegerAndIEEEBehavior(t *testing.T) {
	large := int(1) << (strconv.IntSize - 2)
	elpstest.RunTestSuite(t, elpstest.TestSuite{
		{"integer and float controls", elpstest.TestSequence{
			{`(*)`, `1`, ``},
			{`(int? (* 2 3 4))`, `true`, ``},
			{`(* 2 3 4)`, `24`, ``},
			{fmt.Sprintf("(* %d 4)", large), `0`, ``},
			{`(float? (* 2 3 0.5))`, `true`, ``},
			{`(* 2 3 0.5)`, `3`, ``},
			{`(to-string (* 0 (/ 1 0)))`, `"NaN"`, ``},
			{`(handler-bind ((condition (lambda (&rest _) 'invalid))) (* 2 0.5 "bad"))`, `'invalid`, ``},
		}},
	})
}

func TestToIntFloatRange(t *testing.T) {
	limit := math.Ldexp(1, strconv.IntSize-1)
	minInt := -int(1) << (strconv.IntSize - 1)
	// This float is strictly inside the int range on both 32- and 64-bit
	// platforms, so Go's conversion has a defined result. On 32-bit it also
	// checks that truncation precedes the range check.
	largestFloat := math.Nextafter(limit, 0)
	for _, tc := range []struct {
		name    string
		value   float64
		want    int
		invalid bool
	}{
		{"positive fraction", 42.9, 42, false},
		{"negative fraction", -42.9, -42, false},
		{"zero", 0, 0, false},
		{"minimum int", -limit, minInt, false},
		{"largest in-range float", largestFloat, int(largestFloat), false},
		{"representable large int", limit / 2, int(1) << (strconv.IntSize - 2), false},
		{"NaN", math.NaN(), 0, true},
		{"positive infinity", math.Inf(1), 0, true},
		{"negative infinity", math.Inf(-1), 0, true},
		{"exclusive upper bound", limit, 0, true},
		{"below lower bound", -limit * 2, 0, true},
	} {
		t.Run(tc.name, func(t *testing.T) {
			env, err := (&elpstest.Runner{}).NewEnv(t)
			require.NoError(t, err)
			require.NotEqual(t, lisp.LError, env.PutGlobal(lisp.Symbol("input"), lisp.Float(tc.value)).Type)
			got := env.LoadString("conversion", `(to-int input)`)
			require.False(t, lisp.IsInternalPanic(got), "%v", got)
			if tc.invalid {
				require.Equal(t, lisp.LError, got.Type, "%v", got)
				require.Contains(t, got.String(), "float cannot be represented as an int")
				return
			}
			require.Equal(t, lisp.LInt, got.Type, "%v", got)
			require.Equal(t, tc.want, got.Int)
		})
	}
}
