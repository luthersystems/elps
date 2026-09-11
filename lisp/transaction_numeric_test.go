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
			require.Equal(t, want, got.Float)
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
