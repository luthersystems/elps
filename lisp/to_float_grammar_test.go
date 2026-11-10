// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"math"
	"strings"
	"testing"

	"github.com/luthersystems/elps/elpstest"
	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// TestToFloatAcceptsDecimalTextOnly pins the text to-float converts
// (luthersystems/elps#943).
//
// to-float takes a decimal number: an optional sign, digits with an optional
// point, and an optional decimal exponent.  It refuses the other spellings
// Go's strconv.ParseFloat takes: "nan", "inf" and "infinity" in any case,
// hexadecimal floats and digits separated by underscores.  A phylum that
// converts text from a user must not get NaN or an infinity from it, and the
// reader and to-int refuse those spellings too.
func TestToFloatAcceptsDecimalTextOnly(t *testing.T) {
	const syntax, rng = "invalid syntax", "value out of range"
	for _, tc := range []struct {
		in   string
		want float64
		err  string
	}{
		// Decimal text converts.
		{in: "0", want: 0},
		{in: "1", want: 1},
		{in: "007", want: 7},
		{in: "1.5", want: 1.5},
		{in: "-12.75", want: -12.75},
		{in: "+1.5", want: 1.5},
		{in: ".5", want: 0.5},
		{in: "5.", want: 5},
		{in: "1e5", want: 1e5},
		{in: "1E5", want: 1e5},
		{in: "1e+5", want: 1e5},
		{in: "1e-5", want: 1e-5},
		{in: "-0", want: math.Copysign(0, -1)},
		{in: "-0.0", want: math.Copysign(0, -1)},
		{in: "1e-999", want: 0}, // underflows to zero, as before
		{in: "1.7976931348623157e308", want: math.MaxFloat64},
		// A value past the float range is an error and never an infinity.
		{in: "1e999", err: rng},
		{in: "-1e999", err: rng},
		// Not a number, and the infinities.
		{in: "nan", err: syntax},
		{in: "NaN", err: syntax},
		{in: "NAN", err: syntax},
		{in: "+nan", err: syntax},
		{in: "-nan", err: syntax},
		{in: "inf", err: syntax},
		{in: "Inf", err: syntax},
		{in: "+inf", err: syntax},
		{in: "+Inf", err: syntax},
		{in: "-inf", err: syntax},
		{in: "-INF", err: syntax},
		{in: "infinity", err: syntax},
		{in: "Infinity", err: syntax},
		{in: "+Infinity", err: syntax},
		{in: "-INFINITY", err: syntax},
		// Hexadecimal floats.
		{in: "0x1p3", err: syntax},
		{in: "0X1P-2", err: syntax},
		{in: "-0x1.8p1", err: syntax},
		{in: "0x10", err: syntax},
		// Underscores.
		{in: "1_000", err: syntax},
		{in: "1_0.5", err: syntax},
		{in: "0x_1p3", err: syntax},
		// Text that was never a number.
		{in: "", err: syntax},
		{in: " 1", err: syntax},
		{in: "1 ", err: syntax},
		{in: "1\n", err: syntax},
		{in: "+", err: syntax},
		{in: ".", err: syntax},
		{in: "e5", err: syntax},
		{in: "1e", err: syntax},
		{in: "abc", err: syntax},
		{in: "1.2.3", err: syntax},
		{in: "١", err: syntax},
	} {
		t.Run(tc.in, func(t *testing.T) {
			env, err := (&elpstest.Runner{}).NewEnv(t)
			require.NoError(t, err)
			env.PutGlobal(lisp.Symbol("s"), lisp.String(tc.in))
			got := env.LoadString("to-float.lisp", `(to-float s)`)
			if tc.err != "" {
				require.Equal(t, lisp.LError, got.Type, "%v", got)
				msg := lisp.GoError(got).Error()
				assert.Contains(t, msg, "strconv.ParseFloat: parsing ")
				assert.True(t, strings.HasSuffix(msg, tc.err), "%s", msg)
				return
			}
			require.Equal(t, lisp.LFloat, got.Type, "%v", got)
			assert.Equal(t, math.Float64bits(tc.want), math.Float64bits(got.Float), "%v", got)
		})
	}
}
