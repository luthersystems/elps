// Copyright © 2026 The ELPS authors

package libstring

import (
	"strings"
	"testing"
	"time"

	"github.com/luthersystems/elps/internal/testdeadline"
	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// trimBuiltins pairs each trim builtin with the Go function that defines its
// result.
var trimBuiltins = []struct {
	name string
	fn   lisp.LBuiltin
	want func(s, cutset string) string
}{
	{"trim", builtinTrim, strings.Trim},
	{"trim-left", builtinTrimLeft, strings.TrimLeft},
	{"trim-right", builtinTrimRight, strings.TrimRight},
}

func callTrim(env *lisp.LEnv, fn lisp.LBuiltin, str, cutset string) *lisp.LVal {
	return fn(env, lisp.SExpr([]*lisp.LVal{lisp.String(str), lisp.String(cutset)}))
}

// The work of a trim must be in proportion to what it charges
// (luthersystems/elps#936).  With a cutset that is not ASCII, strings.Trim
// scans the cutset for each rune of the string, so a 200 KB string and a 1 MB
// cutset took seconds of CPU for 195 steps.  A large cutset now becomes a
// set one time, and the charge counts the cutset too.
func TestTrimLargeCutsetWorkIsLinear(t *testing.T) {
	str := strings.Repeat("x", 200_000)
	cutset := strings.Repeat("é", 500_000) + "x"
	for _, b := range trimBuiltins {
		t.Run(b.name, func(t *testing.T) {
			env := newParityEnv(t)
			var got *lisp.LVal
			cpu, ok := testdeadline.Within(time.Second, func() { got = callTrim(env, b.fn, str, cutset) })
			require.True(t, ok, "used %v of CPU", cpu)
			require.Equal(t, lisp.LString, got.Type, "%v", got)
			assert.Empty(t, got.Str)
		})
	}
}

// A large cutset takes the set path.  Its result is the result of the Go
// function for every kind of input, invalid UTF-8 included: an invalid byte
// in the string matches an invalid byte or U+FFFD in the cutset, as it does
// in package strings.
func TestTrimLargeCutsetParity(t *testing.T) {
	pad := strings.Repeat("é", trimSetMinCutset)
	for _, cutset := range []string{
		pad,
		pad + "x ",
		pad + "\xff",
		pad + "�",
		"x" + pad + "世",
		strings.Repeat("ab", trimSetMinCutset), // ASCII: package strings has its own set
	} {
		for _, str := range []string{
			"",
			"x",
			"xxhixx",
			"ééhiéé",
			" x é hi é x ",
			"\xffhi\xff",
			"�hi�",
			"\xff�hi�\xff",
			"世hi世",
			"é\xc3hi\xc3é", // a truncated é
			"ééé",
			"abhiba",
		} {
			for _, b := range trimBuiltins {
				env := newParityEnv(t)
				got := callTrim(env, b.fn, str, cutset)
				require.Equal(t, lisp.LString, got.Type, "%v", got)
				assert.Equal(t, b.want(str, cutset), got.Str, "%s %q, cutset %q...", b.name, str, cutset[len(cutset)-8:])
			}
		}
	}
}

// The charge is one step for each KiB of the string and for each KiB of the
// cutset.  A cutset under 1 KiB adds nothing, so the step count of a program
// with an ordinary cutset does not change.
func TestTrimCharge(t *testing.T) {
	for _, tc := range []struct {
		str, cutset int
		want        int64
	}{
		{10, 10, 0},
		{4096, 1023, 4},
		{1023, 1024, 1},
		{4096, 3 * 1024, 7},
	} {
		for _, b := range trimBuiltins {
			env := newParityEnv(t)
			lisp.WithMaxSteps(1 << 40)(env) // steps are counted only under a limit
			str, cutset := strings.Repeat("s", tc.str), strings.Repeat("c", tc.cutset)
			got := steps(env, func() { callTrim(env, b.fn, str, cutset) })
			assert.Equal(t, tc.want, got, "%s: string %d, cutset %d", b.name, tc.str, tc.cutset)
		}
	}
}
