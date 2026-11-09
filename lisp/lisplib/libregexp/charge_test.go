// Copyright © 2026 The ELPS authors

package libregexp_test

import (
	"regexp"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/lisp/lisplib/libregexp"
	"github.com/luthersystems/elps/parser"
)

func chargeEnv(t *testing.T) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	lisp.WithMaxSteps(1 << 40)(env) // steps are counted only under a limit
	require.NoError(t, lisp.GoError(lisp.InitializeUserEnv(env)))
	require.NoError(t, lisp.GoError(lisplib.LoadLibrary(env)))
	require.NoError(t, lisp.GoError(env.InPackage(lisp.String(lisp.DefaultUserPackage))))
	return env
}

// matchSteps returns the steps that one regexp-match? call charges.
func matchSteps(t *testing.T, env *lisp.LEnv, re *lisp.LVal, text string) int64 {
	t.Helper()
	before := env.Runtime.Steps()
	got := libregexp.BuiltinIsMatch(env, lisp.SExpr([]*lisp.LVal{re, lisp.String(text)}))
	require.NotEqual(t, lisp.LError, got.Type, "%v", got)
	return env.Runtime.Steps() - before
}

// The work of a match grows with the size of the compiled program as well as
// the length of the text (luthersystems/elps#937).  The charge counted the
// text only, so a pattern of 40 bytes that compiles to thousands of
// instructions matched 1023 bytes in tens of milliseconds for no steps.  The
// charge now scales with the program size, in units of 256 instructions.
func TestMatchChargeCountsTheProgramSize(t *testing.T) {
	const big = "(((a?){10}){10}){10}(((a){10}){10}){10}$"
	insts := libregexp.ProgramSize(big)
	require.Greater(t, insts, 2000, "the pattern is not the large program the test needs")
	text := strings.Repeat("a", 1023)
	want := int64(len(text) * (1 + insts/256) / 1024)
	require.Greater(t, want, int64(5))

	env := chargeEnv(t)
	compiled := env.LoadString("test", `(regexp:regexp-compile "`+big+`")`)
	require.NotEqual(t, lisp.LError, compiled.Type, "%v", compiled)

	// The three forms of a regexp argument pay the same.
	assert.Equal(t, want, matchSteps(t, env, compiled, text), "compiled")
	assert.Equal(t, want, matchSteps(t, env, lisp.String(big), text), "pattern string")
	assert.Equal(t, want, matchSteps(t, env, lisp.Native(regexp.MustCompile(big)), text), "host regexp")
}

// A program under 256 instructions pays for the text only, as before, so the
// step count of a program with ordinary patterns does not change.
func TestMatchChargeOfOrdinaryPatternsIsUnchanged(t *testing.T) {
	for _, patt := range []string{
		`^a+$`,
		`^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$`,
		`^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$`,
		`^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(\.\d{1,9})?(Z|[+-]\d{2}:\d{2})$`,
		`(?i)^(GB|DE|FR)\d{2}[A-Z0-9]{11,30}$`,
	} {
		require.Less(t, libregexp.ProgramSize(patt), 256, patt)
		env := chargeEnv(t)
		compiled := libregexp.BuiltinCompile(env, lisp.SExpr([]*lisp.LVal{lisp.String(patt)}))
		require.NotEqual(t, lisp.LError, compiled.Type, "%v", compiled)
		for _, n := range []int{0, 1023, 1024, 10 * 1024} {
			text := strings.Repeat("a", n)
			assert.Equal(t, int64(n/1024), matchSteps(t, env, compiled, text), "%s, compiled, %d bytes", patt, n)
			assert.Equal(t, int64(n/1024), matchSteps(t, env, lisp.String(patt), text), "%s, string, %d bytes", patt, n)
		}
	}
}
