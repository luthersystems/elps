// Copyright © 2026 The ELPS authors

package libstring

import (
	"context"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// parityResult compares a Go form's result with its builtin's: the same
// value, or an error with the same condition and message.
func parityResult(t *testing.T, want *lisp.LVal, got *lisp.LVal, err error, msg string) {
	t.Helper()
	if want.Type == lisp.LError {
		var e *lisp.ErrorVal
		require.ErrorAs(t, err, &e, msg)
		assert.Equal(t, want.Str, e.Condition(), msg)
		assert.Equal(t, (*lisp.ErrorVal)(want).ErrorMessage(), e.ErrorMessage(), msg)
		return
	}
	require.NoError(t, err, msg)
	assert.True(t, lisp.True(want.Equal(got)), "%s: want %v, got %v", msg, want, got)
}

// steps returns the steps f charges.
func steps(env *lisp.LEnv, f func()) int64 {
	before := env.Runtime.Steps()
	f()
	return env.Runtime.Steps() - before
}

func newParityEnv(t *testing.T) *lisp.LEnv {
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	require.Equal(t, lisp.LSExpr, lisp.InitializeUserEnv(env).Type)
	return env
}

// TestSplitParity compares Split with the string:split builtin under several
// allocation caps: the same pieces or error, and the same steps charged.
func TestSplitParity(t *testing.T) {
	env := newParityEnv(t)
	big := strings.Repeat("a:", 1500)
	cases := []struct{ str, sep string }{
		{"", ":"},
		{"a", ":"},
		{"a:b:c", ":"},
		{"héllo", ""},
		{big, ":"},
		{big, ""},
	}
	for _, limit := range []int{0, 2, 8, 5000} {
		env.Runtime.MaxAlloc = limit
		for _, tc := range cases {
			var want *lisp.LVal
			wantSteps := steps(env, func() {
				want = builtinSplit(env, lisp.QExpr([]*lisp.LVal{lisp.String(tc.str), lisp.String(tc.sep)}))
			})
			var got []string
			var err error
			gotSteps := steps(env, func() { got, err = Split(env, tc.str, tc.sep) })
			parityResult(t, want, lisp.StringList(got), err, "sep "+tc.sep)
			assert.Equal(t, wantSteps, gotSteps, "steps, limit %d, sep %q", limit, tc.sep)
		}
	}
}

// TestRepeatParity compares Repeat with the string:repeat builtin under
// several allocation caps: the same string or error, and the same steps
// charged.
func TestRepeatParity(t *testing.T) {
	env := newParityEnv(t)
	cases := []struct {
		str string
		n   int
	}{
		{"ab", -1},
		{"ab", 0},
		{"", 5},
		{"ab", 1},
		{":", 3},
		{"xyz", 2000},
	}
	for _, limit := range []int{0, 2, 8, 5000} {
		env.Runtime.MaxAlloc = limit
		for _, tc := range cases {
			var want *lisp.LVal
			wantSteps := steps(env, func() {
				want = builtinRepeat(env, lisp.QExpr([]*lisp.LVal{lisp.String(tc.str), lisp.Int(tc.n)}))
			})
			var got string
			var err error
			gotSteps := steps(env, func() { got, err = Repeat(env, tc.str, tc.n) })
			parityResult(t, want, lisp.String(got), err, tc.str)
			assert.Equal(t, wantSteps, gotSteps, "steps, limit %d, %q x %d", limit, tc.str, tc.n)
		}
	}
}

// TestSplitRepeatCheckContext pins that Split and Repeat check the context
// first, as CallBuiltin does.
func TestSplitRepeatCheckContext(t *testing.T) {
	env := newParityEnv(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	ran := false
	env.PutGlobal(lisp.Symbol("probe"), lisp.FunInPackage(lisp.DefaultUserPackage, "probe", lisp.Formals(),
		func(env *lisp.LEnv, _ *lisp.LVal) *lisp.LVal {
			cancel()
			ran = true
			_, err := Split(env, "a:b", ":")
			assert.Equal(t, lisp.CondContextCancelled, lisp.ConditionOf(err))
			_, err = Repeat(env, "a", 2)
			assert.Equal(t, lisp.CondContextCancelled, lisp.ConditionOf(err))
			return lisp.Nil()
		}))
	env.EvalContext(ctx, lisp.SExpr([]*lisp.LVal{lisp.Symbol("probe")}))
	require.True(t, ran)
}
