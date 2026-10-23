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

// TestJoinParity compares Join with the string:join builtin: the same value,
// or an error with the same condition and message, under several allocation
// caps, and the same context-cancelled condition after a cancel.
func TestJoinParity(t *testing.T) {
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	require.Equal(t, lisp.LSExpr, lisp.InitializeUserEnv(env).Type)

	big := strings.Repeat("x", 2048)
	cases := []struct {
		sep   string
		parts []string
	}{
		{", ", nil},
		{", ", []string{"a"}},
		{", ", []string{"a", "b", "c"}},
		{"", []string{"a", "b"}},
		{"--", []string{big, big}},
		{strings.Repeat("s", 40), []string{"a", "b", "c"}},
	}
	for _, limit := range []int{0, 8, 64, 5000} {
		env.Runtime.MaxAlloc = limit
		for _, tc := range cases {
			cells := make([]*lisp.LVal, len(tc.parts))
			for i, p := range tc.parts {
				cells[i] = lisp.String(p)
			}
			want := builtinJoin(env, lisp.QExpr([]*lisp.LVal{lisp.QExpr(cells), lisp.String(tc.sep)}))
			got, err := Join(env, tc.parts, tc.sep)
			if want.Type == lisp.LError {
				var e *lisp.ErrorVal
				require.ErrorAs(t, err, &e, "limit %d, %q", limit, tc.parts)
				assert.Equal(t, want.Str, e.Condition())
				assert.Equal(t, (*lisp.ErrorVal)(want).ErrorMessage(), e.ErrorMessage())
			} else {
				require.NoError(t, err, "limit %d, %q", limit, tc.parts)
				assert.Equal(t, want.Str, got)
			}
		}
	}
	env.Runtime.MaxAlloc = 0

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	ran := false
	env.PutGlobal(lisp.Symbol("join-probe"), lisp.FunInPackage(lisp.DefaultUserPackage, "join-probe", lisp.Formals(),
		func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
			cancel()
			ran = true
			_, err := Join(env, []string{"a", "b"}, ",")
			assert.Equal(t, lisp.CondContextCancelled, lisp.ConditionOf(err))
			assert.Equal(t, env.CheckContext().Str, lisp.ConditionOf(err), "Join checks the context first, as CallBuiltin does")
			return lisp.Nil()
		}))
	env.EvalContext(ctx, lisp.SExpr([]*lisp.LVal{lisp.Symbol("join-probe")}))
	require.True(t, ran)
}
