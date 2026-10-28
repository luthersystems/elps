// Copyright © 2026 The ELPS authors

package libschema

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/require"
)

// TestTypeNameTable checks that each type name in typeNameChecks accepts
// the values the validator s:has-key builds for it accepts, with the same
// condition.
func TestTypeNameTable(t *testing.T) {
	env := lisp.NewEnv(nil)
	require.False(t, lisp.InitializeUserEnv(env).IsError())
	values := []*lisp.LVal{
		lisp.Int(1), lisp.Float(1.5), lisp.String("s"), lisp.Nil(), lisp.Symbol("true"),
		lisp.Symbol("false"), lisp.Symbol("x"), lisp.QExpr([]*lisp.LVal{lisp.Int(1)}),
		lisp.SortedMap(), lisp.Array(nil, []*lisp.LVal{lisp.Int(1)}), env.GetGlobal(lisp.Symbol("lisp:car")),
	}
	for name := range typeNameChecks {
		validator := getHandler(env, lisp.String(name), name, nil)
		require.False(t, validator.IsError(), name)
		for _, v := range values {
			want := applyConstraint(env, validator, v)
			got := checkTypeName(env, name, v)
			if want.IsError() {
				require.NotNil(t, got, "%s %v", name, v)
				require.Equal(t, want.Str, got.Str, "%s %v", name, v)
			} else {
				require.Nil(t, got, "%s %v", name, v)
			}
		}
	}
}
