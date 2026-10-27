// Copyright © 2026 The ELPS authors

package libschema

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/require"
)

// TestRecordErrorKeepsOtherConditions checks that recordError adds the type
// name only to libschema's own conditions.  Any other condition, such as
// one a nested :check raised, keeps its data.
func TestRecordErrorKeepsOtherConditions(t *testing.T) {
	env := lisp.NewEnv(nil)
	require.NoError(t, lisp.GoError(lisp.InitializeUserEnv(env)))
	desc := &recordDesc{display: "user:order"}

	own := recordError(env, desc, env.ErrorConditionf(WrongType, "Input is not a string"))
	require.Equal(t, WrongType, own.Str)
	require.Contains(t, own.String(), "user:order")

	other := env.ErrorCondition("too-big", lisp.Int(150))
	require.Same(t, other, recordError(env, desc, other))
}
