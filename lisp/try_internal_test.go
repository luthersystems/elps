// Copyright © 2026 The ELPS authors

package lisp

import (
	"errors"
	"testing"

	"github.com/stretchr/testify/require"
)

// TestTryCatchesRefusesStops pins the rule of luthersystems/elps#973 that
// try drops no condition that stops an evaluation.  The black-box tests in
// try_test.go cannot see it alone: the evaluator raises a stop again on the
// fallback's first step, so they pass without the rule too.
func TestTryCatchesRefusesStops(t *testing.T) {
	env := NewEnv(nil)
	rt := env.Runtime
	// A program may define its own condition under a stop condition;
	// handler-bind matches it as the parent, so try refuses it too.
	require.NoError(t, rt.DefineCondition("my-stop", CondContextCancelled))
	for _, cond := range []string{CondStepLimitExceeded, CondStepBudgetExceeded, CondContextCancelled, "my-stop"} {
		require.False(t, tryCatches(rt, env.ErrorCondition(cond, errors.New("stop"))), cond)
	}
	for _, cond := range []string{"error", CondArgumentError, CondEvalNestingExceeded, "my-condition"} {
		require.True(t, tryCatches(rt, env.ErrorCondition(cond, errors.New("x"))), cond)
	}
}
