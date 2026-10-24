// Copyright © 2026 The ELPS authors

package elpstest_test

import (
	"fmt"
	"testing"

	"github.com/luthersystems/elps/elpstest"
	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// chargeArg charges as many steps as its first argument says and returns
// the argument, or the charge's error.
func chargeArg(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	n := args.Cells[0]
	if err := env.ChargeSteps(int64(n.Int)); err != nil {
		return lisp.Error(err)
	}
	return n
}

func TestCallBuiltinFuncReturnsItsCharge(t *testing.T) {
	t.Parallel()
	env := elpstest.NewStepEnv()
	for _, n := range []int{0, 1, 7} {
		out, steps := elpstest.CallBuiltinFunc(t, env, chargeArg, lisp.Int(n))
		assert.Equal(t, n, out.Int)
		assert.Equal(t, int64(n), steps)
	}
	assert.Equal(t, int64(8), env.Runtime.TotalSteps(), "the charges accumulate in TotalSteps")
}

func TestStepsChargedNeedsCounting(t *testing.T) {
	t.Parallel()
	// Without a context or a limit the runtime counts nothing, so a charge
	// measures 0.  This is why the helpers start from NewStepEnv.
	plain := lisp.NewEnv(nil)
	assert.Zero(t, elpstest.StepsCharged(plain, func() { _ = plain.ChargeSteps(5) }))

	elpstest.EnableSteps(plain)
	assert.Equal(t, int64(5), elpstest.StepsCharged(plain, func() { _ = plain.ChargeSteps(5) }))
}

func TestStepsChargedAcrossEvaluations(t *testing.T) {
	t.Parallel()
	// StepsCharged reads TotalSteps, so a measured function that starts a
	// new top-level evaluation (which resets Steps) still measures right.
	env := elpstest.NewStepEnv()
	_ = env.ChargeSteps(3)
	got := elpstest.StepsCharged(env, func() {
		env.Runtime.ResetSteps()
		_ = env.ChargeSteps(2)
	})
	assert.Equal(t, int64(2), got)
}

func TestNewStepEnvHasNoLimit(t *testing.T) {
	t.Parallel()
	env := elpstest.NewStepEnv()
	require.NoError(t, env.ChargeSteps(1<<40), "counting is on, with no limit to exceed")
	budget, _ := env.Runtime.StepBudget()
	assert.Zero(t, budget)
}

// fatalTB records a Fatalf instead of ending the test, so the failure path
// of CallBuiltinFunc can be tested.
type fatalTB struct {
	testing.TB
	msg string
}

func (f *fatalTB) Helper() {}

func (f *fatalTB) Fatalf(format string, args ...any) {
	f.msg = fmt.Sprintf(format, args...)
}

func TestCallBuiltinFuncFails(t *testing.T) {
	t.Parallel()
	for name, tc := range map[string]struct {
		fn   lisp.LBuiltin
		want string
	}{
		"error": {func(env *lisp.LEnv, _ *lisp.LVal) *lisp.LVal { return env.Errorf("boom") }, "boom"},
		"nil":   {func(*lisp.LEnv, *lisp.LVal) *lisp.LVal { return nil }, "Go nil"},
		"tail call": {func(*lisp.LEnv, *lisp.LVal) *lisp.LVal {
			return &lisp.LVal{Type: lisp.LMarkTerminal}
		}, "tail call"},
	} {
		tb := &fatalTB{TB: t}
		elpstest.CallBuiltinFunc(tb, elpstest.NewStepEnv(), tc.fn)
		assert.Contains(t, tb.msg, tc.want, name)
	}
}
