// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/elpsutil"
	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

const budgetProgram = `(progn (charge 7) (spin 20))`

func requireStepBudget(t *testing.T, res *lisp.LVal) {
	t.Helper()
	require.Equal(t, lisp.LError, res.Type, "expected a step-budget error, got %v", res)
	require.Equal(t, lisp.CondStepBudgetExceeded, res.Str, "wrong condition: %v", res)
}

// usedBy returns the budget usage of one evaluation of budgetProgram.
func usedBy(t *testing.T, env *lisp.LEnv) int64 {
	t.Helper()
	env.Runtime.SetStepBudget(1 << 40)
	require.NotEqual(t, lisp.LError, env.LoadString("test", budgetProgram).Type)
	_, used := env.Runtime.StepBudget()
	require.Positive(t, used)
	return used
}

func TestStepBudgetDefaultUnlimited(t *testing.T) {
	env, _ := chargeTestEnv(t)
	budget, used := env.Runtime.StepBudget()
	assert.Zero(t, budget)
	assert.Zero(t, used)
	require.NotEqual(t, lisp.LError, env.LoadString("test", `(spin 1000)`).Type)
	_, used = env.Runtime.StepBudget()
	assert.Zero(t, used, "no budget counts nothing")
	assert.Zero(t, env.Runtime.Steps(), "no budget keeps the fast path")
}

// TestStepBudgetSharedAcrossEvals pins that the budget spans top-level
// evaluations instead of refilling like WithMaxSteps.
func TestStepBudgetSharedAcrossEvals(t *testing.T) {
	env, _ := chargeTestEnv(t)
	per := usedBy(t, env)

	env.Runtime.SetStepBudget(2*per + per/2)
	require.NotEqual(t, lisp.LError, env.LoadString("test", budgetProgram).Type)
	require.NotEqual(t, lisp.LError, env.LoadString("test", budgetProgram).Type)
	_, used := env.Runtime.StepBudget()
	assert.Equal(t, 2*per, used)
	requireStepBudget(t, env.LoadString("test", budgetProgram))
	// Exhausted: every later evaluation and charge fails.
	requireStepBudget(t, env.LoadString("test", `(+ 1 2)`))
	requireStepBudget(t, env.ChargeSteps(1))

	// Reset refills without changing the budget.
	env.Runtime.ResetStepBudget()
	budget, used := env.Runtime.StepBudget()
	assert.Equal(t, 2*per+per/2, budget)
	assert.Zero(t, used)
	require.NotEqual(t, lisp.LError, env.LoadString("test", budgetProgram).Type)

	// Clearing restores unlimited behaviour.
	env.Runtime.SetStepBudget(0)
	require.NotEqual(t, lisp.LError, env.LoadString("test", `(spin 1000)`).Type)
}

func TestStepBudgetChargeSteps(t *testing.T) {
	env, loop := chargeTestEnv(t)
	env.Runtime.SetStepBudget(100)
	require.Equal(t, lisp.LSExpr, env.ChargeSteps(60).Type)
	requireStepBudget(t, env.LoadString("test", `(native-sum (make-sequence 0 100))`))
	assert.Less(t, loop.processed, 40)
	_, used := env.Runtime.StepBudget()
	assert.Greater(t, used, int64(100))
}

// TestStepBudgetDistinctFromStepLimit pins both conditions and their
// precedence.
func TestStepBudgetDistinctFromStepLimit(t *testing.T) {
	env, _ := chargeTestEnv(t, lisp.WithMaxSteps(1000))
	env.Runtime.SetStepBudget(1500)
	require.Equal(t, "ok", env.LoadString("test", `(charge 900)`).String())
	requireStepLimit(t, env.LoadString("test", `(charge 5000)`)) // both exceeded: limit wins
	env.Runtime.SetStepBudget(1500)
	require.Equal(t, "ok", env.LoadString("test", `(charge 900)`).String())
	requireStepBudget(t, env.LoadString("test", `(charge 900)`))

	// A handler runs on the exhausted budget, so it cannot swallow it.
	res := env.LoadString("test", `(handler-bind ((step-budget-exceeded (lambda (&rest _) 'caught))) (charge 1))`)
	requireStepBudget(t, res)
}

func TestStepBudgetSaturates(t *testing.T) {
	env, _ := chargeTestEnv(t)
	env.Runtime.SetStepBudget(1<<63 - 1)
	requireStepBudget(t, env.LoadString("test", `(progn (charge 10) (charge 9223372036854775807))`))
	_, used := env.Runtime.StepBudget()
	assert.Equal(t, int64(1<<63-1), used)
	env.Runtime.SetStepBudget(-5)
	budget, _ := env.Runtime.StepBudget()
	assert.Zero(t, budget)
}

// TestStepBudgetTemplateParity pins identical counting on a cold env and a
// template VM, VMWithStepBudget, and that the budget is not published.
func TestStepBudgetTemplateParity(t *testing.T) {
	cold, _ := chargeTestEnv(t)
	coldUsed := usedBy(t, cold)

	source := templateTestEnv(t)
	require.NoError(t, lisp.GoError(source.InPackage(lisp.String(lisp.DefaultUserPackage))))
	source.AddBuiltins(true, elpsutil.Function("charge", lisp.Formals("n"), chargeBuiltin))
	require.NotEqual(t, lisp.LError, source.LoadString("test", spinDefn).Type)
	source.Runtime.SetStepBudget(5)
	tmpl, err := lisp.NewTemplate(source, templateCorePolicy())
	require.NoError(t, err)

	plain, err := tmpl.NewVM()
	require.NoError(t, err)
	budget, _ := plain.Runtime.StepBudget()
	assert.Zero(t, budget, "a template never publishes the source's budget")
	assert.Equal(t, coldUsed, usedBy(t, plain))

	vm, err := tmpl.NewVM(lisp.VMWithStepBudget(coldUsed + coldUsed/2))
	require.NoError(t, err)
	require.NotEqual(t, lisp.LError, vm.LoadString("test", budgetProgram).Type)
	_, used := vm.Runtime.StepBudget()
	assert.Equal(t, coldUsed, used)
	requireStepBudget(t, vm.LoadString("test", budgetProgram))

	other, err := tmpl.NewVM(lisp.VMWithStepBudget(coldUsed + coldUsed/2))
	require.NoError(t, err)
	require.NotEqual(t, lisp.LError, other.LoadString("test", budgetProgram).Type, "VM budgets are independent")
}
