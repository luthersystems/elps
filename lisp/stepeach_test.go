// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"context"
	"fmt"
	"math"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// stepLoop is the reference StepEach must match: n calls to Step, stopping
// at the first error.  It is the loop substrate wrote by hand
// (elpsstep.Charge, luthersystems/substrate#837) before StepEach existed.
func stepLoop(env *lisp.LEnv, n int64) *lisp.LVal {
	for range n {
		if err := env.Step(); err != nil {
			return lisp.Error(err)
		}
	}
	return lisp.Nil()
}

// stepEachCase is one runtime state: the limits it has, the context it has,
// and the steps already charged before the call under test.
type stepEachCase struct {
	maxSteps int64
	budget   int64
	ctx      string // "", "live" or "done"
	used     int64
}

func (c stepEachCase) String() string {
	return fmt.Sprintf("max=%d/budget=%d/ctx=%q/used=%d", c.maxSteps, c.budget, c.ctx, c.used)
}

// env builds a bare runtime in state c.  The steps already used are charged
// with ChargeSteps under a live context, so they can overrun a limit and
// set the saturation flags exactly as earlier work would.
func (c stepEachCase) env(t *testing.T) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	require.NoError(t, lisp.GoError(lisp.WithMaxSteps(c.maxSteps)(env)))
	env.Runtime.SetStepBudget(c.budget)
	if c.used > 0 {
		require.NoError(t, lisp.GoError(lisp.WithContext(context.Background())(env)))
		_ = env.ChargeSteps(c.used)
	}
	var ctx context.Context
	switch c.ctx {
	case "live":
		ctx = context.Background()
	case "done":
		done, cancel := context.WithCancel(context.Background())
		cancel()
		ctx = done
	}
	require.NoError(t, lisp.GoError(lisp.WithContext(ctx)(env)))
	return env
}

type stepState struct {
	steps, total, budget, budgetUsed int64
}

func stateOf(env *lisp.LEnv) stepState {
	budget, used := env.Runtime.StepBudget()
	return stepState{env.Runtime.Steps(), env.Runtime.TotalSteps(), budget, used}
}

// requireSameCharge runs StepEach(n) and the reference loop on two runtimes
// in state c and requires the same result (type, condition and message) and
// the same counters afterwards.
func requireSameCharge(t *testing.T, c stepEachCase, n int64) {
	t.Helper()
	want, got := c.env(t), c.env(t)
	wantRes, gotRes := stepLoop(want, n), lisp.ValueOrError(lisp.Nil(), got.StepEach(n))
	require.Equal(t, wantRes.Type, gotRes.Type, "%v n=%d: result %v, want %v", c, n, gotRes, wantRes)
	if wantRes.Type == lisp.LError {
		assert.Equal(t, wantRes.Str, gotRes.Str, "%v n=%d: condition", c, n)
		assert.Equal(t, lisp.GoError(wantRes).Error(), lisp.GoError(gotRes).Error(), "%v n=%d: message", c, n)
	}
	assert.Equal(t, stateOf(want), stateOf(got), "%v n=%d: counters", c, n)
}

// TestStepEachMatchesStepLoop is the contract of StepEach: in every runtime
// state, with every charge, it returns what n Step calls return and leaves
// the counters where they leave them.  In particular a budget that runs out
// part of the way stops at the step that overran it, where ChargeSteps(n)
// would record all n (TestStepEachStopsWhereChargeStepsOvershoots).
func TestStepEachMatchesStepLoop(t *testing.T) {
	t.Parallel()
	for _, maxSteps := range []int64{0, 1, 3, 6} {
		for _, budget := range []int64{0, 1, 4, 6} {
			for _, ctx := range []string{"", "live", "done"} {
				for used := range int64(9) {
					c := stepEachCase{maxSteps, budget, ctx, used}
					for n := range int64(10) {
						requireSameCharge(t, c, n)
					}
				}
			}
		}
	}
}

// TestStepEachNearMaxInt64 covers the counters' saturation: a limit of
// math.MaxInt64 that n steps cannot reach, and counters that saturate on
// the way, with and without a limit to see it.
func TestStepEachNearMaxInt64(t *testing.T) {
	t.Parallel()
	near := int64(math.MaxInt64 - 2)
	for _, c := range []stepEachCase{
		{maxSteps: math.MaxInt64, ctx: "live"},
		{budget: math.MaxInt64, ctx: "live"},
		{maxSteps: math.MaxInt64, used: near},
		{budget: math.MaxInt64, used: near},
		{ctx: "live", used: near},
		{maxSteps: math.MaxInt64, budget: math.MaxInt64, used: near},
		{maxSteps: math.MaxInt64, used: math.MaxInt64},
		{budget: math.MaxInt64, used: math.MaxInt64},
	} {
		for _, n := range []int64{1, 2, 3, 5} {
			requireSameCharge(t, c, n)
		}
	}
	// A charge the loop could not finish in a test's lifetime: StepEach
	// does not loop, so it records all of it when no limit is in reach.
	env := stepEachCase{maxSteps: math.MaxInt64, ctx: "live"}.env(t)
	require.NoError(t, env.StepEach(math.MaxInt64-1))
	assert.Equal(t, int64(math.MaxInt64-1), env.Runtime.Steps())
}

// TestStepEachStopsWhereChargeStepsOvershoots shows why StepEach exists.
// Go code that replaces n evaluated forms must, when the budget runs out,
// record the usage the forms would have recorded: the evaluator stops at
// the step that overran the budget.  ChargeSteps(n) adds all n first.
func TestStepEachStopsWhereChargeStepsOvershoots(t *testing.T) {
	t.Parallel()
	c := stepEachCase{budget: 2, ctx: "live"}

	each := c.env(t)
	res := lisp.ValueOrError(lisp.Nil(), each.StepEach(5))
	require.Equal(t, lisp.CondStepBudgetExceeded, res.Str)
	_, used := each.Runtime.StepBudget()
	assert.Equal(t, int64(3), used, "StepEach stops at the step that overran the budget")

	bulk := c.env(t)
	res = lisp.ValueOrError(lisp.Nil(), bulk.ChargeSteps(5))
	require.Equal(t, lisp.CondStepBudgetExceeded, res.Str)
	_, used = bulk.Runtime.StepBudget()
	assert.Equal(t, int64(5), used, "ChargeSteps records the whole charge")
}

// TestStepEachChecksContext pins the context check each Step makes: a done
// context fails the charge at its first step, after charging that step,
// unless the step also overruns a limit, which is reported first.
func TestStepEachChecksContext(t *testing.T) {
	t.Parallel()
	env := stepEachCase{ctx: "done"}.env(t)
	res := lisp.ValueOrError(lisp.Nil(), env.StepEach(4))
	require.Equal(t, lisp.LError, res.Type)
	assert.Equal(t, lisp.CondContextCancelled, res.Str)
	assert.Contains(t, lisp.GoError(res).Error(), "context cancelled: context canceled")
	assert.Equal(t, int64(1), env.Runtime.Steps())

	env = stepEachCase{maxSteps: 2, ctx: "done", used: 2}.env(t)
	res = lisp.ValueOrError(lisp.Nil(), env.StepEach(4))
	assert.Equal(t, lisp.CondStepLimitExceeded, res.Str)
	assert.Equal(t, int64(3), env.Runtime.Steps())
}

// TestStepEachZeroAndNegative pins the edges StepEach shares with
// ChargeSteps: zero charges and checks nothing, and a negative charge is an
// error that changes no counter.
func TestStepEachZeroAndNegative(t *testing.T) {
	t.Parallel()
	env := stepEachCase{maxSteps: 1, ctx: "done", used: 5}.env(t)
	before := stateOf(env)
	assert.NoError(t, env.StepEach(0), "zero checks nothing, not even an exceeded limit")
	res := lisp.ValueOrError(lisp.Nil(), env.StepEach(-1))
	require.Equal(t, lisp.LError, res.Type)
	assert.Contains(t, lisp.GoError(res).Error(), "negative step charge: -1")
	assert.Equal(t, before, stateOf(env))
}

// TestStepEachNoCountingWithoutLimits pins the fast path: with no limit and
// no context the runtime counts nothing, and StepEach neither counts nor
// allocates.
func TestStepEachNoCountingWithoutLimits(t *testing.T) {
	// Not parallel: AllocsPerRun refuses to run in a parallel test.
	env := lisp.NewEnv(nil)
	require.NoError(t, env.StepEach(1000))
	assert.Zero(t, env.Runtime.TotalSteps())

	live := stepEachCase{maxSteps: math.MaxInt64, budget: math.MaxInt64, ctx: "live"}.env(t)
	allocs := testing.AllocsPerRun(100, func() {
		if lisp.ValueOrError(lisp.Nil(), live.StepEach(16)).Type == lisp.LError {
			t.Fatal("unexpected error")
		}
	})
	assert.Zero(t, allocs)
}
