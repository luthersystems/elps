// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"context"
	"fmt"
	"math"
	"strconv"
	"testing"

	"github.com/luthersystems/elps/elpsutil"
	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// chargeBuiltin is (charge n): charge n steps, returning the charge's error
// or 'ok.  It carries no state, so templates may publish it.
func chargeBuiltin(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	if lerr := env.ChargeSteps(int64(args.Cells[0].Int)); lerr.Type == lisp.LError {
		return lerr
	}
	return lisp.Symbol("ok")
}

// nativeLoop records how far a per-element charging builtin got.
type nativeLoop struct {
	stepsAtEntry int64
	processed    int
}

// install registers (native-sum xs): a native fold that charges one step per
// element before processing it, the pattern ChargeSteps exists for.  Also
// installs (charge n) and (refund-on-error f), which calls f and, if it
// fails, resets the step counter before returning the error so a Lisp
// handler can run.  Without the refund the handler shares the exhausted
// budget and fails at its own first step (see docs/lang.md, "Step Limits").
func (l *nativeLoop) install(env *lisp.LEnv) {
	env.AddBuiltins(true,
		elpsutil.Function("charge", lisp.Formals("n"), chargeBuiltin),
		elpsutil.Function("native-sum", lisp.Formals("xs"), func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
			l.stepsAtEntry = env.Runtime.Steps()
			l.processed = 0
			sum := 0
			for _, x := range args.Cells[0].Cells {
				if lerr := env.ChargeSteps(1); lerr.Type == lisp.LError {
					return lerr
				}
				sum += x.Int
				l.processed++
			}
			return lisp.Int(sum)
		}),
		elpsutil.Function("refund-on-error", lisp.Formals("f"), func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
			res := env.FunCall(args.Cells[0], lisp.QExpr(nil))
			if res.Type == lisp.LError {
				env.Runtime.ResetSteps()
			}
			return res
		}),
	)
}

func chargeTestEnv(t *testing.T, cfg ...lisp.Config) (*lisp.LEnv, *nativeLoop) {
	t.Helper()
	env := newLimitTestEnv(t, cfg...)
	loop := &nativeLoop{}
	loop.install(env)
	require.NotEqual(t, lisp.LError, env.LoadString("test", spinDefn).Type)
	return env, loop
}

func requireStepLimit(t *testing.T, res *lisp.LVal) {
	t.Helper()
	require.Equal(t, lisp.LError, res.Type, "expected a step-limit error, got %v", res)
	require.Equal(t, lisp.CondStepLimitExceeded, res.Str, "wrong condition: %v", res)
}

// TestChargeStepsWithinBudget pins that a charge inside the budget succeeds
// and adds exactly n to Steps and TotalSteps.
func TestChargeStepsWithinBudget(t *testing.T) {
	env, _ := chargeTestEnv(t, lisp.WithMaxSteps(1000))

	res := env.LoadString("test", `(charge 0)`)
	require.Equal(t, "ok", res.String())
	base := env.Runtime.Steps()
	baseTotal := env.Runtime.TotalSteps()

	res = env.LoadString("test", `(charge 100)`)
	require.Equal(t, "ok", res.String())
	assert.Equal(t, base+100, env.Runtime.Steps(), "Steps must include the charge")
	assert.Equal(t, baseTotal+base+100, env.Runtime.TotalSteps(), "TotalSteps must include the charge")

	// A charge that lands exactly on the budget is allowed, as the
	// evaluator allows a step that lands exactly on it.
	res = env.LoadString("test", `(charge `+strconv.FormatInt(1000-base, 10)+`)`)
	require.Equal(t, "ok", res.String())
	assert.Equal(t, int64(1000), env.Runtime.Steps())
}

// TestChargeStepsPastBudgetMatchesEvaluator pins condition and message
// parity: a native overrun is indistinguishable from an evaluator overrun.
func TestChargeStepsPastBudgetMatchesEvaluator(t *testing.T) {
	env, _ := chargeTestEnv(t, lisp.WithMaxSteps(1000))

	evalErr := env.LoadString("test", `(spin 100000)`)
	requireStepLimit(t, evalErr)
	assert.Equal(t, int64(1001), env.Runtime.Steps(), "the evaluator records the overrunning step")

	nativeErr := env.LoadString("test", `(charge 5000)`)
	requireStepLimit(t, nativeErr)
	assert.Equal(t, evalErr.Cells[0].Str, nativeErr.Cells[0].Str,
		"the native charge must raise the evaluator's message")
	assert.Greater(t, env.Runtime.Steps(), int64(5000), "the full charge is recorded")

	// Over budget, later charges and later nested evaluation fail the same
	// way within the same top-level evaluation.  A builtin that ignores the
	// first error observes this directly.
	var after [3]*lisp.LVal
	env.AddBuiltins(true, elpsutil.Function("overrun-and-continue", lisp.Formals(), func(env *lisp.LEnv, _ *lisp.LVal) *lisp.LVal {
		after[0] = env.ChargeSteps(5000)
		after[1] = env.ChargeSteps(1)
		after[2] = env.Eval(lisp.SExpr([]*lisp.LVal{lisp.Symbol("+"), lisp.Int(1), lisp.Int(2)}))
		return lisp.Nil()
	}))
	require.NotEqual(t, lisp.LError, env.LoadString("test", `(overrun-and-continue)`).Type)
	for i, v := range after {
		requireStepLimit(t, v)
		assert.Equal(t, evalErr.Cells[0].Str, v.Cells[0].Str, "result %d", i)
	}
}

// TestChargeStepsHandlerBindParity pins that a Lisp handler-bind selects
// its handler for a native overrun exactly as for an evaluator overrun, and
// receives the same condition and data.
func TestChargeStepsHandlerBindParity(t *testing.T) {
	env, _ := chargeTestEnv(t, lisp.WithMaxSteps(1000))
	const handle = `(handler-bind ((step-limit-exceeded (lambda (c &rest args) (list c args))))
	  (refund-on-error (lambda () %s)))`

	evaluator := env.LoadString("test", fmt.Sprintf(handle, `(spin 100000)`))
	require.NotEqual(t, lisp.LError, evaluator.Type, "handler did not run: %v", evaluator)
	native := env.LoadString("test", fmt.Sprintf(handle, `(charge 5000)`))
	require.NotEqual(t, lisp.LError, native.Type, "handler did not run: %v", native)

	assert.Equal(t, `'('step-limit-exceeded '("step limit exceeded (1000 steps)"))`, evaluator.String())
	assert.Equal(t, evaluator.String(), native.String())

	// A handler for another condition does not intercept it.
	res := env.LoadString("test",
		`(handler-bind ((context-cancelled (lambda (c &rest _) 'caught))) (charge 5000))`)
	requireStepLimit(t, res)
}

// TestChargeStepsWithoutLimitsIsNoOp pins the default embedding: with
// neither a step budget nor a context the evaluator counts nothing, and a
// charge -- however large -- counts nothing either.
func TestChargeStepsWithoutLimitsIsNoOp(t *testing.T) {
	env, _ := chargeTestEnv(t)

	res := env.LoadString("test", `(progn (charge 100) (charge 9223372036854775807) (+ 1 2))`)
	require.Equal(t, "3", res.String())
	assert.Zero(t, env.Runtime.Steps())
	assert.Zero(t, env.Runtime.TotalSteps())
}

// TestChargeStepsWithContextOnly pins that a context without a budget
// counts charges (as it counts evaluator steps) and that a charge observes
// cancellation, so a native loop is interruptible by a deadline.
func TestChargeStepsWithContextOnly(t *testing.T) {
	env, _ := chargeTestEnv(t)

	res := env.LoadStringContext(context.Background(), "test", `(charge 0)`)
	require.Equal(t, "ok", res.String())
	base := env.Runtime.Steps()
	require.Positive(t, base, "a context enables step counting")

	res = env.LoadStringContext(context.Background(), "test", `(charge 100)`)
	require.Equal(t, "ok", res.String())
	assert.Equal(t, base+100, env.Runtime.Steps())

	// Cancel from inside a native loop: the next charge reports it.
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	charged := 0
	env.AddBuiltins(true, elpsutil.Function("cancel-midway", lisp.Formals(), func(env *lisp.LEnv, _ *lisp.LVal) *lisp.LVal {
		for i := range 10 {
			if i == 3 {
				cancel()
			}
			if lerr := env.ChargeSteps(1); lerr.Type == lisp.LError {
				return lerr
			}
			charged++
		}
		return lisp.Nil()
	}))
	res = env.LoadStringContext(ctx, "test", `(cancel-midway)`)
	require.Equal(t, lisp.LError, res.Type)
	assert.Equal(t, lisp.CondContextCancelled, res.Str)
	assert.Equal(t, 3, charged)
}

// TestChargeStepsZeroAndNegative pins the degenerate charges: zero is a
// no-op that checks nothing; negative is an ordinary error under every
// configuration and refunds nothing.
func TestChargeStepsZeroAndNegative(t *testing.T) {
	for _, tc := range []struct {
		name string
		cfg  []lisp.Config
	}{
		{"no-limits", nil},
		{"budget", []lisp.Config{lisp.WithMaxSteps(1000)}},
	} {
		t.Run(tc.name, func(t *testing.T) {
			env, _ := chargeTestEnv(t, tc.cfg...)

			require.Equal(t, "ok", env.LoadString("test", `(charge 0)`).String())
			base := env.Runtime.Steps()
			res := env.LoadString("test", `(charge -5)`)
			require.Equal(t, lisp.LError, res.Type)
			assert.Equal(t, "error", res.Str, "a negative charge is an ordinary error, not a limit")
			assert.Contains(t, res.String(), "negative step charge: -5")
			assert.Equal(t, base, env.Runtime.Steps(), "a negative charge must not refund steps")

			res = env.LoadString("test", `(charge -9223372036854775808)`)
			require.Equal(t, lisp.LError, res.Type)
			assert.Equal(t, base, env.Runtime.Steps())
		})
	}

	t.Run("zero-after-overrun", func(t *testing.T) {
		env, _ := chargeTestEnv(t, lisp.WithMaxSteps(1000))
		// Direct Go call after the evaluation ended: over budget, a zero
		// charge still reports success and does not move the counter.
		requireStepLimit(t, env.LoadString("test", `(charge 5000)`))
		steps := env.Runtime.Steps()
		assert.Equal(t, lisp.LSExpr, env.ChargeSteps(0).Type)
		assert.Equal(t, steps, env.Runtime.Steps())
		requireStepLimit(t, env.ChargeSteps(1))
	})
}

// TestChargeStepsSaturates pins overflow handling: counters saturate at
// math.MaxInt64 instead of wrapping negative, which would refill an
// exhausted budget.
func TestChargeStepsSaturates(t *testing.T) {
	env, _ := chargeTestEnv(t, lisp.WithMaxSteps(1000))

	res := env.LoadString("test", `(progn (charge 10) (charge 9223372036854775807))`)
	requireStepLimit(t, res)
	assert.Equal(t, int64(math.MaxInt64), env.Runtime.Steps())
	assert.Equal(t, int64(math.MaxInt64), env.Runtime.TotalSteps())

	// Further charges stay saturated and keep failing.
	requireStepLimit(t, env.ChargeSteps(math.MaxInt64))
	assert.Equal(t, int64(math.MaxInt64), env.Runtime.Steps())

	// A handler running under the saturated budget cannot wrap it either.
	res = env.LoadString("test", `(handler-bind ((step-limit-exceeded (lambda (&rest _) (+ 1 2))))
	  (charge 9223372036854775807))`)
	requireStepLimit(t, res)
	assert.Equal(t, int64(math.MaxInt64), env.Runtime.Steps())

	// The next top-level evaluation starts a fresh budget; the lifetime
	// total stays saturated.
	res = env.LoadString("test", `(+ 1 2)`)
	require.Equal(t, "3", res.String())
	assert.Less(t, env.Runtime.Steps(), int64(1000))
	assert.Equal(t, int64(math.MaxInt64), env.Runtime.TotalSteps())
	env.Runtime.ResetSteps()
	assert.Equal(t, int64(math.MaxInt64), env.Runtime.TotalSteps())

	// The largest possible budget is still a budget: the counter may reach
	// it exactly, and a count that saturates past it trips the limit.
	env, _ = chargeTestEnv(t, lisp.WithMaxSteps(math.MaxInt64))
	env.Runtime.ResetSteps()
	require.Equal(t, lisp.LSExpr, env.ChargeSteps(math.MaxInt64).Type, "landing exactly on the budget is allowed")
	requireStepLimit(t, env.ChargeSteps(1))
	assert.Equal(t, int64(math.MaxInt64), env.Runtime.Steps())
	res = env.LoadString("test", `(progn (charge 9223372036854775807) (+ 1 2))`)
	requireStepLimit(t, res)
	// An evaluator step past a saturated count trips it too.
	env.AddBuiltins(true, elpsutil.Function("saturate-then-eval", lisp.Formals(), func(env *lisp.LEnv, _ *lisp.LVal) *lisp.LVal {
		env.Runtime.ResetSteps()
		if lerr := env.ChargeSteps(math.MaxInt64); lerr.Type == lisp.LError {
			return lisp.String("charge failed: " + lerr.String())
		}
		return env.Eval(lisp.Int(1))
	}))
	requireStepLimit(t, env.LoadString("test", `(saturate-then-eval)`))
	// ResetSteps clears the overflow along with the count.
	res = env.LoadString("test", `(+ 1 2)`)
	require.Equal(t, "3", res.String())
}

// TestChargeStepsNativeLoopStopsAtBudget pins the motivating pattern: a
// native fold charging one step per element stops exactly where the budget
// runs out, leaving the counter where an evaluator overrun would.
func TestChargeStepsNativeLoopStopsAtBudget(t *testing.T) {
	const budget = 500
	env, loop := chargeTestEnv(t, lisp.WithMaxSteps(budget))

	res := env.LoadString("test", `(native-sum (make-sequence 0 10))`)
	require.Equal(t, "45", res.String(), "a short fold fits the budget")
	assert.Equal(t, 10, loop.processed)

	res = env.LoadString("test", `(native-sum (make-sequence 0 10000))`)
	requireStepLimit(t, res)
	assert.Equal(t, int(budget-loop.stepsAtEntry), loop.processed,
		"the loop must process exactly the elements the remaining budget pays for")
	assert.Equal(t, int64(budget+1), env.Runtime.Steps(),
		"a per-element overrun leaves the counter one past the budget, as the evaluator does")
}

// TestChargeStepsBudgetScope pins the interaction with ResetSteps and the
// exported entry points: nested evaluation shares the budget, a new
// top-level evaluation refills it, and ResetSteps folds charges into
// TotalSteps.
func TestChargeStepsBudgetScope(t *testing.T) {
	env, _ := chargeTestEnv(t, lisp.WithMaxSteps(1000))

	// Nested evaluation shares the enclosing budget.
	res := env.LoadString("test", `(progn (charge 600) (eval '(charge 600)))`)
	requireStepLimit(t, res)

	// Each top-level entry point gets a fresh budget.
	res = env.LoadString("test", `(charge 600)`)
	require.Equal(t, "ok", res.String())
	charge := env.GetGlobal(lisp.Symbol("charge"))
	res = env.FunCall(charge, lisp.QExpr([]*lisp.LVal{lisp.Int(600)}))
	require.Equal(t, "ok", res.String())
	res = env.EvalContext(context.Background(), lisp.SExpr([]*lisp.LVal{lisp.Symbol("charge"), lisp.Int(600)}))
	require.Equal(t, "ok", res.String())

	// ResetSteps folds the charge into TotalSteps and empties Steps.
	total := env.Runtime.TotalSteps()
	steps := env.Runtime.Steps()
	require.GreaterOrEqual(t, steps, int64(600))
	env.Runtime.ResetSteps()
	assert.Zero(t, env.Runtime.Steps())
	assert.Equal(t, total, env.Runtime.TotalSteps())

	// A direct Go charge outside any evaluation charges the counter the next
	// evaluation resets.
	require.Equal(t, lisp.LSExpr, env.ChargeSteps(10).Type)
	assert.Equal(t, int64(10), env.Runtime.Steps())
	assert.Equal(t, total+10, env.Runtime.TotalSteps())
	require.Equal(t, "ok", env.LoadString("test", `(charge 1)`).String())
	assert.Less(t, env.Runtime.Steps(), int64(10))
}

// TestChargeStepsTemplateVMsAreIsolated pins that a charge lands on the
// charging VM's runtime only.
func TestChargeStepsTemplateVMsAreIsolated(t *testing.T) {
	source := templateTestEnv(t)
	require.NoError(t, lisp.GoError(lisp.WithMaxSteps(1000)(source)))
	require.NoError(t, lisp.GoError(source.InPackage(lisp.String(lisp.DefaultUserPackage))))
	source.AddBuiltins(true, elpsutil.Function("charge", lisp.Formals("n"), chargeBuiltin))
	tmpl, err := lisp.NewTemplate(source, templateCorePolicy())
	require.NoError(t, err)

	first, err := tmpl.NewVM()
	require.NoError(t, err)
	second, err := tmpl.NewVM()
	require.NoError(t, err)

	requireStepLimit(t, first.LoadString("test", `(charge 5000)`))
	assert.Greater(t, first.Runtime.Steps(), int64(5000))
	assert.Zero(t, second.Runtime.TotalSteps(), "a charge must not reach another VM")
	assert.Zero(t, source.Runtime.Steps(), "a charge must not reach the template source")

	res := second.LoadString("test", `(charge 900)`)
	require.Equal(t, "ok", res.String(), "each VM has its own budget")
}
