// Copyright © 2018 The ELPS authors

package lisp_test

import (
	"fmt"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// newLimitTestEnv builds a stock environment — no stack/step overrides — so
// these tests exercise the defaults an embedder actually gets from
// lisp.NewEnv(nil), not the tighter limits the elpstest harness installs.
func newLimitTestEnv(t *testing.T, cfg ...lisp.Config) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	require.NoError(t, lisp.GoError(lisp.InitializeUserEnv(env)))
	require.NoError(t, lisp.GoError(lisplib.LoadLibrary(env)))
	for _, c := range cfg {
		require.NoError(t, lisp.GoError(c(env)))
	}
	require.NoError(t, lisp.GoError(env.InPackage(lisp.String(lisp.DefaultUserPackage))))
	return env
}

const spinDefn = `(defun spin (n) (if (= n 0) 'done (spin (- n 1))))`

// TestTailLoopIsNotBoundedByLogicalHeight pins the fix for the regression
// where StandardRuntime defaulted MaxHeightLogical to 50000. Because logical
// height accumulates frames elided by tail-call optimization, that default
// turned into a hard ~25,000-iteration ceiling on every tail-recursive loop:
// (spin 24000) returned but (spin 25000) failed with "logical stack height
// exceeded maximum: 50001". A tail loop must run in constant space for an
// unbounded number of iterations.
func TestTailLoopIsNotBoundedByLogicalHeight(t *testing.T) {
	env := newLimitTestEnv(t)
	res := env.LoadString("test", spinDefn)
	require.NotEqual(t, lisp.LError, res.Type, "defun failed: %v", res)

	// Well past the old 25,000-iteration cliff.
	res = env.LoadString("test", `(spin 100000)`)
	require.NotEqual(t, lisp.LError, res.Type,
		"a 100k-iteration tail loop must not hit a stack limit: %v", res)
	assert.Equal(t, "'done", res.String())

	// The stack unwinds completely: a tail loop leaks no frames.
	assert.Empty(t, env.Runtime.Stack.Frames)
}

// TestTailCallsDoNotConsumePhysicalFrames is the invariant the whole limit
// design rests on: physical stack height and tail-call iteration count are
// orthogonal. A tail loop of 200,000 turns runs under a 100-frame physical
// ceiling; non-tail recursion only 500 deep is refused under the same
// ceiling.
//
// This is why the tail-iteration bound can be set high without any risk of a
// Go stack overflow, and why the physical bound must stay on regardless.
func TestTailCallsDoNotConsumePhysicalFrames(t *testing.T) {
	tight := []lisp.Config{
		lisp.WithMaximumPhysicalStackHeight(100),
		lisp.WithMaximumLogicalStackHeight(0),
		lisp.WithMaxTailIterations(0),
	}

	env := newLimitTestEnv(t, tight...)
	require.NotEqual(t, lisp.LError, env.LoadString("test", spinDefn).Type)
	res := env.LoadString("test", `(spin 200000)`)
	require.NotEqual(t, lisp.LError, res.Type,
		"a 200k-turn tail loop must survive a 100-frame physical cap: %v", res)
	assert.Equal(t, "'done", res.String())

	// And the stack unwinds completely — no frames leak across the loop.
	assert.Empty(t, env.Runtime.Stack.Frames,
		"the physical stack must be empty after a tail loop returns")

	// Same ceiling, non-tail recursion only 500 deep: refused.
	env = newLimitTestEnv(t, tight...)
	require.NotEqual(t, lisp.LError,
		env.LoadString("test", `(defun build (n) (if (= n 0) 0 (+ 1 (build (- n 1)))))`).Type)
	res = env.LoadString("test", `(build 500)`)
	require.Equal(t, lisp.LError, res.Type,
		"non-tail recursion must be bounded by the physical cap")
	assert.Contains(t, res.String(), "physical stack height exceeded maximum")
}

// TestDefaultStackLimits pins the default policy: the physical guard stays
// on (it prevents an unrecoverable Go stack overflow), the logical bound is
// off (its unit is elided frames, which is not a usable default), and the
// tail-iteration backstop is on.
func TestDefaultStackLimits(t *testing.T) {
	for _, tt := range []struct {
		name  string
		stack *lisp.CallStack
	}{
		{"StandardRuntime", lisp.StandardRuntime().Stack},
		{"NewEnv", lisp.NewEnv(nil).Runtime.Stack},
	} {
		t.Run(tt.name, func(t *testing.T) {
			assert.Equal(t, 0, tt.stack.MaxHeightLogical,
				"the logical height bound must be off by default")
			assert.Equal(t, 25000, tt.stack.MaxHeightPhysical,
				"the physical guard must stay on: it prevents a fatal Go stack overflow")
			assert.Equal(t, 1000000, tt.stack.MaxTailIterations,
				"the runaway tail-loop backstop must be on by default")
		})
	}
	assert.Equal(t, 0, lisp.DefaultMaxLogicalStackHeight)
	assert.Equal(t, 25000, lisp.DefaultMaxPhysicalStackHeight)
	assert.Equal(t, 1000000, lisp.DefaultMaxTailIterations)
}

// TestDeepNonTailRecursionIsCatchable pins that unbounded *non-tail*
// recursion still produces an ordinary ELPS error rather than aborting the
// process with a Go stack overflow. Measured with the limit disabled, the Go
// runtime dies somewhere between 200,000 and 400,000 ELPS levels; the
// default physical limit stops us an order of magnitude short of that.
func TestDeepNonTailRecursionIsCatchable(t *testing.T) {
	env := newLimitTestEnv(t)
	res := env.LoadString("test", `(defun boom (n) (if (= n 0) 0 (+ 1 (boom (- n 1)))))`)
	require.NotEqual(t, lisp.LError, res.Type, "defun failed: %v", res)

	res = env.LoadString("test", `(boom 100000)`)
	require.Equal(t, lisp.LError, res.Type, "expected a catchable error, got %v", res)
	msg := res.String()
	assert.Contains(t, msg, "physical stack height exceeded maximum")
	assert.Contains(t, msg, "WithMaximumPhysicalStackHeight",
		"the error must say which knob raises the limit")
}

// TestTailIterationLimit pins the runaway-loop backstop: it trips in units of
// loop turns, and its message must not read like a stack overflow.
func TestTailIterationLimit(t *testing.T) {
	env := newLimitTestEnv(t, lisp.WithMaxTailIterations(1000))
	res := env.LoadString("test", spinDefn)
	require.NotEqual(t, lisp.LError, res.Type, "defun failed: %v", res)

	res = env.LoadString("test", `(spin 999)`)
	require.NotEqual(t, lisp.LError, res.Type, "999 turns is under the limit: %v", res)

	res = env.LoadString("test", `(spin 2000)`)
	require.Equal(t, lisp.LError, res.Type, "expected the iteration limit to trip")
	msg := res.String()
	assert.Contains(t, msg, "tail-call iteration limit exceeded")
	assert.Contains(t, msg, "not a stack overflow")
	assert.Contains(t, msg, "WithMaxTailIterations")
	assert.NotContains(t, msg, "stack height exceeded",
		"the message must not read like runaway recursion")

	// Disabling the limit lets the same loop run.
	env = newLimitTestEnv(t, lisp.WithMaxTailIterations(0))
	require.NotEqual(t, lisp.LError, env.LoadString("test", spinDefn).Type)
	res = env.LoadString("test", `(spin 2000)`)
	assert.NotEqual(t, lisp.LError, res.Type, "0 must disable the check: %v", res)
}

// TestTailIterationUnitIsOneTurnPerIteration pins the unit of
// MaxTailIterations against the unit of MaxHeightLogical. This is the whole
// reason the tail-iteration counter exists: HeightLogical counts *elided
// frames*, so its per-iteration cost depends on the shape of the loop body
// (2 for a trivial (if ...) body, more when terminal forms nest deeper) and
// an operator cannot reason about it. TailIterations counts turns, exactly
// one per iteration, whatever the body looks like.
//
// If this test starts failing, the documented unit has drifted and the
// documentation on DefaultMaxTailIterations / MaxHeightLogical must be
// revisited.
func TestTailIterationUnitIsOneTurnPerIteration(t *testing.T) {
	const turns = 500

	// A trivial body elides 2 frames per turn; a body that nests an extra
	// terminal `if` elides 3. The iteration count is 500 in both cases.
	for _, tt := range []struct {
		name            string
		defn            string
		elidedPerTurn   int
		expectIterLimit bool
	}{
		{
			name:          "trivial body",
			defn:          `(defun loop2 (n) (if (= n 0) 'done (loop2 (- n 1))))`,
			elidedPerTurn: 2,
		},
		{
			name:          "extra terminal nesting",
			defn:          `(defun loop3 (n) (if (= n 0) 'done (if true (loop3 (- n 1)) 'x)))`,
			elidedPerTurn: 3,
		},
	} {
		t.Run(tt.name, func(t *testing.T) {
			fn := strings.Fields(strings.TrimPrefix(tt.defn, "(defun "))[0]

			// The logical bound trips at elidedPerTurn * turns: the same
			// numeric limit buys a different number of iterations.
			env := newLimitTestEnv(t,
				lisp.WithMaximumLogicalStackHeight(tt.elidedPerTurn*turns),
				lisp.WithMaxTailIterations(0))
			require.NotEqual(t, lisp.LError, env.LoadString("test", tt.defn).Type)
			res := env.LoadString("test", fmt.Sprintf("(%s %d)", fn, turns+50))
			require.Equal(t, lisp.LError, res.Type,
				"logical bound of %d elided frames should trip", tt.elidedPerTurn*turns)
			assert.Contains(t, res.String(), "logical stack height exceeded maximum")

			// The iteration bound trips at `turns` regardless of body shape.
			env = newLimitTestEnv(t,
				lisp.WithMaximumLogicalStackHeight(0),
				lisp.WithMaxTailIterations(turns))
			require.NotEqual(t, lisp.LError, env.LoadString("test", tt.defn).Type)
			res = env.LoadString("test", fmt.Sprintf("(%s %d)", fn, turns-1))
			require.NotEqual(t, lisp.LError, res.Type,
				"%d turns is under a %d-turn limit: %v", turns-1, turns, res)
			res = env.LoadString("test", fmt.Sprintf("(%s %d)", fn, turns+50))
			require.Equal(t, lisp.LError, res.Type,
				"%d turns must exceed a %d-turn limit", turns+50, turns)
			assert.Contains(t, res.String(), "tail-call iteration limit exceeded")
		})
	}
}

// --- MaxSteps (per-evaluation budget) ---

// TestMaxStepsIsPerEvaluation pins that the step budget is refilled for each
// top-level evaluation. Before the fix the counter was cumulative and never
// auto-reset, so WithMaxSteps(n) was a lifetime quota: once a long-lived
// Runtime had executed n steps in total, *every* subsequent evaluation
// failed, however small.
func TestMaxStepsIsPerEvaluation(t *testing.T) {
	// Measure what one evaluation costs, then set a budget just above it so
	// the cumulative total blows past the budget within a few iterations.
	probe := newLimitTestEnv(t, lisp.WithMaxSteps(1000000))
	require.NotEqual(t, lisp.LError, probe.LoadString("test", `(+ 1 2)`).Type)
	cost := probe.Runtime.Steps()
	require.Positive(t, cost)

	budget := cost + 1
	env := newLimitTestEnv(t, lisp.WithMaxSteps(budget))

	// Each of these is under the budget on its own. Cumulatively they are
	// far over it.
	const iterations = 50
	for i := 0; i < iterations; i++ {
		res := env.LoadString("test", `(+ 1 2)`)
		require.NotEqual(t, lisp.LError, res.Type,
			"evaluation %d must get a fresh budget, got: %v", i, res)
	}
	assert.Greater(t, env.Runtime.TotalSteps(), budget,
		"the lifetime total must have passed the per-evaluation budget")
}

// TestMaxStepsStillBoundsOneEvaluation pins that per-evaluation reset does
// not defeat the limit: a single runaway evaluation still trips it.
func TestMaxStepsStillBoundsOneEvaluation(t *testing.T) {
	env := newLimitTestEnv(t, lisp.WithMaxSteps(5000))
	require.NotEqual(t, lisp.LError, env.LoadString("test", spinDefn).Type)

	res := env.LoadString("test", `(spin 100000)`)
	require.Equal(t, lisp.LError, res.Type, "expected the step limit to trip")
	assert.Contains(t, res.String(), "step limit exceeded")
}

// TestMaxStepsNotRefilledByNestedEvaluation pins that the budget is anchored
// at the outermost entry point. A builtin (or a tail-call loop) that
// re-enters evaluation must share the enclosing budget rather than resetting
// it — otherwise a runaway loop could evade the limit forever.
func TestMaxStepsNotRefilledByNestedEvaluation(t *testing.T) {
	env := newLimitTestEnv(t, lisp.WithMaxSteps(5000))
	require.NotEqual(t, lisp.LError, env.LoadString("test", spinDefn).Type)

	// `eval` re-enters env.Eval from inside a builtin; the inner evaluation
	// must not refill the budget.
	res := env.LoadString("test", `(eval '(spin 100000))`)
	require.Equal(t, lisp.LError, res.Type,
		"a nested eval must not refill the step budget, got: %v", res)
	assert.Contains(t, res.String(), "step limit exceeded")
}

// TestStepsAndTotalSteps pins the two counters against each other.
func TestStepsAndTotalSteps(t *testing.T) {
	env := newLimitTestEnv(t, lisp.WithMaxSteps(1000000))

	require.NotEqual(t, lisp.LError, env.LoadString("test", `(+ 1 2)`).Type)
	first := env.Runtime.Steps()
	require.Positive(t, first)
	assert.Equal(t, first, env.Runtime.TotalSteps())

	require.NotEqual(t, lisp.LError, env.LoadString("test", `(+ 1 2)`).Type)
	assert.Equal(t, first, env.Runtime.Steps(),
		"Steps reports the most recent evaluation, not a running total")
	assert.Equal(t, 2*first, env.Runtime.TotalSteps(),
		"TotalSteps accumulates across evaluations")
}
