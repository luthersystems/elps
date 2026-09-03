// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"context"
	"testing"
	"time"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// funLocProgram defines three functions whose bodies live at three different
// places: a top-level defun, a lambda closed over a let scope, and a defun
// that calls both.  Every function is therefore entered from a call site on a
// different line than its own definition, which is what makes the location a
// budget error reports observable.
const funLocProgram = `(defun aa (n)
  (+ n 1))
(let ([k 2])
  (set 'bb (lambda (m)
     (* m k))))
(defun cc (x)
  (aa (bb x)))
(cc 3)`

// TestBudgetErrorReportsDefinitionSite pins the location an evaluation-budget
// error reports when it trips at the exact moment a function body is entered.
//
// LEnv.eval READS env.loc before it rebinds it to the expression being
// evaluated: the evalNesting guard and checkLimits both raise through
// env.ErrorConditionf, which stamps env.loc into the error's rendered text
// and into Source().  At a body entry the environment in hand is the call
// environment bind just built, so what that error reports is whatever
// location register bind put there.
//
// It must be the function's definition site.  Every step count below is a
// trip point where the reported location differs from the call site, so a
// call environment that inherited its captured environment's live register
// (the environment's position at the moment of the call, i.e. the call site)
// would fail here.
func TestBudgetErrorReportsDefinitionSite(t *testing.T) {
	t.Parallel()
	tests := []struct {
		name  string
		steps int64
		want  string
	}{
		// Entering cc's body: cc is defined on line 6, called from line 8.
		{"defun-body", 29, "s.lisp:6:1: step-limit-exceeded: step limit exceeded (29 steps)"},
		// Entering bb's body: the lambda form starts at line 4 column 12.
		{"closure-body", 34, "s.lisp:4:12: step-limit-exceeded: step limit exceeded (34 steps)"},
		// Entering aa's body, from inside cc: aa is defined on line 1.
		{"nested-defun-body", 38, "s.lisp:1:1: step-limit-exceeded: step limit exceeded (38 steps)"},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			t.Parallel()
			env := newLimitTestEnv(t, lisp.WithMaxSteps(test.steps))
			res := env.LoadString("s.lisp", funLocProgram)
			require.Equal(t, lisp.LError, res.Type, "expected the step limit to trip")
			assert.Equal(t, test.want, res.String())
			loc, ok := res.Source()
			require.True(t, ok, "the error must carry a source location")
			assert.Equal(t, "s.lisp", loc.File)
		})
	}
}

// countdownContext reports itself cancelled once Err has been probed more
// than n times, which walks the cancellation point across the evaluation one
// checkLimits call at a time.  A real cancelled context would trip at the
// first check; this one trips at a chosen one.
type countdownContext struct {
	n int
	i int
}

func (c *countdownContext) Deadline() (time.Time, bool) { return time.Time{}, false }
func (c *countdownContext) Done() <-chan struct{}       { return nil }
func (c *countdownContext) Value(any) any               { return nil }
func (c *countdownContext) Err() error {
	c.i++
	if c.i > c.n {
		return context.Canceled
	}
	return nil
}

// TestCancellationAtBodyEntryReportsDefinitionSite is the same invariant
// reached through the other caller of checkLimits: cancellation rather than a
// step limit.  It needs no step limit at all, so it also covers embedders who
// run with cancellation as their only budget.
func TestCancellationAtBodyEntryReportsDefinitionSite(t *testing.T) {
	t.Parallel()
	env := newLimitTestEnv(t)
	res := env.LoadStringContext(&countdownContext{n: 38}, "s.lisp", funLocProgram)
	require.Equal(t, lisp.LError, res.Type, "expected the cancellation to trip")
	assert.Equal(t,
		"s.lisp:1:1: context-cancelled: context cancelled: context canceled",
		res.String())
}
