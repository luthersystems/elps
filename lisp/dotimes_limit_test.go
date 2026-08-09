// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"context"
	"strings"
	"testing"
	"time"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
)

// dotimes counts a step per TURN, not only per body form. These tests pin
// what that buys, what it costs, and -- importantly -- what it does NOT buy.
//
// The defect: (dotimes (i 2147483647)) with an EMPTY body evaluates nothing.
// Every evaluation limit in the interpreter is reached through Eval, so an
// empty-bodied loop incremented no counter and consulted no context.
// Measured on this machine before the fix: 2e8 turns ran 44.2s of
// uninterruptible CPU against a 2s context deadline, and the cancellation was
// only observed once the loop had finished on its own. count.Int can be
// MaxInt, so there is no upper bound at all. After the fix the same program
// returns at 2.003s.

func limitEnv(tb testing.TB, configs ...lisp.Config) *lisp.LEnv {
	tb.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	for _, cfg := range configs {
		if rc := cfg(env); rc.Type == lisp.LError {
			tb.Fatalf("config: %v", rc)
		}
	}
	if rc := lisp.InitializeUserEnv(env); rc.Type == lisp.LError {
		tb.Fatalf("initialize-user-env: %v", rc)
	}
	if rc := env.InPackage(lisp.String(lisp.DefaultUserPackage)); rc.Type == lisp.LError {
		tb.Fatalf("in-package: %v", rc)
	}
	return env
}

// TestDoTimesEmptyBodyRespectsContext is the regression test.
//
// Bounded out of band, on its own goroutine: reverting the fix does not
// produce a wrong answer, it produces no answer for ~44s, and a plain
// synchronous test would simply appear to be slow rather than failing.
func TestDoTimesEmptyBodyRespectsContext(t *testing.T) {
	env := limitEnv(t)
	ctx, cancel := context.WithTimeout(context.Background(), 200*time.Millisecond)
	defer cancel()

	done := make(chan *lisp.LVal, 1)
	start := time.Now()
	go func() {
		done <- env.LoadStringContext(ctx, "dotimes-ctx", "(dotimes (i 2000000000))")
	}()
	select {
	case v := <-done:
		elapsed := time.Since(start)
		if v.Type != lisp.LError {
			t.Fatalf("expected a cancellation error, got %v after %v", v, elapsed)
		}
		if !strings.Contains(v.Str, lisp.CondContextCancelled) {
			t.Fatalf("expected condition %q, got %q (%v)", lisp.CondContextCancelled, v.Str, v)
		}
		// 200ms deadline; anything under a couple of seconds means the loop
		// is polling the context rather than running to completion (2e9 turns
		// would take several minutes).
		if elapsed > 5*time.Second {
			t.Errorf("cancellation observed only after %v", elapsed)
		}
	case <-time.After(20 * time.Second):
		t.Fatalf("an empty-bodied dotimes ignored a 200ms context deadline for 20s")
	}
}

// TestDoTimesEmptyBodyRespectsMaxSteps pins the other half: a step budget now
// bounds an empty-bodied loop, where before it bounded nothing.
func TestDoTimesEmptyBodyRespectsMaxSteps(t *testing.T) {
	env := limitEnv(t, lisp.WithMaxSteps(1000))
	done := make(chan *lisp.LVal, 1)
	go func() {
		done <- env.LoadString("dotimes-steps", "(dotimes (i 2000000000))")
	}()
	select {
	case v := <-done:
		if v.Type != lisp.LError {
			t.Fatalf("expected a step-limit error, got %v", v)
		}
		if !strings.Contains(v.Str, lisp.CondStepLimitExceeded) {
			t.Fatalf("expected condition %q, got %q (%v)", lisp.CondStepLimitExceeded, v.Str, v)
		}
	case <-time.After(20 * time.Second):
		t.Fatalf("an empty-bodied dotimes ignored a 1000-step budget for 20s")
	}
}

// TestDoTimesStepCostPerTurn pins the COST of the fix, so that a change to it
// is deliberate rather than discovered by a user whose program stopped
// running.
//
// A turn now costs one step more than it did. Measured on this tree, at 1500
// turns:
//
//	body forms   before   after    per turn      delta
//	0                 4    1504    0 -> 1        (the whole point)
//	1              6004    7504    4 -> 5        +25.0%
//	2             12004   13504    8 -> 9        +12.5%
//
// The absolute numbers include 4 steps of fixed overhead for the enclosing
// load and the dotimes form itself.
//
// THIS IS A BEHAVIOUR CHANGE FOR PROGRAMS NEAR A STEP LIMIT. A one-form loop
// of 1500 turns used to consume 6004 steps and now consumes 7504, so a
// runtime configured with WithMaxSteps(7000) accepted that program before and
// rejects it now. That is ordinary code, not pathological input. Callers who
// pin a budget will need to raise it.
func TestDoTimesStepCostPerTurn(t *testing.T) {
	cases := []struct {
		name       string
		src        string
		wantSteps  int64
		perTurnDoc string
	}{
		{"empty body, 1500 turns", "(dotimes (i 1500))", 1504, "1 step/turn"},
		{"one form, 1500 turns", "(dotimes (i 1500) (+ i 1))", 7504, "5 steps/turn"},
		{"two forms, 1500 turns", "(dotimes (i 1500) (+ i 1) (- i 1))", 13504, "9 steps/turn"},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			env := limitEnv(t)
			env.Runtime.ResetSteps()
			v := env.LoadStringContext(context.Background(), "cost", c.src)
			if v.Type == lisp.LError {
				t.Fatalf("evaluation failed: %v", v)
			}
			if got := env.Runtime.Steps(); got != c.wantSteps {
				t.Errorf("%s (%s): %d steps, want %d", c.name, c.perTurnDoc, got, c.wantSteps)
			}
		})
	}
}

// TestDoTimesUnboundedWithNeitherLimit records what the fix does NOT do, so
// nobody reads the step counting as a guarantee it is not.
//
// LEnv.checkLimits short-circuits when the runtime has NEITHER a context NOR
// a step limit: with both unset there is nothing to check and the fast path
// returns immediately. An embedder who configures neither therefore still has
// an uninterruptible dotimes. The counter staying at its pre-loop value is
// the observable form of that.
//
// Substrate configures a context but no MaxSteps, so what it gains here is
// cancellability, not a bound.
func TestDoTimesUnboundedWithNeitherLimit(t *testing.T) {
	env := limitEnv(t)
	env.Runtime.ResetSteps()
	// LoadString (not LoadStringContext) leaves evalCtx nil, and no
	// WithMaxSteps was configured.
	v := env.LoadString("no-limits", "(dotimes (i 1000))")
	if v.Type == lisp.LError {
		t.Fatalf("evaluation failed: %v", v)
	}
	if got := env.Runtime.Steps(); got != 0 {
		t.Errorf("with neither a context nor a step limit configured, the fast "+
			"path should count nothing; counted %d steps. If this changed "+
			"deliberately, checkLimits' short-circuit changed with it and the "+
			"comment in opDoTimes needs updating.", got)
	}
}
