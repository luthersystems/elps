// Copyright © 2024 The ELPS authors

package lisp

import (
	"context"
	"strings"
	"testing"
	"time"
)

// requireCondition asserts that result is a non-nil LError with the given
// condition type.  Returns the error message for further assertions.
func requireCondition(t *testing.T, result *LVal, condition string) string {
	t.Helper()
	msg := requireLError(t, result)
	if result.Str != condition {
		t.Fatalf("expected condition %q, got %q (message: %s)", condition, result.Str, msg)
	}
	return msg
}

// --- Context cancellation tests ---

func TestContextCancellation(t *testing.T) {
	t.Parallel()
	env := initSafetyTestEnv(t)

	// Pre-cancelled context should return immediately.
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Evaluate a simple expression with a cancelled context.
	result := env.EvalContext(ctx, SExpr([]*LVal{Symbol("+"), Int(1), Int(2)}))
	msg := requireCondition(t, result, CondContextCancelled)
	if !strings.Contains(msg, "context cancelled") {
		t.Errorf("expected 'context cancelled' in message, got: %s", msg)
	}
}

func TestContextTimeout(t *testing.T) {
	t.Parallel()
	env := initSafetyTestEnv(t)

	// Register a builtin that busy-loops, calling checkLimits via Eval on
	// each iteration.  We use a simple loop rather than recursion to avoid
	// hitting the stack depth limit before the context deadline fires.
	env.AddBuiltins(true, &langBuiltin{
		name:    "test-spin",
		formals: Formals(),
		fun: func(env *LEnv, args *LVal) *LVal {
			for {
				// Eval a trivial expression on each iteration.
				// This enters Eval which calls checkLimits.
				r := env.Eval(Int(0))
				if r.Type == LError {
					return r
				}
			}
		},
		docs: "test builtin that spins forever",
	})

	ctx, cancel := context.WithTimeout(context.Background(), 50*time.Millisecond)
	defer cancel()

	result := env.EvalContext(ctx, SExpr([]*LVal{Symbol("test-spin")}))
	requireCondition(t, result, CondContextCancelled)
}

// --- Step limit tests ---

func TestStepLimit(t *testing.T) {
	t.Parallel()
	env := initSafetyTestEnv(t)
	env.Runtime.maxSteps = 5

	// Build a progn with several sub-expressions.  Each Eval entry counts
	// as one step.  progn calls Eval for each child, so (progn 1 2 3 4 5 6)
	// will exceed 5 steps.
	exprs := make([]*LVal, 0, 8)
	exprs = append(exprs, Symbol("progn"))
	for i := range 7 {
		exprs = append(exprs, Int(i))
	}
	result := env.Eval(SExpr(exprs))
	msg := requireCondition(t, result, CondStepLimitExceeded)
	if !strings.Contains(msg, "step limit exceeded") {
		t.Errorf("expected 'step limit exceeded' in message, got: %s", msg)
	}
}

func TestStepLimitTailRecursion(t *testing.T) {
	t.Parallel()
	env := initSafetyTestEnv(t)
	env.Runtime.maxSteps = 50

	// Create a real Lisp function that tail-calls itself:
	//   (defun spin () (spin))
	// This triggers the TRO path in funCall, hitting checkLimits at the
	// goto callf site.
	spinBody := SExpr([]*LVal{Symbol("spin")})
	spinFn := env.Lambda(SExpr([]*LVal{}), []*LVal{spinBody})
	env.Put(Symbol("spin"), spinFn)

	result := env.Eval(SExpr([]*LVal{Symbol("spin")}))
	requireCondition(t, result, CondStepLimitExceeded)
}

func TestResetSteps(t *testing.T) {
	t.Parallel()
	env := initSafetyTestEnv(t)
	env.Runtime.maxSteps = 100

	// Evaluate (+ 1 2).  This enters Eval for the SExpr, then for each
	// of the 3 children (+, 1, 2), totaling 4 steps.
	result := env.Eval(SExpr([]*LVal{Symbol("+"), Int(1), Int(2)}))
	if result.Type == LError {
		t.Fatalf("expected success, got error: %v", result)
	}

	steps1 := env.Runtime.Steps()
	if steps1 != 4 {
		t.Errorf("expected 4 steps for (+ 1 2), got %d", steps1)
	}

	// Reset and verify counter is zero.
	env.Runtime.ResetSteps()
	if env.Runtime.Steps() != 0 {
		t.Errorf("expected 0 steps after reset, got %d", env.Runtime.Steps())
	}

	// Another evaluation should work (steps start from 0 again).
	result = env.Eval(SExpr([]*LVal{Symbol("+"), Int(3), Int(4)}))
	if result.Type == LError {
		t.Fatalf("expected success after reset, got error: %v", result)
	}
	if env.Runtime.Steps() != 4 {
		t.Errorf("expected 4 steps for second eval, got %d", env.Runtime.Steps())
	}
}

func TestNoLimitsZeroOverhead(t *testing.T) {
	t.Parallel()
	env := initSafetyTestEnv(t)

	// Default runtime has nil ctx and zero maxSteps.
	if env.Runtime.ctx != nil {
		t.Error("expected nil ctx on default runtime")
	}
	if env.Runtime.maxSteps != 0 {
		t.Error("expected zero maxSteps on default runtime")
	}
	if env.Runtime.steps != 0 {
		t.Error("expected zero steps on default runtime")
	}

	// Eval should work and steps should remain 0 (fast path skips counting).
	result := env.Eval(SExpr([]*LVal{Symbol("+"), Int(1), Int(2)}))
	if result.Type == LError {
		t.Fatalf("expected success, got error: %v", result)
	}
	if env.Runtime.Steps() != 0 {
		t.Errorf("expected 0 steps when no limits configured, got %d", env.Runtime.Steps())
	}
}

func TestLimitConditionConstants(t *testing.T) {
	t.Parallel()

	// Stability canary: these condition strings are stable API used by
	// handler-bind in Lisp code.  Changing them is a breaking change.
	if CondContextCancelled != "context-cancelled" {
		t.Errorf("CondContextCancelled = %q, want %q", CondContextCancelled, "context-cancelled")
	}
	if CondStepLimitExceeded != "step-limit-exceeded" {
		t.Errorf("CondStepLimitExceeded = %q, want %q", CondStepLimitExceeded, "step-limit-exceeded")
	}
}

// --- Config option tests ---

func TestWithMaxStepsConfig(t *testing.T) {
	t.Parallel()
	env := NewEnv(nil)
	rc := InitializeUserEnv(env, WithMaxSteps(3))
	if rc.Type == LError {
		t.Fatalf("InitializeUserEnv failed: %v", rc)
	}
	rc = env.InPackage(String(DefaultUserPackage))
	if rc.Type == LError {
		t.Fatalf("InPackage failed: %v", rc)
	}

	if env.Runtime.maxSteps != 3 {
		t.Errorf("expected maxSteps=3, got %d", env.Runtime.maxSteps)
	}

	// A progn with many expressions should exceed the step limit.
	exprs := []*LVal{Symbol("progn"), Int(1), Int(2), Int(3), Int(4), Int(5)}
	result := env.Eval(SExpr(exprs))
	requireCondition(t, result, CondStepLimitExceeded)
}

func TestWithContextConfig(t *testing.T) {
	t.Parallel()

	ctx, cancel := context.WithCancel(context.Background())
	cancel() // pre-cancelled

	env := NewEnv(nil)
	rc := InitializeUserEnv(env, WithContext(ctx))
	if rc.Type == LError {
		t.Fatalf("InitializeUserEnv failed: %v", rc)
	}
	rc = env.InPackage(String(DefaultUserPackage))
	if rc.Type == LError {
		t.Fatalf("InPackage failed: %v", rc)
	}

	result := env.Eval(SExpr([]*LVal{Symbol("+"), Int(1), Int(2)}))
	requireCondition(t, result, CondContextCancelled)
}

// --- Context-scoped method tests ---

func TestEvalContextClearsCtx(t *testing.T) {
	t.Parallel()
	env := initSafetyTestEnv(t)

	ctx := context.Background()
	result := env.EvalContext(ctx, SExpr([]*LVal{Symbol("+"), Int(1), Int(2)}))
	if result.Type == LError {
		t.Fatalf("expected success, got error: %v", result)
	}

	// After EvalContext returns, ctx should be cleared.
	if env.Runtime.ctx != nil {
		t.Error("expected nil ctx after EvalContext returns")
	}
}

func TestFunCallContext(t *testing.T) {
	t.Parallel()
	env := initSafetyTestEnv(t)

	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Register a function that loops via Eval so the context check fires.
	env.AddBuiltins(true, &langBuiltin{
		name:    "test-loop",
		formals: Formals(),
		fun: func(env *LEnv, args *LVal) *LVal {
			for {
				r := env.Eval(Int(0))
				if r.Type == LError {
					return r
				}
			}
		},
		docs: "loops via eval",
	})

	loopFn := env.Get(Symbol("test-loop"))
	if loopFn.Type == LError {
		t.Fatalf("failed to look up test-loop: %v", loopFn)
	}

	result := env.FunCallContext(ctx, loopFn, SExpr([]*LVal{}))
	requireCondition(t, result, CondContextCancelled)

	// Context should be cleared after return.
	if env.Runtime.ctx != nil {
		t.Error("expected nil ctx after FunCallContext returns")
	}
}

// NOTE: LoadStringContext, LoadContext, LoadFileContext, LoadLocationContext
// are not tested here because they require a parser.Reader, which would
// create an import cycle (lisp/ cannot import parser/).  They follow the
// same set-ctx/defer-clear/delegate pattern as EvalContext and are
// exercised through integration tests in lisp/lisplib/ and elpstest/.
