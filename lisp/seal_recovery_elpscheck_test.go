// Copyright © 2026 The ELPS authors

//go:build elpscheck

package lisp

import (
	"context"
	"strings"
	"testing"
)

func expectTypedSealPanic(t *testing.T, want string, run func()) {
	t.Helper()
	defer func() {
		t.Helper()
		r := recover()
		if r == nil {
			t.Fatal("seal violation was swallowed instead of escaping as a hard panic")
		}
		failure, ok := r.(sealViolation)
		if !ok {
			t.Fatalf("want a typed sealViolation error, got %T: %v", r, r)
		}
		if !strings.Contains(failure.Error(), want) {
			t.Fatalf("seal panic lost its diagnostic: %v", failure)
		}
	}()
	run()
}

// Issue #657: a checked-build invariant failure is not a recoverable native
// fault. Real load verification must escape both direct host calls and a load
// nested inside Eval, without leaving active evaluation state behind.
func TestSealViolationEscapesEvaluationRecovery(t *testing.T) {
	for _, entry := range []string{"FunCall", "FunCallContext", "EvalNestedLoad"} {
		t.Run(entry, func(t *testing.T) {
			env := newForkTestEnv(t)
			literal := Int(7)
			literal.SealAST()
			program, err := newProgram([]*LVal{literal})
			if err != nil {
				t.Fatal(err)
			}
			calls := 0
			fn := FunInPackage(DefaultUserPackage, "load-corrupt-program", Formals(), func(env *LEnv, _ *LVal) *LVal {
				calls++
				return env.LoadProgram(program)
			})
			// Deliberate corruption is ordered against the race watchdog and
			// restored before any later test or global seal verification.
			resume := pauseSealWatchdog()
			original := literal.Int
			defer func() { literal.Int = original; resume() }()
			literal.Int = original + 1
			expectTypedSealPanic(t, "sealed program AST mutated after parse", func() {
				switch entry {
				case "FunCall":
					env.FunCall(fn, Nil())
				case "FunCallContext":
					env.FunCallContext(context.Background(), fn, Nil())
				case "EvalNestedLoad":
					env.Eval(SExpr([]*LVal{fn}))
				}
			})
			if calls != 1 {
				t.Fatalf("native load called %d times, want 1", calls)
			}
			if len(env.Runtime.Stack.Frames) != 0 || env.Runtime.evalDepth != 0 || env.Runtime.evalNesting != 0 {
				t.Fatalf("seal panic left runtime active: frames=%d depth=%d nesting=%d", len(env.Runtime.Stack.Frames), env.Runtime.evalDepth, env.Runtime.evalNesting)
			}
			literal.Int = original
			if got := env.LoadProgram(program); got.Type != LInt || got.Int != 7 {
				t.Fatalf("restored program cannot load after the failure: %v", got)
			}
		})
	}
}

func TestSealViolationTableVerificationIsTyped(t *testing.T) {
	literal := Int(11)
	literal.SealAST()
	resume := pauseSealWatchdog()
	original := literal.Int
	defer func() { literal.Int = original; resume() }()
	literal.Int++
	expectTypedSealPanic(t, "sealed program AST mutated after parse", func() {
		sealCheck.mu.Lock()
		defer sealCheck.mu.Unlock()
		verifySealCheckTableLocked()
	})
}

func TestSingletonViolationEscapesEvaluationRecovery(t *testing.T) {
	env := newForkTestEnv(t)
	calls := 0
	fn := FunInPackage(DefaultUserPackage, "corrupt-singleton", Formals(), func(*LEnv, *LVal) *LVal {
		calls++
		original := singletonTrue.Str
		defer func() { singletonTrue.Str = original }()
		singletonTrue.Str = "corrupted-by-recovery-proof"
		return Nil() // The real singleton detector runs at its next use.
	})
	expr := SExpr([]*LVal{fn})
	defer pauseSingletonWatchdog()()
	defer pauseSealWatchdog()()
	expectTypedSealPanic(t, "singleton corruption detected: Bool(true)", func() { env.Eval(expr) })
	if calls != 1 {
		t.Fatalf("corrupting native called %d times, want 1", calls)
	}
	if len(env.Runtime.Stack.Frames) != 0 || env.Runtime.evalDepth != 0 || env.Runtime.evalNesting != 0 {
		t.Fatal("singleton failure left runtime evaluation active")
	}
	if drift := initSnapshot.Verify(); drift != "" {
		t.Fatalf("test left singleton corruption: %s", drift)
	}
	if got := env.Eval(Int(7)); got.Type != LInt || got.Int != 7 {
		t.Fatalf("environment did not recover after restored singleton: %v", got)
	}
}

func TestSealDiagnosticTextDoesNotForgeInvariantFailure(t *testing.T) {
	env := newForkTestEnv(t)
	fn := FunInPackage(DefaultUserPackage, "ordinary-fault", Formals(), func(*LEnv, *LVal) *LVal {
		panic("sealcheck: sealed program AST mutated after parse (ordinary host panic text)")
	})
	got := env.Eval(SExpr([]*LVal{fn}))
	if !IsInternalPanic(got) {
		t.Fatalf("ordinary host panic must remain an internal-panic error: %v", got)
	}
	if !strings.Contains(got.String(), "ordinary host panic text") {
		t.Fatalf("ordinary host panic lost its diagnostic: %v", got)
	}
	if stack := got.CallStack(); stack == nil || len(stack.GoStack) == 0 {
		t.Fatal("ordinary host panic lost its Go stack")
	}
}
