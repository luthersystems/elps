// Copyright © 2018 The ELPS authors

package lisp

import (
	"strings"
	"testing"
)

// initSafetyTestEnv creates a minimal ELPS environment for safety tests.
// It does not require parser (to avoid import cycles) — we construct
// expressions directly as LVal trees.
func initSafetyTestEnv(t *testing.T) *LEnv {
	t.Helper()
	env := NewEnv(nil)
	rc := InitializeUserEnv(env)
	if rc.Type == LError {
		t.Fatalf("InitializeUserEnv failed: %v", rc)
	}
	rc = env.InPackage(String(DefaultUserPackage))
	if rc.Type == LError {
		t.Fatalf("InPackage failed: %v", rc)
	}
	return env
}

func TestNilReturnFromBuiltin(t *testing.T) {
	env := initSafetyTestEnv(t)

	// Register a builtin that returns nil (a programming error).
	env.AddBuiltins(true, &langBuiltin{
		name:    "test-nil-return",
		formals: Formals(),
		fun: func(env *LEnv, args *LVal) *LVal {
			return nil
		},
		docs: "test builtin that returns nil",
	})

	// Construct (test-nil-return) as an LVal tree.
	result := env.Eval(SExpr([]*LVal{Symbol("test-nil-return")}))
	if result == nil {
		t.Fatal("Eval returned nil — expected an LError")
	}
	if result.Type != LError {
		t.Fatalf("expected LError, got %v: %v", result.Type, result)
	}
	msg := result.String()
	if !strings.Contains(msg, "returned nil") {
		t.Errorf("error message should mention nil return, got: %s", msg)
	}
}

func TestPanicInBuiltinRecovered(t *testing.T) {
	env := initSafetyTestEnv(t)

	// Register a builtin that panics (simulates an internal assertion failure).
	env.AddBuiltins(true, &langBuiltin{
		name:    "test-panic",
		formals: Formals(),
		fun: func(env *LEnv, args *LVal) *LVal {
			panic("simulated internal failure")
		},
		docs: "test builtin that panics",
	})

	result := env.Eval(SExpr([]*LVal{Symbol("test-panic")}))
	if result == nil {
		t.Fatal("Eval returned nil — expected an LError from recovered panic")
	}
	if result.Type != LError {
		t.Fatalf("expected LError, got %v: %v", result.Type, result)
	}
	msg := result.String()
	if !strings.Contains(msg, "recovered panic") {
		t.Errorf("error message should mention recovered panic, got: %s", msg)
	}
}

func TestPanicMessagePreserved(t *testing.T) {
	env := initSafetyTestEnv(t)

	const panicMsg = "specific assertion: value was 42"
	env.AddBuiltins(true, &langBuiltin{
		name:    "test-panic-msg",
		formals: Formals(),
		fun: func(env *LEnv, args *LVal) *LVal {
			panic(panicMsg)
		},
		docs: "test builtin that panics with specific message",
	})

	result := env.Eval(SExpr([]*LVal{Symbol("test-panic-msg")}))
	if result == nil {
		t.Fatal("Eval returned nil")
	}
	if result.Type != LError {
		t.Fatalf("expected LError, got %v: %v", result.Type, result)
	}
	msg := result.String()
	if !strings.Contains(msg, panicMsg) {
		t.Errorf("error message should preserve panic message %q, got: %s", panicMsg, msg)
	}
	if result.CallStack() == nil {
		t.Error("error should have call stack attached")
	}
}

func TestPanicInNestedCallRecovered(t *testing.T) {
	env := initSafetyTestEnv(t)

	// Register an inner builtin that panics.
	env.AddBuiltins(true, &langBuiltin{
		name:    "test-inner-panic",
		formals: Formals(),
		fun: func(env *LEnv, args *LVal) *LVal {
			panic("panic from inner function")
		},
		docs: "test builtin that panics",
	})

	// Register an outer builtin that calls the inner one via Eval.
	env.AddBuiltins(true, &langBuiltin{
		name:    "test-outer-caller",
		formals: Formals(),
		fun: func(env *LEnv, args *LVal) *LVal {
			return env.Eval(SExpr([]*LVal{Symbol("test-inner-panic")}))
		},
		docs: "test builtin that calls inner-panic",
	})

	result := env.Eval(SExpr([]*LVal{Symbol("test-outer-caller")}))
	if result == nil {
		t.Fatal("Eval returned nil — nested panic not recovered")
	}
	if result.Type != LError {
		t.Fatalf("expected LError from nested panic, got %v: %v", result.Type, result)
	}
	msg := result.String()
	if !strings.Contains(msg, "recovered panic") {
		t.Errorf("error should mention recovered panic, got: %s", msg)
	}
	if !strings.Contains(msg, "panic from inner function") {
		t.Errorf("error should preserve inner panic message, got: %s", msg)
	}
}

func TestPanicWithNonStringValues(t *testing.T) {
	tests := []struct {
		name     string
		panicVal interface{}
		contains string
	}{
		{"integer", 42, "42"},
		{"nil", nil, "nil"},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Each subtest gets its own env to avoid duplicate symbol panics.
			env := initSafetyTestEnv(t)
			panicVal := tc.panicVal
			env.AddBuiltins(true, &langBuiltin{
				name:    "test-panic-type",
				formals: Formals(),
				fun: func(env *LEnv, args *LVal) *LVal {
					panic(panicVal)
				},
				docs: "test builtin that panics with non-string value",
			})

			result := env.Eval(SExpr([]*LVal{Symbol("test-panic-type")}))
			if result == nil || result.Type != LError {
				t.Fatalf("expected LError, got %v", result)
			}
			msg := result.String()
			if !strings.Contains(msg, "recovered panic") {
				t.Errorf("error should mention recovered panic, got: %s", msg)
			}
			if !strings.Contains(msg, tc.contains) {
				t.Errorf("error should contain %q, got: %s", tc.contains, msg)
			}
		})
	}
}
