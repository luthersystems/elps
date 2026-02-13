// Copyright © 2018 The ELPS authors

package lisp

import (
	"fmt"
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

// requireLError asserts that result is a non-nil LError and returns its
// message string.  It calls t.Fatal on failure.
func requireLError(t *testing.T, result *LVal) string {
	t.Helper()
	if result == nil {
		t.Fatal("expected LError, got nil")
	}
	if result.Type != LError {
		t.Fatalf("expected LError, got %v: %v", result.Type, result)
	}
	return result.String()
}

// --- Eval recover() safety net tests ---

func TestNilReturnFromBuiltin(t *testing.T) {
	t.Parallel()
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
	msg := requireLError(t, result)
	if !strings.Contains(msg, "returned nil") {
		t.Errorf("error message should mention nil return, got: %s", msg)
	}
}

func TestPanicInBuiltinRecovered(t *testing.T) {
	t.Parallel()
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
	msg := requireLError(t, result)
	if !strings.Contains(msg, "recovered panic") {
		t.Errorf("error message should mention recovered panic, got: %s", msg)
	}
}

func TestPanicMessagePreserved(t *testing.T) {
	t.Parallel()
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
	msg := requireLError(t, result)
	if !strings.Contains(msg, panicMsg) {
		t.Errorf("error message should preserve panic message %q, got: %s", panicMsg, msg)
	}
	if result.CallStack() == nil {
		t.Fatal("error should have call stack attached")
	}
}

func TestPanicInNestedCallRecovered(t *testing.T) {
	t.Parallel()
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
	msg := requireLError(t, result)
	if !strings.Contains(msg, "recovered panic") {
		t.Errorf("error should mention recovered panic, got: %s", msg)
	}
	if !strings.Contains(msg, "panic from inner function") {
		t.Errorf("error should preserve inner panic message, got: %s", msg)
	}
}

func TestPanicWithNonStringValues(t *testing.T) {
	t.Parallel()
	tests := []struct {
		name     string
		panicVal interface{}
		contains string
	}{
		{"integer", 42, "42"},
		{"nil", nil, "nil"},
		{"error", fmt.Errorf("wrapped error"), "wrapped error"},
		{"struct", struct{ X int }{99}, "{99}"},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
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
			msg := requireLError(t, result)
			if !strings.Contains(msg, "recovered panic") {
				t.Errorf("error should mention recovered panic, got: %s", msg)
			}
			if !strings.Contains(msg, tc.contains) {
				t.Errorf("error should contain %q, got: %s", tc.contains, msg)
			}
		})
	}
}

// --- ErrorAssociate API contract tests ---

func TestErrorAssociateWithNonError(t *testing.T) {
	t.Parallel()
	env := initSafetyTestEnv(t)

	nonError := Int(42)
	result := env.ErrorAssociate(nonError)
	msg := requireLError(t, result)
	if !strings.Contains(msg, "ErrorAssociate called with non-error") {
		t.Errorf("wrong error message: %s", msg)
	}
}

func TestErrorAssociateWithError(t *testing.T) {
	t.Parallel()
	env := initSafetyTestEnv(t)

	lerr := Errorf("test error")
	result := env.ErrorAssociate(lerr)
	if result != nil {
		t.Fatalf("ErrorAssociate should return nil for valid error, got: %v", result)
	}
	if lerr.CallStack() == nil {
		t.Error("ErrorAssociate should attach call stack to error")
	}
}

// --- LVal method error-return tests (formerly panics) ---

func TestUserDataOnNonTagged(t *testing.T) {
	t.Parallel()
	v := Int(42)
	msg := requireLError(t, v.UserData())
	if !strings.Contains(msg, "not tagged") {
		t.Errorf("wrong error message: %s", msg)
	}
}

func TestArrayDimsOnNonArray(t *testing.T) {
	t.Parallel()
	v := Int(42)
	msg := requireLError(t, v.ArrayDims())
	if !strings.Contains(msg, "not an array") {
		t.Errorf("wrong error message: %s", msg)
	}
}

func TestArrayIndexOnNonArray(t *testing.T) {
	t.Parallel()
	v := Int(42)
	msg := requireLError(t, v.ArrayIndex(Int(0)))
	if !strings.Contains(msg, "not an array") {
		t.Errorf("wrong error message: %s", msg)
	}
}

func TestMapSetOnNonSortMap(t *testing.T) {
	t.Parallel()
	v := Int(42)
	msg := requireLError(t, v.MapSet("key", String("val")))
	if !strings.Contains(msg, "not sorted-map") {
		t.Errorf("wrong error message: %s", msg)
	}
}

func TestMakeByteSeqOnInvalidType(t *testing.T) {
	t.Parallel()
	v := Int(42)
	msg := requireLError(t, makeByteSeq(v))
	if !strings.Contains(msg, "not a native byte sequence") {
		t.Errorf("wrong error message: %s", msg)
	}
}
