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

// --- Allocation limit tests (HC-2) ---
//
// Each CheckAlloc call site is tested with:
//   1. An "exceeds" case that verifies the exact error message (size + limit).
//   2. A "within limit" case that verifies the operation succeeds and returns
//      the correct result.

func TestCheckAllocConcatString(t *testing.T) {
	t.Parallel()
	t.Run("exceeds", func(t *testing.T) {
		t.Parallel()
		env := initSafetyTestEnv(t)
		env.Runtime.MaxAlloc = 100
		a := String(strings.Repeat("a", 60))
		b := String(strings.Repeat("b", 60))
		args := SExpr([]*LVal{Symbol("string"), a, b})
		result := builtinConcatString(env, args)
		msg := requireLError(t, result)
		if !strings.Contains(msg, "allocation size 120 exceeds maximum (100)") {
			t.Errorf("wrong error: %s", msg)
		}
	})
	t.Run("within limit", func(t *testing.T) {
		t.Parallel()
		env := initSafetyTestEnv(t)
		env.Runtime.MaxAlloc = 100
		a := String("hello")
		b := String(" world")
		args := SExpr([]*LVal{Symbol("string"), a, b})
		result := builtinConcatString(env, args)
		if result.Type == LError {
			t.Fatalf("unexpected error: %v", result)
		}
		if result.Str != "hello world" {
			t.Errorf("expected 'hello world', got %q", result.Str)
		}
	})
}

func TestCheckAllocConcatBytes(t *testing.T) {
	t.Parallel()
	t.Run("exceeds", func(t *testing.T) {
		t.Parallel()
		env := initSafetyTestEnv(t)
		env.Runtime.MaxAlloc = 5
		a := Bytes([]byte{1, 2, 3})
		b := Bytes([]byte{4, 5, 6})
		args := SExpr([]*LVal{Symbol("bytes"), a, b})
		result := builtinConcatBytes(env, args)
		msg := requireLError(t, result)
		if !strings.Contains(msg, "allocation size 6 exceeds maximum (5)") {
			t.Errorf("wrong error: %s", msg)
		}
	})
	t.Run("within limit", func(t *testing.T) {
		t.Parallel()
		env := initSafetyTestEnv(t)
		env.Runtime.MaxAlloc = 10
		a := Bytes([]byte{1, 2})
		b := Bytes([]byte{3, 4})
		args := SExpr([]*LVal{Symbol("bytes"), a, b})
		result := builtinConcatBytes(env, args)
		if result.Type == LError {
			t.Fatalf("unexpected error: %v", result)
		}
		if len(result.Bytes()) != 4 {
			t.Errorf("expected 4 bytes, got %d", len(result.Bytes()))
		}
	})
}

func TestCheckAllocConcatSeq(t *testing.T) {
	t.Parallel()
	t.Run("exceeds", func(t *testing.T) {
		t.Parallel()
		env := initSafetyTestEnv(t)
		env.Runtime.MaxAlloc = 3
		a := QExpr([]*LVal{Int(1), Int(2)})
		b := QExpr([]*LVal{Int(3), Int(4)})
		args := SExpr([]*LVal{Symbol("list"), a, b})
		result := builtinConcatSeq(env, args)
		msg := requireLError(t, result)
		if !strings.Contains(msg, "allocation size 4 exceeds maximum (3)") {
			t.Errorf("wrong error: %s", msg)
		}
	})
	t.Run("within limit", func(t *testing.T) {
		t.Parallel()
		env := initSafetyTestEnv(t)
		env.Runtime.MaxAlloc = 10
		a := QExpr([]*LVal{Int(1)})
		b := QExpr([]*LVal{Int(2)})
		args := SExpr([]*LVal{Symbol("list"), a, b})
		result := builtinConcatSeq(env, args)
		if result.Type == LError {
			t.Fatalf("unexpected error: %v", result)
		}
		if result.Len() != 2 {
			t.Errorf("expected 2 elements, got %d", result.Len())
		}
	})
}

func TestCheckAllocAppend(t *testing.T) {
	t.Parallel()
	t.Run("exceeds", func(t *testing.T) {
		t.Parallel()
		env := initSafetyTestEnv(t)
		env.Runtime.MaxAlloc = 5
		seq := QExpr([]*LVal{Int(1), Int(2), Int(3)})
		args := SExpr([]*LVal{Symbol("list"), seq, Int(4), Int(5), Int(6)})
		result := builtinAppend(env, args)
		msg := requireLError(t, result)
		if !strings.Contains(msg, "allocation size 6 exceeds maximum (5)") {
			t.Errorf("wrong error: %s", msg)
		}
	})
	t.Run("within limit", func(t *testing.T) {
		t.Parallel()
		env := initSafetyTestEnv(t)
		env.Runtime.MaxAlloc = 10
		seq := QExpr([]*LVal{Int(1), Int(2)})
		args := SExpr([]*LVal{Symbol("list"), seq, Int(3)})
		result := builtinAppend(env, args)
		if result.Type == LError {
			t.Fatalf("unexpected error: %v", result)
		}
		if result.Len() != 3 {
			t.Errorf("expected 3 elements, got %d", result.Len())
		}
	})
}

func TestCheckAllocAppendBytes(t *testing.T) {
	t.Parallel()
	t.Run("exceeds", func(t *testing.T) {
		t.Parallel()
		env := initSafetyTestEnv(t)
		env.Runtime.MaxAlloc = 3
		b := Bytes([]byte{1, 2})
		args := SExpr([]*LVal{Symbol("bytes"), b, Int(3), Int(4)})
		result := builtinAppend_Bytes(env, args)
		msg := requireLError(t, result)
		if !strings.Contains(msg, "allocation size 4 exceeds maximum (3)") {
			t.Errorf("wrong error: %s", msg)
		}
	})
	t.Run("within limit", func(t *testing.T) {
		t.Parallel()
		env := initSafetyTestEnv(t)
		env.Runtime.MaxAlloc = 10
		b := Bytes([]byte{1, 2})
		args := SExpr([]*LVal{Symbol("bytes"), b, Int(3)})
		result := builtinAppend_Bytes(env, args)
		if result.Type == LError {
			t.Fatalf("unexpected error: %v", result)
		}
		if len(result.Bytes()) != 3 {
			t.Errorf("expected 3 bytes, got %d", len(result.Bytes()))
		}
	})
}

func TestCheckAllocAppendMutate(t *testing.T) {
	t.Parallel()
	t.Run("exceeds", func(t *testing.T) {
		t.Parallel()
		env := initSafetyTestEnv(t)
		env.Runtime.MaxAlloc = 3
		vec := Array(QExpr([]*LVal{Int(2)}), []*LVal{Int(1), Int(2)})
		args := SExpr([]*LVal{vec, Int(3), Int(4)})
		result := builtinAppendMutate(env, args)
		msg := requireLError(t, result)
		if !strings.Contains(msg, "allocation size 4 exceeds maximum (3)") {
			t.Errorf("wrong error: %s", msg)
		}
	})
	t.Run("within limit", func(t *testing.T) {
		t.Parallel()
		env := initSafetyTestEnv(t)
		env.Runtime.MaxAlloc = 10
		vec := Array(QExpr([]*LVal{Int(2)}), []*LVal{Int(1), Int(2)})
		args := SExpr([]*LVal{vec, Int(3)})
		result := builtinAppendMutate(env, args)
		if result.Type == LError {
			t.Fatalf("unexpected error: %v", result)
		}
		if result.Len() != 3 {
			t.Errorf("expected 3 elements, got %d", result.Len())
		}
	})
}

func TestCheckAllocZip(t *testing.T) {
	t.Parallel()
	t.Run("exceeds", func(t *testing.T) {
		t.Parallel()
		env := initSafetyTestEnv(t)
		env.Runtime.MaxAlloc = 3
		// zip 'list [1 2] [3 4] → 2*2=4 elements total
		a := QExpr([]*LVal{Int(1), Int(2)})
		b := QExpr([]*LVal{Int(3), Int(4)})
		args := SExpr([]*LVal{Symbol("list"), a, b})
		result := builtinZip(env, args)
		msg := requireLError(t, result)
		if !strings.Contains(msg, "allocation size 4 exceeds maximum (3)") {
			t.Errorf("wrong error: %s", msg)
		}
	})
	t.Run("within limit", func(t *testing.T) {
		t.Parallel()
		env := initSafetyTestEnv(t)
		env.Runtime.MaxAlloc = 10
		a := QExpr([]*LVal{Int(1)})
		b := QExpr([]*LVal{Int(2)})
		args := SExpr([]*LVal{Symbol("list"), a, b})
		result := builtinZip(env, args)
		if result.Type == LError {
			t.Fatalf("unexpected error: %v", result)
		}
		if result.Len() != 1 {
			t.Errorf("expected 1 element, got %d", result.Len())
		}
	})
}

func TestCheckAllocReverse(t *testing.T) {
	t.Parallel()
	t.Run("exceeds", func(t *testing.T) {
		t.Parallel()
		env := initSafetyTestEnv(t)
		env.Runtime.MaxAlloc = 2
		seq := QExpr([]*LVal{Int(1), Int(2), Int(3)})
		args := SExpr([]*LVal{Symbol("list"), seq})
		result := builtinReverse(env, args)
		msg := requireLError(t, result)
		if !strings.Contains(msg, "allocation size 3 exceeds maximum (2)") {
			t.Errorf("wrong error: %s", msg)
		}
	})
	t.Run("within limit", func(t *testing.T) {
		t.Parallel()
		env := initSafetyTestEnv(t)
		env.Runtime.MaxAlloc = 10
		seq := QExpr([]*LVal{Int(1), Int(2), Int(3)})
		args := SExpr([]*LVal{Symbol("list"), seq})
		result := builtinReverse(env, args)
		if result.Type == LError {
			t.Fatalf("unexpected error: %v", result)
		}
		if result.Len() != 3 {
			t.Errorf("expected 3 elements, got %d", result.Len())
		}
		// Verify reversed order.
		if result.Cells[0].Int != 3 || result.Cells[2].Int != 1 {
			t.Errorf("expected reversed [3 2 1], got %v", result)
		}
	})
}

func TestCheckAllocMap(t *testing.T) {
	t.Parallel()
	t.Run("exceeds", func(t *testing.T) {
		t.Parallel()
		env := initSafetyTestEnv(t)
		env.Runtime.MaxAlloc = 2
		// map needs a function — use the identity function.
		identity := env.Lambda(
			SExpr([]*LVal{Symbol("x")}),
			[]*LVal{Symbol("x")},
		)
		seq := QExpr([]*LVal{Int(1), Int(2), Int(3)})
		args := SExpr([]*LVal{Symbol("list"), identity, seq})
		result := builtinMap(env, args)
		msg := requireLError(t, result)
		if !strings.Contains(msg, "allocation size 3 exceeds maximum (2)") {
			t.Errorf("wrong error: %s", msg)
		}
	})
	t.Run("within limit", func(t *testing.T) {
		t.Parallel()
		env := initSafetyTestEnv(t)
		env.Runtime.MaxAlloc = 10
		identity := env.Lambda(
			SExpr([]*LVal{Symbol("x")}),
			[]*LVal{Symbol("x")},
		)
		seq := QExpr([]*LVal{Int(1), Int(2)})
		args := SExpr([]*LVal{Symbol("list"), identity, seq})
		result := builtinMap(env, args)
		if result.Type == LError {
			t.Fatalf("unexpected error: %v", result)
		}
		if result.Len() != 2 {
			t.Errorf("expected 2 elements, got %d", result.Len())
		}
	})
}

// --- Macro expansion depth limit tests (HC-9) ---

func TestMacroExpansionDepthLimit(t *testing.T) {
	t.Parallel()

	t.Run("infinite expansion caught", func(t *testing.T) {
		t.Parallel()
		env := initSafetyTestEnv(t)
		env.Runtime.MaxMacroExpansionDepth = 10

		env.AddMacros(true, &langBuiltin{
			name:    "infinite-macro",
			formals: Formals(VarArgSymbol, "args"),
			fun: func(env *LEnv, args *LVal) *LVal {
				return SExpr([]*LVal{Symbol("infinite-macro")})
			},
			docs: "test macro that expands infinitely",
		})

		result := env.Eval(SExpr([]*LVal{Symbol("infinite-macro")}))
		msg := requireLError(t, result)
		// The counter increments before the check, so the first failure is at
		// macroDepth = 11 (one past the limit of 10).
		if !strings.Contains(msg, "macro expansion depth exceeded (11 expansions)") {
			t.Errorf("error should report exactly 11 expansions, got: %s", msg)
		}
	})

	t.Run("finite expansion within limit succeeds", func(t *testing.T) {
		t.Parallel()
		env := initSafetyTestEnv(t)
		env.Runtime.MaxMacroExpansionDepth = 10

		// A macro that expands to (countdown N-1) until N=0, then to 42.
		// With N=5, it re-expands 5 times, well within the limit of 10.
		env.AddMacros(true, &langBuiltin{
			name:    "countdown",
			formals: Formals("n"),
			fun: func(env *LEnv, args *LVal) *LVal {
				n := args.Cells[0]
				if n.Type != LInt || n.Int <= 0 {
					return Int(42)
				}
				return SExpr([]*LVal{Symbol("countdown"), Int(n.Int - 1)})
			},
			docs: "test macro that expands N times then terminates",
		})

		result := env.Eval(SExpr([]*LVal{Symbol("countdown"), Int(5)}))
		if result.Type == LError {
			t.Fatalf("expected success for 5 expansions within limit 10, got error: %v", result)
		}
		if result.Type != LInt || result.Int != 42 {
			t.Errorf("expected 42, got: %v", result)
		}
	})
}
