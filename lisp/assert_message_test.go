// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"fmt"
	"strings"
	"testing"

	"github.com/luthersystems/elps/elpsutil"
	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/require"
)

// Issue #657: assertion messages are expressions, evaluated only after a
// false test. Their syntax and type must not preempt the test's result.
func TestAssertMessageSuccessSkipsAllMessageForms(t *testing.T) {
	for _, message := range []string{`7`, `missing-message`, `(message-value)`, `"{}"`} {
		t.Run(message, func(t *testing.T) {
			env := newCallSemanticsEnv(t)
			var events []string
			env.AddBuiltins(true,
				elpsutil.Function("test-value", lisp.Formals(), func(*lisp.LEnv, *lisp.LVal) *lisp.LVal {
					events = append(events, "test")
					return lisp.Bool(true)
				}),
				elpsutil.Function("message-value", lisp.Formals(), func(*lisp.LEnv, *lisp.LVal) *lisp.LVal {
					events = append(events, "message")
					return lisp.Int(7)
				}),
				elpsutil.Function("argument-value", lisp.Formals(), func(*lisp.LEnv, *lisp.LVal) *lisp.LVal {
					events = append(events, "argument")
					return lisp.Int(9)
				}))
			got := env.LoadString("assert-message-success.lisp", fmt.Sprintf(`(assert (test-value) %s (argument-value))`, message))
			require.False(t, lisp.IsInternalPanic(got), "%v", got)
			require.True(t, got.IsNil(), "%v", got)
			require.Equal(t, []string{"test"}, events)
		})
	}
}

func TestAssertMessageEvaluatesInOrderOnEveryCall(t *testing.T) {
	env := newCallSemanticsEnv(t)
	var events []string
	for _, callback := range []struct {
		name  string
		value *lisp.LVal
	}{
		{"test-value", lisp.Bool(false)},
		{"message-value", lisp.String("{} + {}")},
		{"first-value", lisp.Int(20)},
		{"second-value", lisp.Int(22)},
	} {
		env.AddBuiltins(true, elpsutil.Function(callback.name, lisp.Formals(), func(*lisp.LEnv, *lisp.LVal) *lisp.LVal {
			events = append(events, callback.name)
			return callback.value
		}))
	}
	exprs, err := env.Runtime.Reader.Read("assert-message-order.lisp", strings.NewReader(`(assert (test-value) (message-value) (first-value) (second-value))`))
	require.NoError(t, err)
	require.Len(t, exprs, 1)
	for range 2 {
		events = nil
		got := env.Eval(exprs[0])
		require.False(t, lisp.IsInternalPanic(got), "%v", got)
		require.Equal(t, lisp.LError, got.Type, "%v", got)
		require.Equal(t, "error", got.Str)
		require.Contains(t, got.String(), "20 + 22")
		require.Equal(t, []string{"test-value", "message-value", "first-value", "second-value"}, events,
			"each invocation must evaluate the original forms in order, without rewriting the source")
	}
}

func TestAssertMessageAcceptsBoundString(t *testing.T) {
	got := newCallSemanticsEnv(t).LoadString("assert-bound-message.lisp", `(let ((message "bad input")) (assert false message))`)
	require.False(t, lisp.IsInternalPanic(got), "%v", got)
	require.Equal(t, lisp.LError, got.Type, "%v", got)
	require.Equal(t, "error", got.Str)
	require.Contains(t, got.String(), "bad input")
}

func TestAssertMessageRejectsEvaluatedNonStringBeforeArguments(t *testing.T) {
	env := newCallSemanticsEnv(t)
	var events []string
	for _, callback := range []struct {
		name  string
		value *lisp.LVal
	}{
		{"test-value", lisp.Bool(false)},
		{"message-value", lisp.Int(7)},
		{"argument-value", lisp.Int(9)},
	} {
		env.AddBuiltins(true, elpsutil.Function(callback.name, lisp.Formals(), func(*lisp.LEnv, *lisp.LVal) *lisp.LVal {
			events = append(events, callback.name)
			return callback.value
		}))
	}
	got := env.LoadString("assert-message-type.lisp", `(assert (test-value) (message-value) (argument-value))`)
	require.False(t, lisp.IsInternalPanic(got), "%v", got)
	require.Equal(t, lisp.LError, got.Type, "%v", got)
	require.Equal(t, "error", got.Str)
	require.Contains(t, got.String(), "second argument is not a string: int")
	require.Equal(t, []string{"test-value", "message-value"}, events)
}

func TestAssertMessagePreservesFirstErrorIdentity(t *testing.T) {
	for failureIndex, failureSite := range []string{"test", "message", "first argument", "second argument"} {
		for _, marked := range []bool{false, true} {
			t.Run(fmt.Sprintf("%s/marked=%t", failureSite, marked), func(t *testing.T) {
				env := newCallSemanticsEnv(t)
				original := env.LoadString("original-assert-error.lisp", `(error 'assert-probe "original data")`)
				if marked {
					env.AddBuiltins(true, elpsutil.Function("host-fault", lisp.Formals(), func(*lisp.LEnv, *lisp.LVal) *lisp.LVal {
						panic("assert message host fault")
					}))
					original = env.LoadString("original-assert-error.lisp", `(host-fault)`)
				}
				require.Equal(t, lisp.LError, original.Type)
				require.Equal(t, marked, lisp.IsInternalPanic(original))
				location, ok := original.Source()
				require.True(t, ok)
				require.Positive(t, location.Line, "ordinary source association must not be mistaken for assertion mutation")
				stack, rendered := original.CallStack(), original.String()
				var events []string
				names := []string{"test-value", "message-value", "first-value", "second-value", "trailing-value"}
				values := []*lisp.LVal{lisp.Bool(false), lisp.String("{} {} {}"), lisp.Int(1), lisp.Int(2), lisp.Int(3)}
				for index, name := range names {
					env.AddBuiltins(true, elpsutil.Function(name, lisp.Formals(), func(*lisp.LEnv, *lisp.LVal) *lisp.LVal {
						events = append(events, name)
						if index == failureIndex {
							return original
						}
						return values[index]
					}))
				}
				got := env.LoadString("assert-message-error.lisp", `(assert (test-value) (message-value) (first-value) (second-value) (trailing-value))`)
				require.Same(t, original, got)
				require.Same(t, stack, got.CallStack())
				require.Equal(t, rendered, got.String())
				require.Equal(t, marked, lisp.IsInternalPanic(got))
				require.Equal(t, names[:failureIndex+1], events, "the first failure stops every later expression")
			})
		}
	}
}
