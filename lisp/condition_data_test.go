// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"errors"
	"fmt"
	"testing"

	"github.com/luthersystems/elps/elpsutil"
	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/require"
)

// Issue #657: host failures must have usable Lisp data at the constructor,
// including errors returned through a Go Reader's error interface.
func TestConditionGoErrorData(t *testing.T) {
	env := newSortErrorEnv(t)
	original := errors.New("host failure")
	for name, makeError := range map[string]func(error) *lisp.LVal{
		"Error":              lisp.Error,
		"ErrorCondition":     func(err error) *lisp.LVal { return lisp.ErrorCondition("backend-error", err) },
		"env.Error":          func(err error) *lisp.LVal { return env.Error(err) },
		"env.ErrorCondition": func(err error) *lisp.LVal { return env.ErrorCondition("backend-error", err) },
	} {
		t.Run(name, func(t *testing.T) {
			got := makeError(original)
			require.Equal(t, lisp.LError, got.Type)
			require.Len(t, got.Cells, 1)
			require.Equal(t, lisp.LString, got.Cells[0].Type)
			require.Equal(t, "host failure", got.Cells[0].Str)
			require.ErrorContains(t, lisp.GoError(got), "host failure")
			require.ErrorIs(t, lisp.GoError(got), original)
		})
	}
}

func TestConditionPreservesErrorValIdentity(t *testing.T) {
	env := newSortErrorEnv(t)
	original := env.LoadString("original.lisp", `(error 'backend-error "amount" 17)`)
	require.Equal(t, lisp.LError, original.Type)
	stack := original.CallStack()
	loc, _ := original.Source()
	for _, wrapped := range []bool{false, true} {
		err := lisp.GoError(original)
		if wrapped {
			err = fmt.Errorf("reader context: %w", err)
		}
		for name, got := range map[string]*lisp.LVal{
			"Error":              lisp.Error(err),
			"ErrorCondition":     lisp.ErrorCondition("replacement", err),
			"env.Error":          env.Error(err),
			"env.ErrorCondition": env.ErrorCondition("replacement", err),
		} {
			t.Run(fmt.Sprintf("%s/wrapped=%t", name, wrapped), func(t *testing.T) {
				require.Same(t, original, got)
				require.Same(t, stack, got.CallStack())
				gotLoc, _ := got.Source()
				require.Equal(t, loc, gotLoc)
				require.Equal(t, "backend-error", got.Str)
				require.Equal(t, "amount", got.Cells[0].Str)
				require.Equal(t, 17, got.Cells[1].Int)
			})
		}
	}
}

func TestConditionHostPanicAndRethrowIdentity(t *testing.T) {
	env := newSortErrorEnv(t)
	env.AddBuiltins(true, elpsutil.Function("host-failure", lisp.Formals(),
		func(*lisp.LEnv, *lisp.LVal) *lisp.LVal { panic("host fault") }))
	original := env.LoadString("origin.lisp", `(host-failure)`)
	require.True(t, lisp.IsInternalPanic(original))
	env.AddBuiltins(true, elpsutil.Function("return-failure", lisp.Formals(),
		func(env *lisp.LEnv, _ *lisp.LVal) *lisp.LVal {
			return env.Error(fmt.Errorf("host wrapper: %w", lisp.GoError(original)))
		}))
	result := env.LoadString("rethrow.lisp", `
  (handler-bind ((internal-panic (lambda (c message)
                   (assert (equal? (type message) 'string))
                   (to-string message)
                   (rethrow))))
    (return-failure))`)
	require.Same(t, original, result)
	require.True(t, lisp.IsInternalPanic(result))
	require.Equal(t, original.CallStack().GoStack, result.CallStack().GoStack)
	result = env.LoadString("catch-all.lisp", `
  (handler-bind ((condition (lambda (&rest _) 'swallowed))) (return-failure))`)
	require.Same(t, original, result)
}

func TestHandlerRaisedErrorPropagatesOutward(t *testing.T) {
	env := newSortErrorEnv(t)
	inner := `(handler-bind ((initial (lambda (&rest _) (error 'secondary "handler failed")))
                         (secondary (lambda (&rest _) 'same-handler)))
            (error 'initial "body failed"))`
	result := env.LoadString("inner.lisp", inner)
	require.Equal(t, lisp.LError, result.Type)
	require.Equal(t, "secondary", result.Str)
	require.Equal(t, "handler failed", result.Cells[0].Str)
	result = env.LoadString("outer.lisp", `
  (handler-bind ((secondary (lambda (c message) (list c message)))) `+inner+`)`)
	require.Equal(t, `'('secondary "handler failed")`, result.String())
}

type conditionCloneError struct{ message string }

func (e *conditionCloneError) Error() string { return e.message }
func (e *conditionCloneError) CloneNative() interface{} {
	return &conditionCloneError{message: e.message}
}

func TestConditionGoErrorCopyAndDetach(t *testing.T) {
	original := &conditionCloneError{message: "host failure"}
	value := lisp.Error(original)
	for name, copyValue := range map[string]func(*lisp.LVal) (*lisp.LVal, error){
		"copy":   func(v *lisp.LVal) (*lisp.LVal, error) { return v.Copy(), nil },
		"detach": lisp.Detach,
	} {
		t.Run(name, func(t *testing.T) {
			copied, err := copyValue(value)
			require.NoError(t, err)
			require.Equal(t, lisp.LString, copied.Cells[0].Type)
			var payload *conditionCloneError
			require.ErrorAs(t, lisp.GoError(copied), &payload)
			require.NotSame(t, original, payload)
			require.Equal(t, "host failure", payload.message)
			payload.message = "changed"
			require.Equal(t, "host failure", original.message)
		})
	}
	opaque := errors.New("opaque")
	value = lisp.Error(opaque)
	_, err := lisp.Detach(value)
	require.Error(t, err, "detachment must not silently share an opaque error")
	require.ErrorIs(t, lisp.GoError(value.Copy()), opaque)
}

type conditionHookError struct{ hook string }

func (e conditionHookError) Error() string {
	if e.hook == "Error" {
		panic("Error hook fault")
	}
	return "host failure"
}
func (e conditionHookError) As(interface{}) bool {
	if e.hook == "As" {
		panic("As hook fault")
	}
	return false
}
func (e conditionHookError) Unwrap() error {
	if e.hook == "Unwrap" {
		panic("Unwrap hook fault")
	}
	return nil
}

func TestConditionContainsHostErrorHookPanic(t *testing.T) {
	for _, hook := range []string{"Error", "As", "Unwrap"} {
		t.Run(hook, func(t *testing.T) {
			env := newSortErrorEnv(t)
			var got *lisp.LVal
			require.NotPanics(t, func() { got = env.Error(conditionHookError{hook: hook}) })
			require.True(t, lisp.IsInternalPanic(got), "%v", got)
			require.Contains(t, got.Cells[0].Str, hook+" hook fault")
		})
	}
}

func TestConditionNilGoErrorData(t *testing.T) {
	var typedNil *lisp.ErrorVal
	for name, err := range map[string]error{"nil": nil, "typed nil": typedNil} {
		t.Run(name, func(t *testing.T) {
			var got *lisp.LVal
			require.NotPanics(t, func() { got = lisp.Error(err) })
			require.Equal(t, lisp.LString, got.Cells[0].Type)
			require.Error(t, lisp.GoError(got))
			if err == nil {
				require.Equal(t, "<nil>", got.Cells[0].Str)
			} else {
				require.Equal(t, "<nil error>", got.Cells[0].Str)
			}
		})
	}
}
