// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"errors"
	"fmt"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestIsError(t *testing.T) {
	assert.True(t, lisp.Errorf("boom").IsError())
	for _, v := range []*lisp.LVal{lisp.Nil(), lisp.Int(1), lisp.String("x"), lisp.SortedMap()} {
		assert.False(t, v.IsError(), v.Type.String())
	}
}

func TestResult(t *testing.T) {
	v, err := lisp.Result(lisp.Int(3))
	assert.NoError(t, err)
	assert.Equal(t, 3, v.Int)

	lerr := lisp.ErrorConditionf("my-condition", "boom")
	v, err = lisp.Result(lerr)
	assert.Nil(t, v)
	var ev *lisp.ErrorVal
	if assert.ErrorAs(t, err, &ev) {
		assert.Same(t, lerr, (*lisp.LVal)(ev), "Result returns the error value itself")
	}
}

// TestConditionOfMatchesLisp checks that ConditionOf names the condition the
// error has once env.Error raises it, which is what handler-bind sees.
func TestConditionOfMatchesLisp(t *testing.T) {
	env := testEnv(t)
	env.PutGlobal(lisp.Symbol("host-panic"), lisp.FunInPackage(lisp.DefaultUserPackage, "host-panic", lisp.Formals(),
		func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal { panic("host fault") }))
	panicked := env.LoadString("panic.lisp", `(host-panic)`)
	require.True(t, lisp.IsInternalPanic(panicked), "%v", panicked)
	budget := lisp.ErrorConditionf(lisp.CondStepBudgetExceeded, "step budget exceeded")
	for name, err := range map[string]error{
		"bare":           lisp.GoError(budget),
		"wrapped %w":     fmt.Errorf("load: %w", lisp.GoError(budget)),
		"wrapped %v":     fmt.Errorf("load: %v", lisp.GoError(budget)),
		"plain":          errors.New("plain"),
		"typed nil":      (*lisp.ErrorVal)(nil),
		"step limit":     lisp.GoError(lisp.ErrorConditionf(lisp.CondStepLimitExceeded, "x")),
		"internal panic": lisp.GoError(panicked),
		"wrapped panic":  fmt.Errorf("host: %w", lisp.GoError(panicked)),
	} {
		t.Run(name, func(t *testing.T) {
			raised := env.Error(err)
			assert.Equal(t, raised.Str, lisp.ConditionOf(err))
		})
	}
	assert.Equal(t, "", lisp.ConditionOf(nil))
}

func TestFuncE(t *testing.T) {
	env := testEnv(t)
	inner := lisp.ErrorConditionf("my-condition", "inner")
	bind := func(name string, f func(env *lisp.LEnv, args *lisp.LVal) (*lisp.LVal, error)) {
		env.PutGlobal(lisp.Symbol(name), lisp.FunInPackage(lisp.DefaultUserPackage, name, lisp.Formals("x"), lisp.FuncE(f)))
	}
	bind("ok", func(env *lisp.LEnv, args *lisp.LVal) (*lisp.LVal, error) {
		return lisp.Int(args.Cells[0].Int + 1), nil
	})
	bind("nothing", func(env *lisp.LEnv, args *lisp.LVal) (*lisp.LVal, error) { return nil, nil })
	bind("bare", func(env *lisp.LEnv, args *lisp.LVal) (*lisp.LVal, error) { return lisp.Int(1), lisp.GoError(inner) })
	bind("plain", func(env *lisp.LEnv, args *lisp.LVal) (*lisp.LVal, error) { return nil, errors.New("plain failure") })
	bind("wrapped", func(env *lisp.LEnv, args *lisp.LVal) (*lisp.LVal, error) {
		return nil, fmt.Errorf("context: %w", lisp.GoError(inner))
	})

	assert.Equal(t, "2", env.LoadString("t", `(ok 1)`).String())
	assert.Equal(t, "()", env.LoadString("t", `(nothing 1)`).String())

	got := env.LoadString("t", `(bare 1)`)
	assert.Same(t, inner, got, "a bare *ErrorVal is returned as itself")

	got = env.LoadString("t", `(plain 1)`)
	require.Equal(t, lisp.LError, got.Type)
	assert.Equal(t, "error", got.Str)
	assert.Contains(t, got.String(), "plain failure")

	got = env.LoadString("t", `(wrapped 1)`)
	assert.Equal(t, "error", got.Str, "a wrapped *ErrorVal gets condition error")

	caught := env.LoadString("t", `(handler-bind ((my-condition (lambda (c &rest _) "caught"))) (bare 1))`)
	assert.Equal(t, `"caught"`, caught.String())
}
