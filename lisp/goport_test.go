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
