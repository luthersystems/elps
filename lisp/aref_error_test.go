// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"fmt"
	"testing"

	"github.com/luthersystems/elps/elpsutil"
	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/require"
)

func TestARefPreservesStoredError(t *testing.T) {
	env := newSortErrorEnv(t)
	original := env.LoadString("original.lisp", `(error 'backend-error "amount" 17)`)
	require.Equal(t, lisp.LError, original.Type)
	stack := original.CallStack()
	env.PutGlobal(lisp.Symbol("source"), lisp.Vector([]*lisp.LVal{original}))
	got := env.LoadString("aref-error.lisp", `(aref source 0)`)
	require.Same(t, original, got, "array access must propagate the element, not wrap it as a different condition")
	require.Same(t, stack, got.CallStack())
	require.Equal(t, "backend-error", got.Str)
	require.Len(t, got.Cells, 2)
	require.Equal(t, "amount", got.Cells[0].Str)
	require.Equal(t, 17, got.Cells[1].Int)
	got = env.LoadString("aref-error.lisp", `
		(handler-bind ((backend-error (lambda (c message amount) amount)))
		  (aref source 0))`)
	require.Equal(t, lisp.LInt, got.Type, "%v", got)
	require.Equal(t, 17, got.Int)
}

func TestARefPreservesStoredPanicMarker(t *testing.T) {
	env := newSortErrorEnv(t)
	env.AddBuiltins(true, elpsutil.Function("host-failure", lisp.Formals(),
		func(*lisp.LEnv, *lisp.LVal) *lisp.LVal { panic("stored host panic") }))
	original := env.LoadString("original.lisp", `(host-failure)`)
	require.True(t, lisp.IsInternalPanic(original))
	env.PutGlobal(lisp.Symbol("source"), lisp.Vector([]*lisp.LVal{original}))
	for _, form := range []string{
		`%s`,
		`(ignore-errors %s)`,
		`(handler-bind ((condition (lambda (&rest _) 'swallowed))) %s)`,
	} {
		got := env.LoadString("aref-error.lisp", fmt.Sprintf(form, `(aref source 0)`))
		require.True(t, lisp.IsInternalPanic(got), "%s: %v", form, got)
		require.Same(t, original, got)
	}
	got := env.LoadString("aref-error.lisp", `
		(handler-bind ((internal-panic (lambda (&rest _) 'explicit)))
		  (aref source 0))`)
	require.Equal(t, "'explicit", got.String())
}

func TestARefIndexErrorsRemainOrdinary(t *testing.T) {
	for _, expr := range []string{
		`(aref (vector 1) -1)`, `(aref (vector 1) 1)`,
		`(aref (vector 1) "0")`, `(aref (vector 1))`,
	} {
		env := newSortErrorEnv(t)
		got := env.LoadString("aref-error.lisp", expr)
		require.Equal(t, lisp.LError, got.Type, "%s: %v", expr, got)
		require.False(t, lisp.IsInternalPanic(got), "%v", got)
		require.Contains(t, got.String(), "aref-error.lisp:1:1")
		require.NotNil(t, got.CallStack())
	}
}
