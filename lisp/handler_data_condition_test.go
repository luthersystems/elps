// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/elpsutil"
	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// handlerDataDepthEnv returns an environment whose value-depth limit is small
// enough to reach with a value a test can build quickly, along with condition
// data that exceeds it.
func handlerDataDepthEnv(t *testing.T, limit int) (*lisp.LEnv, *lisp.LVal) {
	t.Helper()
	env := newCallSemanticsEnv(t)
	env.Runtime.MaxValueDepth = limit
	data := lisp.QExpr(nil)
	for range limit + 100 {
		data = lisp.QExpr([]*lisp.LVal{data})
	}
	env.AddBuiltins(true, elpsutil.Function("raise-data", lisp.Formals(),
		func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal { return env.ErrorCondition("original", data) }))
	return env, data
}

// TestHandlerDataDepthFailureKeepsCopierCondition pins what handler-bind
// raises when copying condition data for a handler fails.
//
// The copier's failure is already a classified condition carrying its own Go
// error, and handler-bind's docstring promises "an ordinary depth error that
// propagates to an outer handler".  Re-raising the copier's rendered text
// under a freshly built 'error dropped both halves of that: the value-depth
// error was no longer reachable with errors.As, so neither a host nor an
// outer handler could tell a nesting limit apart from any other copy failure.
func TestHandlerDataDepthFailureKeepsCopierCondition(t *testing.T) {
	const limit = 1024
	env, _ := handlerDataDepthEnv(t, limit)

	result := env.LoadString("handler-data.lisp", `
		(set 'handled false)
		(handler-bind ((condition (lambda (c data) (set! handled true) data))) (raise-data))`)
	assert.Equal(t, "false", env.LoadString("handler-data.lisp", "handled").String())
	require.Equal(t, lisp.LError, result.Type, "%v", result)
	assert.False(t, lisp.IsInternalPanic(result))
	text := diagnosticText(result)
	assert.Contains(t, text, "handler data cannot be copied")
	assert.Contains(t, text, "value nesting depth exceeds maximum: 1024")

	var depthErr lisp.ValueDepthError
	require.ErrorAs(t, lisp.GoError(result), &depthErr, "the copier's own error must stay reachable: %v", result)
	assert.Equal(t, lisp.ValueDepthError(limit), depthErr)

	// The condition is the copier's own, not one handler-bind invented.
	assert.Equal(t, lisp.Error(lisp.ValueDepthError(limit)).Str, result.Str)
	assert.Nil(t, env.Runtime.CurrentCondition())
}

// The copy failure propagates past the handler-bind that could not dispatch
// and reaches an outer handler, which is what the docstring describes.
func TestHandlerDataCopyFailureReachesOuterHandler(t *testing.T) {
	const limit = 1024
	env, _ := handlerDataDepthEnv(t, limit)

	result := env.LoadString("handler-data.lisp", `
		(handler-bind ((error (lambda (c &rest args) 'outer-caught)))
		  (handler-bind ((original (lambda (c data) data))) (raise-data)))`)
	require.NoError(t, lisp.GoError(result), "%v", result)
	assert.Equal(t, "'outer-caught", result.String())
	assert.Nil(t, env.Runtime.CurrentCondition())
}
