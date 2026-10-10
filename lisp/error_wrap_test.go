// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/elpsutil"
	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestRethrowContext(t *testing.T) {
	env := newCallSemanticsEnv(t)
	load := func(src string) string {
		t.Helper()
		v := env.LoadString("wrap.lisp", src)
		require.NotEqual(t, lisp.LError, v.Type, "%v", v)
		return v.String()
	}
	load(`(define-condition 'storage-error 'error)
	      (define-condition 'not-found 'storage-error)
	      (defun load-user (id) (error 'not-found "no user" id))
	      (defun get-profile (id)
	        (handler-bind ((not-found
	                         (lambda (c &rest _)
	                           (rethrow :context (format-string "loading user {}" id)))))
	          (load-user id)))`)

	// The condition and data are the original's; the message gains the context.
	assert.Equal(t, `'('not-found '("no user" 42) "loading user 42: no user 42")`,
		load(`(handler-bind ((storage-error
		                       (lambda (c &rest data) (list c data (error-message)))))
		        (get-profile 42))`))
	// A second wrap goes in front of the first.
	assert.Equal(t, `"outer: loading user 7: no user 7"`,
		load(`(handler-bind ((error (lambda (&rest _) (error-message))))
		        (handler-bind ((not-found (lambda (&rest _) (rethrow :context "outer"))))
		          (get-profile 7)))`))
	// The stack is the original's: error-stack still starts in load-user.
	assert.Equal(t, `"user:load-user"`,
		load(`(handler-bind ((error (lambda (&rest _) (get (second (error-stack)) "function"))))
		        (get-profile 1))`))
	// Without :context, rethrow re-raises the error unchanged.
	assert.Equal(t, `"no user 3"`,
		load(`(handler-bind ((error (lambda (&rest _) (error-message))))
		        (handler-bind ((not-found (lambda (&rest _) (rethrow))))
		          (load-user 3)))`))
	// :context must be a string.
	assert.Equal(t, `'('argument-error "context is not a string: int")`,
		load(`(handler-bind ((error (lambda (c &rest _) (list c (error-message)))))
		        (handler-bind ((not-found (lambda (&rest _) (rethrow :context 3))))
		          (load-user 3)))`))

	// Uncaught, the error renders with its condition and the context.
	v := env.LoadString("wrap.lisp", `(get-profile 9)`)
	require.Equal(t, lisp.LError, v.Type)
	e := (*lisp.ErrorVal)(v)
	assert.Equal(t, "not-found", e.Condition())
	assert.Equal(t, "loading user 9: no user 9", e.ErrorMessage())
	assert.Contains(t, e.Error(), "not-found: loading user 9: no user 9")
	assert.Equal(t, []string{"loading user 9"}, e.ErrorContext())

	// error-message is only for a handler.
	v = env.LoadString("wrap.lisp", `(error-message)`)
	require.Equal(t, lisp.LError, v.Type)
	assert.Contains(t, v.String(), "not inside a handler-bind handler")
}

func TestWrapError(t *testing.T) {
	inner := lisp.ErrorConditionf("not-found", "no user %d", 42)
	wrapped := lisp.WrapError(inner, "loading user %d", 42)
	twice := lisp.WrapError(wrapped, "outer")
	assert.Equal(t, "no user 42", (*lisp.ErrorVal)(inner).ErrorMessage(), "the original is not changed")
	assert.Nil(t, (*lisp.ErrorVal)(inner).ErrorContext())
	assert.Equal(t, "loading user 42: no user 42", (*lisp.ErrorVal)(wrapped).ErrorMessage())
	assert.Equal(t, "outer: loading user 42: no user 42", (*lisp.ErrorVal)(twice).ErrorMessage())
	assert.Equal(t, []string{"outer", "loading user 42"}, (*lisp.ErrorVal)(twice).ErrorContext())
	assert.Equal(t, "not-found", twice.Str)
	assert.Same(t, inner.Cells[0], twice.Cells[0], "the data is shared, not copied")
	notErr := lisp.Int(1)
	assert.Same(t, notErr, lisp.WrapError(notErr, "x"))

	// A builtin that returns a wrapped error with no stack gets a stack when
	// the evaluator associates it, and keeps the context.
	env := newCallSemanticsEnv(t)
	env.AddBuiltins(true, elpsutil.Function("fail-wrapped", lisp.Formals(), func(*lisp.LEnv, *lisp.LVal) *lisp.LVal {
		return lisp.WrapError(lisp.ErrorConditionf("not-found", "no user"), "in builtin")
	}))
	v := env.LoadString("wrap.lisp", `(handler-bind ((not-found (lambda (&rest _)
	                                       (list (error-message) (> (length (error-stack)) 0)))))
	                                     (fail-wrapped))`)
	require.NotEqual(t, lisp.LError, v.Type, "%v", v)
	assert.Equal(t, `'("in builtin: no user" true)`, v.String())
}
