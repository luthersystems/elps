// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/elpstest"
	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
)

// TestArgumentErrorCondition checks that a helper's argument failure raises
// argument-error, a child of error (luthersystems/elps#829).  load-string
// reads its arguments with ArgReader.
func TestArgumentErrorCondition(t *testing.T) {
	tests := elpstest.TestSuite{
		{"handlers", elpstest.TestSequence{
			{`(handler-bind ((argument-error (lambda (c &rest _) c))) (load-string 1))`, `'argument-error`, ""},
			{`(handler-bind ((error (lambda (c &rest _) c))) (load-string 1))`, `'argument-error`, ""},
			{`(handler-bind ((condition (lambda (c &rest _) c))) (load-string 1))`, `'argument-error`, ""},
			{`(ignore-errors (load-string 1))`, `()`, ""},
			// The innermost matching handler wins.
			{`(handler-bind ((error (lambda (c &rest _) 'outer)))
			   (handler-bind ((argument-error (lambda (c &rest _) 'inner)))
			     (load-string 1)))`, `'inner`, ""},
			{`(handler-bind ((argument-error (lambda (c &rest _) 'outer)))
			   (handler-bind ((error (lambda (c &rest _) 'inner)))
			     (load-string 1)))`, `'inner`, ""},
			// A handler for an unrelated condition does not catch it.
			{`(handler-bind ((my-condition (lambda (c &rest _) 'caught)))
			   (load-string 1))`, "test:2:7: lisp:load-string: first argument is not a string: int", ""},
			// A handler for argument-error does not catch its parent.
			{`(handler-bind ((argument-error (lambda (c &rest _) 'caught)))
			   (error 'error "plain"))`, "test:2:7: lisp:error: plain", ""},
			// The message renders as error's does: function name, no
			// condition name.
			{`(load-string 1)`, "test:1:1: lisp:load-string: first argument is not a string: int", ""},
		}},
	}
	elpstest.RunTestSuite(t, tests)
}

func TestConditionIsA(t *testing.T) {
	assert.True(t, lisp.ConditionIsA(lisp.CondArgumentError, lisp.CondArgumentError))
	assert.True(t, lisp.ConditionIsA(lisp.CondArgumentError, "error"))
	assert.True(t, lisp.ConditionIsA("error", "error"))
	assert.False(t, lisp.ConditionIsA("error", lisp.CondArgumentError))
	assert.False(t, lisp.ConditionIsA(lisp.CondArgumentError, "condition"))
	assert.False(t, lisp.ConditionIsA("my-condition", "error"))
	assert.False(t, lisp.ConditionIsA("", "error"))
}

// TestArgumentErrorKeepsPanicExclusion checks the catch-all exclusion with
// the condition hierarchy in place.
func TestArgumentErrorKeepsPanicExclusion(t *testing.T) {
	assert.False(t, lisp.ConditionIsA(lisp.CondInternalPanic, "error"))
}
