// Copyright © 2018 The ELPS authors

package lisp_test

import (
	"bytes"
	"io"
	"strings"
	"testing"

	"github.com/luthersystems/elps/elpstest"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestErrors(t *testing.T) {
	tests := elpstest.TestSuite{
		{"ignore-errors", elpstest.TestSequence{
			// silencing errors and returning nil.
			{`(ignore-errors (progn 0 (error 'test-error "test message") 1))`, `()`, ""},
		}},
		{"handler-bind", elpstest.TestSequence{
			// handling specific types of errors and returning meaningful data.
			{`(handler-bind ((condition list))
				(progn
					(debug-print "do stuff")
					(error 'custom-error "custom data")))`,
				`'('custom-error "custom data")`, "\"do stuff\"\n"},
			{`(handler-bind ((custom-error (lambda (c &rest _) 1)) (condition list))
				(progn
					(debug-print "do stuff")
					(error 'custom-error "custom data")))`,
				`1`, "\"do stuff\"\n"},
			{`(handler-bind ((custom-error (lambda (c &rest _) 1)) (condition list))
				(progn
					(debug-print "do stuff")
					(error 'other-error "other data")))`,
				`'('other-error "other data")`, "\"do stuff\"\n"},
		}},
	}
	elpstest.RunTestSuite(t, tests)
}

func TestRethrow(t *testing.T) {
	tests := elpstest.TestSuite{
		{"rethrow-basic", elpstest.TestSequence{
			// rethrow re-raises the error so the outer handler-bind catches it.
			{`(handler-bind ((condition (lambda (c &rest args) (list 'caught c))))
				(handler-bind ((condition (lambda (c &rest args)
								(rethrow))))
					(error 'my-error "data")))`,
				`'('caught 'my-error)`, ""},
		}},
		{"rethrow-with-side-effects", elpstest.TestSequence{
			// side effects run before rethrow, outer handler catches.
			{`(handler-bind ((condition (lambda (c &rest args) (list 'outer c))))
				(handler-bind ((condition (lambda (c &rest args)
								(debug-print "logged")
								(rethrow))))
					(error 'my-error "data")))`,
				`'('outer 'my-error)`, "\"logged\"\n"},
		}},
		{"rethrow-preserves-condition-type", elpstest.TestSequence{
			// the re-thrown error preserves its original condition type.
			{`(handler-bind ((my-error (lambda (c &rest args) (list 'specific c))))
				(handler-bind ((condition (lambda (c &rest args)
								(rethrow))))
					(error 'my-error "data")))`,
				`'('specific 'my-error)`, ""},
		}},
		{"rethrow-preserves-data", elpstest.TestSequence{
			// the re-thrown error preserves its original data arguments.
			{`(handler-bind ((condition (lambda (c &rest args) (list c args))))
				(handler-bind ((condition (lambda (c &rest args)
								(rethrow))))
					(error 'my-error "arg1" "arg2")))`,
				`'('my-error '("arg1" "arg2"))`, ""},
		}},
		{"rethrow-recatch", elpstest.TestSequence{
			// a re-thrown error can be caught and re-thrown multiple times.
			{`(handler-bind ((condition (lambda (c &rest args) (list 'final c))))
				(handler-bind ((condition (lambda (c &rest args)
								(debug-print "middle")
								(rethrow))))
					(handler-bind ((condition (lambda (c &rest args)
									(debug-print "inner")
									(rethrow))))
						(error 'deep-error "data"))))`,
				`'('final 'deep-error)`, "\"inner\"\n\"middle\"\n"},
		}},
		{"rethrow-specific-then-catch-all", elpstest.TestSequence{
			// specific handler rethrows, catch-all in outer handles it.
			{`(handler-bind ((condition (lambda (c &rest args) "fallback")))
				(handler-bind ((my-error (lambda (c &rest args)
								(rethrow))))
					(error 'my-error "data")))`,
				`"fallback"`, ""},
		}},
		{"rethrow-no-match-propagates", elpstest.TestSequence{
			// if inner handler doesn't match, error propagates naturally
			// (no rethrow needed). Outer handler catches it.
			{`(handler-bind ((condition (lambda (c &rest args) "caught")))
				(handler-bind ((other-error (lambda (c &rest args) "wrong")))
					(error 'my-error "data")))`,
				`"caught"`, ""},
		}},
	}
	elpstest.RunTestSuite(t, tests)
}

// TestRethrowOutsideHandler verifies that calling rethrow outside a
// handler-bind handler produces a clear error.
func TestRethrowOutsideHandler(t *testing.T) {
	env := lisp.NewEnv(nil)
	err := lisp.GoError(lisp.InitializeUserEnv(env,
		lisp.WithReader(parser.NewReader()),
		lisp.WithStderr(io.Discard),
	))
	require.NoError(t, err)

	exprs, parseErr := env.Runtime.Reader.Read("test", strings.NewReader(`(rethrow)`))
	require.NoError(t, parseErr)
	require.Len(t, exprs, 1)

	result := env.Eval(exprs[0])
	assert.Equal(t, lisp.LError, result.Type, "rethrow outside handler should return an error")
	errVal := (*lisp.ErrorVal)(result)
	assert.Contains(t, errVal.Error(), "not inside a handler-bind handler")
}

// TestRethrowPreservesStackTrace verifies that the stack trace from the
// original error site is preserved when rethrowing, not replaced with the
// handler's stack.
func TestRethrowPreservesStackTrace(t *testing.T) {
	env := lisp.NewEnv(nil)
	err := lisp.GoError(lisp.InitializeUserEnv(env,
		lisp.WithMaximumLogicalStackHeight(50000),
		lisp.WithMaximumPhysicalStackHeight(25000),
		lisp.WithReader(parser.NewReader()),
		lisp.WithStderr(io.Discard),
	))
	require.NoError(t, err)

	// Define a function that errors, then catch+rethrow, then catch again.
	// Compare the stack trace of the rethrown error with what we'd get
	// from a fresh (apply error ...) to confirm the original is preserved.
	source := `
(defun failing-fn ()
	(error 'test-error "from failing-fn"))

; Rethrow path: catch and rethrow, preserving stack.
(handler-bind ((condition (lambda (c &rest args)
                (rethrow))))
  (failing-fn))
`
	exprs, parseErr := env.Runtime.Reader.Read("test", strings.NewReader(source))
	require.NoError(t, parseErr)

	var result *lisp.LVal
	for _, expr := range exprs {
		result = env.Eval(expr)
		if result.Type == lisp.LError {
			break
		}
	}
	require.Equal(t, lisp.LError, result.Type, "should produce an error")

	// The rethrown error should have the original stack trace with
	// failing-fn on it.
	stack := result.CallStack()
	require.NotNil(t, stack, "rethrown error should have a call stack")

	var buf bytes.Buffer
	_, writeErr := (*lisp.ErrorVal)(result).WriteTrace(&buf)
	require.NoError(t, writeErr)
	trace := buf.String()
	assert.Contains(t, trace, "failing-fn", "stack trace should reference the original function")
	assert.Contains(t, trace, "test-error", "error should preserve the condition type")
}
