// Copyright © 2026 The ELPS authors

package lint

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestHandlerOrder_CatchAllFirst(t *testing.T) {
	diags := lintCheck(t, AnalyzerHandlerOrder, `(handler-bind ((condition (lambda (&rest _) 1))
               (boom (lambda (&rest _) 2)))
  (error 'boom))`)
	assert.Len(t, diags, 1)
	assertHasDiag(t, diags, "handler for boom follows a handler for its ancestor condition")
	assertDiagOnLine(t, diags, 2, "handler for boom")
}

func TestHandlerOrder_BuiltinParentFirst(t *testing.T) {
	diags := lintCheck(t, AnalyzerHandlerOrder, `(handler-bind ((error (lambda (&rest _) 1))
               (argument-error (lambda (&rest _) 2)))
  (f))`)
	assert.Len(t, diags, 1)
	assertHasDiag(t, diags, "its ancestor error")
}

func TestHandlerOrder_DefinedParentFirst(t *testing.T) {
	diags := lintCheck(t, AnalyzerHandlerOrder, `(define-condition 'storage-error 'error)
(define-condition "not-found" (quote storage-error))
(handler-bind ((error (lambda (&rest _) 1))
               (not-found (lambda (&rest _) 2)))
  (f))`)
	assert.Len(t, diags, 1)
	assertHasDiag(t, diags, "handler for not-found follows a handler for its ancestor error")
}

func TestHandlerOrder_Duplicate(t *testing.T) {
	diags := lintCheck(t, AnalyzerHandlerOrder, `(handler-bind ((boom (lambda (&rest _) 1))
               (boom (lambda (&rest _) 2)))
  (f))`)
	assert.Len(t, diags, 1)
	assertHasDiag(t, diags, "handler for boom never runs")
}

func TestHandlerOrder_Negative(t *testing.T) {
	diags := lintCheck(t, AnalyzerHandlerOrder, `(define-condition 'not-found 'error)
(handler-bind ((not-found (lambda (&rest _) 1))
               (argument-error (lambda (&rest _) 2))
               (error (lambda (&rest _) 3))
               (other (lambda (&rest _) 4))
               (condition (lambda (&rest _) 5)))
  (f))
(handler-bind ((error (lambda (&rest _) 1))
               (unrelated (lambda (&rest _) 2)))
  (f))
(define-condition child-var 'error)
(handler-bind ((error (lambda (&rest _) 1))
               (child-var (lambda (&rest _) 2)))
  (f))`)
	assertNoDiags(t, diags)
}

func TestHandlerOrder_Nolint(t *testing.T) {
	diags := lintCheck(t, AnalyzerHandlerOrder, `(handler-bind ((condition (lambda (&rest _) 1))
               (boom (lambda (&rest _) 2))) ; nolint:handler-order
  (error 'boom))`)
	assertNoDiags(t, diags)
}

// A binding for internal-panic after condition is not reported: condition
// never catches a recovered panic, so the binding has always run.
func TestHandlerOrder_InternalPanicAfterCatchAll(t *testing.T) {
	assertNoDiags(t, lintCheck(t, AnalyzerHandlerOrder, `(handler-bind ((condition (lambda (&rest _) 1))
               (internal-panic (lambda (&rest _) 2)))
  (f))`))
}

// lisp:handler-bind and lisp:define-condition are checked like the bare names.
func TestHandlerOrder_QualifiedHeads(t *testing.T) {
	diags := lintCheck(t, AnalyzerHandlerOrder, `(lisp:define-condition 'storage-error 'error)
(lisp:handler-bind ((error (lambda (&rest _) 1))
                    (storage-error (lambda (&rest _) 2)))
  (f))`)
	assert.Len(t, diags, 1)
	assertHasDiag(t, diags, "handler for storage-error follows a handler for its ancestor error")
}
