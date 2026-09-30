// Copyright © 2026 The ELPS authors

package lint

import (
	"testing"

	"github.com/stretchr/testify/require"
)

// builtin-arity asks the code walker which lists are code: a call-shaped
// list inside quoted data is not a call.
func TestBuiltinArity_SkipsQuotedData(t *testing.T) {
	for _, src := range []string{
		`(set 'a (quote (car)))`,
		`(set 'b '((car) 1))`,
		`(set 'c '(x (cdr)))`,
	} {
		assertNoDiags(t, lintCheck(t, AnalyzerBuiltinArity, src))
	}
	// Code still is.
	require.Len(t, lintCheck(t, AnalyzerBuiltinArity, `(list (car))`), 1)
	require.Len(t, lintCheck(t, AnalyzerBuiltinArity, `(let ([x (car)]) x)`), 1)
}

func TestRethrowTemplateRegressions(t *testing.T) {
	// Templates inside a handler in a macro body, and bracketed bindings,
	// are not reported.
	assertNoDiags(t, lintCheck(t, AnalyzerRethrowContext,
		`(defmacro m () (handler-bind ((condition (lambda (c) (let ([v (rethrow)]) v)))) 1))`))
	assertNoDiags(t, lintCheck(t, AnalyzerRethrowContext,
		`(defmacro m () (handler-bind ((condition (lambda (c) (quasiquote (rethrow))))) 1))`))
}
