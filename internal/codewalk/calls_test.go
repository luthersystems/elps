// Copyright © 2026 The ELPS authors

package codewalk_test

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/internal/codewalk"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/rdparser"
	"github.com/luthersystems/elps/parser/token"
	"github.com/stretchr/testify/require"
)

func TestCallsMatchesSyntacticEvents(t *testing.T) {
	source := `(lambda ()) (lisp:defun 1 ()) (defmacro m malformed)
(deftype ty ()) (benchmark "b" ()) (lambda bad (lambda (&rest) 1))
(flet ((1 (x x) (call)) (empty () (other))) (body))
(lisp:labels ((f () (first))) (last))
(macrolet ((m (x) (template))) (m 1))
(let ((x (init)) bad) (body)) (lisp:let* ((x (init))) (body))
(handler-bind ((condition (lambda () (handler)))) (body))
(cond ((predicate) (consequent)) bad ((otherwise)))
(quote (lambda (x x))) '(lambda (x x))
(quasiquote (unquote (lambda (x x))))
(lisp:quasiquote (unquote (lambda (&rest))))
((lambda (x) (body)) (arg)) (unknown (nested)) (expr (+ % 1))`
	exprs, err := rdparser.New(token.NewScanner("calls.lisp", strings.NewReader(source))).ParseProgram()
	require.NoError(t, err)
	var wantCalls, gotCalls []*lisp.LVal
	var wantFormals, gotFormals []codewalk.Node
	w := codewalk.Walker{
		SyntacticCalls: true,
		Form: func(v *lisp.LVal, _ string, _ int) bool {
			wantCalls = append(wantCalls, v)
			return true
		},
		Formals: func(n *codewalk.Node) { wantFormals = append(wantFormals, *n) },
	}
	for _, expr := range exprs {
		require.Same(t, expr, w.Walk(expr))
	}
	codewalk.Calls(exprs,
		func(v *lisp.LVal) { gotCalls = append(gotCalls, v) },
		func(n codewalk.Node) { gotFormals = append(gotFormals, n) })
	require.Equal(t, wantCalls, gotCalls)
	require.Equal(t, wantFormals, gotFormals)
}

func TestCallsAllocations(t *testing.T) {
	exprs, err := rdparser.New(token.NewScanner("calls.lisp", strings.NewReader(
		`(lambda (x) (let ((y (init x))) (call y))) (flet ((f () (nested))) (f))`))).ParseProgram()
	require.NoError(t, err)
	var calls int
	require.Zero(t, testing.AllocsPerRun(100, func() {
		seen := make(map[*lisp.LVal]bool)
		codewalk.Calls(exprs, func(v *lisp.LVal) {
			seen[v] = true
			calls++
		}, nil)
	}))
	require.Positive(t, calls)
	var formals int
	require.Zero(t, testing.AllocsPerRun(100, func() {
		codewalk.Calls(exprs, nil, func(codewalk.Node) { formals++ })
	}))
	require.Equal(t, 202, formals)
}
