// Copyright © 2026 The ELPS authors

package lint

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/require"
)

func TestWalkEvaluatedPreservesSyntacticPolicy(t *testing.T) {
	for _, tc := range []struct {
		source string
		calls  []string
	}{
		{`(lambda (formal (default-call)) (let ((entry (init))) (body)))`,
			[]string{"lambda", "formal", "default-call", "let", "", "entry", "init", "body"}},
		{`(quasiquote (nested (quasiquote (unquote (first-hole)))
 (quote (unquote-splicing (second-hole))) '(unquote (quoted-hole))
 (unquote (malformed-first) (malformed-second))
 (lisp:unquote (qualified-data)) (unquote (quote (quoted-data)))))`,
			[]string{"first-hole", "second-hole", "quoted-hole", "malformed-first", "malformed-second"}},
		{`(lisp:quasiquote (unquote (lisp:quote (data))))`, nil},
		{`(stop (unvisited))`, []string{"stop"}},
	} {
		t.Run(tc.source, func(t *testing.T) {
			var calls []string
			walkEvaluated(parseTestSource(t, tc.source)[0], func(v *lisp.LVal) bool {
				calls = append(calls, HeadSymbol(v))
				return HeadSymbol(v) != "stop"
			})
			require.Equal(t, tc.calls, calls)
		})
	}
}

func TestWalkLambdaListCallsPreservesSyntacticPolicy(t *testing.T) {
	exprs := parseTestSource(t, `(lisp:lambda malformed
 (lisp:let ([entry (init)]) (body))
 (lisp:flet ([local malformed (local-body)]) (call))
 (lisp:handler-bind (['condition (handler)]) (handled))
 (lisp:cond [(predicate) (branch)])
 (with-cleanup ((cleanup)) (protected))
 (quasiquote (unquote (hole))))`)
	var calls []string
	walkLambdaListCalls(exprs, func(v *lisp.LVal) { calls = append(calls, HeadSymbol(v)) })
	require.Equal(t, []string{
		"lisp:lambda", "lisp:let", "init", "body", "lisp:flet", "local-body", "call",
		"lisp:handler-bind", "handler", "handled", "lisp:cond", "predicate", "branch",
		"with-cleanup", "", "cleanup", "protected",
	}, calls)
}
