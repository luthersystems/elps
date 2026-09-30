// Copyright © 2026 The ELPS authors

package perf

import (
	"testing"

	"github.com/stretchr/testify/require"
)

func TestScanFilePreservesSyntacticPolicy(t *testing.T) {
	exprs := parseSource(t, `(defun f malformed
 (let ((entry (init))) (body))
 (lambda malformed (callback))
 (lisp:lambda malformed (qualified-body))
 (quote (quoted-body)) (quasiquote (unquote (template-hole)))
 (unquote (ordinary-hole))
 (funcall (ignored-function-argument) (argument))
 (defun nested () (nested-body)))
(lisp:defun qualified-definition () (ignored-body))`)
	summaries := ScanFile(exprs, "syntax.lisp", DefaultConfig())
	require.Len(t, summaries, 2)
	require.Equal(t, "f", summaries[0].Name)
	var callees []string
	for _, edge := range summaries[0].Calls {
		callees = append(callees, edge.Callee)
		require.Zero(t, edge.Context.LoopDepth)
		require.False(t, edge.Context.InLoop)
	}
	// Binding lists still look like dynamic calls to this cost heuristic,
	// and qualified kernel names still look like ordinary named calls.
	require.Equal(t, []string{
		"<dynamic>", "entry", "init", "body", "callback", "lisp:lambda", "qualified-body",
		"unquote", "ordinary-hole", "<dynamic>", "argument",
	}, callees)
	require.Equal(t, 9, summaries[0].LocalCost)
	require.Equal(t, "nested", summaries[1].Name)
	require.Equal(t, 1, summaries[1].LocalCost)
	require.Equal(t, "nested-body", summaries[1].Calls[0].Callee)
}
