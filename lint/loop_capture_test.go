// Copyright © 2026 The ELPS authors

package lint

import (
	"testing"

	"github.com/luthersystems/elps/analysis"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestLoopVariableCapture_Positive(t *testing.T) {
	for _, src := range []string{
		"(dotimes (i 3)\n  (append! fs (lambda () i)))",
		"(dotimes (i 3)\n  (set 'last (lambda () (+ i 1))))",
		"(dotimes (i 3)\n  (set! f (lambda () (when true i))))",
		"(dotimes (i 3)\n  (assoc! m i (lambda () i)))",
		"(dotimes (i 3)\n  (let ((k (* i 2))) (append! fs (cons (lambda () i) k))))",
		"(dotimes (i 3)\n  (append! fs (expr (+ % i))))",
	} {
		t.Run(src, func(t *testing.T) {
			diags := lintCheck(t, AnalyzerLoopVariableCapture, src)
			require.Len(t, diags, 1)
			assertDiagOnLine(t, diags, 2, "captures dotimes variable i")
			assert.Equal(t, SeverityWarning, diags[0].Severity)
		})
	}
}

func TestLoopVariableCapture_Negative(t *testing.T) {
	for _, src := range []string{
		// A fresh binding per turn: the documented fix.
		`(dotimes (i 3) (let ((i i)) (append! fs (lambda () i))))`,
		`(dotimes (i 3) (let ((saved i)) (append! fs (lambda () saved))))`,
		// Called during the same turn, not stored.
		`(dotimes (i 3) (map 'list (lambda (x) (+ x i)) xs))`,
		`(dotimes (i 3) (funcall (lambda () i)))`,
		// The closure does not use the loop variable.
		`(dotimes (i 3) (append! fs (lambda () 1)))`,
		// Its own parameter shadows it.
		`(dotimes (i 3) (append! fs (lambda (i) i)))`,
		// Quoted data, not a closure.
		`(dotimes (i 3) (append! fs '(lambda () i)))`,
	} {
		t.Run(src, func(t *testing.T) {
			assertNoDiags(t, lintCheck(t, AnalyzerLoopVariableCapture, src))
		})
	}
}

func TestLoopVariableCapture_Nolint(t *testing.T) {
	assertNoDiags(t, lintSource(t,
		"(set 'fs (vector))\n(dotimes (i 3) (append! fs (lambda () i))) ; nolint:loop-variable-capture\n"))
}

// With a macro expander the check sees a dotimes, or a stored closure, that
// a user macro produces.
func TestLoopVariableCapture_ThroughMacros(t *testing.T) {
	env := newRethrowEnv(t, `
(defmacro repeat (var n &rest body) (quasiquote (dotimes ((unquote var) (unquote n)) (unquote-splicing body))))
(defmacro remember (x) (quasiquote (append! fs (lambda () (unquote x)))))
`)
	l := &Linter{Analyzers: []*Analyzer{AnalyzerLoopVariableCapture}}
	diags, err := l.LintFileWithAnalysis([]byte("(repeat i 3\n  (remember i))"), "test.lisp",
		&analysis.Config{MacroExpander: &analysis.EnvMacroExpander{Env: env}})
	require.NoError(t, err)
	require.Len(t, diags, 1)
	assert.Equal(t, 2, diags[0].Pos.Line)
	// Without the expander neither form is visible.
	assertNoDiags(t, lintCheck(t, AnalyzerLoopVariableCapture, "(repeat i 3\n  (remember i))"))
}
