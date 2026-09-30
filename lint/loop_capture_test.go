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


// A closure in the dotimes result form runs after the loop, once: it sees
// the final value on purpose.  Only the body is checked.
func TestLoopVariableCapture_ResultFormNotReported(t *testing.T) {
	assertNoDiags(t, lintCheck(t, AnalyzerLoopVariableCapture,
		`(dotimes (i 3 (set 'f (lambda () i))) (g i))`))
}

// A local function named like a storing call does not store the closure.
func TestLoopVariableCapture_ShadowedStoringCall(t *testing.T) {
	assertNoDiags(t, lintCheck(t, AnalyzerLoopVariableCapture,
		`(flet ((list (f) (funcall f))) (dotimes (i 3) (list (lambda () i))))`))
}

// A symbol in a quasiquote template is data, not a capture.
func TestLoopVariableCapture_TemplateIsNotCapture(t *testing.T) {
	assertNoDiags(t, lintCheck(t, AnalyzerLoopVariableCapture,
		`(dotimes (i 3) (set 'f (lambda () (quasiquote (i)))))`))
	require.Len(t, lintCheck(t, AnalyzerLoopVariableCapture,
		`(dotimes (i 3) (set 'f (lambda () (quasiquote ((unquote i))))))`), 1)
}

// A deftype in the loop body does not hide the loop variable.
func TestLoopVariableCapture_DeftypeInBody(t *testing.T) {
	require.Len(t, lintCheck(t, AnalyzerLoopVariableCapture,
		`(dotimes (i 3) (deftype pt (x) x) (set 'f (lambda () i)))`), 1)
}

// With workspace config, custom definition forms keep their meaning: the
// check reuses the file's analysis rather than re-analyzing without it.
func TestLoopVariableCapture_UsesFileAnalysis(t *testing.T) {
	l := &Linter{Analyzers: []*Analyzer{AnalyzerLoopVariableCapture}}
	cfg := &analysis.Config{DefForms: []analysis.DefFormSpec{{Head: "defthing", FormalsIndex: 2, BindsName: true, NameIndex: 1}}}
	diags, err := l.LintFileWithAnalysis([]byte("(dotimes (i 3)\n  (defthing t1 (i) (set 'f (lambda () i))))"), "test.lisp", cfg)
	require.NoError(t, err)
	assertNoDiags(t, diags) // defthing's parameter i shadows the loop variable
}
