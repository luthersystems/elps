// Copyright © 2026 The ELPS authors

package lint

import (
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestMatchUnreachable_Positive(t *testing.T) {
	diags := lintCheck(t, AnalyzerMatchUnreachable, `(match v
  (x 1)
  (2 2))
(match v
  (_ 1)
  ((list a) 2)
  (:else 3))
(match v
  (:else 1)
  (_ 2))
(lisp:match v (y y) (_ 0))`)
	assert.Len(t, diags, 4, "%v", diags)
	assertDiagOnLine(t, diags, 11, "match clause never runs")
	assertDiagOnLine(t, diags, 3, "match clause never runs")
	assertDiagOnLine(t, diags, 6, "match clause never runs")
	assertDiagOnLine(t, diags, 10, "match clause never runs")
}

func TestMatchUnreachable_Negative(t *testing.T) {
	assertNoDiags(t, lintCheck(t, AnalyzerMatchUnreachable, `(match v
  (x :when (> x 1) 1)
  (:k 2)
  (true 3)
  ('foo 4)
  ([x s:int] 5)
  (user:x 7)
  (_ 6))
(match v (_ 1))
(match v)`))
}

func TestMatchUnreachable_Nolint(t *testing.T) {
	assertNoDiags(t, lintCheck(t, AnalyzerMatchUnreachable, `(match v
  (_ 1)
  (2 2)) ; nolint:match-unreachable`))
}

// The names a match pattern binds are in scope in its guard and body, and
// only there (luthersystems/elps#905).
func TestMatchBindings_UndefinedSymbol(t *testing.T) {
	diags := lintCheckSemantic(t, AnalyzerUndefinedSymbol, `(defun f (v)
  (match v
    ((list a b) (+ a b))
    ((sorted-map "k" x) :when (> x 1) x)
    ([s s:string] s)
    ((vector p &rest q) (list p q))
    (_ (+ y 1))))
(defun g () a)
(defun h (v) (lisp:match v (z z)))`)
	assert.Len(t, diags, 2, "%v", diags)
	assertDiagOnLine(t, diags, 7, "undefined symbol: y")
	assertDiagOnLine(t, diags, 8, "undefined symbol: a")
}

// The names of a let binding pattern are bound in the let's scope
// (luthersystems/elps#910): let binds them after every initializer, let*
// in order.
func TestLetPatternScope(t *testing.T) {
	src := `(defun f (v)
  (let* ([(list a b) v] [(sorted-map "k" c) a] [[d s:int] c] [(vector e &rest _) d])
    (list a b c d e)))
(defun g (v)
  (let ([(list x y) v] [z x])
    (list x y z)))
(f 1) (g 1)`
	diags := lintCheckSemantic(t, AnalyzerUndefinedSymbol, src)
	require.Len(t, diags, 1, "%v", diags)
	assert.Contains(t, diags[0].Message, "undefined symbol: x")
	assert.Equal(t, 5, diags[0].Pos.Line)

	diags = lintCheckSemantic(t, AnalyzerShadowing, `(defun f (v) (let ([(list car) v]) car)) (f 1)`)
	require.Len(t, diags, 1, "%v", diags)
	assert.Contains(t, diags[0].Message, "'car' shadows builtin")
}
