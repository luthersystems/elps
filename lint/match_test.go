// Copyright © 2026 The ELPS authors

package lint

import (
	"testing"

	"github.com/stretchr/testify/assert"
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
  (_ 2))`)
	assert.Len(t, diags, 3, "%v", diags)
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
    (_ (+ y 1))))
(defun g () a)`)
	assert.Len(t, diags, 2, "%v", diags)
	assertDiagOnLine(t, diags, 6, "undefined symbol: y")
	assertDiagOnLine(t, diags, 7, "undefined symbol: a")
}
