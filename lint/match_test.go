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

// A clause whose pattern repeats the pattern of an earlier clause with no
// :when guard never runs (luthersystems/elps#915).
func TestMatchUnreachable_RepeatedPattern(t *testing.T) {
	for _, tc := range []struct {
		src  string
		line int // 0: no report
	}{
		{"(match v\n (1 'a)\n (1 'b))", 3},
		{"(match v\n (\"s\" 'a)\n (2 'x)\n (\"s\" 'b))", 4},
		// A later clause with a guard never runs either.
		{"(match v\n (1 'a)\n (1 :when (f) 'b))", 3},
		// An earlier clause with a guard can fall through.
		{"(match v\n (1 :when (f) 'a)\n (1 'b))", 0},
		{"(match v\n (1 :when (f) 'a)\n (1 'b)\n (1 'c))", 4},
		// Names count by position.
		{"(match v\n ((list x 1) x)\n ((list y 1) y))", 3},
		{"(match v\n ((list x y) x)\n ((list b a) a))", 3},
		// _ is not a name.
		{"(match v\n ((list _ 1) 0)\n ((list x 1) x))", 0},
		{"(match v\n ((list x 1) x)\n ((list _ 1) 0))", 0},
		{"(match v\n ((list _ 1) 0)\n ((list _ 1) 1))", 3},
		// Nested lists, vectors and maps.
		{"(match v\n ((list (vector a _) (sorted-map \"k\" b)) a)\n ((list (vector c _) (sorted-map \"k\" d)) d))", 3},
		{"(match v\n ((vector a &rest r) a)\n ((vector b &rest s) b))", 3},
		{"(match v\n ((list a &rest r) a)\n ((vector a &rest r) a))", 0},
		{"(match v\n ((sorted-map \"k\" a) a)\n ((sorted-map \"j\" a) a))", 0},
		{"(match v\n ((vector a b) a)\n ((vector a b c) a))", 0},
		// A type that is a symbol is compared as text.
		{"(match v\n ([x s:int] x)\n ([y s:int] y))", 3},
		{"(match v\n ([x s:int] x)\n ([x s:string] x))", 0},
		{"(match v\n ((point :x a) a)\n ((point :x b) b))", 3},
		{"(match v\n ((user:point :x a) a)\n ((user:point :x b) b))", 3},
		{"(match v\n ((point :x a) a)\n ((shape :x a) a))", 0},
		// Any other type expression is evaluated each time the pattern is
		// tried, so it can give a new type: the pattern gets no key.
		{"(match v\n ([x (next-type!)] x)\n ([x (next-type!)] x))", 0},
		{"(match v\n ((list a [b (t a)]) a)\n ((list c [d (t a)]) c))", 0},
		{"(match v\n (((make-point) :x a) a)\n (((make-point) :x a) a))", 0},
		{"(match v\n ((vector [a (t)] b) a)\n ((vector [a (t)] b) a))", 0},
		// Different literals, and a literal against a name.
		{"(match v\n (1 'a)\n (2 'b)\n (1.5 'c)\n (\"1\" 'd)\n ('a 'e)\n (:a 'f))", 0},
		{"(match v\n ('a 1)\n ((quote a) 2)\n (a 3))", 0},
		{"(match v\n ((list 'a) 1)\n ((list a) 2))", 0},
		// A bracket clause is a clause, as match reads it.
		{"(match v\n [1 'a]\n [1 'b])", 3},
		{"(match v\n [(list x 1) x]\n ((list y 1) y))", 3},
		{"(match v\n [1 :when (f) 'a]\n [1 'b])", 0},
		{"(match v\n [1 'a]\n [2 'b])", 0},
		// A bad pattern and a macro template get no key.
		{"(match v\n ((list a a) 1)\n ((list b b) 2))", 0},
		{"(defmacro m (p) (quasiquote (match v ((unquote p) 1) ((unquote p) 2))))", 0},
	} {
		t.Run(tc.src, func(t *testing.T) {
			diags := lintCheck(t, AnalyzerMatchUnreachable, tc.src)
			if tc.line == 0 {
				assertNoDiags(t, diags)
				return
			}
			require.Len(t, diags, 1, "%v", diags)
			assertDiagOnLine(t, diags, tc.line, "an earlier clause with no :when guard has the same pattern")
		})
	}
	// Each repeat is reported, and a catch-all still stops the check.
	diags := lintCheck(t, AnalyzerMatchUnreachable, "(match v\n (1 'a)\n (1 'b)\n (1 'c)\n (_ 'd)\n (2 'e))")
	require.Len(t, diags, 3, "%v", diags)
	assertDiagOnLine(t, diags, 3, "same pattern")
	assertDiagOnLine(t, diags, 4, "same pattern")
	assertDiagOnLine(t, diags, 6, "matches every value")
}

// A bracket clause can be a catch-all clause too
// (luthersystems/elps#915).
func TestMatchUnreachable_BracketCatchAll(t *testing.T) {
	diags := lintCheck(t, AnalyzerMatchUnreachable, "(match v\n [_ 1]\n [2 2])")
	require.Len(t, diags, 1, "%v", diags)
	assertDiagOnLine(t, diags, 3, "the clause before it matches every value")
	diags = lintCheck(t, AnalyzerMatchUnreachable, "(match v\n [x 1]\n (2 2))")
	require.Len(t, diags, 1, "%v", diags)
	assertDiagOnLine(t, diags, 3, "the clause before it matches every value")
	assertNoDiags(t, lintCheck(t, AnalyzerMatchUnreachable, "(match v\n [x :when (f x) 1]\n [2 2])"))
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
