package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/elpstest"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/require"
)

// let* splits its scope only after an initializer that created a closure.
// These cases pin that the split is taken exactly when capture is possible,
// wherever the closure ends up, and skipped otherwise.
func TestLetSeqScopeSplitsOnlyAfterClosure(t *testing.T) {
	elpstest.RunTestSuite(t, elpstest.TestSuite{
		{"shadow after a directly returned closure", elpstest.TestSequence{
			{`(let* ((x 1) (f (lambda () x)) (x 2)) (list (f) x))`, `'(1 2)`, ""},
		}},
		{"closure stored inside a vector", elpstest.TestSequence{
			{`(let* ((x 1) (v (vector (lambda () x))) (x 2)) (list ((aref v 0)) x))`, `'(1 2)`, ""},
		}},
		{"closure created by a called function", elpstest.TestSequence{
			{`(defun mk () (lambda () 0))`, `()`, ""},
			{`(let* ((x 1) (g (mk)) (x 2)) (list (g) x))`, `'(0 2)`, ""},
		}},
		{"closure cannot see a later binding", elpstest.TestSequence{
			{`(let* ((f (lambda () later)) (later 2)) (ignore-errors (f)))`, `()`, ""},
		}},
		{"closure cannot see a later binding through a shadow", elpstest.TestSequence{
			{`(let* ((x 1) (f (lambda () x)) (x 2) (g (lambda () x))) (list (f) (g)))`, `'(1 2)`, ""},
		}},
		{"captured bindings stay live", elpstest.TestSequence{
			{`(let* ((x 1) (f (lambda () x))) (set! x 5) (f))`, `5`, ""},
		}},
		{"set! from the body reaches the captured binding, not a later shadow", elpstest.TestSequence{
			{`(let* ((x 1) (f (lambda () x)) (x 2)) (set! x 9) (list (f) x))`, `'(1 9)`, ""},
		}},
		{"sequential rebinding without closures stays flat", elpstest.TestSequence{
			{`(let* ((x 1) (x (+ x 1)) (x (* x 10))) x)`, `20`, ""},
		}},
		{"labels still provides local recursion", elpstest.TestSequence{
			{`(labels ((f (n) (if (<= n 0) 0 (+ n (f (- n 1)))))) (f 4))`, `10`, ""},
		}},
	})
}

// A let* whose initializers create no closure must cost what a let of the
// same bindings costs: one scope, not one per binding. The per-binding
// scope is the price of capture safety and is paid only when capture is
// possible, so this pins the flat path against regressing back to it.
func TestLetSeqWithoutClosuresAllocatesLikeLet(t *testing.T) {
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	require.NotEqual(t, lisp.LError, lisp.InitializeUserEnv(env).Type)
	measure := func(src string) float64 {
		return testing.AllocsPerRun(200, func() {
			v := env.LoadString("bench.lisp", src)
			if v.Type == lisp.LError {
				t.Fatal(v)
			}
		})
	}
	flat := measure(`(let ((a 1) (b 2) (c 3) (d 4) (e 5) (f 6) (g 7) (h 8)) (+ a h))`)
	seq := measure(`(let* ((a 1) (b 2) (c 3) (d 4) (e 5) (f 6) (g 7) (h 8)) (+ a h))`)
	split := measure(`(let* ((a 1) (k (lambda () a)) (b 2) (c 3) (d 4) (e 5) (f 6) (g 7) (h 8)) (+ a h))`)
	require.LessOrEqual(t, seq, flat, "let* without closures must not allocate more than let")
	require.Greater(t, split, seq, "a closure-creating initializer must open a new scope")
}
