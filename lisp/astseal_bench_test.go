// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/elpstest"
)

// Stress benchmarks for the AST→value leak-point seals (lisp/astseal.go).
// They concentrate the sealed paths far beyond any real program so the
// per-use copy cost is visible:
//
//   - BenchmarkQuoteLiteral50 evaluates a function whose body is a ~50
//     element quoted literal 1000 times per iteration.  Every call passes
//     eval's Quoted fast path, which now copies the container spine.
//   - BenchmarkQuoteLiteralNested does the same with a deeply nested quoted
//     literal (containers dominate over atoms).
//   - BenchmarkMacroExpandStress calls a quasiquote-based user macro 1000
//     times per iteration: each call seals the macro arguments, copies the
//     quasiquote literal skeleton, and re-seals quoted parts of the
//     expansion when it is evaluated.
//   - BenchmarkLambdaCreate creates a closure with a non-trivial body 1000
//     times per iteration, exercising the env.Lambda body seal.
//   - BenchmarkAtomLiteralArith is the control for the atom-copy decision:
//     it is dominated by self-evaluating numeric literals and quoted-symbol
//     atoms, the paths that stay zero-alloc while copyAtomsAtLeakPoints is
//     false.

func BenchmarkQuoteLiteral50(b *testing.B) {
	elpstest.RunBenchmark(b, `
	(defun quoted-lit-50 ()
	  '(a00 a01 a02 a03 a04 a05 a06 a07 a08 a09
	    a10 a11 a12 a13 a14 a15 a16 a17 a18 a19
	    a20 a21 a22 a23 a24 a25 a26 a27 a28 a29
	    a30 a31 a32 a33 a34 "s35" 36 37.5 :k38 a39
	    a40 a41 a42 a43 a44 a45 a46 a47 a48 a49))
	(dotimes (n 1000) (quoted-lit-50))
	`)
}

func BenchmarkQuoteLiteralNested(b *testing.B) {
	elpstest.RunBenchmark(b, `
	(defun quoted-lit-nested ()
	  '(l0 (l1 (l2 (l3 (l4 (l5 (l6 (l7 (l8 (l9 "deep")))))))))
	    (m1 1 2) (m2 3 4) (m3 5 6) (m4 7 8) (m5 9 10)))
	(dotimes (n 1000) (quoted-lit-nested))
	`)
}

func BenchmarkMacroExpandStress(b *testing.B) {
	elpstest.RunBenchmark(b, `
	(defmacro stress-mac (a b c)
	  (quasiquote
	    (let ([x0 (unquote a)]
	          [y0 (unquote b)])
	      (if (< x0 y0)
	          (list 'lt x0 y0 (unquote c) '(const tail 1 2 3))
	          (list 'ge y0 x0 (unquote c) '(const tail 4 5 6))))))
	(dotimes (n 1000) (stress-mac (+ n 1) (- n 1) '(payload p1 p2)))
	`)
}

func BenchmarkLambdaCreate(b *testing.B) {
	elpstest.RunBenchmark(b, `
	(dotimes (n 1000)
	  (lambda (a b c)
	    (let ([s (+ a b c)])
	      (list s '(inner quoted lit) "body-str" 42))))
	`)
}

func BenchmarkAtomLiteralArith(b *testing.B) {
	elpstest.RunBenchmark(b, `
	(dotimes (n 1000)
	  (+ 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
	`)
}
