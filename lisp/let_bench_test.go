// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/elpstest"
)

// BenchmarkLet measures a parallel let of symbol bindings, the path that
// binding patterns (luthersystems/elps#910) must not slow down.
func BenchmarkLet(b *testing.B) {
	elpstest.RunBenchmark(b, `
	  (dotimes (n 1000)
	    (let ([a0 0] [a1 1] [a2 2] [a3 3] [a4 4] [a5 5] [a6 6] [a7 7] [a8 8] [a9 9])
	      (+ a0 a1 a2 a3 a4 a5 a6 a7 a8 a9)))
	`)
}

// BenchmarkLetPattern destructures a list in a let binding
// (luthersystems/elps#910).  BenchmarkLetPatternMatch is the same work
// written with match, for comparison.
func BenchmarkLetPattern(b *testing.B) {
	elpstest.RunBenchmark(b, `
	  (dotimes (n 1000)
	    (let ([(list a b c &rest r) '(1 2 3 4 5)])
	      (+ a b c)))
	`)
}

func BenchmarkLetPatternMatch(b *testing.B) {
	elpstest.RunBenchmark(b, `
	  (dotimes (n 1000)
	    (match '(1 2 3 4 5)
	      ((list a b c &rest r) (+ a b c))))
	`)
}
