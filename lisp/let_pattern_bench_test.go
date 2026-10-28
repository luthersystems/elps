package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/elpstest"
)

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
