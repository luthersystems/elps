package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/elpstest"
)

func BenchmarkEnvGet(b *testing.B) {
	elpstest.RunBenchmark(b, `
	  (dotimes (n 1000)
	    (let ([a0 0])
		(let ([a1 1])
		(let ([a2 2])
		(let ([a3 3])
		(let ([a4 4])
		(let ([a5 5])
		(let ([a6 6])
		(let ([a7 7])
		(let ([a8 8])
		(let ([a9 9])
		(+ a0 a1 a2 a3 a4 a5 a6 a7 a8 a9))))))))))))
	`)
}

func BenchmarkEnvFunCallBuiltin(b *testing.B) {
	elpstest.RunBenchmark(b, `
	  (dotimes (n 1000)
		(+ 0 1 2 3 4 5 6 7 8 9))
	`)
}

func BenchmarkEnvFunCallRecursion(b *testing.B) {
	elpstest.RunBenchmark(b, `
	  (defun loopn (n fn) (if (<= n 0) ()
         (progn (loopn (- n 1) fn)
                (fn n))))
	  (loopn 1000 identity)
	`)
}

func BenchmarkEnvFunCallTailRec(b *testing.B) {
	elpstest.RunBenchmark(b, `
	  (defun loopn (n fn) (if (<= n 0)()
         (progn (fn n)
                (loopn (- n 1) fn))))
	  (loopn 1000 identity)
	`)
}

func BenchmarkEnvFunCallPos(b *testing.B) {
	elpstest.RunBenchmark(b, `
      (defun benchmark (a b c) (vector a b c))
	  (dotimes (n 1000) (benchmark "a" "b" "c"))
	`)
}

func BenchmarkEnvFunCallKey(b *testing.B) {
	elpstest.RunBenchmark(b, `
      (defun benchmark (&key a b c) (vector a b c))
	  (dotimes (n 1000) (benchmark :a "a" :b "b" :c "c"))
	`)
}

func BenchmarkEnvFunCallOpt(b *testing.B) {
	elpstest.RunBenchmark(b, `
      (defun benchmark (&optional a b c) (vector a b c))
	  (dotimes (n 1000) (benchmark "a" "b" "c"))
	`)
}

func BenchmarkEnvFunCallVar(b *testing.B) {
	elpstest.RunBenchmark(b, `
      (defun benchmark (&rest a) (apply vector a))
	  (dotimes (n 1000) (benchmark "a" "b" "c"))
	`)
}
