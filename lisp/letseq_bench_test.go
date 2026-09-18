package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/elpstest"
)

// BenchmarkLetSeq measures let* with independent bindings: each sequential
// binding opens its own scope so an initializer closure cannot observe a
// later binding, so the cost grows with the number of bindings.
func BenchmarkLetSeq(b *testing.B) {
	elpstest.RunBenchmark(b, `
	  (dotimes (n 1000)
	    (let* ([a0 0] [a1 1] [a2 2] [a3 3] [a4 4] [a5 5] [a6 6] [a7 7] [a8 8] [a9 9])
	      (+ a0 a1 a2 a3 a4 a5 a6 a7 a8 a9)))
	`)
}

// BenchmarkLetSeqChained is the shape where every later initializer reads an
// earlier binding, so each lookup walks the scopes opened before it.
func BenchmarkLetSeqChained(b *testing.B) {
	elpstest.RunBenchmark(b, `
	  (dotimes (n 1000)
	    (let* ([a0 1] [a1 (+ a0 1)] [a2 (+ a1 1)] [a3 (+ a2 1)] [a4 (+ a3 1)]
	           [a5 (+ a4 1)] [a6 (+ a5 1)] [a7 (+ a6 1)] [a8 (+ a7 1)] [a9 (+ a8 1)])
	      (+ a0 a9)))
	`)
}

// BenchmarkLetSeqWide uses forty bindings whose initializers all read the
// first one, the case where lookup depth grows with the binding count.
func BenchmarkLetSeqWide(b *testing.B) {
	elpstest.RunBenchmark(b, `
	  (dotimes (n 200)
	    (let* ([v0 1]
	           [v1 v0] [v2 v0] [v3 v0] [v4 v0] [v5 v0] [v6 v0] [v7 v0] [v8 v0] [v9 v0]
	           [v10 v0] [v11 v0] [v12 v0] [v13 v0] [v14 v0] [v15 v0] [v16 v0] [v17 v0] [v18 v0] [v19 v0]
	           [v20 v0] [v21 v0] [v22 v0] [v23 v0] [v24 v0] [v25 v0] [v26 v0] [v27 v0] [v28 v0] [v29 v0]
	           [v30 v0] [v31 v0] [v32 v0] [v33 v0] [v34 v0] [v35 v0] [v36 v0] [v37 v0] [v38 v0] [v39 v0])
	      (+ v0 v39)))
	`)
}
