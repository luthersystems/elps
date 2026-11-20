// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"runtime"
	"strconv"
	"testing"

	"github.com/luthersystems/elps/elpsutil"
	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/require"
)

// TestEvalGoFramesPerLevel pins how many Go frames the evaluator keeps on the
// goroutine's stack for each level of Lisp nesting.
//
// Why: a request goroutine starts with a small stack, and each time the
// stack grows the Go runtime copies it, walking every frame on it.  That
// walk, not the bytes, is most of the cost: substrate's chaincode profile
// spent about 7% of its CPU in runtime.copystack, and the work grows with
// the number of frames per level.  Returning terminal marks to the frame
// that made the call (beginTerminal), and calling funCallFrame and eval
// directly from evalSExpr and evalSExprCells, cut these counts from 14, 18
// and 14 (elps v1.100.2) to 10, 15 and 10.  A change that puts a wrapper
// frame back on one of these paths fails here.  If a change has to add a
// frame, update the count and say why in the commit.
//
// runtime.Callers counts logical frames: a call the compiler inlines counts
// too, although it adds no frame for the copy to walk.  So the counts do not
// depend on inlining decisions (a -gcflags=-l or PGO build gives the same
// numbers), and a new helper on the path fails here even when it is
// inlined.  The test does not see frame sizes.
func TestEvalGoFramesPerLevel(t *testing.T) {
	tests := []struct {
		name string
		def  string
		want int
	}{
		// A call in a call's argument, under if: one lambda call, one
		// special operator and one nested argument per level.
		{"argument", `(defun probe (n) (if (<= n 0) (go-frames) (+ 0 (probe (- n 1)))))`, 10},
		// A call in a let binding's value, under if.
		{"let-binding", `(defun probe (n) (if (<= n 0) (go-frames) (let ((x (probe (- n 1)))) x)))`, 15},
		// A call in a call's argument, in a cond clause.
		{"cond", `(defun probe (n) (cond ((<= n 0) (go-frames)) (:else (+ 0 (probe (- n 1))))))`, 10},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			env := testEnv(t)
			env.AddBuiltins(true, elpsutil.Function("go-frames", lisp.Formals(), func(_ *lisp.LEnv, _ *lisp.LVal) *lisp.LVal {
				pcs := make([]uintptr, 4096)
				return lisp.Int(runtime.Callers(0, pcs))
			}))
			rc := env.LoadString("probe.lisp", tc.def)
			require.NotEqual(t, lisp.LError, rc.Type, "%v", rc)
			frames := func(depth int) int {
				v := env.LoadString("call.lisp", "(probe "+strconv.Itoa(depth)+")")
				require.Equal(t, lisp.LInt, v.Type, "%v", v)
				return v.Int
			}
			perLevel := (frames(12) - frames(2)) / 10
			require.Equal(t, 0, (frames(12)-frames(2))%10, "frames per level are not constant")
			require.Equal(t, tc.want, perLevel, "Go frames per Lisp call level")
		})
	}
}
