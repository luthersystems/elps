// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"fmt"
	"testing"

	"github.com/luthersystems/elps/elpstest"
	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/require"
)

// TestErrorStackChargesPerStartedSixtyFourFrames pins that error-stack, which
// builds one sorted-map per frame of the handled error's stack, charges one
// evaluation step per started 64 frames: ceil(frames/64).
//
// Each depth is run twice, once with a handler that calls (error-stack) and
// once with one that calls (list) instead.  The two differ only in that call,
// so the difference is a constant evaluation cost plus the charge; the
// constant is taken from a stack of at most 64 frames.
func TestErrorStackChargesPerStartedSixtyFourFrames(t *testing.T) {
	run := func(depth int, handler string) (int64, *lisp.LVal) {
		env, err := (&elpstest.Runner{}).NewEnv(t)
		require.NoError(t, err)
		env.Runtime.SetStepBudget(1 << 40)
		prog := fmt.Sprintf(`
(defun dive (n) (if (<= n 0) (error 'boom "bottom") (+ 1 (dive (- n 1)))))
(handler-bind ((condition (lambda (c &rest _) (length %s)))) (dive %d))`, handler, depth)
		before := env.Runtime.TotalSteps()
		got := env.LoadString("error-stack-charge.lisp", prog)
		require.NotEqual(t, lisp.LError, got.Type, "%v", got)
		return env.Runtime.TotalSteps() - before, got
	}
	charge := func(depth int) (frames int, delta int64) {
		with, n := run(depth, "(error-stack)")
		without, _ := run(depth, "(list)")
		return n.Int, with - without
	}
	baseFrames, baseDelta := charge(1)
	require.LessOrEqual(t, baseFrames, 64)
	constant := baseDelta - 1
	for _, depth := range []int{20, 50, 60, 70, 130, 200} {
		frames, delta := charge(depth)
		want := constant + int64((frames+63)/64)
		require.Equal(t, want, delta, "depth %d: %d frames", depth, frames)
	}
}
