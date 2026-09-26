// Copyright © 2026 The ELPS authors

package libutil

import "github.com/luthersystems/elps/lisp"

// ChargeKiB charges env's evaluation one step per complete KiB (1024 bytes)
// of n bytes of native work done by a stdlib builtin, so an enforced step
// budget (lisp.WithMaxSteps, lisp.Runtime.SetStepBudget) bounds work that is
// proportional to input or output size rather than to the number of calls
// (luthersystems/substrate#543).
//
// The charge is floor(n/1024): the call's own evaluation step already covers
// the first KiB, so a value under 1 KiB costs nothing extra and the hot path
// for ordinary small values is one compare, with no call into the evaluator
// and no change to the step counts of programs that never handle large
// values.  A builtin call therefore does at most about 2 KiB of sized work per
// step charged for it.  The charge depends only on n, which callers compute
// from argument or result values (never from caches or timing), so it is
// deterministic.
//
// ChargeKiB returns nil to continue, or the LError from lisp.LEnv.ChargeSteps
// (step limit exceeded or context cancelled), which the builtin must return
// as is.  With no step limit and no context it never fails.
func ChargeKiB(env *lisp.LEnv, n int) *lisp.LVal {
	if n < 1024 {
		return nil
	}
	if lerr := env.ChargeSteps(int64(n >> 10)); lerr.Type == lisp.LError {
		return lerr
	}
	return nil
}
