// Copyright © 2026 The ELPS authors

package lisp

// Step and context helpers for Go builtins (luthersystems/elps#745).
//
// Every helper here returns Nil() to continue, or the LError the builtin must
// return as is -- the convention of LEnv.ChargeSteps, so a result is checked
// with `lerr.Type == LError` and never dereferences Go nil.  None of them
// charges anything beyond what its name says, so replacing a hand-written
// ChargeSteps call with one of them changes no step count.
//
// ChargeStartedKiB charges ceil(n/1024), the convention of substrate's
// storage builtins, where any non-empty value costs at least one step.  (elps's
// own stdlib charges floor(n/1024) through an internal helper; which
// convention a builtin uses is observable in its step counts.)  ChargeRecord
// is ChargeStartedKiB with a floor of one step, for per-record work (a range
// fold's reducer call) that costs a step even when empty.

// ChargeStartedKiB charges env one step per started KiB of n bytes of native
// work: ceil(n/1024).  0 bytes cost nothing, 1..1024 cost one step, 1025 cost
// two.
func ChargeStartedKiB(env *LEnv, n int) *LVal {
	if n <= 0 {
		return Nil()
	}
	return env.ChargeSteps(int64(startedKiB(n)))
}

// ChargeRecord charges env for one record of n value bytes:
// max(1, ceil(n/1024)).  A record of up to 1 KiB, empty included, costs
// exactly one step; a larger one costs what ChargeStartedKiB(n) does.
func ChargeRecord(env *LEnv, n int) *LVal {
	return env.ChargeSteps(int64(max(1, startedKiB(n))))
}

func startedKiB(n int) int {
	if n <= 0 {
		return 0
	}
	return (n-1)/1024 + 1 // (n+1023)/1024 without overflow near MaxInt
}

// Step charges env one evaluation step, the per-element charge of a native
// loop that replaces a Lisp loop.  Like LEnv.ChargeSteps it also reports a
// done context, so a long native loop stays interruptible.
func (env *LEnv) Step() *LVal {
	return env.ChargeSteps(1)
}

// CheckContext returns the standard context-cancelled condition
// (CondContextCancelled, "context cancelled: <cause>") when the evaluation's
// context is done, and Nil() otherwise.  It charges no step.  It is the check
// the evaluator makes at every call boundary, for a builtin that does
// expensive work between charges.
func (env *LEnv) CheckContext() *LVal {
	if ctx := env.evalCtx; ctx != nil {
		if err := ctx.Err(); err != nil {
			return env.ErrorConditionf(CondContextCancelled, "context cancelled: %v", err)
		}
	}
	return Nil()
}
