// Copyright © 2026 The ELPS authors

package lisp

import "math"

// Step and context helpers for Go builtins (luthersystems/elps#745).
//
// Every helper here returns a nil error to continue, or the builtin's error
// as a *ErrorVal, which lisp.Error(err) returns as the builtin's value -- the
// convention of LEnv.ChargeSteps (luthersystems/elps#883).  None of them
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
func ChargeStartedKiB(env *LEnv, n int) error {
	if n <= 0 {
		return nil
	}
	return errorOf(env.chargeSteps(int64(startedKiB(n))))
}

// ChargeRecord charges env for one record of n value bytes:
// max(1, ceil(n/1024)).  A record of up to 1 KiB, empty included, costs
// exactly one step; a larger one costs what ChargeStartedKiB(n) does.
func ChargeRecord(env *LEnv, n int) error {
	return errorOf(env.chargeSteps(int64(max(1, startedKiB(n)))))
}

func startedKiB(n int) int {
	if n <= 0 {
		return 0
	}
	return (n-1)/1024 + 1 // (n+1023)/1024 without overflow near MaxInt
}

// step is Step as an error value or ().
func (env *LEnv) step() *LVal {
	return env.chargeSteps(1)
}

// Step charges env one evaluation step, the per-element charge of a native
// loop that replaces a Lisp loop.  Like LEnv.ChargeSteps it also reports a
// done context, so a long native loop stays interruptible.
func (env *LEnv) Step() error {
	return errorOf(env.step())
}

// stepEach is StepEach as an error value or ().
func (env *LEnv) stepEach(n int64) *LVal {
	if n <= 0 {
		return env.chargeSteps(n)
	}
	r := env.Runtime
	ctx := env.evalCtx
	if ctx == nil && r.maxSteps|r.stepBudget == 0 { // the fast path of ChargeSteps
		return Nil()
	}
	k := n
	if ctx != nil && ctx.Err() != nil {
		k = 1 // the first Step charges itself, then reports the context
	}
	if r.maxSteps > 0 {
		k = min(k, stepsToOverrun(r.steps, r.stepsOverflowed, r.maxSteps))
	}
	if r.stepBudget > 0 {
		k = min(k, stepsToOverrun(r.stepBudgetUsed, r.stepBudgetOverflowed, r.stepBudget))
	}
	// What ChargeSteps(k) does past its checks, which StepEach has made.
	r.addStepsToCurrent(k)
	if lerr := env.limitViolation(ctx); lerr != nil {
		return lerr
	}
	return Nil()
}

// StepEach charges env n steps as n calls to Step would, stopping at the
// first one that fails.  Use it when Go code replaces n evaluated forms and
// a budget that runs out must stop at the usage those forms would have
// recorded (luthersystems/elps#863).
//
// ChargeSteps(n) adds all n steps and then checks the limits once, so when
// a limit runs out part of the way it records more steps than the evaluator
// would: with 2 steps left, ChargeSteps(5) records 5 and StepEach(5)
// records 3, the step that overran the limit included, as the evaluator
// does.  The error is the same either way, so the outcome of the
// evaluation is the same; only the recorded usage differs.  A host that
// logs or bills the usage of a failed evaluation (substrate's step budget
// does) needs the evaluator's figure, so it needs StepEach.  ChargeSteps
// stays right for a charge that stands for one unit of work, such as one
// step per KiB, where no evaluated form had a step of its own.
//
// StepEach gives exactly what the loop gives: the same result, condition
// and message, and the same Steps, TotalSteps and StepBudget usage.  Like
// Step, it reports a done context only after it charges the first step,
// and an overrun limit before a done context.  It does not loop: it works
// out how many steps the loop would charge before it stopped, and charges
// them at once, so its cost does not grow with n.  The one
// difference is a context that is cancelled by another goroutine while the
// call runs: the loop could see it at any step, and StepEach sees it at
// its last.
//
// A zero or negative n behaves as in ChargeSteps: zero charges and checks
// nothing, and a negative n is an error that changes no counter.
func (env *LEnv) StepEach(n int64) error {
	return errorOf(env.stepEach(n))
}

// stepsToOverrun returns how many more single steps take a counter at used
// past limit (> 0): the step that overruns is the one that makes used
// exceed limit.  A counter already at or past the limit, or saturated,
// overruns at the next step.  From 0 to a limit of math.MaxInt64 the count
// does not fit an int64; math.MaxInt64 stands for it, since no charge can
// take more steps than that.
func stepsToOverrun(used int64, overflowed bool, limit int64) int64 {
	if overflowed || used >= limit {
		return 1
	}
	if left := limit - used; left < math.MaxInt64 {
		return left + 1
	}
	return math.MaxInt64
}

// checkAlloc is CheckAlloc as an error value or ().
func (env *LEnv) checkAlloc(n int) *LVal {
	if msg := env.Runtime.CheckAlloc(n); msg != "" {
		return env.Errorf("%s", msg)
	}
	return Nil()
}

// CheckAlloc returns an error when n exceeds the runtime's allocation cap
// (Runtime.MaxAlloc), as a *ErrorVal, and nil otherwise.  The error is the one builtins
// raise for that check: env.Errorf("%s", env.Runtime.CheckAlloc(n)), with
// condition "error" and the message "allocation size N exceeds maximum (M)".
// Runtime.CheckAlloc returns the message as a string instead.  CheckAlloc
// makes no context check and charges no step.
//
//	if err := env.CheckAlloc(len(cells)); err != nil {
//		return lisp.Error(err)
//	}
func (env *LEnv) CheckAlloc(n int) error {
	return errorOf(env.checkAlloc(n))
}

// CheckAllocs makes the CheckAlloc check of each size in sizes, in order, and
// returns the first error, or nil.  A Go port of Lisp code that built several
// lists, with no other check between them, calls it with their sizes in the
// order the Lisp built them.  The port then fails under an allocation cap
// with the error the Lisp raised first.  Where the Lisp charged a step or
// made another check between two lists, the port checks each size at its
// own place.  Like CheckAlloc, CheckAllocs makes no context check and
// charges no step.
func (env *LEnv) CheckAllocs(sizes ...int) error {
	for _, n := range sizes {
		if err := env.CheckAlloc(n); err != nil {
			return err
		}
	}
	return nil
}

// checkContext is CheckContext as an error value or ().
func (env *LEnv) checkContext() *LVal {
	if ctx := env.evalCtx; ctx != nil {
		if err := ctx.Err(); err != nil {
			return env.ErrorConditionf(CondContextCancelled, "context cancelled: %v", err)
		}
	}
	return Nil()
}

// CheckContext returns the standard context-cancelled condition
// (CondContextCancelled, "context cancelled: <cause>") when the evaluation's
// context is done, as a *ErrorVal, and nil otherwise.  It charges no step.  It is the check
// the evaluator makes at every call boundary, for a builtin that does
// expensive work between charges.
func (env *LEnv) CheckContext() error {
	return errorOf(env.checkContext())
}
