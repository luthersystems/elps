// Copyright © 2026 The ELPS authors

package lisp

import (
	"context"
	"log"
)

// The functions in this file are the cold paths of the evaluator's
// recursive frames: eval, evalSExprCells, call, funCall, funCallFrame and
// specialOpCall.  Each Lisp call level holds about ten of those frames on
// the goroutine stack, and a request goroutine starts with a small stack
// that the Go runtime grows by copying it (runtime.morestack, copystack),
// walking every frame on it each time.  An error built inline needs a
// variadic ...any array and interface temporaries, and the compiler gives
// each a slot in the caller's frame.  Built here, behind a call the compiler
// may not inline, those temporaries live in a frame that exists only while
// the error is built.  The messages, conditions, call stacks and locations
// are the ones the inline code produced: env is the same, and none of these
// functions moves env.loc or the call stack.

// errorfCold is env.Errorf(format, a) out of line.
//
//go:noinline
func errorfCold[T any](env *LEnv, format string, a T) *LVal {
	return env.Errorf(format, a)
}

// errorCold is env.Error(err) out of line.
//
//go:noinline
func (env *LEnv) errorCold(err error) *LVal {
	return env.Error(err)
}

// funNameErrorCold is env.Errorf(format, env.GetFunName(fun)) out of line.
//
//go:noinline
func (env *LEnv) funNameErrorCold(format string, fun *LVal) *LVal {
	return env.Errorf(format, env.GetFunName(fun))
}

// contextCancelledCold is call's error for a context cancelled before the
// call, out of line.
//
//go:noinline
func (env *LEnv) contextCancelledCold(err error) *LVal {
	return env.ErrorConditionf(CondContextCancelled, "context cancelled: %v", err)
}

// evalNestingCold is eval's error for an evaluation nested past
// MaxEvalNesting, out of line.
//
//go:noinline
func (env *LEnv) evalNestingCold() *LVal {
	return env.ErrorConditionf(CondEvalNestingExceeded,
		"evaluation nesting depth exceeded maximum: %d"+
			" (expressions nested this deeply consume Go stack without pushing"+
			" call frames, so the limit stops the Go runtime from aborting the"+
			" process with an unrecoverable stack overflow; raise or disable it"+
			" with WithMaxEvalNesting)", env.Runtime.evalNesting)
}

// tailRecInArgsCold reports a tail-recursion mark met while evaluating a
// call form's elements, an evaluator bug: it prints the call stack and
// panics, as evalSExprCells did inline.  mark is the mark, or a function
// value of type LMarkTailRec.
//
//go:noinline
func (env *LEnv) tailRecInArgsCold(ctx context.Context, mark *LVal, fun bool) {
	_, _ = env.Runtime.Stack.debugPrintContext(liveRenderContext(ctx), env.Runtime.getStderr(), diagnosticLimit(env.Runtime.MaxAllocBytes()))
	if fun {
		log.Panicf("tail-recursion optimization attempted during argument evaluation: %v", mark.Cells)
	}
	log.Panicf("tail-recursion optimization attempted during argument evaluation: %s", env.Render(SExpr(mark.Cells)))
}

// The debugger hooks below are the evaluator's, out of line.  Each caller
// tests only that a debugger is attached; the hook loads it again and makes
// the rest of the test, so the caller's frame holds no Debugger interface.

// debugEvalCold is eval's hook before it evaluates v.
//
//go:noinline
func (env *LEnv) debugEvalCold(ctx context.Context, v *LVal) {
	if d := env.Runtime.Debugger; d != nil && d.IsEnabled() {
		env.debugOnEval(ctx, d, v)
	}
}

// debugAfterFunCallCold is eval's hook after the call form v returns.
//
//go:noinline
func (env *LEnv) debugAfterFunCallCold(v *LVal) {
	if d := env.Runtime.Debugger; d != nil && d.IsEnabled() && v.loc() != nil {
		if d.AfterFunCall(env) {
			d.WaitIfPaused(env, v)
		}
	}
}

// debugFunEntryCold is call's hook once fun's arguments are bound in fenv.
//
//go:noinline
func (env *LEnv) debugFunEntryCold(fun *LVal, fenv *LEnv) {
	if d := env.Runtime.Debugger; d != nil && d.IsEnabled() {
		// fenv != env means this is a user-defined function (not a builtin).
		// Builtins return the caller's env from bind.
		if fenv != nil && fenv != env {
			d.OnFunEntry(env, fun, fenv)
		}
	}
}

// debugFunReturnCold is funCallFrame's hook when fun returns r.
//
//go:noinline
func (env *LEnv) debugFunReturnCold(fun, r *LVal) {
	if d := env.Runtime.Debugger; d != nil && d.IsEnabled() {
		d.OnFunReturn(env, fun, r)
	}
}
