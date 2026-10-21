// Copyright © 2026 The ELPS authors

package elpstest

import (
	"context"
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// Helpers to measure the steps a Go builtin charges (luthersystems/elps#863).
//
// A builtin that replaces Lisp code charges steps with LEnv.ChargeSteps,
// LEnv.Step or LEnv.StepEach, and an embedder pins those charges in tests,
// since under a step limit they decide whether an evaluation succeeds.  The
// simplest test calls the builtin directly from Go: no form is evaluated, so
// the evaluator counts nothing, and every step the runtime records is the
// builtin's own charge.  luthersystems/substrate wrote these helpers for
// that and used them in dozens of tests; they live here so every embedder
// measures charges the same way.

// NewStepEnv returns a bare env, with no packages loaded, whose runtime
// counts steps, for calling a builtin directly from Go.  See EnableSteps.
func NewStepEnv() *lisp.LEnv {
	env := lisp.NewEnv(nil)
	EnableSteps(env)
	return env
}

// EnableSteps turns on step counting for env and adds no limit.  The
// runtime counts steps only while it has a step limit or a context; with
// neither, LEnv.ChargeSteps records nothing and every charge measures 0.  So
// EnableSteps gives env a background context, which is never cancelled and
// has no deadline, as a host that runs evaluations under a context does.
//
// The context belongs to env, not to its runtime: call the builtin with this
// env (a child env made after the call inherits it, one made before does
// not).  EnableSteps replaces a context env already had, and leaves any
// limit env's runtime already has in place.
func EnableSteps(env *lisp.LEnv) {
	_ = lisp.WithContext(context.Background())(env)
}

// StepsCharged runs fn and returns how far env's Runtime.TotalSteps moved
// while it ran.  Around a direct Go call to a builtin, that is exactly the
// builtin's charge.  It reads TotalSteps, not Steps, because Steps restarts
// at each top-level evaluation and TotalSteps does not.  env must count
// steps (NewStepEnv, EnableSteps), or the result is always 0.  TotalSteps
// saturates at math.MaxInt64, so a charge measured on a runtime near that
// count comes out short.
func StepsCharged(env *lisp.LEnv, fn func()) int64 {
	before := env.Runtime.TotalSteps()
	fn()
	return env.Runtime.TotalSteps() - before
}

// CallBuiltinFunc calls the Go function of a builtin directly, with args as
// its argument list, and returns its result and the steps it charged.
//
// It binds no formals, so pass args in the shape the function reads: for a
// builtin with only required formals that is the evaluated arguments, as the
// evaluator passes them, and for &optional, &rest or &key formals it is the
// bound list (a missing optional as lisp.Nil(), the rest arguments as one
// list).  Unlike LEnv.CallBuiltin, which calls a language builtin as Lisp
// would, it adds nothing to the call: no context check and no evaluation of
// a tail call the builtin hands back, so the steps it measures are the
// function's own charges and nothing else.
//
// It fails tb when the function returns an error, Go nil or a tail call
// (funcall and apply hand one back to the evaluator), whose callee's steps
// the measure would leave out.  To measure an error path, call the function
// inside StepsCharged instead.
func CallBuiltinFunc(tb testing.TB, env *lisp.LEnv, builtin lisp.LBuiltin, args ...*lisp.LVal) (*lisp.LVal, int64) {
	tb.Helper()
	var out *lisp.LVal
	n := StepsCharged(env, func() { out = builtin(env, lisp.QExpr(args)) })
	switch {
	case out == nil:
		tb.Fatalf("builtin returned Go nil")
	case out.IsError():
		tb.Fatalf("builtin returned an error: %v", out)
	case out.Type == lisp.LMarkTerminal || out.Type == lisp.LMarkTailRec:
		tb.Fatalf("builtin returned a tail call (%v), whose steps the measure leaves out; evaluate the call instead", out.Type)
	}
	return out, n
}
