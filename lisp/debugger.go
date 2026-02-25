// Copyright © 2018 The ELPS authors

package lisp

// Debugger is called by the interpreter at key execution points to support
// breakpoints, stepping, and variable inspection. When Runtime.Debugger is
// nil, no hook calls are made and there is zero overhead on the hot path.
//
// Hook calls use a two-check gate pattern:
//
//	if d := env.Runtime.Debugger; d != nil && d.IsEnabled() { ... }
//
// The nil check is free (branch-predicted not-taken). IsEnabled allows a
// debugger to remain attached but dormant, further reducing overhead when
// not actively debugging.
type Debugger interface {
	// IsEnabled returns true when the debugger is actively debugging.
	// A dormant debugger (attached but not yet activated) returns false,
	// allowing the interpreter to skip all other hook calls.
	IsEnabled() bool

	// OnEval is called before evaluating any expression with a real source
	// location (v.Source != nil). Synthetic expressions from macro expansion
	// are skipped.
	// Returns true if the debugger wants execution to pause (breakpoint hit
	// or step complete).
	OnEval(env *LEnv, expr *LVal) bool

	// WaitIfPaused blocks until the debugger allows execution to continue.
	// Called when OnEval returns true. The eval goroutine blocks here while
	// the DAP server goroutine processes user commands.
	// Returns the action to take after resuming.
	WaitIfPaused(env *LEnv, expr *LVal) DebugAction

	// OnFunEntry is called when a function is entered, after formal
	// parameters have been bound in the function's lexical environment.
	// env is the caller's environment; fenv is the function's environment
	// with parameter bindings in scope. For builtins (which have no lexical
	// env), this hook is not called.
	OnFunEntry(env *LEnv, fun *LVal, fenv *LEnv)

	// OnFunReturn is called after a function returns.
	// fun is the function value, result is the return value.
	OnFunReturn(env *LEnv, fun, result *LVal)

	// AfterFunCall is called in Eval after EvalSExpr returns, giving the
	// debugger a chance to pause at the call-site expression when the
	// stack depth has decreased (step-out from tail position). Returns
	// true if execution should pause.
	AfterFunCall(env *LEnv) bool

	// OnError is called when an error condition is created (in
	// ErrorCondition and ErrorConditionf). Returns true if the debugger
	// wants execution to pause (exception breakpoint). When true, the
	// interpreter calls WaitIfPaused with the error value.
	OnError(env *LEnv, lerr *LVal) bool
}

// DebugAction represents the action the interpreter should take after
// the debugger resumes execution from a paused state.
type DebugAction int

const (
	// DebugContinue resumes execution until the next breakpoint.
	DebugContinue DebugAction = iota

	// DebugStepInto pauses on the next OnEval call regardless of depth.
	DebugStepInto

	// DebugStepOver pauses on the next OnEval call at the same or
	// lesser stack depth (does not descend into function calls).
	DebugStepOver

	// DebugStepOut pauses on the next OnEval call at a lesser stack
	// depth (waits until the current function returns).
	DebugStepOut
)
