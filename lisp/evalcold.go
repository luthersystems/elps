// Copyright © 2026 The ELPS authors

package lisp

import (
	"context"
	"log"

	"github.com/luthersystems/elps/parser/token"
)

// The functions in this file are the cold paths of the evaluator's recursive
// functions (eval, evalSExpr, evalSExprCells, funCallFrame, specialOpCall,
// call and callBuiltin): error construction, debugger hooks and
// internal-error panics.  They are split out, and kept out of line, because
// those functions are on the Go stack once or more for every level of Lisp
// nesting.
//
// Why it matters: a chaincode request runs on a fresh goroutine, whose stack
// starts small and is copied each time it grows.  Substrate's chaincode
// profile spends about 7% of its CPU in runtime.copystack.  The copy walks
// every frame on the stack, and for each frame it decodes the function's PC
// tables from the function's start to the call site, so the cost is mostly
// per frame and grows with the code ahead of the recursive call.  The cold
// code here sat ahead of those calls, and its variadic Errorf calls and
// interface calls (the Debugger) made the frames larger, so a stack grew
// sooner.  A helper costs a call only when its path is taken, and none is
// taken on a normal call.
//
// The other part of the same work removes whole frames from each level:
// evalSExpr calls funCallFrame directly (no funCall frame), evalSExprCells
// calls eval directly (no evalOperand frame), and a builtin's terminal mark
// is evaluated by the frame that made the call (no call and callBuiltin
// frames under every if, let, progn and cond; see beginTerminal).
// TestEvalGoFramesPerLevel pins the result.
//
// Each helper builds exactly the value the inline code built: an error's
// content depends on env (its location and call stack), never on the Go
// caller, so moving the construction does not change it.

// errEvalNestingExceeded is eval's nesting-limit error.
//
//go:noinline
func (env *LEnv) errEvalNestingExceeded() *LVal {
	return env.ErrorConditionf(CondEvalNestingExceeded,
		"evaluation nesting depth exceeded maximum: %d"+
			" (expressions nested this deeply consume Go stack without pushing"+
			" call frames, so the limit stops the Go runtime from aborting the"+
			" process with an unrecoverable stack overflow; raise or disable it"+
			" with WithMaxEvalNesting)", env.Runtime.evalNesting)
}

// errSplicedExpression is eval's error for a spliced value in expression
// position.
//
//go:noinline
func (env *LEnv) errSplicedExpression() *LVal {
	return env.Errorf("spliced value used as expression")
}

// errMacroExpansionDepth is eval's error for a macro that expands too many
// times in a row.
//
//go:noinline
func (env *LEnv) errMacroExpansionDepth(depth int) *LVal {
	return env.Errorf("macro expansion depth exceeded (%d expansions)", depth)
}

// debugOnEvalIfEnabled is eval's debugger hook on entry.  The caller has
// checked that a debugger is attached and that v has a location.
//
//go:noinline
func (env *LEnv) debugOnEvalIfEnabled(ctx context.Context, v *LVal) {
	if d := env.Runtime.Debugger; d != nil && d.IsEnabled() {
		env.debugOnEval(ctx, d, v)
	}
}

// debugAfterFunCall is eval's debugger hook after a call form returns.  It
// catches a step-out through a tail-position return, which would otherwise
// unwind without reaching OnEval.
//
//go:noinline
func (env *LEnv) debugAfterFunCall(v *LVal) {
	if d := env.Runtime.Debugger; d != nil && d.IsEnabled() && v.loc() != nil {
		if d.AfterFunCall(env) {
			d.WaitIfPaused(env, v)
		}
	}
}

// errNotAFunction is evalSExprCells's error for a call form whose first
// element evaluates to f, which is not a function.
//
//go:noinline
func (env *LEnv) errNotAFunction(f *LVal) *LVal {
	return env.Errorf("first element of expression is not a function: %v", f)
}

// panicTailRecInArgs reports an evaluator bug: a tail-recursion mark reached
// evalSExprCells, in the function position (arg false) or as an argument
// (arg true).  It prints the call stack and panics with the message each
// position printed before.
//
//go:noinline
func (env *LEnv) panicTailRecInArgs(ctx context.Context, mark *LVal, arg bool) {
	_, _ = env.Runtime.Stack.debugPrintContext(liveRenderContext(ctx), env.Runtime.getStderr(), diagnosticLimit(env.Runtime.MaxAllocBytes()))
	if arg {
		log.Panicf("tail-recursion optimization attempted during argument evaluation: %s", env.Render(SExpr(mark.Cells)))
	}
	log.Panicf("tail-recursion optimization attempted during argument evaluation: %v", mark.Cells)
}

// restoreAfterArgs is evalSExprCells's deferred restore.  It sets the top
// frame's Terminal flag again when evalSExprCells cleared it, and then puts
// back the location env had when the call form's evaluation began.
func (env *LEnv) restoreAfterArgs(loc *token.Location, resetTerminal bool) {
	if resetTerminal {
		env.Runtime.Stack.Top().Terminal = true
	}
	env.loc = loc
}

// errInvalidFunType is evalSExpr's error for a function value whose FunType
// is none of the known kinds.
//
//go:noinline
func (env *LEnv) errInvalidFunType(fun *LVal) *LVal {
	return env.Errorf("internal error: invalid function type %v", fun.FunType)
}

// errFromGo is env.Error(err) for a Go error from the call stack (a frame
// push or a tail call over its limit).
//
//go:noinline
func (env *LEnv) errFromGo(err error) *LVal {
	return env.Error(err)
}

// errReturnedNil is the error for a function or special operator (kind)
// whose implementation returned a nil *LVal.
//
//go:noinline
func (env *LEnv) errReturnedNil(kind string, fun *LVal) *LVal {
	return env.Errorf("internal error: "+kind+" %s returned nil", env.GetFunName(fun))
}

// debugOnFunReturn is funCallFrame's debugger hook on return.  The caller
// has checked that a debugger is attached.
//
//go:noinline
func (env *LEnv) debugOnFunReturn(fun, r *LVal) {
	if d := env.Runtime.Debugger; d != nil && d.IsEnabled() {
		if r.Type == LMarkTailRec {
			env.Runtime.disownTailRecMark(r)
		}
		d.OnFunReturn(env, fun, r)
	}
}

// errContextCancelled is call's error for a context cancelled after Eval's
// entry check.
//
//go:noinline
func (env *LEnv) errContextCancelled(err error) *LVal {
	return env.ErrorConditionf(CondContextCancelled, "context cancelled: %v", err)
}

// debugOnFunEntry is call's debugger hook on entry.  The caller has checked
// that a debugger is attached.
//
//go:noinline
func (env *LEnv) debugOnFunEntry(fun *LVal, fenv *LEnv) {
	if d := env.Runtime.Debugger; d != nil && d.IsEnabled() {
		// fenv != env means this is a user-defined function (not a builtin).
		// Builtins return the caller's env from bind.
		if fenv != nil && fenv != env {
			d.OnFunEntry(env, fun, fenv)
		}
	}
}

// errTerminalMarkNoEnv is callBuiltin's error for a terminal mark that
// carries no environment.
//
//go:noinline
func (env *LEnv) errTerminalMarkNoEnv() *LVal {
	return env.Errorf("internal error: terminal mark has no environment")
}
