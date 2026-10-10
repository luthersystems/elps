// Copyright © 2026 The ELPS authors

package lisp

import "fmt"

// WrapError returns a copy of the error lerr with a line of context, built
// like fmt.Sprintf(format, v...), in front of its message
// (luthersystems/elps#831).  It is Go's fmt.Errorf("...: %w", err) for a
// Lisp error: the copy renders as "context: message", and keeps lerr's
// condition, data, source location and call stack, so every handler-bind
// binding and condition-is? test that matched lerr matches the copy.  A
// further wrap puts its line in front of the earlier ones.  Lisp code wraps
// the error it is handling with (rethrow :context ...).
//
// lerr is not changed.  WrapError returns lerr itself when it is not an
// error.  An error with no call stack yet gets its stack when the evaluator
// associates it, as any error a builtin returns does, and keeps its context.
func WrapError(lerr *LVal, format string, v ...any) *LVal {
	return wrapError(lerr, fmt.Sprintf(format, v...))
}

// ErrorContext returns the lines of context WrapError added to the error,
// outermost first, or nil when there are none.  The slice is a copy.
func (e *ErrorVal) ErrorContext() []string {
	if e == nil {
		return nil
	}
	stack, ok := e.Native.(*CallStack)
	if !ok || stack == nil || len(stack.wraps) == 0 {
		return nil
	}
	return append([]string(nil), stack.wraps...)
}

func wrapError(lerr *LVal, line string) *LVal {
	if !lerr.IsError() {
		return lerr
	}
	stack := &CallStack{}
	if old := lerr.CallStack(); old != nil {
		stack = old
	}
	lines := make([]string, 0, len(stack.wraps)+1)
	lines = append(lines, line)
	lines = append(lines, stack.wraps...)
	// A header copy shares lerr's data, which is never written in place
	// once raised (rethrow hands out the same value); only the stack, which
	// carries the context, is new.
	cp := *lerr
	stack = stack.Copy()
	stack.wraps = lines
	cp.SetCallStack(stack)
	return &cp
}
