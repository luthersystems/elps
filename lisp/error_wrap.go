// Copyright © 2026 The ELPS authors

package lisp

import "fmt"

// wrapLine is one line of context WrapError added to an error, in a list
// from the outermost line in.  Nodes are never written once built, so every
// copy of an error's stack shares them.
type wrapLine struct {
	next *wrapLine
	text string
}

// clone returns a copy of the list that shares no node with it.
func (w *wrapLine) clone() *wrapLine {
	var head *wrapLine
	for tail := &head; w != nil; w = w.next {
		*tail = &wrapLine{text: w.text}
		tail = &(*tail).next
	}
	return head
}

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
// error or when the line is empty.  An error with no call stack yet gets its
// stack when the evaluator associates it, as any error a builtin returns
// does, and keeps its context.  A wrap costs the same however many lines
// the error has.
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
	if !ok || stack == nil {
		return nil
	}
	var lines []string
	for w := stack.wraps; w != nil; w = w.next {
		lines = append(lines, w.text)
	}
	return lines
}

func wrapError(lerr *LVal, line string) *LVal {
	if !lerr.IsError() || line == "" {
		return lerr
	}
	// The new stack shares the old one's frames and lines: neither is
	// written once the error is raised.
	var stack CallStack
	if old := lerr.CallStack(); old != nil {
		stack = *old
	}
	stack.wraps = &wrapLine{text: line, next: stack.wraps}
	// A header copy shares lerr's data, which is never written in place
	// once raised (rethrow hands out the same value); only the stack, which
	// carries the context, is new.
	cp := *lerr
	//elps:mutates the private header copy made on the line above, stamped with its own new stack before anything else can see it
	cp.Native = &stack //elpsvet:allow-native a stack stamped onto an in-flight error, as SetCallStack does: checkDiagnosticPayload (lisp/template.go) refuses to publish any value carrying a CallStack, so this payload never becomes shared template state
	return &cp
}
