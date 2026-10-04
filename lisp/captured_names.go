// Copyright © 2026 The ELPS authors

package lisp

import "slices"

// CapturedNames returns the sorted, deduplicated names bound in the lexical
// environment that the lambda fn captured: its innermost frame and each
// frame above it, the root environment's own scope included.  A host can
// bind lexical names in the root with Put, and evaluation finds them before
// package globals, so they count as captured.  Package globals are not
// included; they live in package tables, not in any environment's scope.
// A name bound in more than one frame appears once.
//
// The second result is false, and the names nil, when fn is not a lambda:
// a non-function value, a builtin, a macro, a special operator, or a
// function value carrying no function data.  A lambda that captured no
// local bindings, such as one made by defun at top level, returns an
// empty, non-nil slice and true.
//
// CapturedNames is a Go-side read of function metadata.  It evaluates
// nothing, charges no steps, and returns a fresh slice: the captured
// environment itself stays unexported (issue #382), so the caller cannot
// rebind or reach the captured values through the result.
func CapturedNames(fn *LVal) ([]string, bool) {
	if fn == nil || fn.Type != LFun || fn.IsSpecialFun() {
		return nil, false
	}
	fd, ok := fn.Native.(*funData)
	if !ok || fd == nil || fd.builtin != nil {
		return nil, false
	}
	names := []string{}
	for env := fd.env; env != nil; env = env.parent {
		for name := range env.Bindings() {
			names = append(names, name)
		}
	}
	slices.Sort(names)
	return slices.Compact(names), true
}
