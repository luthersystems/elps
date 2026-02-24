// Copyright © 2018 The ELPS authors

package dapserver

import (
	"github.com/google/go-dap"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/x/debugger"
)

// elpsThreadID is the single thread ID used for ELPS (single-threaded interpreter).
const elpsThreadID = 1

// translateStackFrames converts ELPS CallStack frames to DAP StackFrame objects.
// Frames are returned in reverse order (most recent first), matching DAP convention.
// If pausedExpr is non-nil, the top frame's line/column are overridden with
// the paused expression's source location, which represents where execution
// actually stopped (as opposed to the call site stored in the CallFrame).
func translateStackFrames(stack *lisp.CallStack, pausedExpr *lisp.LVal) []dap.StackFrame {
	if stack == nil || len(stack.Frames) == 0 {
		return nil
	}
	frames := make([]dap.StackFrame, 0, len(stack.Frames))
	for i := len(stack.Frames) - 1; i >= 0; i-- {
		f := &stack.Frames[i]
		sf := dap.StackFrame{
			Id:   i + 1, // 1-based IDs
			Name: f.QualifiedFunName(),
		}
		if f.Source != nil {
			sf.Source = &dap.Source{
				Name: f.Source.File,
				Path: f.Source.Path,
			}
			sf.Line = f.Source.Line
			sf.Column = f.Source.Col
		}
		// For the top frame (first appended), override with the paused
		// expression's source to show where execution actually stopped.
		if len(frames) == 0 && pausedExpr != nil && pausedExpr.Source != nil {
			sf.Line = pausedExpr.Source.Line
			sf.Column = pausedExpr.Source.Col
			if sf.Source == nil {
				sf.Source = &dap.Source{
					Name: pausedExpr.Source.File,
					Path: pausedExpr.Source.Path,
				}
			}
		}
		frames = append(frames, sf)
	}
	return frames
}

// translateVariables converts scope bindings to DAP Variable objects.
func translateVariables(bindings []debugger.ScopeBinding) []dap.Variable {
	vars := make([]dap.Variable, len(bindings))
	for i, b := range bindings {
		vars[i] = dap.Variable{
			Name:  b.Name,
			Value: debugger.FormatValue(b.Value),
			Type:  lvalTypeName(b.Value),
		}
	}
	return vars
}

// lvalTypeName returns a human-readable type name for an LVal.
func lvalTypeName(v *lisp.LVal) string {
	if v == nil {
		return "nil"
	}
	return v.Type.String()
}

// translateBreakpoints converts engine breakpoints to DAP Breakpoint objects.
func translateBreakpoints(bps []*debugger.Breakpoint) []dap.Breakpoint {
	result := make([]dap.Breakpoint, len(bps))
	for i, bp := range bps {
		result[i] = dap.Breakpoint{
			Id:       bp.ID,
			Verified: true,
			Source: &dap.Source{
				Name: bp.File,
				Path: bp.File,
			},
			Line: bp.Line,
		}
	}
	return result
}
