// Copyright © 2018 The ELPS authors

package dapserver

import (
	"fmt"
	"path/filepath"
	"regexp"

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
// If sourceRoot is non-empty, relative Source.Path values are resolved to
// absolute paths so that DAP clients (VS Code) can open the source files.
func translateStackFrames(stack *lisp.CallStack, pausedExpr *lisp.LVal, sourceRoot string) []dap.StackFrame {
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
				Path: resolveSourcePath(f.Source.Path, f.Source.File, sourceRoot),
			}
			sf.Line = f.Source.Line
			sf.Column = f.Source.Col
		}
		// For the top frame (first appended), override with the paused
		// expression's source to show where execution actually stopped.
		// Always use the paused expression's file — when stepping into a
		// function defined in a different file, the call frame still points
		// to the caller's file, but we need to show the callee's source.
		if len(frames) == 0 && pausedExpr != nil && pausedExpr.Source != nil {
			sf.Line = pausedExpr.Source.Line
			sf.Column = pausedExpr.Source.Col
			sf.Source = &dap.Source{
				Name: pausedExpr.Source.File,
				Path: resolveSourcePath(pausedExpr.Source.Path, pausedExpr.Source.File, sourceRoot),
			}
			// Annotate with macro expansion name when paused inside a
			// macro expansion, so the user can see which macro is active.
			if pausedExpr.MacroExpansion != nil && pausedExpr.MacroExpansion.MacroExpansionContext != nil {
				sf.Name = sf.Name + " [macro: " + pausedExpr.MacroExpansion.Name + "]"
			}
		}
		frames = append(frames, sf)
	}
	return frames
}

// resolveSourcePath returns an absolute path for DAP clients. If path is
// already absolute, it is returned as-is. Otherwise, if sourceRoot is set,
// the path (or file as fallback) is joined with sourceRoot.
func resolveSourcePath(path, file, sourceRoot string) string {
	if path == "" {
		path = file
	}
	if path == "" {
		return ""
	}
	if filepath.IsAbs(path) {
		return path
	}
	if sourceRoot != "" {
		return filepath.Join(sourceRoot, path)
	}
	return path
}

// translateVariables converts scope bindings to DAP Variable objects.
// allocRef assigns a variable reference for expandable values. eng is used
// for custom native type formatting.
func translateVariables(bindings []debugger.ScopeBinding, allocRef func(*lisp.LVal) int, eng *debugger.Engine) []dap.Variable {
	vars := make([]dap.Variable, len(bindings))
	for i, b := range bindings {
		vars[i] = dap.Variable{
			Name:               b.Name,
			Value:              debugger.FormatValueWith(b.Value, eng),
			Type:               lvalTypeName(b.Value),
			VariablesReference: allocRef(b.Value),
		}
		setChildHints(&vars[i], b.Value)
	}
	return vars
}

// expandVariable returns the child variables of a structured LVal.
// mapKeyFilter, if non-nil, filters sorted-map entries to only those whose
// formatted key name matches the regex. It is ignored for non-map types.
func expandVariable(v *lisp.LVal, allocRef func(*lisp.LVal) int, eng *debugger.Engine, mapKeyFilter *regexp.Regexp) []dap.Variable {
	if v == nil {
		return nil
	}
	switch v.Type {
	case lisp.LSExpr:
		vars := make([]dap.Variable, len(v.Cells))
		for i, cell := range v.Cells {
			vars[i] = dap.Variable{
				Name:               fmt.Sprintf("[%d]", i),
				Value:              debugger.FormatValueWith(cell, eng),
				Type:               lvalTypeName(cell),
				VariablesReference: allocRef(cell),
			}
			setChildHints(&vars[i], cell)
		}
		return vars
	case lisp.LSortMap:
		entries := v.MapEntries()
		if entries.Type == lisp.LError {
			return nil
		}
		var vars []dap.Variable
		for _, pair := range entries.Cells {
			key := pair.Cells[0]
			val := pair.Cells[1]
			name := debugger.FormatValue(key)
			if mapKeyFilter != nil && !mapKeyFilter.MatchString(name) {
				continue
			}
			v := dap.Variable{
				Name:               name,
				Value:              debugger.FormatValueWith(val, eng),
				Type:               lvalTypeName(val),
				VariablesReference: allocRef(val),
			}
			setChildHints(&v, val)
			vars = append(vars, v)
		}
		return vars
	case lisp.LArray:
		// Cells[0] = dimensions, Cells[1] = flat data.
		data := v.Cells[1]
		vars := make([]dap.Variable, len(data.Cells))
		for i, cell := range data.Cells {
			vars[i] = dap.Variable{
				Name:               fmt.Sprintf("[%d]", i),
				Value:              debugger.FormatValueWith(cell, eng),
				Type:               lvalTypeName(cell),
				VariablesReference: allocRef(cell),
			}
			setChildHints(&vars[i], cell)
		}
		return vars
	case lisp.LTaggedVal:
		if len(v.Cells) == 0 {
			return nil
		}
		inner := v.Cells[0]
		child := dap.Variable{
			Name:               "data",
			Value:              debugger.FormatValueWith(inner, eng),
			Type:               lvalTypeName(inner),
			VariablesReference: allocRef(inner),
		}
		setChildHints(&child, inner)
		return []dap.Variable{child}
	case lisp.LNative:
		if eng == nil {
			return nil
		}
		children := eng.NativeChildren(v.Native)
		if len(children) == 0 {
			return nil
		}
		vars := make([]dap.Variable, len(children))
		for i, ch := range children {
			vars[i] = dap.Variable{
				Name:               ch.Name,
				Value:              debugger.FormatValueWith(ch.Value, eng),
				Type:               lvalTypeName(ch.Value),
				VariablesReference: allocRef(ch.Value),
			}
			setChildHints(&vars[i], ch.Value)
		}
		return vars
	default:
		return nil
	}
}

// childInfo returns the number of indexed and named children for an LVal.
// These counts are used as DAP pagination hints (IndexedVariables/NamedVariables)
// so that clients like VS Code can paginate large collections.
func childInfo(v *lisp.LVal) (indexedChildren, namedChildren int) {
	if v == nil {
		return 0, 0
	}
	switch v.Type {
	case lisp.LSExpr:
		return len(v.Cells), 0
	case lisp.LArray:
		if len(v.Cells) > 1 {
			return len(v.Cells[1].Cells), 0
		}
		return 0, 0
	case lisp.LSortMap:
		return 0, v.Len()
	default:
		return 0, 0
	}
}

// setChildHints sets IndexedVariables and NamedVariables on a DAP variable
// based on the child counts of the given LVal.
func setChildHints(v *dap.Variable, lval *lisp.LVal) {
	indexed, named := childInfo(lval)
	if indexed > 0 {
		v.IndexedVariables = indexed
	}
	if named > 0 {
		v.NamedVariables = named
	}
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
