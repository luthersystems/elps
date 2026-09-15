// Copyright © 2018 The ELPS authors

package dapserver

import (
	"fmt"
	"path/filepath"
	"regexp"
	"strings"

	"github.com/google/go-dap"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/x/debugger"
	"github.com/luthersystems/elps/parser/token"
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
func translateStackFrames(stack *lisp.CallStack, pausedExpr *lisp.LVal, sourceRoot string, envs ...*lisp.LEnv) []dap.StackFrame {
	var env *lisp.LEnv
	if len(envs) > 0 {
		env = envs[0]
	}
	formatter := debugger.NewProtocolValueFormatter(env, nil)
	if stack == nil || len(stack.Frames) == 0 {
		return []dap.StackFrame{}
	}
	frames := make([]dap.StackFrame, 0, min(len(stack.Frames), 64))
	for i := len(stack.Frames) - 1; i >= 0; i-- {
		if formatter.Exhausted() {
			break
		}
		overhead := formatter.Text(strings.Repeat(" ", 64)) // up to 384 encoded bytes of fixed fields
		if formatter.Exhausted() {
			frames = append(frames, dap.StackFrame{Id: i + 1, Name: overhead})
			break
		}
		f := &stack.Frames[i]
		name := f.Name
		if name == "" {
			name = f.FID
		}
		if f.Package != "" {
			name = formatter.Text(f.Package, ":", name)
		} else {
			name = formatter.Text(name)
		}
		sf := dap.StackFrame{
			Id:   i + 1, // 1-based IDs
			Name: name,
		}
		loc := f.Source
		var pausedLoc token.Location
		pausedOK := false
		if pausedExpr != nil {
			pausedLoc, pausedOK = pausedExpr.Source()
		}
		if len(frames) == 0 && pausedOK {
			loc = &pausedLoc
		}
		if loc != nil {
			sf.Source = &dap.Source{Name: formatter.Text(loc.File), Path: boundedSourcePath(formatter, loc.Path, loc.File, sourceRoot)}
			sf.Line, sf.Column = loc.Line, loc.Col
		}
		if len(frames) == 0 && pausedOK {
			if m, ok := pausedExpr.MacroExpansion(); ok {
				sf.Name += formatter.Text(" [macro: ", m.Name, "]")
			}
		}
		frames = append(frames, sf)
	}
	if formatter.Exhausted() {
		frames = append(frames, dap.StackFrame{Name: "#<truncated>"})
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
func translateVariables(bindings []debugger.ScopeBinding, allocRef func(*lisp.LVal) int, eng *debugger.Engine, envs ...*lisp.LEnv) (result []dap.Variable) {
	var env *lisp.LEnv
	if len(envs) > 0 {
		env = envs[0]
	}
	formatter := debugger.NewProtocolValueFormatter(env, eng)
	defer func() {
		if formatter.Exhausted() {
			result = append(result, dap.Variable{Name: "#<truncated>"})
		}
	}()
	vars := make([]dap.Variable, 0, min(len(bindings), 64))
	for _, b := range bindings {
		if formatter.Exhausted() {
			return vars
		}
		v := dap.Variable{
			Name:               variableName(formatter, b.Name),
			Value:              formatter.Format(b.Value),
			Type:               lvalTypeName(b.Value),
			VariablesReference: allocRef(b.Value),
		}
		setChildHints(&v, b.Value)
		vars = append(vars, v)
	}
	return vars
}

// expandVariable returns the child variables of a structured LVal.
// mapKeyFilter, if non-nil, filters sorted-map entries to only those whose
// formatted key name matches the regex. It is ignored for non-map types.
func expandVariable(v *lisp.LVal, allocRef func(*lisp.LVal) int, eng *debugger.Engine, mapKeyFilter *regexp.Regexp) (result []dap.Variable) {
	if v == nil {
		return []dap.Variable{}
	}
	formatter := debugger.NewProtocolValueFormatter(nil, eng)
	defer func() {
		if formatter.Exhausted() {
			result = append(result, dap.Variable{Name: "#<truncated>"})
		}
	}()
	switch v.Type {
	case lisp.LSExpr:
		vars := make([]dap.Variable, len(v.Cells))
		for i, cell := range v.Cells {
			if formatter.Exhausted() {
				return vars[:i]
			}
			vars[i] = dap.Variable{
				Name:               variableName(formatter, fmt.Sprintf("[%d]", i)),
				Value:              formatter.Format(cell),
				Type:               lvalTypeName(cell),
				VariablesReference: allocRef(cell),
			}
			setChildHints(&vars[i], cell)
		}
		return vars
	case lisp.LSortMap:
		entries := v.MapEntries()
		if entries.Type == lisp.LError {
			return []dap.Variable{}
		}
		var vars []dap.Variable
		for _, pair := range entries.Cells {
			if formatter.Exhausted() {
				break
			}
			key := pair.Cells[0]
			val := pair.Cells[1]
			overhead := formatter.Text(strings.Repeat(" ", 48))
			if formatter.Exhausted() {
				vars = append(vars, dap.Variable{Name: overhead})
				break
			}
			name := formatter.Format(key)
			if mapKeyFilter != nil && !mapKeyFilter.MatchString(name) {
				continue
			}
			v := dap.Variable{
				Name:               name,
				Value:              formatter.Format(val),
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
			if formatter.Exhausted() {
				return vars[:i]
			}
			vars[i] = dap.Variable{
				Name:               variableName(formatter, fmt.Sprintf("[%d]", i)),
				Value:              formatter.Format(cell),
				Type:               lvalTypeName(cell),
				VariablesReference: allocRef(cell),
			}
			setChildHints(&vars[i], cell)
		}
		return vars
	case lisp.LTaggedVal:
		if len(v.Cells) == 0 {
			return []dap.Variable{}
		}
		inner := v.Cells[0]
		child := dap.Variable{
			Name:               variableName(formatter, "data"),
			Value:              formatter.Format(inner),
			Type:               lvalTypeName(inner),
			VariablesReference: allocRef(inner),
		}
		setChildHints(&child, inner)
		return []dap.Variable{child}
	case lisp.LNative:
		if eng == nil {
			return []dap.Variable{}
		}
		children := eng.NativeChildren(v.Native)
		if len(children) == 0 {
			return []dap.Variable{}
		}
		vars := make([]dap.Variable, len(children))
		for i, ch := range children {
			if formatter.Exhausted() {
				return vars[:i]
			}
			vars[i] = dap.Variable{
				Name:               variableName(formatter, ch.Name),
				Value:              formatter.Format(ch.Value),
				Type:               lvalTypeName(ch.Value),
				VariablesReference: allocRef(ch.Value),
			}
			setChildHints(&vars[i], ch.Value)
		}
		return vars
	default:
		return []dap.Variable{}
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

// Fixed per-variable fields, quotes and separators fit in 48*6 wire bytes.
func variableName(f *debugger.ValueFormatter, name string) string {
	overhead := f.Text(strings.Repeat(" ", 48))
	if f.Exhausted() {
		return overhead
	}
	return f.Text(name)
}

func boundedSourcePath(f *debugger.ValueFormatter, path, file, root string) string {
	if path == "" {
		path = file
	}
	if path == "" {
		return ""
	}
	if filepath.IsAbs(path) || root == "" {
		return f.Text(path)
	}
	// Charge before filepath.Join can allocate or scan arbitrarily large names.
	bounded := f.Text(root, string(filepath.Separator), path)
	if f.Exhausted() {
		return bounded
	}
	return filepath.Clean(bounded)
}
