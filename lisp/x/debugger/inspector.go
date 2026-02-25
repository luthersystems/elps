// Copyright © 2018 The ELPS authors

package debugger

import (
	"fmt"
	"sort"
	"strings"

	"github.com/luthersystems/elps/lisp"
)

// VariableFormatter customizes how native Go values wrapped in LNative
// are displayed in the debugger. Embedders register formatters by Go type
// name (fmt.Sprintf("%T", value)) to provide rich display and drill-down.
type VariableFormatter interface {
	// FormatValue returns a human-readable string for the native value.
	FormatValue(v any) string
	// Children returns expandable child bindings for the native value.
	// Return nil if the value has no children.
	Children(v any) []NativeChild
}

// NativeChild represents a single child binding of a native value,
// exposed for drill-down in the debugger variables view.
type NativeChild struct {
	Name  string
	Value *lisp.LVal
}

// FormatterFunc adapts a simple format function into a VariableFormatter
// with no children. Useful when you only need custom display text.
type FormatterFunc func(v any) string

func (f FormatterFunc) FormatValue(v any) string   { return f(v) }
func (f FormatterFunc) Children(v any) []NativeChild { return nil }

// ScopeBinding represents a single variable binding in a scope.
type ScopeBinding struct {
	Name  string
	Value *lisp.LVal
}

// InspectLocals returns the local variable bindings from the given
// environment's immediate scope (not parent scopes). Bindings are
// returned sorted by name.
func InspectLocals(env *lisp.LEnv) []ScopeBinding {
	if env == nil {
		return nil
	}
	bindings := make([]ScopeBinding, 0, len(env.Scope))
	for name, val := range env.Scope {
		bindings = append(bindings, ScopeBinding{Name: name, Value: val})
	}
	sort.Slice(bindings, func(i, j int) bool {
		return bindings[i].Name < bindings[j].Name
	})
	return bindings
}

// InspectScope returns all bindings visible from the given environment,
// walking up the parent chain. Local bindings shadow parent bindings.
// Bindings are returned sorted by name.
func InspectScope(env *lisp.LEnv) []ScopeBinding {
	if env == nil {
		return nil
	}
	seen := make(map[string]bool)
	var bindings []ScopeBinding
	current := env
	for current != nil {
		for name, val := range current.Scope {
			if !seen[name] {
				seen[name] = true
				bindings = append(bindings, ScopeBinding{Name: name, Value: val})
			}
		}
		current = current.Parent
	}
	sort.Slice(bindings, func(i, j int) bool {
		return bindings[i].Name < bindings[j].Name
	})
	return bindings
}

// InspectFunctionLocals returns all bindings visible from the given
// environment up through parent scopes, stopping at the root env
// (Parent==nil) which contains builtins. In ELPS, package symbols live
// in Runtime.Package, not in the env chain, so walking up to (but not
// including) the root env collects exactly the function-local bindings.
// This gives users the local variables they expect to see in a debugger,
// even when paused inside a sub-expression like (if ...).
func InspectFunctionLocals(env *lisp.LEnv) []ScopeBinding {
	if env == nil {
		return nil
	}
	seen := make(map[string]bool)
	var bindings []ScopeBinding
	current := env
	for current != nil {
		// Stop at the root env (builtins). Package symbols are in
		// Runtime.Package.Symbols, not in env.Scope.
		if current.Parent == nil {
			break
		}
		for name, val := range current.Scope {
			if !seen[name] {
				seen[name] = true
				bindings = append(bindings, ScopeBinding{Name: name, Value: val})
			}
		}
		current = current.Parent
	}
	sort.Slice(bindings, func(i, j int) bool {
		return bindings[i].Name < bindings[j].Name
	})
	return bindings
}

// FormatValue returns a human-readable string representation of an LVal,
// suitable for display in a debugger variables view.
func FormatValue(v *lisp.LVal) string {
	if v == nil {
		return "<nil>"
	}
	switch v.Type {
	case lisp.LInt:
		return fmt.Sprintf("%d", v.Int)
	case lisp.LFloat:
		return fmt.Sprintf("%g", v.Float)
	case lisp.LString:
		return fmt.Sprintf("%q", v.Str)
	case lisp.LSymbol:
		return v.Str
	case lisp.LSExpr:
		if v.IsNil() {
			return "()"
		}
		return formatList(v)
	case lisp.LFun:
		name := v.Str
		if name == "" && v.FunData() != nil {
			name = v.FunData().FID
		}
		switch {
		case v.IsMacro():
			return fmt.Sprintf("<macro %s>", name)
		case v.IsSpecialOp():
			return fmt.Sprintf("<special-op %s>", name)
		default:
			return fmt.Sprintf("<function %s>", name)
		}
	case lisp.LError:
		return fmt.Sprintf("<error: %s>", v.Str)
	case lisp.LNative:
		if v.Native == nil {
			return "<native nil>"
		}
		return fmt.Sprintf("<native %T>", v.Native)
	case lisp.LTaggedVal:
		return fmt.Sprintf("<tagged %s>", v.Str)
	case lisp.LArray:
		return fmt.Sprintf("<array len=%d>", v.Len())
	case lisp.LSortMap:
		return fmt.Sprintf("<sorted-map len=%d>", v.Len())
	case lisp.LQSymbol:
		return fmt.Sprintf("'%s", v.Str)
	case lisp.LBytes:
		return fmt.Sprintf("<bytes len=%d>", len(v.Bytes()))
	default:
		return fmt.Sprintf("<%s>", v.Type)
	}
}

func formatList(v *lisp.LVal) string {
	if v.Len() > 10 {
		return fmt.Sprintf("(%d elements)", v.Len())
	}
	var parts []string
	for _, cell := range v.Cells {
		parts = append(parts, FormatValue(cell))
	}
	open, close := "(", ")"
	if v.Quoted {
		open, close = "[", "]"
	}
	return open + strings.Join(parts, " ") + close
}

// FormatValueWith returns a human-readable string representation of an LVal,
// using the engine's registered formatters for LNative values. For all other
// types it delegates to FormatValue. If eng is nil, falls back to FormatValue.
func FormatValueWith(v *lisp.LVal, eng *Engine) string {
	if eng != nil && v != nil && v.Type == lisp.LNative {
		if s := eng.FormatNative(v.Native); s != "" {
			return s
		}
	}
	return FormatValue(v)
}

// EvalInContext parses and evaluates an expression in the paused
// environment. This allows the debug console to inspect and mutate
// state. Returns the result or an error LVal.
func EvalInContext(env *lisp.LEnv, source string) *lisp.LVal {
	if env.Runtime.Reader == nil {
		return env.Errorf("no reader for debug evaluation")
	}
	exprs, err := env.Runtime.Reader.Read("debug-eval", strings.NewReader(source))
	if err != nil {
		return env.Errorf("debug eval parse error: %v", err)
	}
	if len(exprs) == 0 {
		return lisp.Nil()
	}
	return env.Eval(exprs[0])
}
