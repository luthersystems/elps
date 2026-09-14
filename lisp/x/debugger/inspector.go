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

func (f FormatterFunc) FormatValue(v any) string     { return f(v) }
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
	bindings := make([]ScopeBinding, 0, env.NumBindings())
	for name, val := range env.Bindings() {
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
		for name, val := range current.Bindings() {
			if !seen[name] {
				seen[name] = true
				bindings = append(bindings, ScopeBinding{Name: name, Value: val})
			}
		}
		current = current.Parent()
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
		if current.Parent() == nil {
			break
		}
		for name, val := range current.Bindings() {
			if !seen[name] {
				seen[name] = true
				bindings = append(bindings, ScopeBinding{Name: name, Value: val})
			}
		}
		current = current.Parent()
	}
	sort.Slice(bindings, func(i, j int) bool {
		return bindings[i].Name < bindings[j].Name
	})
	return bindings
}

// InspectMacroExpansion returns scope bindings representing the macro
// expansion context: macro name, call-site arguments paired with their
// positional index, and the call-site location. Returns nil if the
// expression has no macro expansion info.
func InspectMacroExpansion(expr *lisp.LVal) []ScopeBinding {
	m, ok := expr.MacroExpansion()
	if !ok {
		return nil
	}
	bindings := []ScopeBinding{
		{Name: "(macro)", Value: lisp.String(m.Name)},
	}
	for i, arg := range m.Args {
		name := fmt.Sprintf("arg[%d]", i)
		bindings = append(bindings, ScopeBinding{Name: name, Value: arg})
	}
	if m.CallSite != nil {
		bindings = append(bindings, ScopeBinding{
			Name:  "(call-site)",
			Value: lisp.String(m.CallSite.String()),
		})
	}
	return bindings
}

// FormatValue returns a human-readable string representation of an LVal,
// suitable for display in a debugger variables view.
func FormatValue(v *lisp.LVal) string {
	return NewValueFormatter(nil, nil).Format(v)
}

// ValueFormatter formats all bindings in one debugger response with a shared
// runtime rendering budget. Create a new formatter for each response.
type ValueFormatter struct {
	renderer *lisp.DiagnosticRenderer
	engine   *Engine
}

// NewValueFormatter uses env's limits and context, or the engine's paused
// environment when env is nil. Without either it uses the runtime defaults.
func NewValueFormatter(env *lisp.LEnv, eng *Engine) *ValueFormatter {
	if env == nil && eng != nil {
		env, _ = eng.PausedState()
	}
	if env == nil {
		env = lisp.NewEnv(nil)
	}
	return &ValueFormatter{renderer: env.NewRenderer(env.Context()), engine: eng}
}

// NewProtocolValueFormatter reserves framing and worst-case JSON escaping for
// one DAP response. Six encoded bytes suffice for every input byte, including
// invalid UTF-8. Each translated item must also charge its fixed JSON fields.
func NewProtocolValueFormatter(env *lisp.LEnv, eng *Engine) *ValueFormatter {
	if env == nil && eng != nil {
		env, _ = eng.PausedState()
	}
	if env == nil {
		env = lisp.NewEnv(nil)
	}
	return &ValueFormatter{renderer: env.NewRendererWithLimit(env.Context(), (env.Runtime.MaxAllocBytes()-512)/6), engine: eng}
}

// Text charges names, source fields and display punctuation to the same budget
// as values, without first concatenating program-controlled strings.
func (f *ValueFormatter) Text(parts ...string) string { return f.renderer.Text(parts...) }

// Exhausted reports whether this response has exhausted its rendering budget.
func (f *ValueFormatter) Exhausted() bool { return f.renderer.Exhausted() }

// Format renders a value through the runtime's bounded, context-aware renderer.
// Root summaries preserve the debugger's compact display for expandable values.
func (f *ValueFormatter) Format(v *lisp.LVal) string {
	if f.Exhausted() {
		return ""
	}
	if s := f.renderer.Text(""); s != "" || f.Exhausted() {
		return s
	}
	if v == nil {
		return f.renderer.Text("<nil>")
	}
	switch v.Type {
	case lisp.LSExpr:
		if v.Len() > 10 {
			return f.renderer.Text(fmt.Sprintf("(%d elements)", v.Len()))
		}
		s := f.renderer.Render(v)
		if v.IsQuoted() && strings.HasPrefix(s, "'(") && strings.HasSuffix(s, ")") {
			return "[" + s[2:len(s)-1] + "]"
		}
		return s
	case lisp.LFun:
		name := v.Str
		if name == "" {
			name = v.FID()
		}
		switch {
		case v.IsMacro():
			return f.renderer.Text("<macro ", name, ">")
		case v.IsSpecialOp():
			return f.renderer.Text("<special-op ", name, ">")
		default:
			return f.renderer.Text("<function ", name, ">")
		}
	case lisp.LError:
		return f.renderer.Text("<error: ", v.Str, ">")
	case lisp.LNative:
		if f.engine != nil {
			if s := f.engine.FormatNative(v.Native); s != "" {
				return f.renderer.Text(s)
			}
		}
		if v.Native == nil {
			return f.renderer.Text("<native nil>")
		}
		return f.renderer.Text(fmt.Sprintf("<native %T>", v.Native))
	case lisp.LTaggedVal:
		return f.renderer.Text("<tagged ", v.Str, ">")
	case lisp.LArray:
		return f.renderer.Text(fmt.Sprintf("<array len=%d>", v.Len()))
	case lisp.LSortMap:
		return f.renderer.Text(fmt.Sprintf("<sorted-map len=%d>", v.Len()))
	case lisp.LBytes:
		return f.renderer.Text(fmt.Sprintf("<bytes len=%d>", len(v.Bytes())))
	default:
		return f.renderer.Render(v)
	}
}

// FormatValueWith formats one value using the engine's paused environment and
// registered native formatters. Use NewValueFormatter for multiple bindings.
func FormatValueWith(v *lisp.LVal, eng *Engine) string {
	return NewValueFormatter(nil, eng).Format(v)
}

// EvalInContext parses and evaluates all expressions in the paused
// environment, returning the result of the last one (progn semantics).
// Short-circuits on first error. This allows the debug console to
// evaluate multi-expression input like "(set 'x 10) (+ x y)".
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
	var result *lisp.LVal
	for _, expr := range exprs {
		result = env.Eval(expr)
		if result.Type == lisp.LError {
			return result
		}
	}
	return result
}

// EvalSingleInContext parses and evaluates only the first expression in
// the paused environment. Used for hover tooltips where multi-expression
// evaluation is inappropriate.
func EvalSingleInContext(env *lisp.LEnv, source string) *lisp.LVal {
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
