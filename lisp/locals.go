// Copyright © 2026 The ELPS authors

package lisp

import "sort"

// Binding is one local variable reported by LEnv.Locals: the symbol's name
// and the value it is bound to.
type Binding struct {
	Value *LVal
	Name  string
}

// Locals returns the local variables visible from env, sorted by name.
//
// A local is a binding made by a function's parameters or by let, let*,
// flet, labels and the like, in env or any enclosing
// lexical scope up to (not including) the root environment. Package globals
// (set at top level, defun, defmacro) are not locals: they live in the
// package, not in the environment chain, and are read with Get. Local
// functions bound by flet and labels are locals too: they appear under their
// names with the function as the value. When an inner
// scope shadows a name, only the innermost binding is reported.
//
// A Go builtin receives an environment whose parents are the caller's
// lexical scopes, so env.Locals() inside a builtin reports the variables
// visible at the call site, the way Python's locals() does. The debugger's
// variables pane and completion use the same walk.
//
// The values are the bound values themselves, not copies: treat them as
// borrowed, never mutate them, and copy (LVal.Copy) any value kept past the
// builtin's return. The result reflects the bindings at the time of the call.
// Locals is nil-receiver safe and returns nil for an env with no locals.
func (env *LEnv) Locals() []Binding {
	var bindings []Binding
	seen := make(map[string]bool)
	for current := env; current != nil && current.parent != nil; current = current.parent {
		for name, val := range current.Bindings() {
			if !seen[name] {
				seen[name] = true
				bindings = append(bindings, Binding{Name: name, Value: val})
			}
		}
	}
	sort.Slice(bindings, func(i, j int) bool { return bindings[i].Name < bindings[j].Name })
	return bindings
}
