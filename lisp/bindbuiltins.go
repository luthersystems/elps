// Copyright © 2026 The ELPS authors

package lisp

// BindOpts controls LEnv.BindBuiltins.
type BindOpts struct {
	// Export exports every bound name from the package.
	Export bool
	// Shadow binds a name the package already has -- typically one it
	// imports, such as get from the language package -- the way a Lisp
	// defun of that name would, replacing the binding in this package only.
	// Without Shadow such a name is an error, as it is for AddBuiltins.
	Shadow bool
}

// BindBuiltins binds defs in env's current package (luthersystems/elps#745).
// It is AddBuiltins with two differences:
//
//   - With opts.Shadow a name the package already binds is rebound, as a
//     defun would rebind it.  A Go native replacing a Lisp defun that
//     shadowed an imported name (statedb:get shadowing lisp:get) keeps that
//     shadowing; AddBuiltins refuses the name by panicking.
//   - Every problem is returned as an error rather than a panic: a bound
//     name without Shadow, a constant (true, false), a name repeated within
//     defs, a nil definition, or formals the evaluator could never bind.
//     All definitions are checked before any is bound, so an error leaves
//     the package unchanged.
//
// The bound values are exactly what AddBuiltins builds: the same function
// value shape, the definition's docstring (when it has a Docstring method),
// and its sealed formals shared or unsealed formals copied.  Binding charges
// no step.  Shadowing in the language package itself is refused once its
// bindings are sealed, as a Lisp set there is.
func (env *LEnv) BindBuiltins(opts BindOpts, defs ...LBuiltinDef) *LVal {
	pkg := env.Runtime.Package
	if pkg == nil {
		return env.Errorf("no current package")
	}
	seen := make(map[string]bool, len(defs))
	for i, def := range defs {
		if def == nil {
			return env.Errorf("definition %d is nil", i+1)
		}
		name := def.Name()
		if seen[name] {
			return env.Errorf("builtin defined twice: %s", name)
		}
		seen[name] = true
		if name == TrueSymbol || name == FalseSymbol {
			return env.Errorf("cannot rebind constant: %v", name)
		}
		if message := formalSymbolsMessage(def.Formals(), false); message != "" {
			return env.Errorf("builtin %s cannot be registered: %s", name, message)
		}
		if exist, bound := registrationBound(pkg, name); bound && !replaceableLateOp(pkg, name, exist) && exist.Type != LError {
			if !opts.Shadow {
				return env.Errorf("symbol already defined: %s", name)
			}
			if pkg.bindingsSealed {
				return env.Errorf("cannot rebind lisp package binding: %s", name)
			}
		}
	}
	formals := newFormalsCopier(defs)
	for _, f := range defs {
		name := f.Name()
		v := registrationFunValue(pkg.Name, name, "<builtin-function ``"+name+"''>", LFunNone,
			registrationFormals(&formals, f.Formals()), f.Eval, builtinDocstring(f))
		pkg.putName(name, v)
		if opts.Export {
			pkg.appendExternal(name)
		}
	}
	return Nil()
}
