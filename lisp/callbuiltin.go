// Copyright © 2026 The ELPS authors

package lisp

// BuiltinRef is a resolved handle on one of the builtins DefaultBuiltins
// returns (the language package's builtins plus any RegisterDefaultBuiltin
// registration made before it was resolved).  Resolve one with BuiltinFunc and
// call it with LEnv.CallBuiltin.
//
// It exists for Go builtins that reuse a language builtin for its exact
// error text and guards (sealed maps, MaxAlloc, typed keys) -- a native that
// replaces a Lisp definition calling get, + or assoc! must raise exactly what
// the Lisp raised (luthersystems/elps#745).  The zero BuiltinRef is invalid.
//
// A BuiltinRef holds no *LVal of its own beyond the builtin's sealed formals,
// so a package-level BuiltinRef shares nothing mutable between runtimes.
type BuiltinRef struct {
	def *langBuiltin
}

// BuiltinFunc resolves the default builtin named name.  It panics when no
// such builtin exists, so resolve handles at package initialization, where a
// misspelled name fails the program at start-up rather than a call:
//
//	var builtinGet = lisp.BuiltinFunc("get")
//
// Resolution is a linear scan, done once per handle; the handle itself is
// what makes each call cheap.
func BuiltinFunc(name string) BuiltinRef {
	for _, table := range [][]*langBuiltin{langBuiltins, userBuiltins} {
		for _, def := range table {
			if def.name == name {
				return BuiltinRef{def}
			}
		}
	}
	panic("lisp.BuiltinFunc: no default builtin named " + name)
}

// Name returns the builtin's name, or "" for the zero BuiltinRef.
func (b BuiltinRef) Name() string {
	if b.def == nil {
		return ""
	}
	return b.def.name
}

// CallBuiltin calls the builtin b with already-evaluated args, as the
// evaluator does once a call form's arguments are evaluated, and returns its
// result:
//
//   - args are bound against the builtin's formals exactly as a call from
//     Lisp binds them, so an omitted &optional or &key argument arrives as
//     nil and a wrong argument count or a malformed keyword list fails with
//     the evaluator's own error.  The caller's args slice is never written.
//   - If the evaluation's context is done it returns the context-cancelled
//     condition before calling, as the evaluator's call boundary does.
//   - A terminal expression the builtin returns (funcall, apply and the like
//     hand one back for tail calls) is evaluated in its environment, so
//     CallBuiltin always returns a value or an error, never an internal
//     marker.  That evaluation costs the steps it would cost from Lisp.
//
// CallBuiltin charges no evaluation step of its own and pushes no stack
// frame: an error the builtin raises is attributed to the calling builtin's
// frame, as it was for the hand-written b.Eval(env, QExpr(args)) this
// replaces.  It does not recover panics; call it from a builtin, which the
// evaluator already guards.
func (env *LEnv) CallBuiltin(b BuiltinRef, args ...*LVal) *LVal {
	if b.def == nil {
		return env.Errorf("CallBuiltin: zero BuiltinRef")
	}
	if lerr := env.CheckContext(); lerr != nil {
		return lerr
	}
	list := bindNativePositional(b.def.formals.Cells, args)
	if list == nil {
		fun := FunInPackage(env.Runtime.Registry.Lang, b.def.name, b.def.formals, b.def.fun)
		_, list = env.bindGeneral(fun, QExpr(args))
		if list.Type == LError {
			return list
		}
	}
	val := b.def.fun(env, list)
	if val == nil {
		return env.Errorf("internal error: builtin %s returned nil", b.def.name)
	}
	if val.Type == LMarkTerminal {
		termEnv := val.Native.(*LEnv)
		ctx := env.evalCtx
		if termEnv != env {
			prev := termEnv.evalCtx
			defer func() { termEnv.evalCtx = prev }()
			termEnv.evalCtx = ctx
		}
		return termEnv.eval(ctx, val.Cells[0])
	}
	return val
}
