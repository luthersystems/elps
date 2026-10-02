// Copyright © 2026 The ELPS authors

package lisp

import "strings"

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
// A BuiltinRef is only a position in the builtin tables, which are
// append-only, so it keeps no *LVal reachable: a package-level BuiltinRef
// shares nothing between runtimes and passes elpsvet's elpsownership rule.
type BuiltinRef struct {
	table uint8 // builtinTableNone (zero value), builtinTableLang or builtinTableUser
	idx   int
	// nreq is the number of formals when all are required, else -1: a call
	// with exactly nreq args needs no binding.
	nreq int
}

// requiredOnly returns len(formals) when every formal is a plain required
// name, else -1.
func requiredOnly(formals *LVal) int {
	for _, f := range formals.Cells {
		if f == nil || f.Type != LSymbol || strings.HasPrefix(f.Str, MetaArgPrefix) {
			return -1
		}
	}
	return len(formals.Cells)
}

const (
	builtinTableNone uint8 = iota
	builtinTableLang
	builtinTableUser
)

// def returns the definition b names, or nil for the zero BuiltinRef.
func (b BuiltinRef) def() *langBuiltin {
	switch b.table {
	case builtinTableLang:
		return langBuiltins[b.idx]
	case builtinTableUser:
		return userBuiltins[b.idx]
	}
	return nil
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
	for i, def := range langBuiltins {
		if def.name == name {
			return BuiltinRef{table: builtinTableLang, idx: i, nreq: requiredOnly(def.formals)}
		}
	}
	for i, def := range userBuiltins {
		if def.name == name {
			return BuiltinRef{table: builtinTableUser, idx: i, nreq: requiredOnly(def.formals)}
		}
	}
	panic("lisp.BuiltinFunc: no default builtin named " + name)
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
	def := b.def()
	if def == nil {
		return env.Errorf("CallBuiltin: zero BuiltinRef")
	}
	if lerr := env.CheckContext(); lerr.Type == LError {
		return lerr
	}
	var list *LVal
	if len(args) == b.nreq {
		// Exactly the required formals and nothing else: the argument list
		// is args itself, capped so an append cannot write past it.  No
		// language builtin writes its argument cells.
		list = QExpr(args[:len(args):len(args)])
	} else {
		list = bindNativePositional(def.formals.Cells, args)
	}
	if list == nil {
		fun := FunInPackage(env.Runtime.Registry.Lang, def.name, def.formals, def.fun)
		_, list = env.bindGeneral(fun, QExpr(args))
		if list.Type == LError {
			return list
		}
	}
	// CallBuiltin pushes no frame, so the top frame is the calling
	// builtin's.  funcall and apply mark the top frame terminal before their
	// FunCall, assuming it is their own; here that would leave the caller's
	// frame terminal and let tail-recursion optimization unwind through Go.
	// Keep the caller's frame non-terminal while b runs and restore it after.
	top := env.Runtime.Stack.Top()
	if top != nil {
		terminal := top.Terminal
		top.Terminal = false
		defer func() { top.Terminal = terminal }()
	}
	val := def.fun(env, list)
	if val == nil {
		return env.Errorf("internal error: builtin %s returned nil", def.name)
	}
	if val.Type == LMarkTailRec {
		// The callee matched a terminal chain through the frame funcall or
		// apply marked.  Go frames cannot be unwound, so make the call
		// plainly, as a non-tail call from Lisp would.
		if top != nil {
			top.Terminal = false
		}
		fun, fargs := extractMarkTailRec(val)
		return env.funCall(env.evalCtx, fun, fargs)
	}
	if val.Type == LMarkTerminal {
		termEnv, ok := val.Native.(*LEnv)
		if !ok {
			return env.Errorf("internal error: terminal mark has no environment")
		}
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

// CallGlobal calls the function bound to the global symbol named sym -- a
// qualified name such as "utils:set-exception-business" is the usual form --
// with already-evaluated args, resolving the binding at call time exactly as
// a Lisp call site naming sym would, in the current package.  It is for a Go
// builtin whose callee exists only in Lisp; to call a language builtin, use
// CallBuiltin, and call Go implementations directly.
//
// CallGlobal charges no evaluation step of its own: the callee costs what its
// body evaluates (a Lisp call form would add a step for the form and one per
// argument expression).  sym is a string, not a symbol LVal, so a caller
// keeps no package-level LVal.  A special operator or macro is refused with
// "not a regular function".
func (env *LEnv) CallGlobal(sym string, args ...*LVal) *LVal {
	fn := env.GetFunGlobal(Symbol(sym))
	if fn.Type == LError {
		return fn
	}
	if fn.IsSpecialFun() {
		return env.Errorf("not a regular function: %v", fn.FunType)
	}
	return env.FunCallContext(env.Context(), fn, SExpr(args))
}
