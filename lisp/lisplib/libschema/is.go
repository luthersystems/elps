// Copyright © 2026 The ELPS authors

package libschema

import "github.com/luthersystems/elps/lisp"

// Type tests (luthersystems/elps#905): s:is? and s:as, and Is and As for
// Go.  A type is what a typed formal takes: a built-in type name, a record
// type (Lisp or Go, the full check), s:ref of one, an interface or another
// validator.  A built-in type name is read from typeNameChecks, so a test
// builds nothing and takes no gensym.
//
// Only wrong-type and failed-constraint mean "v is not a T".  Any other
// condition propagates: a value that is not a type (bad-arguments), a
// step limit, or a condition a record's :check raises itself.  A :check
// that raises failed-constraint makes s:is? false.

// Is reports whether v passes typ.  It returns lisp.Bool(true or false),
// or an error that is not a type failure.
func Is(env *lisp.LEnv, v, typ *lisp.LVal) *lisp.LVal {
	r := typeTest(env, v, typ)
	switch {
	case r == nil:
		return lisp.Bool(true)
	case isTypeFailure(r):
		return lisp.Bool(false)
	}
	return r
}

// As returns v when it passes typ, or the error of the check.
func As(env *lisp.LEnv, v, typ *lisp.LVal) *lisp.LVal {
	if r := typeTest(env, v, typ); r != nil {
		return r
	}
	return v
}

// typeTest checks v against typ and returns nil when v passes.
func typeTest(env *lisp.LEnv, v, typ *lisp.LVal) *lisp.LVal {
	if typ.IsError() {
		return typ
	}
	if isTypeName(typ) {
		return checkTypeName(env, typ.Str, v)
	}
	typ = resolveArgType(env, typ)
	if typ.IsError() {
		return typ
	}
	return checkArgType(env, typ, v)
}

// isTypeFailure reports whether lerr says only that a value is not of a
// type.
func isTypeFailure(lerr *lisp.LVal) bool {
	return !lisp.IsInternalPanic(lerr) && (lerr.Str == WrongType || lerr.Str == FailedConstraint)
}

// builtinIs is s:is?.
func builtinIs(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	if len(args.Cells) < 2 {
		return args.ReqArg(env, len(args.Cells))
	}
	return Is(env, args.Cells[0], args.Cells[1])
}

// builtinAs is s:as.
func builtinAs(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	if len(args.Cells) < 2 {
		return args.ReqArg(env, len(args.Cells))
	}
	return As(env, args.Cells[0], args.Cells[1])
}
