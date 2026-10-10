// Copyright © 2026 The ELPS authors

package lisp

import "errors"

// Result splits v into the (value, error) pair of Go style code.  When v is
// an LError, Result returns a nil value and v as a *ErrorVal.  Otherwise it
// returns v and a nil error.  The error is v itself, so returning it from a
// FuncE body raises the same condition, data and stack.
//
//	fields, err := lisp.Result(env.CallBuiltin(sortedMap, k, v))
//	if err != nil {
//		return nil, err
//	}
//
// Result makes no check and allocates nothing.  It is the bridge from a
// helper that returns *LVal to code that returns error; GoError is the bridge
// for a helper that only reports success or failure.
func Result(v *LVal) (*LVal, error) {
	if v.Type == LError {
		return nil, (*ErrorVal)(v)
	}
	return v, nil
}

// ConditionOf returns the condition Lisp code sees when err is raised as an
// error, as FuncE and LEnv.Error raise it.  It returns:
//
//   - "" when err is nil;
//   - the condition of err when err is a *ErrorVal itself;
//   - the condition of a wrapped internal panic, which keeps its identity
//     through a wrap (see ErrorCondition);
//   - "error" for every other error.  A *ErrorVal wrapped with fmt.Errorf
//     and %w becomes a new error with condition "error", so handler-bind
//     no longer sees the inner condition.  ConditionOf agrees with that.
//
// ConditionOf does not look through wraps the way errors.Is does, because
// handler-bind does not.
func ConditionOf(err error) string {
	if err == nil {
		return ""
	}
	if e, ok := err.(*ErrorVal); ok { //nolint:errorlint // a wrapped *ErrorVal gets a new condition, as ErrorCondition decides
		if e == nil {
			return "error"
		}
		return e.Str
	}
	var wrapped *ErrorVal
	if errors.As(err, &wrapped) && wrapped != nil && IsInternalPanic((*LVal)(wrapped)) {
		return wrapped.Str
	}
	return "error"
}

// FuncE returns an LBuiltin whose body f is written in (value, error) form.
//
//	var builtinLoad = lisp.FuncE(func(env *lisp.LEnv, args *lisp.LVal) (*lisp.LVal, error) {
//		v, err := lisp.Result(env.CallBuiltin(coreKeys, args.Cells[0]))
//		if err != nil {
//			return nil, err
//		}
//		return v, nil
//	})
//
// The builtin converts f's result to the *LVal a builtin returns:
//
//   - A non-nil error that is a *ErrorVal is returned as itself.  It keeps
//     its condition, data and stack, and the debugger is not told about it
//     a second time.
//   - Any other non-nil error becomes env.Error(err), with condition
//     "error" (see ConditionOf).  The value is ignored.
//   - A nil value with a nil error becomes Nil().
//
// elpsvet's elpsownpkg and elpsbuiltinstate rules check f like any other
// builtin body.  FuncE adds one call and no allocation to the body's cost.
func FuncE(f func(env *LEnv, args *LVal) (*LVal, error)) LBuiltin {
	return func(env *LEnv, args *LVal) *LVal {
		v, err := f(env, args)
		return builtinResult(env, v, err)
	}
}

// builtinResult converts the (value, error) result of a FuncE or Func*E body
// to the value its builtin returns.
func builtinResult(env *LEnv, v *LVal, err error) *LVal {
	if err != nil {
		if e, ok := err.(*ErrorVal); ok && e != nil { //nolint:errorlint // only a bare *ErrorVal keeps its identity, as in ErrorCondition
			return (*LVal)(e)
		}
		return env.Error(err)
	}
	if v == nil {
		return Nil()
	}
	return v
}
