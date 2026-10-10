// Copyright © 2026 The ELPS authors

package lisp

// Typed builtins in (value, error) form: Func1E, Func2E and Func3E.
//
// The body is a plain Go function.  Go infers the argument and result types
// from it, and elps picks each argument's decoder from its type, so no
// decoder is passed:
//
//	var builtinEncode = lisp.Func1E(func(env *lisp.LEnv, in lisp.Text) ([]byte, error) {
//		return hex.AppendEncode(nil, in), nil
//	})
//
// An argument of the wrong type raises the standard elps message for its
// position: "argument is not a string: int" for a builtin of one argument,
// else "first argument ...", "second argument ..." or "third argument ...".
// These positional messages are for ports that keep only the error
// condition of the code they replace.  elps's own builtins keep their
// messages and do not use Func*E.
//
// Func*E checks types only.  Allocation limits, steps and context checks
// stay the body's job.  The body takes only required positional arguments:
// Func2E takes exactly two.  For &optional or &key arguments keep Func1,
// Func2 or Func3 with OptArg, OptStringArg or OptIntArg; for &rest keep a
// plain LBuiltin.

// Text is the bytes of an argument that is a string or bytes.  As a Func*E
// argument type it accepts either: a string is copied into a new slice (one
// allocation), and bytes are the argument's own storage, so treat them as
// read-only.  Its message is "<what> is not a string or bytes: <type>".
type Text []byte

// funcResult is the set of result types a Func*E body can return.  elps
// converts the result to an LVal: a string, int, float64 or bool to that
// value, []byte to bytes, []*LVal or Cells to a list (the slice becomes the
// list's storage, so it must be fresh), and *LVal as is, with a nil *LVal
// becoming ().  A native goes out as an *LVal from NativeOf.  Any other
// result type does not compile.
type funcResult interface {
	*LVal | string | int | float64 | bool | []byte | []*LVal | Cells
}

// Func1E returns an LBuiltin of one required argument whose body f returns
// (R, error).  See the comment above Text for the argument decoders, and
// funcResult for the result.  A non-nil error is returned as FuncE returns
// it.
func Func1E[A any, R funcResult](f func(env *LEnv, a A) (R, error)) LBuiltin {
	na := typeNoun[A]()
	return func(env *LEnv, args *LVal) *LVal {
		if lerr := checkArgCount(env, args, 1); lerr != nil {
			return lerr
		}
		r := ReadArgs(env, args)
		a := decodeArg[A](&r, 0, "argument", na)
		if r.err != nil {
			return r.err
		}
		out, err := f(env, a)
		return funcResultLVal(env, out, err)
	}
}

// Func2E returns an LBuiltin of two required arguments whose body f returns
// (R, error).  The arguments are decoded in order, and the first failure is
// returned.
func Func2E[A, B any, R funcResult](f func(env *LEnv, a A, b B) (R, error)) LBuiltin {
	na, nb := typeNoun[A](), typeNoun[B]()
	return func(env *LEnv, args *LVal) *LVal {
		if lerr := checkArgCount(env, args, 2); lerr != nil {
			return lerr
		}
		r := ReadArgs(env, args)
		a := decodeArg[A](&r, 0, "first argument", na)
		b := decodeArg[B](&r, 1, "second argument", nb)
		if r.err != nil {
			return r.err
		}
		out, err := f(env, a, b)
		return funcResultLVal(env, out, err)
	}
}

// Func3E returns an LBuiltin of three required arguments whose body f
// returns (R, error).  The arguments are decoded in order, and the first
// failure is returned.
func Func3E[A, B, C any, R funcResult](f func(env *LEnv, a A, b B, c C) (R, error)) LBuiltin {
	na, nb, nc := typeNoun[A](), typeNoun[B](), typeNoun[C]()
	return func(env *LEnv, args *LVal) *LVal {
		if lerr := checkArgCount(env, args, 3); lerr != nil {
			return lerr
		}
		r := ReadArgs(env, args)
		a := decodeArg[A](&r, 0, "first argument", na)
		b := decodeArg[B](&r, 1, "second argument", nb)
		c := decodeArg[C](&r, 2, "third argument", nc)
		if r.err != nil {
			return r.err
		}
		out, err := f(env, a, b, c)
		return funcResultLVal(env, out, err)
	}
}

// checkArgCount refuses an argument list that does not have exactly n cells,
// which happens when a Func*E builtin is registered with other formals.  The
// message is the evaluator's arity message.
func checkArgCount(env *LEnv, args *LVal, n int) *LVal {
	if len(args.Cells) != n {
		return env.ErrorConditionf(CondArgumentError, "invalid number of arguments: %d", len(args.Cells))
	}
	return nil
}

// decodeArg reads argument i as a T and records "<what> is not <noun>:
// <type>" in r on a mismatch.  noun is typeNoun[T](), computed once per
// builtin.
func decodeArg[T any](r *ArgReader, i int, what, noun string) T {
	var out T
	v := r.Value(i)
	if r.err != nil {
		return out
	}
	x, ok := valueAs[T](v)
	if !ok {
		r.err = r.env.ErrorConditionf(CondArgumentError, "%s is not %s: %v", what, noun, v.Type)
		return out
	}
	return x
}

// funcResultLVal converts the result of a Func*E body to the value its
// builtin returns.
func funcResultLVal[R funcResult](env *LEnv, out R, err error) *LVal {
	if err != nil {
		return builtinResult(env, nil, err)
	}
	var v *LVal
	switch p := any(&out).(type) {
	case **LVal:
		v = *p
	case *string:
		v = String(*p)
	case *int:
		v = Int(*p)
	case *float64:
		v = Float(*p)
	case *bool:
		v = Bool(*p)
	case *[]byte:
		v = Bytes(*p)
	case *[]*LVal:
		v = QExpr(*p)
	case *Cells:
		v = QExpr(*p)
	}
	return builtinResult(env, v, nil)
}

// textNoun is the message noun of a Text argument.
const textNoun = "a string or bytes"
