// Copyright © 2026 The ELPS authors

package lisp

// Typed builtins with generics (luthersystems/elps#745, item 9).
//
// A builtin written against Go types instead of an argument list:
//
//	var builtinRepeat = lisp.Func2(
//		lisp.StringArg("first argument"),
//		lisp.TypedArg(lisp.LInt, "second argument is not an int: %v"),
//		func(env *lisp.LEnv, s string, n *lisp.LVal) *lisp.LVal { ... })
//
// The argument order, the type check each argument gets and its error text
// are all fixed at compile time by the decoder list, and every decoder is an
// ArgReader read, so the messages are the ones ArgReader produces from the
// same subject or format: byte-identical to the hand-written checks they
// replace.  Decoders run in argument order and the first failure is
// returned, which is the order a hand-written builtin checks in.  There is no
// reflection: Func1 and Func2 are ordinary generic functions, a decoder is a plain Go
// function, and a call allocates nothing beyond what the body allocates.
// Nothing here charges a step; the body charges what it charges today.
//
// Func1 and Func2 cover one and two positional arguments.  A builtin with more, or one
// that needs &rest, keeps the LBuiltin signature and uses ArgReader
// directly; that is where generics stop, because Go has no variadic type
// parameters.  An &optional or &key argument is simply a position with an
// decoder that calls ArgReader.Opt: the evaluator hands a builtin one cell per
// formal, in formal order, so the decoder for position i reads the i'th formal.

// ArgDecoder decodes argument i of a builtin's argument list as a T,
// recording a failure in the ArgReader.  Construct decoders once, at package
// initialization, with the functions below; they capture only their message
// text and default.  A decoder of your own is any function of this type; use
// ArgReader.Check to record its failure.
type ArgDecoder[T any] func(a *ArgReader, i int) T

// ValueArg decodes a required argument unchecked.
func ValueArg() ArgDecoder[*LVal] {
	return func(a *ArgReader, i int) *LVal { return a.Value(i) }
}

// TypedArg decodes a required argument of type t; otherwise the failure is
// env.Errorf(format, actualType), as ArgReader.Typed.
func TypedArg(t LType, format string) ArgDecoder[*LVal] {
	return func(a *ArgReader, i int) *LVal { return a.Typed(i, t, format) }
}

// StringArg decodes a required string: "<what> is not a string: <type>".
func StringArg(what string) ArgDecoder[string] {
	return func(a *ArgReader, i int) string { return a.String(i, what) }
}

// Func1 returns an LBuiltin that decodes one argument and calls f.
func Func1[A any](da ArgDecoder[A], f func(env *LEnv, a A) *LVal) LBuiltin {
	return func(env *LEnv, args *LVal) *LVal {
		r := ReadArgs(env, args)
		a := da(&r, 0)
		if r.err != nil {
			return r.err
		}
		return f(env, a)
	}
}

// Func2 returns an LBuiltin that decodes two arguments, in order, and calls f.
func Func2[A, B any](da ArgDecoder[A], db ArgDecoder[B], f func(env *LEnv, a A, b B) *LVal) LBuiltin {
	return func(env *LEnv, args *LVal) *LVal {
		r := ReadArgs(env, args)
		a := da(&r, 0)
		b := db(&r, 1)
		if r.err != nil {
			return r.err
		}
		return f(env, a, b)
	}
}
