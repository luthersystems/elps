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
// reflection: FuncN is an ordinary generic function, a decoder is a plain Go
// function, and a call allocates nothing beyond what the body allocates.
// Nothing here charges a step; the body charges what it charges today.
//
// FuncN covers up to four positional arguments.  A builtin with more, or one
// that needs &rest, keeps the LBuiltin signature and uses ArgReader
// directly; that is where generics stop, because Go has no variadic type
// parameters.  An &optional or &key argument is simply a position with an
// Opt decoder: the evaluator hands a builtin one cell per formal, in formal
// order, so the decoder for position i reads the i'th formal.

// ArgDecoder decodes argument i of a builtin's argument list as a T,
// recording a failure in the ArgReader.  Construct decoders once, at package
// initialization, with the functions below; they capture only their message
// text and default.
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

// IntArg decodes a required integer: "<what> is not an integer: <type>".
func IntArg(what string) ArgDecoder[int] {
	return func(a *ArgReader, i int) int { return a.Int(i, what) }
}

// MapArg decodes a required sorted-map: "<what> is not a map: <type>".
func MapArg(what string) ArgDecoder[*LVal] {
	return func(a *ArgReader, i int) *LVal { return a.Map(i, what) }
}

// OptArg decodes an &optional or &key argument unchecked (nil when absent).
func OptArg() ArgDecoder[*LVal] {
	return func(a *ArgReader, i int) *LVal { return a.Opt(i) }
}

// OptStringArg decodes an &optional or &key string, def when nil.
func OptStringArg(what, def string) ArgDecoder[string] {
	return func(a *ArgReader, i int) string { return a.OptString(i, what, def) }
}

// OptIntArg decodes an &optional or &key integer, def when nil.
func OptIntArg(what string, def int) ArgDecoder[int] {
	return func(a *ArgReader, i int) int { return a.OptInt(i, what, def) }
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

// Func3 returns an LBuiltin that decodes three arguments, in order, and calls
// f.
func Func3[A, B, C any](da ArgDecoder[A], db ArgDecoder[B], dc ArgDecoder[C], f func(env *LEnv, a A, b B, c C) *LVal) LBuiltin {
	return func(env *LEnv, args *LVal) *LVal {
		r := ReadArgs(env, args)
		a := da(&r, 0)
		b := db(&r, 1)
		c := dc(&r, 2)
		if r.err != nil {
			return r.err
		}
		return f(env, a, b, c)
	}
}

// Func4 returns an LBuiltin that decodes four arguments, in order, and calls
// f.
func Func4[A, B, C, D any](da ArgDecoder[A], db ArgDecoder[B], dc ArgDecoder[C], dd ArgDecoder[D], f func(env *LEnv, a A, b B, c C, d D) *LVal) LBuiltin {
	return func(env *LEnv, args *LVal) *LVal {
		r := ReadArgs(env, args)
		a := da(&r, 0)
		b := db(&r, 1)
		c := dc(&r, 2)
		d := dd(&r, 3)
		if r.err != nil {
			return r.err
		}
		return f(env, a, b, c, d)
	}
}
