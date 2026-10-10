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
// reflection: Func1 and Func2 are ordinary generic functions.
//
// A decoder from ValueArg, TypedArg, StringArg, OptArg, OptStringArg or
// OptIntArg is data, not a function.  Func1 and Func2 switch on it and call
// the ArgReader method directly, so the ArgReader stays in the builtin's
// frame and a call allocates nothing beyond what the body allocates.  That
// is the same count as the equivalent hand-written LBuiltin.  A decoder from
// CustomArg is an indirect call: it costs one heap allocation per call, for
// a copy of the ArgReader.  Nothing here charges a step; the body charges
// what it charges today.
//
// Func1, Func2 and Func3 cover one, two and three positional arguments.  A
// builtin with more, or one that needs &rest, keeps the LBuiltin signature
// and uses ArgReader directly; that is where generics stop, because Go has no
// variadic type parameters.  An &optional or &key argument is a position
// with an OptArg, OptStringArg or OptIntArg decoder: the evaluator hands a
// builtin one cell per formal, in formal order, so the decoder for position
// i reads the i'th formal.

// argKind selects the ArgReader read an ArgDecoder performs.
type argKind uint8

const (
	argCustom argKind = iota
	argValue
	argTyped
	argString
	argOpt
	argOptString
	argOptInt
)

// ArgDecoder decodes argument i of a builtin's argument list as a T,
// recording a failure in the ArgReader.  Construct decoders once, at package
// initialization, with the functions below; they hold only their message
// text and default.  For a decoder of your own, pass a function to
// CustomArg and use ArgReader.Check to record its failure.
type ArgDecoder[T any] struct {
	custom func(a *ArgReader, i int) T
	// text is the subject (StringArg, OptStringArg, OptIntArg) or the whole
	// format (TypedArg) of the failure message.
	text   string
	defStr string
	defInt int
	t      LType
	kind   argKind
}

// ValueArg decodes a required argument unchecked.
func ValueArg() ArgDecoder[*LVal] {
	return ArgDecoder[*LVal]{kind: argValue}
}

// TypedArg decodes a required argument of type t; otherwise the failure is
// env.Errorf(format, actualType), as ArgReader.Typed.
func TypedArg(t LType, format string) ArgDecoder[*LVal] {
	return ArgDecoder[*LVal]{kind: argTyped, t: t, text: format}
}

// StringArg decodes a required string: "<what> is not a string: <type>".
func StringArg(what string) ArgDecoder[string] {
	return ArgDecoder[string]{kind: argString, text: what}
}

// OptArg decodes an &optional or &key argument unchecked, nil when it was
// not supplied, as ArgReader.Opt.
func OptArg() ArgDecoder[*LVal] {
	return ArgDecoder[*LVal]{kind: argOpt}
}

// OptStringArg decodes an &optional or &key string, def when it was not
// supplied: "<what> is not a string: <type>", as ArgReader.OptString.
func OptStringArg(what string, def string) ArgDecoder[string] {
	return ArgDecoder[string]{kind: argOptString, text: what, defStr: def}
}

// OptIntArg decodes an &optional or &key integer, def when it was not
// supplied: "<what> is not an integer: <type>", as ArgReader.OptInt.
func OptIntArg(what string, def int) ArgDecoder[int] {
	return ArgDecoder[int]{kind: argOptInt, text: what, defInt: def}
}

// CustomArg returns a decoder that calls f.  The call is indirect, so a
// builtin with a custom decoder puts its ArgReader on the heap: one
// allocation per call, shared by all of the call's decoders.  Prefer the
// constructors above when one of them fits.
func CustomArg[T any](f func(a *ArgReader, i int) T) ArgDecoder[T] {
	return ArgDecoder[T]{kind: argCustom, custom: f}
}

// decode reads argument i with d.  Each built-in kind calls an ArgReader
// method directly, which does not leak r, so r stays in the caller's frame.
// The constructors fix T for each kind, so a kind never meets the wrong T.
// argCustom never reaches here: decodeShared handles it, so this switch has
// no indirect call and r does not escape.
func (d *ArgDecoder[T]) decode(r *ArgReader, i int) T {
	var out T
	switch p := any(&out).(type) {
	case **LVal:
		switch d.kind {
		case argValue:
			*p = r.Value(i)
		case argTyped:
			*p = r.Typed(i, d.t, d.text)
		case argOpt:
			*p = r.Opt(i)
		default:
			panic(errArgDecoderKind)
		}
	case *string:
		switch d.kind {
		case argString:
			*p = r.String(i, d.text)
		case argOptString:
			*p = r.OptString(i, d.text, d.defStr)
		default:
			panic(errArgDecoderKind)
		}
	case *int:
		if d.kind != argOptInt {
			panic(errArgDecoderKind)
		}
		*p = r.OptInt(i, d.text, d.defInt)
	default:
		panic(errArgDecoderKind)
	}
	return out
}

// errArgDecoderKind is decode's panic when a decoder's kind does not match
// its type parameter, or a custom decoder reaches the direct decoder.
const errArgDecoderKind = "lisp: ArgDecoder kind does not match the direct decoder"

// decodeShared is decode for the shared-reader variant of Func1/Func2,
// used when any decoder of the builtin is custom.  r is the call's one heap
// reader, and every decoder, custom or not, reads and records failures
// through it.
func (d *ArgDecoder[T]) decodeShared(r *ArgReader, i int) T {
	if d.kind == argCustom {
		return d.custom(r, i)
	}
	return d.decode(r, i)
}

// Func1 returns an LBuiltin that decodes one argument and calls f.
func Func1[A any](da ArgDecoder[A], f func(env *LEnv, a A) *LVal) LBuiltin {
	if da.kind == argCustom {
		return func(env *LEnv, args *LVal) *LVal {
			r := new(ArgReader)
			*r = ReadArgs(env, args)
			a := da.decodeShared(r, 0)
			if r.err != nil {
				return r.err
			}
			return f(env, a)
		}
	}
	return func(env *LEnv, args *LVal) *LVal {
		r := ReadArgs(env, args)
		a := da.decode(&r, 0)
		if r.err != nil {
			return r.err
		}
		return f(env, a)
	}
}

// Func2 returns an LBuiltin that decodes two arguments, in order, and calls f.
func Func2[A, B any](da ArgDecoder[A], db ArgDecoder[B], f func(env *LEnv, a A, b B) *LVal) LBuiltin {
	if da.kind == argCustom || db.kind == argCustom {
		return func(env *LEnv, args *LVal) *LVal {
			r := new(ArgReader)
			*r = ReadArgs(env, args)
			a := da.decodeShared(r, 0)
			b := db.decodeShared(r, 1)
			if r.err != nil {
				return r.err
			}
			return f(env, a, b)
		}
	}
	return func(env *LEnv, args *LVal) *LVal {
		r := ReadArgs(env, args)
		a := da.decode(&r, 0)
		b := db.decode(&r, 1)
		if r.err != nil {
			return r.err
		}
		return f(env, a, b)
	}
}

// Func3 returns an LBuiltin that decodes three arguments, in order, and calls
// f.
func Func3[A, B, C any](da ArgDecoder[A], db ArgDecoder[B], dc ArgDecoder[C], f func(env *LEnv, a A, b B, c C) *LVal) LBuiltin {
	if da.kind == argCustom || db.kind == argCustom || dc.kind == argCustom {
		return func(env *LEnv, args *LVal) *LVal {
			r := new(ArgReader)
			*r = ReadArgs(env, args)
			a := da.decodeShared(r, 0)
			b := db.decodeShared(r, 1)
			c := dc.decodeShared(r, 2)
			if r.err != nil {
				return r.err
			}
			return f(env, a, b, c)
		}
	}
	return func(env *LEnv, args *LVal) *LVal {
		r := ReadArgs(env, args)
		a := da.decode(&r, 0)
		b := db.decode(&r, 1)
		c := dc.decode(&r, 2)
		if r.err != nil {
			return r.err
		}
		return f(env, a, b, c)
	}
}
