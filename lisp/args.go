// Copyright © 2026 The ELPS authors

package lisp

// ArgReader decodes a Go builtin's argument list into Go values, raising the
// same "<what> is not a <type>: <actual type>" errors builtins write by hand
// (luthersystems/elps#745).  Error text is program output -- a phylum can
// catch and return it, and every endorsing peer must produce the same bytes
// -- so ArgReader never picks a message: the caller passes the subject
// ("first argument", "name") exactly as its hand-written Errorf spelled it,
// or a whole format to Typed.
//
// The first failure is recorded and every later read returns a zero value
// without checking, so a builtin reads all its arguments and then checks Err
// once.  That is equivalent to returning at the first failed check, provided
// the reads come in the order the checks did: decoding has no side effects
// and charges no steps.
//
//	a := lisp.ReadArgs(env, args)
//	source := a.String(0, "first argument")
//	name := a.OptString(1, "name", "")
//	if lerr := a.Err(); lerr != nil {
//		return lerr
//	}
//
// An ArgReader is a small value meant to live in the builtin's frame; it does
// not allocate unless a check fails.
type ArgReader struct {
	env  *LEnv
	args *LVal
	err  *LVal
}

// ReadArgs returns an ArgReader over a builtin's argument list.
func ReadArgs(env *LEnv, args *LVal) ArgReader {
	return ArgReader{env: env, args: args}
}

// Err returns the first failure, or nil.
func (a *ArgReader) Err() *LVal {
	return a.err
}

// Value returns required argument i unchecked.  A missing cell (an embedder
// bound the builtin to too few formals) fails with LVal.ReqArg's error.
func (a *ArgReader) Value(i int) *LVal {
	if a.err != nil {
		return Nil()
	}
	v := a.args.ReqArg(a.env, i)
	if v.Type == LError {
		a.err = v
		return Nil()
	}
	return v
}

// Typed returns required argument i when it has type t.  Otherwise it records
// env.Errorf(format, actualType): format must contain exactly one verb, for
// the argument's type, e.g. "first argument is not a map: %s".
func (a *ArgReader) Typed(i int, t LType, format string) *LVal {
	v := a.Value(i)
	if a.err != nil {
		return v
	}
	if v.Type != t {
		a.err = a.env.Errorf(format, v.Type)
		return Nil()
	}
	return v
}

// named is Typed with the message "<what> is not <noun>: <type>".  what is
// an argument, not a format, so a '%' in it is printed as is.
func (a *ArgReader) named(i int, t LType, what, noun string) *LVal {
	v := a.Value(i)
	if a.err != nil {
		return v
	}
	if v.Type != t {
		a.err = a.env.Errorf("%s is not %s: %v", what, noun, v.Type)
		return Nil()
	}
	return v
}

// optNamed is OptTyped with named's message.
func (a *ArgReader) optNamed(i int, t LType, what, noun string) *LVal {
	v := a.Opt(i)
	if a.err != nil || v.IsNil() {
		return Nil()
	}
	if v.Type != t {
		a.err = a.env.Errorf("%s is not %s: %v", what, noun, v.Type)
		return Nil()
	}
	return v
}

// String returns required argument i, which must be a string; otherwise it
// records "<what> is not a string: <type>".
func (a *ArgReader) String(i int, what string) string {
	return a.named(i, LString, what, "a string").Str
}

// Int returns required argument i, which must be an integer; otherwise it
// records "<what> is not an integer: <type>".
func (a *ArgReader) Int(i int, what string) int {
	return a.named(i, LInt, what, "an integer").Int
}

// Map returns required argument i, which must be a sorted-map; otherwise it
// records "<what> is not a map: <type>".
func (a *ArgReader) Map(i int, what string) *LVal {
	return a.named(i, LSortMap, what, "a map")
}

// Opt returns &optional or &key argument i, or nil when it was not supplied
// (LVal.KeyArg).
func (a *ArgReader) Opt(i int) *LVal {
	if a.err != nil {
		return Nil()
	}
	return a.args.KeyArg(i)
}

// OptTyped returns &optional or &key argument i when it has type t, nil when
// it is nil (not supplied), and otherwise records env.Errorf(format,
// actualType) as Typed does.
func (a *ArgReader) OptTyped(i int, t LType, format string) *LVal {
	v := a.Opt(i)
	if a.err != nil || v.IsNil() {
		return Nil()
	}
	if v.Type != t {
		a.err = a.env.Errorf(format, v.Type)
		return Nil()
	}
	return v
}

// OptString returns &optional or &key argument i as a string, def when it is
// nil, and otherwise records "<what> is not a string: <type>".
func (a *ArgReader) OptString(i int, what string, def string) string {
	v := a.optNamed(i, LString, what, "a string")
	if v.IsNil() {
		return def
	}
	return v.Str
}

// OptInt returns &optional or &key argument i as an integer, def when it is
// nil, and otherwise records "<what> is not an integer: <type>".
func (a *ArgReader) OptInt(i int, what string, def int) int {
	v := a.optNamed(i, LInt, what, "an integer")
	if v.IsNil() {
		return def
	}
	return v.Int
}
