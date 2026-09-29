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
// once.  Err follows the helpers' convention: Nil() when every read
// succeeded, so `lerr.Type == lisp.LError` is the check.  That is equivalent to returning at the first failed check, provided
// the reads come in the order the checks did: decoding has no side effects
// and charges no steps.
//
//	a := lisp.ReadArgs(env, args)
//	source := a.String(0, "first argument")
//	name := a.OptString(1, "name", "")
//	if lerr := a.Err(); lerr.Type == lisp.LError {
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

// Err returns the first failure, or Nil() when there was none.
func (a *ArgReader) Err() *LVal {
	if a.err == nil {
		return Nil()
	}
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

// Fail records lerr as the reader's failure unless an earlier read already
// failed: the first failure wins.  It is the hook for a custom decoder (an
// ArgDecoder of your own) whose check is not a type test.  A non-error lerr
// is ignored.
func (a *ArgReader) Fail(lerr *LVal) {
	if a.err == nil && lerr != nil && lerr.Type == LError {
		a.err = lerr
	}
}

// Check records env.Errorf(format, args...) when ok is false and no earlier
// read failed.  It reports whether the reader is still free of failures, so a
// custom decoder can stop at the first problem:
//
//	v := a.Value(i)
//	if !a.Check(v.Type == lisp.LString, "argument is not a date: %v", v.Type) {
//		return date{}
//	}
func (a *ArgReader) Check(ok bool, format string, args ...any) bool {
	if a.err != nil {
		return false
	}
	if !ok {
		a.err = a.env.Errorf(format, args...)
		return false
	}
	return true
}

// Stringf returns required argument i, which must be a string; otherwise it
// records env.Errorf(format, actualType).  Use it where the message is not
// "<what> is not a string: <type>".
func (a *ArgReader) Stringf(i int, format string) string {
	return a.Typed(i, LString, format).Str
}

// Intf returns required argument i, which must be an integer; otherwise it
// records env.Errorf(format, actualType).
func (a *ArgReader) Intf(i int, format string) int {
	return a.Typed(i, LInt, format).Int
}

// StringOrSymbol returns required argument i's text when it is a string or a
// symbol; otherwise it records env.Errorf(format, actualType).
func (a *ArgReader) StringOrSymbol(i int, format string) string {
	return a.OneOf(i, format, LString, LSymbol).Str
}

// Bytes returns required argument i as bytes when it is bytes or a string;
// otherwise it records env.Errorf(format, actualType).  For a bytes argument
// it returns the value's own slice, which the builtin must not modify; for a
// string it returns a fresh copy.
func (a *ArgReader) Bytes(i int, format string) []byte {
	v := a.OneOf(i, format, LBytes, LString)
	if v.Type == LBytes {
		return v.Bytes()
	}
	if v.Type == LString {
		return []byte(v.Str)
	}
	return nil
}

// OneOf returns required argument i when its type is one of types; otherwise
// it records env.Errorf(format, actualType).
func (a *ArgReader) OneOf(i int, format string, types ...LType) *LVal {
	v := a.Value(i)
	if a.err != nil {
		return v
	}
	for _, t := range types {
		if v.Type == t {
			return v
		}
	}
	a.err = a.env.Errorf(format, v.Type)
	return Nil()
}

// ReqKey returns &key (or &optional) argument i, recording missing as the
// failure's message, verbatim, when the argument was not supplied (is nil).
func (a *ArgReader) ReqKey(i int, missing string) *LVal {
	v := a.Opt(i)
	if a.err != nil {
		return v
	}
	if v.IsNil() {
		a.err = a.env.Errorf("%s", missing)
		return Nil()
	}
	return v
}
