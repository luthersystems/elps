// Copyright © 2026 The ELPS authors

// Package libcodec provides the codec package: canonical binary encoding of
// lisp values over lisp.EncodeCanonical and lisp.DecodeCanonical
// (luthersystems/elps#747).
package libcodec

import (
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/internal/libutil"
)

// DefaultPackageName is the package name used by LoadPackage.
const DefaultPackageName = "codec"

// LoadPackage adds the codec package to env.
func LoadPackage(env *lisp.LEnv) *lisp.LVal {
	prevPkg := env.Runtime.Package.Name
	defer env.InPackage(lisp.Symbol(prevPkg))
	name := lisp.Symbol(DefaultPackageName)
	e := env.DefinePackage(name)
	if !e.IsNil() {
		return e
	}
	e = env.InPackage(name)
	if !e.IsNil() {
		return e
	}
	env.SetPackageDoc(`Canonical binary encoding of lisp values.

Values of the same types and structure always encode to the same bytes, on
every machine and in every release (the format is versioned and frozen), so
the bytes can be hashed, used as a key, or stored and decoded later.  The
encoding is type-faithful and so finer than equal?: 1 and 1.0 encode
differently.`)
	for _, fn := range builtins {
		env.AddBuiltins(true, fn)
	}
	return lisp.Nil()
}

//elpsvet:allow package builtin table; formals are sealed by libutil at construction and shared via registrationFormals (lisp.LEnv.AddBuiltins)
var builtins = []*libutil.Builtin{
	libutil.FunctionDoc("encode", lisp.Formals("value"), builtinEncode,
		`Returns the canonical encoding of value as bytes.

Values of the same types and structure always give identical bytes, whatever
order a map was built in and whichever cells are shared.  equal? is coarser:
1 and 1.0, or a string and a symbol map key of one spelling, are equal? but
encode differently.  Accepts ints, floats, strings, bytes, symbols,
keywords, lists, arrays, sorted-maps and tagged values; functions, errors,
native values and values that contain themselves raise an error.  Shared
structure is written out in full at each occurrence.  Costs one step per
started KiB of output, charged as the output grows.`),
	libutil.FunctionDoc("decode", lisp.Formals("bytes"), builtinDecode,
		`Returns the value encoded in bytes, which must be exactly what
encode produced; anything else raises an error.

The value is newly allocated and shares nothing with any other value.
Lists always come back as data lists, as list builds them.  A tagged value
comes back with its type name and data, and is not checked against any
type defined with deftype, nor is the type's constructor run.  Costs one
step per started KiB of input, charged before decoding.`),
}

// options bounds a call by the runtime's per-operation allocation cap as
// well as the codec's default limits, and charges steps as output grows.
func options(env *lisp.LEnv) []lisp.CodecOption {
	limit := env.Runtime.MaxAllocBytes()
	return []lisp.CodecOption{
		lisp.WithCodecMaxBytes(min(lisp.DefaultCodecMaxBytes, limit)),
		lisp.WithCodecMaxValues(min(lisp.DefaultCodecMaxValues, limit)),
	}
}

func builtinEncode(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	b, err := lisp.EncodeCanonical(args.Cells[0], options(env)...)
	if err != nil {
		return env.Errorf("%v", err)
	}
	if lerr := lisp.ChargeStartedKiB(env, len(b)); lerr.Type == lisp.LError {
		return lerr
	}
	return lisp.Bytes(b)
}

func builtinDecode(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	in := args.Cells[0]
	if in.Type != lisp.LBytes {
		return env.Errorf("argument is not bytes: %v", lisp.GetType(in))
	}
	b := in.Bytes()
	if lerr := lisp.ChargeStartedKiB(env, len(b)); lerr.Type == lisp.LError {
		return lerr
	}
	v, err := lisp.DecodeCanonical(b, options(env)...)
	if err != nil {
		return env.Errorf("%v", err)
	}
	return v
}
