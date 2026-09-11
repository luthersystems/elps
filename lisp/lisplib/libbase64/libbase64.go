// Copyright © 2018 The ELPS authors

package libbase64

import (
	"encoding/base64"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/internal/libutil"
)

// DefaultPackageName is the package name used by LoadPackage.
const DefaultPackageName = "base64"

// LoadPackage adds the base64 package to env
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
	env.SetPackageDoc("Base64 encoding and decoding using the standard alphabet (RFC 4648).")
	for _, fn := range builtins {
		env.AddBuiltins(true, fn)
	}
	return lisp.Nil()
}

//elpsvet:allow package builtin table; formals are sealed by libutil at construction and shared via registrationFormals (lisp.LEnv.AddBuiltins)
var builtins = []*libutil.Builtin{
	libutil.FunctionDoc("encode", lisp.Formals("data"), builtinEncode,
		`Encodes data using standard base64 encoding and returns the
		result as bytes. The argument may be a string or bytes value.
		The encoded byte length, including padding, must fit the
		allocation limit.`),
	libutil.FunctionDoc("decode", lisp.Formals("base64-data"), builtinDecode,
		`Decodes base64-encoded data and returns the result as bytes.
		The argument may be a string or bytes value containing valid
		standard base64. Returns an error if the input is not valid
		base64. The decoded byte length must fit the allocation limit;
		padding and ignored CR/LF characters do not count toward it.`),
}

func builtinEncode(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	v := args.Cells[0]
	if v.Type != lisp.LString && v.Type != lisp.LBytes {
		return env.Errorf("argument is not a string: %v", v.Type)
	}
	groups := v.Len() / 3
	if v.Len()%3 != 0 {
		groups++
	}
	// Check before multiplying: EncodedLen itself can overflow for an
	// enormous input, even when the host chose a correspondingly large cap.
	if groups > env.Runtime.MaxAllocBytes()/4 {
		return env.Errorf("base64 encoding would exceed maximum allocation size (%d bytes)", env.Runtime.MaxAllocBytes())
	}
	b := make([]byte, groups*4)
	switch v.Type {
	case lisp.LString:
		base64.StdEncoding.Encode(b, []byte(v.Str))
	case lisp.LBytes:
		base64.StdEncoding.Encode(b, v.Bytes())
	}
	return lisp.Bytes(b)
}

func builtinDecode(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	v := args.Cells[0]
	switch v.Type {
	case lisp.LString:
		size := decodedOutputLen(v.Str)
		if msg := env.Runtime.CheckAlloc(size); msg != "" {
			return env.Errorf("%s", msg)
		}
		b := make([]byte, size)
		n, err := base64.StdEncoding.Decode(b, []byte(v.Str))
		if err != nil {
			return env.Error(err)
		}
		return lisp.Bytes(b[:n])
	case lisp.LBytes:
		size := decodedOutputLen(v.Bytes())
		if msg := env.Runtime.CheckAlloc(size); msg != "" {
			return env.Errorf("%s", msg)
		}
		b := make([]byte, size)
		n, err := base64.StdEncoding.Decode(b, v.Bytes())
		if err != nil {
			return env.Error(err)
		}
		return lisp.Bytes(b[:n])
	default:
		return env.Errorf("argument is not a string: %v", v.Type)
	}
}

// decodedOutputLen is exact for valid standard base64, including ignored
// newlines and padding. For malformed input it still bounds what Decode can
// write before returning its error; partial groups are never emitted.
func decodedOutputLen[T string | []byte](data T) int {
	count, padding := 0, 0
	for i := 0; i < len(data); i++ {
		switch data[i] {
		case '\r', '\n':
			continue
		case '=':
			if padding < 2 {
				padding++
			}
		default:
			padding = 0
		}
		count++
	}
	size := (count / 4) * 3
	if count%4 == 0 {
		size -= padding
	}
	return size
}
