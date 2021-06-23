// Copyright © 2018 The ELPS authors

// Package lisplib is used to conveniently load the standard library for the
// elps environment
package lisplib

import (
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libbase64"
	"github.com/luthersystems/elps/lisp/lisplib/libelpslang"
	"github.com/luthersystems/elps/lisp/lisplib/libgolang"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
	"github.com/luthersystems/elps/lisp/lisplib/libmath"
	"github.com/luthersystems/elps/lisp/lisplib/libregexp"
	"github.com/luthersystems/elps/lisp/lisplib/libschema"
	"github.com/luthersystems/elps/lisp/lisplib/libstring"
	"github.com/luthersystems/elps/lisp/lisplib/libtesting"
	"github.com/luthersystems/elps/lisp/lisplib/libtime"
)

type loadfunc = func(*lisp.LEnv) *lisp.LVal

// NOTE:  Please keep the following list sorted.
var loaders = []loadfunc{
	libbase64.LoadPackage,
	libelpslang.LoadPackage,
	libgolang.LoadPackage,
	libjson.LoadPackage,
	libmath.LoadPackage,
	libregexp.LoadPackage,
	libschema.LoadPackage,
	libstring.LoadPackage,
	libtesting.LoadPackage,
	libtime.LoadPackage,
	func(env *lisp.LEnv) *lisp.LVal {
		return env.InPackage(lisp.Symbol(lisp.DefaultUserPackage))
	},
}

// LoadLibrary loads the standard library into env and returns env to the
// default user package.
func LoadLibrary(env *lisp.LEnv) *lisp.LVal {
	for _, fn := range loaders {
		if e := fn(env); !e.IsNil() {
			return e
		}
	}
	return lisp.Nil()
}
