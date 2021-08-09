// Copyright © 2018 The ELPS authors

// Package lisplib is used to conveniently load the standard library for the
// elps environment
package lisplib

import (
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libbase64"
	"github.com/luthersystems/elps/lisp/lisplib/libgolang"
	"github.com/luthersystems/elps/lisp/lisplib/libhelp"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
	"github.com/luthersystems/elps/lisp/lisplib/libmath"
	"github.com/luthersystems/elps/lisp/lisplib/libregexp"
	"github.com/luthersystems/elps/lisp/lisplib/libschema"
	"github.com/luthersystems/elps/lisp/lisplib/libstring"
	"github.com/luthersystems/elps/lisp/lisplib/libtesting"
	"github.com/luthersystems/elps/lisp/lisplib/libtime"
)

// LoadLibrary loads the standard library into env and returns env to the
// default user package.
func LoadLibrary(env *lisp.LEnv) *lisp.LVal {
	e := libtime.LoadPackage(env)
	if !e.IsNil() {
		return e
	}
	e = libhelp.LoadPackage(env)
	if !e.IsNil() {
		return e
	}
	e = libgolang.LoadPackage(env)
	if !e.IsNil() {
		return e
	}
	e = libmath.LoadPackage(env)
	if !e.IsNil() {
		return e
	}
	e = libstring.LoadPackage(env)
	if !e.IsNil() {
		return e
	}
	e = libbase64.LoadPackage(env)
	if !e.IsNil() {
		return e
	}
	e = libjson.LoadPackage(env)
	if !e.IsNil() {
		return e
	}
	e = libregexp.LoadPackage(env)
	if !e.IsNil() {
		return e
	}
	e = libtesting.LoadPackage(env)
	if !e.IsNil() {
		return e
	}
	e = libschema.LoadPackage(env)
	if !e.IsNil() {
		return e
	}
	e = env.InPackage(lisp.Symbol(lisp.DefaultUserPackage))
	if !e.IsNil() {
		return e
	}
	return lisp.Nil()
}
