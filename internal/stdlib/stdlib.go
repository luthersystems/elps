// Copyright © 2026 The ELPS authors

// Package stdlib assembles ELPS's standard packages for public library loading
// and template-based embedders.
package stdlib

import (
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libbase64"
	"github.com/luthersystems/elps/lisp/lisplib/libelpspath"
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

// Load loads the standard library and selects the default user package.
// LoadRuntimeLibrary and template fixtures omit the mutable testing registry,
// then install libtesting separately in each VM before loading test definitions.
// LoadLibrary includes it. A package error stops loading immediately.
func Load(env *lisp.LEnv, testing bool) *lisp.LVal {
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
	e = libelpspath.LoadPackage(env)
	if !e.IsNil() {
		return e
	}
	if testing {
		e = libtesting.LoadPackage(env)
		if !e.IsNil() {
			return e
		}
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
