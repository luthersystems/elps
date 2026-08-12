// Copyright © 2026 The ELPS authors

// Package hook is the untyped injection slot behind internal/funraw.  It
// exists only to break an import cycle: funraw's accessor is typed in terms
// of lisp types, so funraw must import lisp — which means lisp cannot import
// funraw to inject the accessor.  Both packages instead meet here, in a
// package that imports nothing: lisp's init stores the accessor as an
// untyped value, and funraw's init recovers the typed function from it
// (import order guarantees lisp initializes first, since funraw imports
// lisp).
//
// Nothing outside lisp (writer) and funraw (reader) should touch this
// package.
package hook

// Env holds a func(*lisp.LVal) *lisp.LEnv, stored untyped.  It is set by
// package lisp's init and consumed by package funraw's init.
var Env any
