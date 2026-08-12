// Copyright © 2026 The ELPS authors

// Package hook is the untyped injection slot behind internal/macroexp.  It
// exists only to break an import cycle: macroexp's attach helper is typed in
// terms of *lisp.LVal, so macroexp must import lisp — which means lisp
// cannot import macroexp to inject the helper.  Both packages instead meet
// here, in a package that imports nothing: lisp's init stores the helper as
// an untyped value, and macroexp's init recovers the typed function from it
// (import order guarantees lisp initializes first, since macroexp imports
// lisp).
//
// Nothing outside lisp (writer) and macroexp (reader) should touch this
// package.
package hook

// Attach holds a func(v *lisp.LVal, name string, callSite, defSite
// *token.Location, args []*lisp.LVal, id int64) stored untyped.  It is set
// by package lisp's init and consumed by package macroexp's init.
var Attach any
