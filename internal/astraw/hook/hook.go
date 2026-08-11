// Copyright © 2026 The ELPS authors

// Package hook is the untyped injection slot behind internal/astraw.  It
// exists only to break an import cycle: astraw's accessor is typed in terms
// of lisp.Program, so astraw must import lisp — which means lisp cannot
// import astraw to inject the accessor.  Both packages instead meet here, in
// a package that imports nothing: lisp's init stores the accessor as an
// untyped value, and astraw's init recovers the typed function from it
// (import order guarantees lisp initializes first, since astraw imports
// lisp).
//
// Nothing outside lisp (writer) and astraw (reader) should touch this
// package.
package hook

// ProgramExprs holds a func(lisp.Program) []*lisp.LVal, stored untyped.  It
// is set by package lisp's init and consumed by package astraw's init.
var ProgramExprs any
