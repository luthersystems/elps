// Copyright © 2026 The ELPS authors

// Package hook is the untyped injection slot behind internal/walkraw.  It
// exists only to break an import cycle: walkraw's accessor is typed in terms
// of lisp types, so walkraw must import lisp — which means lisp cannot
// import walkraw to inject the accessor.  Both packages instead meet here,
// in a package that imports nothing: lisp's init stores the accessor as an
// untyped value, and walkraw's init recovers the typed function from it
// (import order guarantees lisp initializes first, since walkraw imports
// lisp).
//
// Nothing outside lisp (writer) and walkraw (reader) should touch this
// package.
package hook

// Detach holds a func(*lisp.LVal) (*lisp.LVal, error), stored untyped.  It
// is set by package lisp's init and consumed by package walkraw's init.
var Detach any
