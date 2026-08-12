// Copyright © 2026 The ELPS authors

// Package hook is the untyped injection slot behind internal/fmtraw.  It
// exists only to break an import cycle: fmtraw's accessors are typed in
// terms of *lisp.LVal, so fmtraw must import lisp — which means lisp cannot
// import fmtraw to inject the accessors.  Both packages instead meet here,
// in a package that imports nothing: lisp's init stores the accessors as
// untyped values, and fmtraw's init recovers the typed functions from them
// (import order guarantees lisp initializes first, since fmtraw imports
// lisp).
//
// Nothing outside lisp (writer) and fmtraw (reader) should touch this
// package.
package hook

// Meta holds a func(*lisp.LVal) *fmtmeta.Meta, stored untyped.  It is set
// by package lisp's init and consumed by package fmtraw's init.
var Meta any

// SetMeta holds a func(*lisp.LVal, *fmtmeta.Meta), stored untyped.  It is
// set by package lisp's init and consumed by package fmtraw's init.
var SetMeta any
