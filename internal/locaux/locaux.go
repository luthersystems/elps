// Copyright © 2026 The ELPS authors

// Package locaux is the untyped injection slot for the tooling record a
// token.Location can carry.
//
// lisp.LVal has room for one source pointer and no more: a second pointer
// moves the struct up a size class for every value the interpreter
// allocates (luthersystems/elps#944).  The formatter's per-node metadata and
// the debugger's macro-expansion record therefore ride on the node's
// Location.  Package lisp owns their types, and package token cannot import
// lisp, so the slot on Location is an opaque value and its two accessors
// meet here, in a package that imports nothing.
//
// Package token's init stores the accessors; package lisp reads them when
// it initializes (lisp imports token, so token initializes first).  Nothing
// else should touch this package: the record is not public API, and
// internal/ visibility keeps it inside this module.
package locaux

// Get holds a func(*token.Location) any that returns the record stored on a
// Location, or nil.
var Get any

// Set holds a func(*token.Location, any) that stores a record on a
// Location.  A nil record clears the slot.
var Set any
