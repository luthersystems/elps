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

// detach holds a func(*lisp.LVal) (*lisp.LVal, error), stored untyped.  It
// is set by package lisp's init and consumed by package walkraw's init.
//
// The slot is write-once and unexported rather than a plain exported var:
// an in-module package that swapped it would silently blind the detach arm
// of the alias guard, which is the one arm that exists because a bug hid
// there for a week (issue #585).  Nothing legitimate sets it twice.
var detach any

// SetDetach stores the accessor.  It panics on a second call rather than
// letting a later writer take over a slot the guard depends on.
func SetDetach(fn any) {
	if detach != nil {
		panic("walkraw/hook: the Detach accessor is already set")
	}
	detach = fn
}

// Detach returns the stored accessor, or nil before lisp's init has run.
func Detach() any { return detach }
