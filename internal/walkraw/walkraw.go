// Copyright © 2026 The ELPS authors

// Package walkraw grants in-repo tooling access to the value-rebuilding
// walkers that package lisp does not export.
//
// (*lisp.LVal).detach is deliberately unexported: it has no production
// consumers yet, and the kernel philosophy is not to widen the API until a
// real embedder consumer materializes (see its doc comment).  That leaves
// the class-level alias guard unable to drive it — which is the exact gap
// issue #598 is about, since detach and the lisp `copy` builtin share one
// walker and the guard that only ever drove Fork stayed green while the
// identical bug sat live in that walker for a week (issues #576, #585).
//
// This package resolves the two: detach stays off the module's public API,
// and in-repo tooling reaches it through internal/, which the Go compiler
// limits to this module.  An embedder importing elps cannot reach it at
// all.
//
// The accessor is injected by package lisp's init through the untyped slot
// in the hook subpackage (see hook's doc comment for the cycle it breaks).
package walkraw

import (
	"github.com/luthersystems/elps/internal/walkraw/hook"
	"github.com/luthersystems/elps/lisp"
)

// Detach returns a hermetic deep copy of v that shares no memory with it,
// or the error naming the path to the first cell that cannot be copied (an
// LFun, or an LNative whose payload supplies no CloneNative).  It is
// (*lisp.LVal).detach, unchanged.  Injected by package lisp's init;
// importing this package imports lisp, so the accessor is always non-nil by
// the time user code runs.
var Detach func(v *lisp.LVal) (*lisp.LVal, error)

func init() {
	fn, ok := hook.Detach.(func(*lisp.LVal) (*lisp.LVal, error))
	if !ok {
		// Unreachable: importing walkraw imports lisp, whose init stores
		// the accessor before this init runs.
		panic("walkraw: package lisp did not inject the Detach accessor")
	}
	Detach = fn
}
