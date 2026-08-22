// Copyright © 2026 The ELPS authors

// Package funraw grants in-repo tooling access to the environment captured
// by a lisp function value.
//
// lisp.LVal's FunData/Env accessors went unexported in issue #382: the
// captured environment was the deepest aliasing channel left in the exported
// API — an *LEnv handed to an embedder reaches the bindings of every closure
// sharing it, and rebinding one (Put, or the scope map itself before the
// same issue unexported it) is invisible to every other guard.  Function
// identity stays on the exported surface (FID, Package, Builtin are string/
// func reads), but the environment itself is now reachable only through
// this package: it lives under internal/, so the Go compiler limits it to
// this module — the debugger classifies user-defined functions and the
// profiler resolves display names, and an embedder importing elps cannot
// reach the captured environment at all.
//
// The accessor is injected by package lisp's init through the untyped slot
// in the hook subpackage (see hook's doc comment for the cycle it breaks).
// Callers MUST treat the returned environment and everything reachable from
// it as read-only.
package funraw

import (
	"github.com/luthersystems/elps/internal/funraw/hook"
	"github.com/luthersystems/elps/lisp"
)

// Env returns the environment captured by the function value v (nil for
// builtins, for LFun values carrying no function data, and for non-function
// values).  The returned environment is the closure's own — read-only by
// contract.  Injected by package lisp's init; importing this package
// imports lisp, so the accessor is always non-nil by the time user code
// runs.
var Env func(v *lisp.LVal) *lisp.LEnv

func init() {
	fn, ok := hook.Env.(func(*lisp.LVal) *lisp.LEnv)
	if !ok {
		// Unreachable: importing funraw imports lisp, whose init stores the
		// accessor before this init runs.
		panic("funraw: package lisp did not inject the Env accessor")
	}
	Env = fn
}
