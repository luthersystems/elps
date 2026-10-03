// Copyright © 2026 The ELPS authors

// Package funraw grants ELPS libraries explicit builtin construction and
// in-repo tooling read-only access to a function's captured environment.
//
// lisp.LVal's FunData/Env accessors went unexported in issue #382: the
// captured environment was the deepest aliasing channel left in the exported
// API — an *LEnv handed to an embedder reaches the bindings of every closure
// sharing it, and rebinding one (Put, or the scope map itself before the
// same issue unexported it) is invisible to the seal.  Function
// identity stays on the exported surface (FID, Package, Builtin are string/
// func reads), but the environment itself is now reachable only through
// this package: it lives under internal/, so the Go compiler limits it to
// this module — the debugger classifies user-defined functions and the
// profiler resolves display names, and an embedder importing elps cannot
// reach the captured environment at all.
//
// The accessor is injected by package lisp's init through the untyped slot
// in the hook subpackage (see hook's doc comment for the cycle it breaks).
// Callers MUST treat graphs returned by the accessors as read-only. The
// constructor retains its caller-supplied captures by reference within that VM.
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

// Captures returns the explicit graph owned by a NewCapturedBuiltin function,
// or nil when v has no declared captures. The graph and everything reachable
// from it are read-only to tooling; this is not an embedder mutation API.
var Captures func(v *lisp.LVal) *lisp.LVal

// CapturedBuiltin describes a Go function whose VM state is explicit. Eval
// receives the ordinary arguments and the capture graph for the invoking VM.
// All mutable VM state used by Eval must be reachable through Captures; Eval
// may otherwise retain only immutable, concurrency-safe Go data.
//
// Captures is retained by reference so aliases within the VM remain intact.
// Template.NewVM remaps the complete graph, including aliases and cycles.
// Sharing the function directly between VMs does not clone its captures.
type CapturedBuiltin struct {
	Formals  *lisp.LVal
	Captures *lisp.LVal
	Eval     func(env *lisp.LEnv, args, captures *lisp.LVal) *lisp.LVal
	Package  string
	FID      string
}

var newCapturedBuiltin func(string, string, *lisp.LVal, *lisp.LVal, func(*lisp.LEnv, *lisp.LVal, *lisp.LVal) *lisp.LVal) *lisp.LVal

// NewCapturedBuiltin constructs a function with a remappable capture graph.
// Nil Captures declares no VM captures; nil Eval returns a Lisp error.
func NewCapturedBuiltin(spec CapturedBuiltin) *lisp.LVal {
	return newCapturedBuiltin(spec.Package, spec.FID, spec.Formals, spec.Captures, spec.Eval)
}

func init() {
	constructor, ok := hook.NewCapturedBuiltin.(func(string, string, *lisp.LVal, *lisp.LVal, func(*lisp.LEnv, *lisp.LVal, *lisp.LVal) *lisp.LVal) *lisp.LVal)
	if !ok {
		panic("funraw: package lisp did not inject the captured builtin constructor")
	}
	newCapturedBuiltin = constructor
	fn, ok := hook.Env.(func(*lisp.LVal) *lisp.LEnv)
	if !ok {
		// Unreachable: importing funraw imports lisp, whose init stores the
		// accessor before this init runs.
		panic("funraw: package lisp did not inject the Env accessor")
	}
	Env = fn
	captures, ok := hook.Captures.(func(*lisp.LVal) *lisp.LVal)
	if !ok {
		panic("funraw: package lisp did not inject the Captures accessor")
	}
	Captures = captures
}
