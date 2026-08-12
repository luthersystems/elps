// Copyright © 2026 The ELPS authors

// Package macroexp grants in-repo tests a construction path for the
// macro-expansion debug metadata sealed inside package lisp.
//
// lisp.LVal deliberately has no exported way to attach macro-expansion
// metadata — the #370 corruption was exactly such a write landing on shared
// parser nodes, so the in-kernel stamp (stampMacroExpansion) is the only
// production writer and external readers get the read-only
// (*LVal).MacroExpansion snapshot (issue #382).  But the debugger's unit
// tests inside this module fabricate paused expressions carrying expansion
// metadata to exercise inspection and DAP translation without driving a
// whole evaluator to a breakpoint.  This package is the sanctioned bypass:
// it lives under internal/, so the Go compiler limits it to this module —
// an embedder importing elps cannot reach it, and the compile-time closure
// of the write channel holds at the module boundary where it matters.
//
// The helper is injected by package lisp's init through the untyped slot in
// the hook subpackage (see hook's doc comment for the cycle it breaks).
// Nothing outside _test.go files should call Attach.
package macroexp

import (
	"github.com/luthersystems/elps/internal/macroexp/hook"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/token"
)

// Attach fabricates macro-expansion debug metadata on v, as if v had been
// stamped during expansion of macro name invoked at callSite with the given
// unevaluated args.  Test-only; see the package comment.  Injected by
// package lisp's init; importing this package imports lisp, so the helper
// is always non-nil by the time user code runs.
var Attach func(v *lisp.LVal, name string, callSite, defSite *token.Location, args []*lisp.LVal, id int64)

func init() {
	fn, ok := hook.Attach.(func(*lisp.LVal, string, *token.Location, *token.Location, []*lisp.LVal, int64))
	if !ok {
		// Unreachable: importing macroexp imports lisp, whose init stores
		// the helper before this init runs.
		panic("macroexp: package lisp did not inject the attach helper")
	}
	Attach = fn
}
