// Copyright © 2026 The ELPS authors

// Package astraw grants in-repo tooling zero-copy access to the expressions
// sealed inside a lisp.Program.
//
// lisp.Program deliberately has no accessor returning its AST nodes — that
// is the whole point of the type (see lisp/program.go).  But analysis, lint,
// and lsp code inside this module reads ASTs without evaluating them, and
// forcing a detach deep copy on every analysis pass would be pure waste.
// This package is the sanctioned bypass: it lives under internal/, so the Go
// compiler limits it to this module — an embedder importing elps cannot
// reach it, and the compile-time seal on Program holds at the module
// boundary where it matters.
//
// The accessor is injected by package lisp's init through the untyped slot
// in the hook subpackage (see hook's doc comment for the cycle it breaks).
// Callers MUST treat the returned slice and every node reachable from it as
// read-only: the nodes ARE the sealed program, not copies.
package astraw

import (
	"github.com/luthersystems/elps/internal/astraw/hook"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/token"
)

// Exprs returns the expressions sealed inside p without copying.  The
// returned slice and its nodes are the program's own — read-only by
// contract.  Injected by package lisp's init; importing this package imports
// lisp, so the accessor is always non-nil by the time user code runs.
var Exprs func(p lisp.Program) []*lisp.LVal

// SourceRef returns the *token.Location a value STORES, by reference, or nil
// when it records no position.
//
// lisp.LVal.Source() deliberately returns a value copy: the stored Location is
// mutable and may be shared, so handing out the pointer is what issue #362
// removed.  What that also removes is any way to ASK whether two values share
// one — and "no two nodes share a Location" is a repository invariant with
// real teeth: it is what makes a copy of a parse tree private (#446), what
// keeps a prefix form's fixups off its operand (#426), and what stops a write
// through an unsealed copy moving a position in the sealed tree every
// environment in the process is evaluating.
//
// This is the sanctioned way for in-repo checks to state that invariant.  Like
// Exprs, it lives under internal/, so no embedder can reach it, and the
// returned pointer is READ-ONLY by contract — writing through it is precisely
// the corruption the invariant exists to exclude.
var SourceRef func(v *lisp.LVal) *token.Location

func init() {
	fn, ok := hook.ProgramExprs.(func(lisp.Program) []*lisp.LVal)
	if !ok {
		// Unreachable: importing astraw imports lisp, whose init stores the
		// accessor before this init runs.
		panic("astraw: package lisp did not inject the Program accessor")
	}
	Exprs = fn

	loc, ok := hook.SourceRef.(func(*lisp.LVal) *token.Location)
	if !ok {
		// Unreachable, for the same reason.
		panic("astraw: package lisp did not inject the SourceRef accessor")
	}
	SourceRef = loc
}
