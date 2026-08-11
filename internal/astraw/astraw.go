// Copyright © 2026 The ELPS authors

// Package astraw grants in-repo tooling zero-copy access to the expressions
// sealed inside a lisp.Program.
//
// lisp.Program deliberately has no accessor returning its AST nodes — that
// is the whole point of the type (see lisp/program.go).  But analysis, lint,
// and lsp code inside this module reads ASTs without evaluating them, and
// forcing a Detach deep copy on every analysis pass would be pure waste.
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
)

// Exprs returns the expressions sealed inside p without copying.  The
// returned slice and its nodes are the program's own — read-only by
// contract.  Injected by package lisp's init; importing this package imports
// lisp, so the accessor is always non-nil by the time user code runs.
var Exprs func(p lisp.Program) []*lisp.LVal

func init() {
	fn, ok := hook.ProgramExprs.(func(lisp.Program) []*lisp.LVal)
	if !ok {
		// Unreachable: importing astraw imports lisp, whose init stores the
		// accessor before this init runs.
		panic("astraw: package lisp did not inject the Program accessor")
	}
	Exprs = fn
}
