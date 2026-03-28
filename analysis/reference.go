// Copyright © 2024 The ELPS authors

package analysis

import (
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/token"
)

// Reference records a resolved symbol usage.
type Reference struct {
	Symbol *Symbol
	Source *token.Location
	Node   *lisp.LVal
}

// UnresolvedRef records a symbol usage that could not be resolved.
type UnresolvedRef struct {
	Name   string
	Source *token.Location
	Node   *lisp.LVal
	// InsideMacroCall is true when the unresolved reference appears inside
	// a user-defined macro call body. Macros may introduce bindings at
	// expansion time that are invisible to static analysis.
	InsideMacroCall bool
}
