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
}
