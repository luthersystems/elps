// Copyright © 2024 The ELPS authors

package analysis

import (
	"github.com/luthersystems/elps/astutil"
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

// Store a reference and its private source copy in one allocation. The public
// record retains its original shape, and locations never alias the input or
// another occurrence's record.
func newReference(sym *Symbol, node *lisp.LVal) *Reference {
	loc, ok := node.Source()
	if !ok {
		return &Reference{Symbol: sym, Node: node}
	}
	if node.IsQuoted() || node.Type == lisp.LString {
		loc = *astutil.SymbolLoc(node)
	}
	record := &struct {
		value  Reference
		source token.Location
	}{value: Reference{Symbol: sym, Node: node}, source: loc}
	record.value.Source = &record.source
	return &record.value
}

func newUnresolvedRef(node *lisp.LVal, insideMacroCall bool) *UnresolvedRef {
	loc, ok := node.Source()
	if !ok {
		return &UnresolvedRef{Name: node.Str, Node: node, InsideMacroCall: insideMacroCall}
	}
	if node.IsQuoted() || node.Type == lisp.LString {
		loc = *astutil.SymbolLoc(node)
	}
	record := &struct {
		value  UnresolvedRef
		source token.Location
	}{value: UnresolvedRef{Name: node.Str, Node: node, InsideMacroCall: insideMacroCall}, source: loc}
	record.value.Source = &record.source
	return &record.value
}
