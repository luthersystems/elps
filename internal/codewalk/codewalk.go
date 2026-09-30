// Copyright © 2026 The ELPS authors

// Package codewalk adapts the interpreter's syntax walker for source analysis.
// The resolver's historical interpretation of malformed forms, qualification,
// template references and custom definitions is internal tooling policy.
package codewalk

import (
	"github.com/luthersystems/elps/internal/codewalk/hook"
	"github.com/luthersystems/elps/lisp"
)

// Binding describes a custom definition-like form. Children before
// FormalsIndex are outer-scope code except NameIndex; later children are its
// body. BindingForm must validate the indices and formals.
type Binding = hook.Binding

// Node is a source event. Owner contains a definition's form; Formals holds
// its signature; Init is a let initializer. Outer selects the enclosing scope
// for a parallel initializer or flet closure. Template marks template symbols.
// The walker reuses this node: copy it to retain an event.
type Node = hook.Node[lisp.LVal, lisp.WalkEvent]

// End balances each Form, including forms whose visitor skips the children.
const End lisp.WalkEvent = lisp.WalkLeave + 1

// Walker preserves source occurrences and has no nesting cap or implicit
// macro expansion. The resolver controls expansion through Visit.
type Walker struct {
	Visit       func(*Node) bool
	BindingForm func(*lisp.LVal) *Binding
	// Reference, when set, receives references and set! targets instead of
	// Visit. Resolution does not need an event's other fields for these
	// frequent leaves, so this avoids constructing and copying an event.
	Reference func(*lisp.LVal)
	// Form, when set, receives forms, operator names and depths instead of
	// Form events. Its result has the same meaning as Visit's result.
	Form func(*lisp.LVal, string, int) bool
	// End, when set, receives completed form depths instead of End events.
	End func(int)
	// EndDepth optionally selects the depth that needs an End callback.
	// The caller updates it as selected forms nest; -1 selects no form.
	EndDepth *int
	// SkipLiterals omits self-evaluating leaves from Visit.
	SkipLiterals bool
	// DeclarationsOnly omits definition signatures, bodies and End events.
	// Prescan uses it for defun, defmacro and deftype declaration metadata.
	DeclarationsOnly bool
	walker           lisp.CodeWalker
}

var walk func(*lisp.CodeWalker, hook.Options[lisp.LVal, lisp.WalkEvent], *lisp.LVal) *lisp.LVal

// Forms walks runtime syntax with only Form events. Lexical tracking,
// expansion, and the visitor's ability to skip children are unchanged.
// Callers that only check calls need not construct events for every leaf.
var Forms func(*lisp.CodeWalker, *lisp.LVal) *lisp.LVal

// Walk visits form without modifying it. A false Visit skips a Form's children.
func (w *Walker) Walk(form *lisp.LVal) *lisp.LVal {
	return walk(&w.walker, hook.Options[lisp.LVal, lisp.WalkEvent]{
		Visit: w.Visit, BindingForm: w.BindingForm, Reference: w.Reference,
		Form: w.Form, End: w.End, EndDepth: w.EndDepth,
		SkipLiterals: w.SkipLiterals, DeclarationsOnly: w.DeclarationsOnly,
	}, form)
}

// PackageForms selects top-level forms and nested package declarations for
// prescan. It omits quoted data and quasiquote, including template holes.
var PackageForms func([]*lisp.LVal) []*lisp.LVal

func init() {
	var ok bool
	walk, ok = hook.Walk.(func(*lisp.CodeWalker, hook.Options[lisp.LVal, lisp.WalkEvent], *lisp.LVal) *lisp.LVal)
	if !ok {
		panic("codewalk: lisp did not inject the source walker")
	}
	Forms, ok = hook.Forms.(func(*lisp.CodeWalker, *lisp.LVal) *lisp.LVal)
	if !ok {
		panic("codewalk: lisp did not inject the form visitor")
	}
	PackageForms, ok = hook.PackageForms.(func([]*lisp.LVal) []*lisp.LVal)
	if !ok {
		panic("codewalk: lisp did not inject the package scanner")
	}
}
