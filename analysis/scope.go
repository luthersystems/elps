// Copyright © 2024 The ELPS authors

package analysis

import "github.com/luthersystems/elps/lisp"

// ScopeKind classifies the kind of scope.
type ScopeKind int

const (
	ScopeGlobal   ScopeKind = iota // file/module level
	ScopeFunction                  // defun/defmacro body
	ScopeLambda                    // lambda body
	ScopeLet                       // let/let* body
	ScopeFlet                      // flet/labels body
	ScopeDotimes                   // dotimes body
)

func (k ScopeKind) String() string {
	switch k {
	case ScopeGlobal:
		return "global"
	case ScopeFunction:
		return "function"
	case ScopeLambda:
		return "lambda"
	case ScopeLet:
		return "let"
	case ScopeFlet:
		return "flet"
	case ScopeDotimes:
		return "dotimes"
	default:
		return "unknown"
	}
}

// Scope represents a lexical scope in the source.
type Scope struct {
	Kind     ScopeKind
	Parent   *Scope
	Children []*Scope
	Symbols  map[string]*Symbol
	Node     *lisp.LVal // the AST node that introduced this scope
}

// NewScope creates a new scope of the given kind with the given parent.
func NewScope(kind ScopeKind, parent *Scope, node *lisp.LVal) *Scope {
	s := &Scope{
		Kind:    kind,
		Parent:  parent,
		Symbols: make(map[string]*Symbol),
		Node:    node,
	}
	if parent != nil {
		parent.Children = append(parent.Children, s)
	}
	return s
}

// Define adds a symbol to this scope.
func (s *Scope) Define(sym *Symbol) {
	sym.Scope = s
	s.Symbols[sym.Name] = sym
}

// Lookup resolves a symbol by walking the parent chain.
// Returns nil if the symbol is not found.
func (s *Scope) Lookup(name string) *Symbol {
	for scope := s; scope != nil; scope = scope.Parent {
		if sym, ok := scope.Symbols[name]; ok {
			return sym
		}
	}
	return nil
}

// LookupLocal resolves a symbol only in this scope (not parents).
func (s *Scope) LookupLocal(name string) *Symbol {
	return s.Symbols[name]
}
