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
	Kind           ScopeKind
	Parent         *Scope
	Children       []*Scope
	Symbols        map[string]*Symbol
	PackageSymbols map[string]*Symbol
	PackageImports map[string]map[string]*Symbol
	Node           *lisp.LVal // the AST node that introduced this scope
}

// NewScope creates a new scope of the given kind with the given parent.
func NewScope(kind ScopeKind, parent *Scope, node *lisp.LVal) *Scope {
	s := &Scope{
		Kind:           kind,
		Parent:         parent,
		Symbols:        make(map[string]*Symbol),
		PackageSymbols: make(map[string]*Symbol),
		PackageImports: make(map[string]map[string]*Symbol),
		Node:           node,
	}
	if parent != nil {
		parent.Children = append(parent.Children, s)
	}
	return s
}

// Define adds a symbol to this scope.
func (s *Scope) Define(sym *Symbol) {
	sym.Scope = s
	if sym.Package != "" {
		s.PackageSymbols[sym.Package+":"+sym.Name] = sym
		return
	}
	s.Symbols[sym.Name] = sym
}

// DefineImported adds a symbol that should be visible both by package-qualified
// name and as an unqualified import in the current scope.
func (s *Scope) DefineImported(sym *Symbol, pkg string) {
	sym.Scope = s
	if sym.Package != "" {
		s.PackageSymbols[sym.Package+":"+sym.Name] = sym
	}
	if pkg == "" {
		s.Symbols[sym.Name] = sym
		return
	}
	if s.PackageImports[pkg] == nil {
		s.PackageImports[pkg] = make(map[string]*Symbol)
	}
	s.PackageImports[pkg][sym.Name] = sym
}

// DefineQualifiedOnly adds a package-qualified symbol without exposing a bare-name alias.
func (s *Scope) DefineQualifiedOnly(sym *Symbol) {
	sym.Scope = s
	if sym.Package == "" {
		s.Symbols[sym.Name] = sym
		return
	}
	s.PackageSymbols[sym.Package+":"+sym.Name] = sym
}

// Lookup resolves a symbol by walking the parent chain.
// Returns nil if the symbol is not found.
func (s *Scope) Lookup(name string) *Symbol {
	for scope := s; scope != nil; scope = scope.Parent {
		if sym, ok := scope.Symbols[name]; ok {
			return sym
		}
		if sym, ok := scope.PackageSymbols[lisp.DefaultUserPackage+":"+name]; ok {
			return sym
		}
	}
	return nil
}

// LookupLocal resolves a symbol only in this scope (not parents).
func (s *Scope) LookupLocal(name string) *Symbol {
	if sym, ok := s.Symbols[name]; ok {
		return sym
	}
	return s.PackageSymbols[lisp.DefaultUserPackage+":"+name]
}

// LookupInPackage resolves a symbol by preferring a package-qualified match in
// the current scope chain before falling back to a bare-name lookup.
func (s *Scope) LookupInPackage(name, pkg string) *Symbol {
	for scope := s; scope != nil; scope = scope.Parent {
		if pkg != "" {
			if sym, ok := scope.PackageSymbols[pkg+":"+name]; ok {
				return sym
			}
			if imports := scope.PackageImports[pkg]; imports != nil {
				if sym, ok := imports[name]; ok {
					return sym
				}
			}
		}
		if sym, ok := scope.Symbols[name]; ok {
			return sym
		}
	}
	return nil
}

// LookupLocalInPackage resolves a symbol in the current scope, preferring the
// package-qualified key when a package is provided.
func (s *Scope) LookupLocalInPackage(name, pkg string) *Symbol {
	if pkg != "" {
		if sym, ok := s.PackageSymbols[pkg+":"+name]; ok {
			return sym
		}
	}
	if pkg == lisp.DefaultUserPackage {
		if sym, ok := s.Symbols[name]; ok {
			return sym
		}
	}
	return nil
}

// LookupLocalVisible resolves a symbol as it would be seen unqualified in the
// current scope: first the active package, then imported/bare symbols.
func (s *Scope) LookupLocalVisible(name, pkg string) *Symbol {
	if sym := s.LookupLocalInPackage(name, pkg); sym != nil {
		return sym
	}
	if pkg != "" {
		if imports := s.PackageImports[pkg]; imports != nil {
			if sym, ok := imports[name]; ok {
				return sym
			}
		}
	}
	return s.Symbols[name]
}
