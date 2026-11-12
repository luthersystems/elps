// Copyright © 2024 The ELPS authors

package analysis

import (
	"strings"

	"github.com/luthersystems/elps/lisp"
)

// ScopeKind classifies the kind of scope.
type ScopeKind int

const (
	ScopeGlobal   ScopeKind = iota // file/module level
	ScopeFunction                  // defun/defmacro body
	ScopeLambda                    // lambda body
	ScopeLet                       // let/let* body
	ScopeFlet                      // flet/labels body
	ScopeMacrolet                  // macrolet body
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
	case ScopeMacrolet:
		return "macrolet"
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
	bareNameIndex  map[string]*Symbol // bare name → an arbitrary PackageSymbols entry for O(1) existence checks. When multiple packages define the same bare name, one wins non-deterministically.
	Node           *lisp.LVal         // the AST node that introduced this scope
	// globals is set on a root scope built with Config.GlobalIndex: the
	// package-qualified workspace globals are then found in the index,
	// not in PackageSymbols and bareNameIndex. Every lookup method reads
	// it after the maps, so a definition of the file still hides a
	// global, as it does when the global is in the maps. Code that ranges
	// over the maps sees only the globals the file has looked up.
	globals *scopeGlobals
}

// NewScope creates a new scope of the given kind with the given parent.
func NewScope(kind ScopeKind, parent *Scope, node *lisp.LVal) *Scope {
	s := &Scope{
		Kind:           kind,
		Parent:         parent,
		Symbols:        make(map[string]*Symbol),
		PackageSymbols: make(map[string]*Symbol),
		PackageImports: make(map[string]map[string]*Symbol),
		bareNameIndex:  make(map[string]*Symbol),
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
		s.bareNameIndex[sym.Name] = sym
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
		s.bareNameIndex[sym.Name] = sym
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

// DefineQualifiedOnly adds a package-qualified symbol without adding it to the
// bare-name Symbols map. The symbol is still reachable through Lookup/LookupLocal
// via the bareNameIndex for package-agnostic callers (e.g., minifier, lint arity
// checker).
func (s *Scope) DefineQualifiedOnly(sym *Symbol) {
	sym.Scope = s
	if sym.Package == "" {
		s.Symbols[sym.Name] = sym
		return
	}
	s.PackageSymbols[sym.Package+":"+sym.Name] = sym
	s.bareNameIndex[sym.Name] = sym
}

// Lookup resolves a symbol by walking the parent chain.
// Returns nil if the symbol is not found.
func (s *Scope) Lookup(name string) *Symbol {
	for scope := s; scope != nil; scope = scope.Parent {
		if sym, ok := scope.Symbols[name]; ok {
			return sym
		}
		if sym := scope.lookupAnyPackageSymbol(name); sym != nil {
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
	return s.lookupAnyPackageSymbol(name)
}

// lookupAnyPackageSymbol searches PackageSymbols for any entry matching the
// given bare name, regardless of package. This ensures callers that don't
// know or specify a package can still find symbols defined in non-user packages.
func (s *Scope) lookupAnyPackageSymbol(name string) *Symbol {
	if sym := s.bareNameIndex[name]; sym != nil {
		return sym
	}
	return s.globals.byBare(s, name)
}

// packageSymbol returns the PackageSymbols entry for key ("pkg:name"),
// or the workspace global with that key.
func (s *Scope) packageSymbol(key string) (*Symbol, bool) {
	if sym, ok := s.PackageSymbols[key]; ok {
		return sym, true
	}
	if sym := s.globals.byKey(s, key); sym != nil {
		return sym, true
	}
	return nil, false
}

// LookupAllLocal returns all symbols matching a bare name in this scope,
// across Symbols and all PackageSymbols entries. Used by call hierarchy to
// find all package variants of a function name.
func (s *Scope) LookupAllLocal(name string) []*Symbol {
	var out []*Symbol
	seen := make(map[*Symbol]bool)
	if sym, ok := s.Symbols[name]; ok && sym != nil {
		seen[sym] = true
		out = append(out, sym)
	}
	if s.globals != nil {
		// A global whose key PackageSymbols also holds is hidden by
		// that entry, which the loop below adds.
		for _, i := range s.globals.index.keysByBare[name] {
			g := &s.globals.index.globals[i]
			if _, hidden := s.PackageSymbols[g.Package+":"+g.Name]; hidden {
				continue
			}
			if sym := s.globals.lookup(s, i); !seen[sym] {
				seen[sym] = true
				out = append(out, sym)
			}
		}
	}
	// Check bareNameIndex first: if no package has this bare name, skip the scan.
	if s.bareNameIndex[name] == nil {
		return out
	}
	for key, sym := range s.PackageSymbols {
		if sym == nil || seen[sym] {
			continue
		}
		// Key format is "pkg:name" — check suffix.
		if idx := len(key) - len(name); idx > 0 && key[idx-1] == ':' && key[idx:] == name {
			seen[sym] = true
			out = append(out, sym)
		}
	}
	return out
}

// EachPackageSymbol calls fn for each package-qualified symbol of pkg in
// this scope, in no fixed order, until fn returns false: the entries of
// PackageSymbols whose key starts with pkg+":" and, in a root scope built
// with Config.GlobalIndex, the workspace globals of pkg that no entry
// hides. Use it, not a range over PackageSymbols, to list what a package
// defines: with an index, PackageSymbols holds only the globals the file
// looked up.
func (s *Scope) EachPackageSymbol(pkg string, fn func(key string, sym *Symbol) bool) {
	prefix := pkg + ":"
	for key, sym := range s.PackageSymbols {
		if strings.HasPrefix(key, prefix) && !fn(key, sym) {
			return
		}
	}
	if s.globals == nil {
		return
	}
	for _, i := range s.globals.index.keysByPackage[pkg] {
		key := prefix + s.globals.index.globals[i].Name
		if _, hidden := s.PackageSymbols[key]; hidden {
			continue
		}
		if !fn(key, s.globals.lookup(s, i)) {
			return
		}
	}
}

// LookupInPackage resolves a symbol by preferring a package-qualified match in
// the current scope chain before falling back to a bare-name lookup. The
// bare-name fallback into Symbols is intentional: builtins and user-package
// symbols are registered with Package == "" and must be reachable when no
// qualified entry matches.
//
// Unlike Lookup, LookupInPackage does not consult bareNameIndex — a symbol
// registered only via DefineQualifiedOnly is invisible to LookupInPackage
// unless the correct package is specified.
func (s *Scope) LookupInPackage(name, pkg string) *Symbol {
	// Lexical scopes usually have no package declarations. Only build a
	// qualified key when there is a package table to search. Keep the
	// concatenation in the lookup so short keys use Go's stack buffer.
	for scope := s; scope != nil; scope = scope.Parent {
		if pkg != "" {
			if len(scope.PackageSymbols) > 0 || scope.globals != nil {
				if sym, ok := scope.packageSymbol(pkg + ":" + name); ok {
					return sym
				}
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
//
// The bare-name fallback into Symbols is restricted to the default user package.
// This prevents builtins (Package == "") from blocking package-local definitions
// like (in-package 'foo) (defun set ...). Unlike LookupInPackage, this method
// does not consult bareNameIndex.
func (s *Scope) LookupLocalInPackage(name, pkg string) *Symbol {
	if pkg != "" {
		if sym, ok := s.packageSymbol(pkg + ":" + name); ok {
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
