// Copyright © 2018 The ELPS authors

package lisp

import "sort"

// PackageRegistry contains a set of packages.
type PackageRegistry struct {
	Packages map[string]*Package
	Lang     string // A default package used by all other packages
}

// NewRegistry initializes and returns a new PackageRegistry.
func NewRegistry() *PackageRegistry {
	return &PackageRegistry{
		Packages: make(map[string]*Package),
	}
}

func (r *PackageRegistry) DefinePackage(name string) *Package {
	p, ok := r.Packages[name]
	if ok {
		return p
	}
	p = NewPackage(name)
	r.Packages[name] = p
	return p
}

// Package is a named set of bound symbols.  A package is interpreted code and
// belongs to the LEnv that creates it.
type Package struct {
	Name string
	Doc  string
	// Symbols holds the package's bindings.  Write it through Put or
	// Update rather than assigning to the map directly: those maintain
	// FunNames alongside it, and nothing on the read path repairs a
	// FunNames entry that a direct assignment skipped.  A function bound
	// by direct assignment still works; it just renders without its name
	// in stack traces.
	Symbols    map[string]*LVal
	SymbolDocs map[string]string
	// FunNames maps a function's FID to the name it was most recently
	// bound under in this package.  It is populated exclusively by the
	// write path (see put).  Reads must not write it: a *Package is
	// routinely shared by pointer across goroutines.  See issue #397.
	FunNames  map[string]string
	Externals []string
}

// NewPackage initializes and returns a package with the given name.
func NewPackage(name string) *Package {
	return &Package{
		Name:       name,
		Symbols:    make(map[string]*LVal),
		SymbolDocs: make(map[string]string),
		FunNames:   make(map[string]string),
	}
}

// Get takes an LSymbol k and returns the LVal it is bound to in pkg.
//
// Get is a pure read.  It used to record FunNames[fid] = k.Str on every
// successful function lookup, so that the name the caller used won over the
// name the binding was created with.  That made a read method write a map
// that is shared by pointer across goroutines — embedders hand the same
// *Package to concurrent requests — with no synchronisation.  Under -race it
// is a data race; without -race the Go runtime kills the process outright
// with "fatal error: concurrent map read and map write", which is a runtime
// throw that neither recover() nor handler-bind can intercept.  See issue
// #397.
//
// FunNames is maintained on the write path instead: put records the name for
// every LFun that enters Symbols, so the map is already populated by the time
// anything reads it.  The one behaviour that goes away is the "last lookup
// wins" preference when a single function value is bound under several names
// in the same package: GetFunName now reports the name most recently *bound*
// rather than the name most recently *looked up*.  That is cosmetic — it
// affects the function name rendered in stack traces and error messages only.
func (pkg *Package) Get(k *LVal) *LVal {
	return pkg.get(k)
}

func (pkg *Package) get(k *LVal) *LVal {
	// LQSymbols are allowed...
	if k.Type != LSymbol && k.Type != LQSymbol {
		return Nil()
	}
	if k.Str == TrueSymbol {
		return Symbol(TrueSymbol)
	}
	if k.Str == FalseSymbol {
		return Symbol(FalseSymbol)
	}
	v, ok := pkg.Symbols[k.Str]
	if ok {
		return v
	}
	lerr := Errorf("unbound symbol: %v", k)
	lerr.Source = k.Source
	return lerr
}

// Exports declares symbols exported by the package.  The symbols are not
// required to be bound at the time Exports is called.
func (pkg *Package) Exports(sym ...string) {
	// Copy sym before sorting to avoid mutating the caller's backing
	// array (e.g., a package-level var passed via ...).
	sorted := make([]string, len(sym))
	copy(sorted, sym)
	sort.Strings(sorted)
	externs := pkg.Externals
addloop:
	for _, symnew := range sorted {
		for _, s := range pkg.Externals {
			if s == symnew {
				continue addloop
			}
		}
		externs = append(externs, symnew)
	}
	sort.Strings(externs)
	pkg.Externals = externs
}

// GetFunName returns the function name (if any) known to be bound to the given
// FID.
func (pkg *Package) GetFunName(fid string) string {
	name, ok := pkg.FunNames[fid]
	if ok {
		return name
	}
	return ""
}

// Put takes an LSymbol k and binds it to v in pkg.
func (pkg *Package) Put(k, v *LVal) *LVal {
	if k.Type != LSymbol && k.Type != LQSymbol {
		return Errorf("key is not a symbol: %v", k.Type)
	}
	if k.Str == TrueSymbol || k.Str == FalseSymbol {
		return Errorf("cannot rebind constant: %v", k.Str)
	}
	pkg.put(k, v)
	return Nil()
}

// Update takes an LSymbol k and updates the binding of k in pkg so that k is
// bound v.  If k is not bound in package an error is returned.
func (pkg *Package) Update(k, v *LVal) *LVal {
	if k.Type != LSymbol && k.Type != LQSymbol {
		return Errorf("key is not a symbol: %v", k.Type)
	}
	if k.Str == TrueSymbol || k.Str == FalseSymbol {
		return Errorf("cannot rebind constant: %v", k.Str)
	}
	_, ok := pkg.Symbols[k.Str]
	if !ok {
		return Errorf("symbol not bound: %v (set! only mutates existing bindings; use set to create new ones)", k)
	}
	pkg.put(k, v)
	return Nil()
}

func (pkg *Package) put(k, v *LVal) {
	if v.Type == LFun {
		pkg.FunNames[v.FID()] = k.Str
	}
	pkg.Symbols[k.Str] = v
}
