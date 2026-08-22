// Copyright © 2018 The ELPS authors

package lisp

import "sort"

// PackageRegistry contains a set of packages.
type PackageRegistry struct {
	packages map[string]*Package
	Lang     string // A default package used by all other packages
}

// NewRegistry initializes and returns a new PackageRegistry.
func NewRegistry() *PackageRegistry {
	return &PackageRegistry{
		packages: make(map[string]*Package),
	}
}

func (r *PackageRegistry) DefinePackage(name string) *Package {
	p, ok := r.packages[name]
	if ok {
		return p
	}
	p = NewPackage(name)
	r.packages[name] = p
	return p
}

// Package returns the package registered under name, or nil if no such
// package exists.
func (r *PackageRegistry) Package(name string) *Package {
	return r.packages[name]
}

// PackageNames returns the names of all registered packages in sorted order.
// PackageNames allocates a new slice on every call.
func (r *PackageRegistry) PackageNames() []string {
	names := make([]string, 0, len(r.packages))
	for name := range r.packages {
		names = append(names, name)
	}
	sort.Strings(names)
	return names
}

// AddPackage registers p under p.Name if no package with that name exists
// already.  AddPackage returns true when p was added and false when a
// package named p.Name was already registered (in which case the registry
// is unchanged).
func (r *PackageRegistry) AddPackage(p *Package) bool {
	if p == nil {
		return false
	}
	if _, ok := r.packages[p.Name]; ok {
		return false
	}
	r.packages[p.Name] = p
	return true
}

// Package is a named set of bound symbols.  A package is interpreted code and
// belongs to the LEnv that creates it.
type Package struct {
	Name string
	Doc  string
	// symbols holds the package's bindings.  Unexported (issue #382): the
	// registry's LVal-bearing surface is the widest write channel into
	// another environment's interpreter state, so external packages read it
	// through Symbol/SymbolNames and write it through Put or Update.  Those
	// maintain funNames alongside it, and nothing on the read path repairs a
	// funNames entry a direct assignment would have skipped.
	symbols    map[string]*LVal
	symbolDocs map[string]string
	// funNames maps a function's FID to the name it was most recently bound
	// under in this package.  It is populated exclusively by the write path
	// (see put).  Reads must not write it: a *Package is routinely shared by
	// pointer across goroutines.  See issue #397.
	funNames  map[string]string
	externals []string
}

// NewPackage initializes and returns a package with the given name.
func NewPackage(name string) *Package {
	return &Package{
		Name:       name,
		symbols:    make(map[string]*LVal),
		symbolDocs: make(map[string]string),
		funNames:   make(map[string]string),
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
	v, ok := pkg.symbols[k.Str]
	if ok {
		return v
	}
	lerr := Errorf("unbound symbol: %v", k)
	lerr.source = k.source
	return lerr
}

// Symbol returns the value bound to name in pkg and reports whether name is
// bound.  Unlike Get, Symbol performs a raw table lookup: it does not resolve
// the true/false constants, does not record function names, and returns
// (nil, false) instead of an error LVal when name is unbound.
func (pkg *Package) Symbol(name string) (*LVal, bool) {
	v, ok := pkg.symbols[name]
	return v, ok
}

// SymbolNames returns the names of all symbols bound in pkg in sorted order.
// SymbolNames allocates a new slice on every call.
func (pkg *Package) SymbolNames() []string {
	names := make([]string, 0, len(pkg.symbols))
	for name := range pkg.symbols {
		names = append(names, name)
	}
	sort.Strings(names)
	return names
}

// SymbolDoc returns the documentation string bound to name in pkg, or the
// empty string when name has no documentation.
func (pkg *Package) SymbolDoc(name string) string {
	return pkg.symbolDocs[name]
}

// Externals returns the package's exported symbol names in declaration
// order.  Externals allocates and returns a copy on every call so callers
// cannot modify the package's export list.
func (pkg *Package) Externals() []string {
	externals := make([]string, len(pkg.externals))
	copy(externals, pkg.externals)
	return externals
}

// NumExternals returns the number of exported symbol names without copying
// the export list.
func (pkg *Package) NumExternals() int {
	return len(pkg.externals)
}

// Export appends names to the package's export list verbatim, preserving
// existing order and without deduplicating (matching historical append
// semantics on the package's export list).  Use Exports for the
// deduplicating, sorting variant.
func (pkg *Package) Export(names ...string) {
	pkg.externals = append(pkg.externals, names...)
}

// Exports declares symbols exported by the package.  The symbols are not
// required to be bound at the time Exports is called.
func (pkg *Package) Exports(sym ...string) {
	// Copy sym before sorting to avoid mutating the caller's backing
	// array (e.g., a package-level var passed via ...).
	sorted := make([]string, len(sym))
	copy(sorted, sym)
	sort.Strings(sorted)
	externs := pkg.externals
addloop:
	for _, symnew := range sorted {
		for _, s := range pkg.externals {
			if s == symnew {
				continue addloop
			}
		}
		externs = append(externs, symnew)
	}
	sort.Strings(externs)
	pkg.externals = externs
}

// GetFunName returns the function name (if any) known to be bound to the given
// FID.
func (pkg *Package) GetFunName(fid string) string {
	name, ok := pkg.funNames[fid]
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
	_, ok := pkg.symbols[k.Str]
	if !ok {
		return Errorf("symbol not bound: %v (set! only mutates existing bindings; use set to create new ones)", k)
	}
	pkg.put(k, v)
	return Nil()
}

func (pkg *Package) put(k, v *LVal) {
	if v.Type == LFun {
		pkg.funNames[v.FID()] = k.Str
	}
	pkg.symbols[k.Str] = v
}
