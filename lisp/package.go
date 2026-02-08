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
	Name       string
	Doc        string
	Symbols    map[string]*LVal
	SymbolDocs map[string]string
	FunNames   map[string]string
	Externals  []string
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
func (pkg *Package) Get(k *LVal) *LVal {
	v := pkg.get(k)
	if v.Type == LFun {
		// Set the function's name here in case the same function is defined
		// with multiple names.  We want to try and use the name the programmer
		// used.  The name may even come from a higher scope.
		pkg.FunNames[v.FID()] = k.Str
	}
	return v
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
		if v.Type == LFun {
			// Set the function's name here in case the same function is
			// defined with multiple names.  We want to try and use the name
			// the programmer used.
			pkg.FunNames[v.FID()] = k.Str
		}
		return v
	}
	lerr := Errorf("unbound symbol: %v", k)
	lerr.Source = k.Source
	return lerr
}

// Exports declares symbols exported by the package.  The symbols are not
// required to be bound at the time Exports is called.
func (pkg *Package) Exports(sym ...string) {
	sort.Strings(sym)
	externs := pkg.Externals
addloop:
	for _, symnew := range sym {
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
