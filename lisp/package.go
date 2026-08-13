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
//
// Ownership: AddPackage stores p BY POINTER and copies nothing.  Registering
// one *Package with the registries of two environments therefore gives both
// environments the same bindings and the same *LVal values — a fact both
// in-repo callers (cmd/doc.go, mcpserver/service.go) rely on to merge an
// embedder's packages into freshly built documentation environments.
//
// The caller owns what that sharing means for the VALUES it installs.  An
// unsealed list value shared this way is mutable storage shared between
// runtimes: (stable-sort > shared:data) evaluated in one environment
// rewrites the list in place and every other environment holding the package
// reads the sorted result (lisp/package_sharing_test.go pins both halves of
// this).  Call SealAST on any value a package may carry into more than one
// environment — the kernel's mutation sites copy-on-write a sealed value
// (lisp/seal.go), which is exactly the protection the parser gives program
// literals.
//
// This is a contract rather than a check, deliberately, and it is NOT the
// same situation as lisp.Program (issue #394), which repairs an unsealed
// tree at construction:
//
//   - There is nothing to repair.  A Package's symbols are arbitrary runtime
//     values — builtin LFun closures, LNative payloads, sorted maps — which
//     Copy cannot deep-copy and SealAST explicitly declines to descend into.
//     The private-copy-and-seal that works for a parse tree has no analogue
//     for a package.
//   - A check here would run at the wrong time.  Packages are registered
//     empty and populated afterwards (DefinePackage, then Put/Export), so
//     AddPackage inspecting p's contents would inspect nothing.
//   - The only guard AddPackage could actually implement — refusing a
//     *Package already registered elsewhere — would forbid the sharing its
//     two in-repo callers exist to perform, and would make *Package
//     single-registry for its whole lifetime.
//
// Tightening this is a design change to Package (sealing at Put time for
// parser-shaped values, or an ownership assertion under the elpscheck build
// tag, which does not currently fire here) and belongs in its own issue, not
// in #394's fix.
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
	Name       string
	Doc        string
	symbols    map[string]*LVal
	symbolDocs map[string]string
	funNames   map[string]string
	externals  []string
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
func (pkg *Package) Get(k *LVal) *LVal {
	v := pkg.get(k)
	if v.Type == LFun {
		// Set the function's name here in case the same function is defined
		// with multiple names.  We want to try and use the name the programmer
		// used.  The name may even come from a higher scope.
		if fid := v.FID(); pkg.funNames[fid] != k.Str {
			pkg.funNames[fid] = k.Str
		}
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
	v, ok := pkg.symbols[k.Str]
	if ok {
		if v.Type == LFun {
			// Set the function's name here in case the same function is
			// defined with multiple names.  We want to try and use the name
			// the programmer used.
			if fid := v.FID(); pkg.funNames[fid] != k.Str {
				pkg.funNames[fid] = k.Str
			}
		}
		return v
	}
	lerr := Errorf("unbound symbol: %v", k)
	// Copied, not aliased: the error escapes to the evaluator (and possibly
	// the embedder) while k remains live program state, so the two must not
	// share a *token.Location (cold error path; the copy is free in
	// practice).  copyLocation preserves nil.
	lerr.source = copyLocation(k.source)
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
