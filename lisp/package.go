// Copyright © 2018 The ELPS authors

package lisp

import (
	"sort"
	"strings"

	"github.com/luthersystems/elps/parser/lexer"
	"github.com/luthersystems/elps/parser/token"
)

// validPackageName checks the Lisp builtin boundary only. Go registration and
// LEnv.InPackage/UsePackage deliberately continue to accept arbitrary names.
func validPackageName(name string) bool {
	if name == "" || strings.Contains(name, ":") {
		return false
	}
	if isPlainASCIIName(name) {
		return true
	}
	// Use the reader's lexer rather than a second symbol alphabet. The only
	// parser rule needed without colons is ParseNegative: a leading minus
	// followed by a SYMBOL becomes one symbol; a numeric token does not.
	lex := lexer.New(token.NewScannerString("", name))
	tok := lex.ReadToken()[0]
	if tok.Type == token.NEGATIVE {
		tok = lex.ReadToken()[0]
		if tok.Text != name[1:] {
			return false
		}
	} else if tok.Text != name {
		return false
	}
	return tok.Type == token.SYMBOL && lex.ReadToken()[0].Type == token.EOF
}

// isPlainASCIIName is the allocation-free fast path of validPackageName: an
// ASCII letter or underscore followed by ASCII letters, digits, underscores
// and hyphens.  Every such string lexes as exactly one SYMBOL token
// (isWordStart admits letters and '_', isWord admits these plus digits and
// '-', and nothing here is a quote, colon, dispatch or number prefix), so the
// lexer below would return true; the lexer stays the authority for every
// other spelling.  Running the lexer here cost a lexer, a scanner and a token
// slice per in-package, use-package and export, which is every phylum load
// (substrate#504).  TestValidPackageNameFastPathAgreesWithLexer pins the
// agreement exhaustively over short strings.
func isPlainASCIIName(name string) bool {
	for i := 0; i < len(name); i++ {
		c := name[i]
		switch {
		case 'a' <= c && c <= 'z', 'A' <= c && c <= 'Z', c == '_':
		case i > 0 && ('0' <= c && c <= '9' || c == '-'):
		default:
			return false
		}
	}
	return len(name) > 0
}

// PackageRegistry contains a set of packages.
type PackageRegistry struct {
	packages map[string]*Package
	// runtime is the Runtime this registry is the interpreter state of.  It
	// is set where a Runtime is first paired with an environment
	// (NewEnvRuntime, StandardRuntime), and it is nil in a registry that has
	// not been attached to one yet.  Admission reads
	// runtime.ValueDepthLimit() through it, so a registry admitting into a
	// runtime configured with WithMaxValueDepth applies the configured limit
	// rather than the MaxValueDepth default; a detached registry keeps the
	// default.  It is a back pointer to state the registry already belongs
	// to and never a second owner of it.
	runtime *Runtime
	Lang    string // A default package used by all other packages
}

// bindRegistryRuntime attaches rt to its own registry so that package
// admission can read rt.ValueDepthLimit().  It is called where a Runtime is
// first paired with an environment rather than on every environment: a child
// env inherits its parent's runtime, which is already bound, so the hot path
// pays nothing.  A registry already bound to another runtime keeps it -- two
// runtimes sharing one registry is not a configuration admission can resolve,
// and the first binding is the one whose limits the registry was filled under.
func bindRegistryRuntime(rt *Runtime) {
	if rt == nil || rt.Registry == nil || rt.Registry.runtime != nil {
		return
	}
	rt.Registry.runtime = rt
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

// AddPackage registers a private snapshot of p under p.Name if no package
// with that name exists already.  AddPackage returns true when p was added
// and false when a package named p.Name was already registered (in which
// case the registry is unchanged).
//
// AddPackage is an ADMISSION point, not a store (issue #524).  A registry is
// the interpreter state of a Runtime, so a package built outside it — by an
// embedder, or by another Runtime whose registry is being merged into this
// one — arrives full of values the caller still holds pointers to.  What
// gets registered is therefore a snapshot of p rather than p itself, and
// each bound value is admitted according to what the seal can promise about
// it (lisp/package_admit.go states the rule per value class):
//
//   - a value that is sealed throughout, and a singleton, are shared by
//     reference — the sanctioned cross-environment share;
//   - a code-like tree that is NOT sealed (a runtime-built list, symbol,
//     string or number: fresh mutable storage the caller still aliases) is
//     copied privately and sealed, so neither side can write what the other
//     evaluates;
//   - everything else — functions, natives, sorted-maps, arrays, byte
//     strings, and trees holding one — is shared by reference, because no
//     seal covers those classes.  For them AddPackage transfers custody: the
//     caller must stop mutating them, and evaluating a shared closure under
//     two Runtimes remains the caller's problem (checked builds report it).
//
// Two consequences worth stating for callers.  The registry does not hold p,
// so binding into p afterwards does not change the registered package —
// bind through the environment (or add a finished package).  And the
// snapshot reads p's maps on the calling goroutine, so, like every other
// read of a *Package, it requires that no other goroutine is writing p
// (issue #397).
func (r *PackageRegistry) AddPackage(p *Package) bool {
	if p == nil {
		return false
	}
	if _, ok := r.packages[p.Name]; ok {
		return false
	}
	r.packages[p.Name] = admitPackage(p, r.runtime.ValueDepthLimit())
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
	symbols map[string]*LVal
	// symbolDocs maps a bound name to its documentation string.  It is
	// allocated LAZILY, by setSymbolDoc, and is nil in a package whose
	// symbols carry no docs — which is most of them: a docstring on a
	// *symbol* comes only from `(set 'name v "doc")` or the Go-side
	// SetSymbolDoc, while a function's docstring rides in the LFun's own
	// cells and never lands here.  Reading a nil map is legal Go and
	// returns the empty string, which is exactly SymbolDoc's answer for an
	// undocumented name, so every read path is nil-safe by construction;
	// only setSymbolDoc may write it, and it allocates first.  Ten of the
	// thirteen packages a fully loaded environment holds have no symbol
	// docs at all, and each of those used to cost an empty map in
	// NewPackage, another in every Fork, and another in every
	// AddPackage admission.
	symbolDocs map[string]string
	// funNames maps a function's FID to the name it was most recently bound
	// under in this package.  It is populated exclusively by the write path
	// (see put).  Reads must not write it: a *Package is routinely shared by
	// pointer across goroutines.  See issue #397.
	funNames  map[string]string
	externals []string
	// bindingsSealed protects the core namespace at Lisp mutation boundaries.
	// Go registration APIs remain available to the host after initialization.
	bindingsSealed bool
}

// checkLispPackageBinding checks a Lisp assignment's destination without
// allocating on the ordinary user-package path. Qualified set! needs this
// check too, even though Update otherwise searches literal lexical keys.
func (env *LEnv) checkLispPackageBinding(name string) *LVal {
	pkg := env.Runtime.Package
	if ns, local, qualified := strings.Cut(name, ":"); qualified {
		pkg = env.Runtime.Registry.packages[ns]
		name = local
	}
	if pkg != nil && pkg.bindingsSealed {
		return env.Errorf("cannot rebind lisp package binding: %s", name)
	}
	return nil
}

// NewPackage initializes and returns a package with the given name.
func NewPackage(name string) *Package {
	return &Package{
		Name:     name,
		symbols:  make(map[string]*LVal),
		funNames: make(map[string]string),
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

// setSymbolDoc records doc as the documentation string for name, allocating
// the package's doc table on first use.  It is the ONLY writer of
// symbolDocs; the field's doc comment states why that matters (a nil table
// is the common case, and only a writer may not assume the map exists).
func (pkg *Package) setSymbolDoc(name, doc string) {
	if pkg.symbolDocs == nil {
		pkg.symbolDocs = make(map[string]string, 1)
	}
	pkg.symbolDocs[name] = doc
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
//
// Put stores v as given: it takes no admission walk (see AddPackage and
// lisp/package_admit.go).  It is the write path every `set` reaches through
// LEnv.PutGlobal, where the value being bound belongs to the environment
// that is already evaluating it and LEnv.Put/PutGlobal have already taken
// the checked-mode ownership sighting — so a per-binding walk here would
// tax the interpreter's hot path to guard a transfer that is not happening.
// A Go caller that reaches around the environment to Put into a package
// another Runtime is serving is doing the cross-Runtime sharing AddPackage's
// snapshot exists to prevent, and owns the consequences.
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
	pkg.putName(k.Str, v)
}

// putName binds v to name without requiring the caller to allocate a symbol
// LVal for the key.  It is the registration fast path used by the LEnv.Add*
// methods and by UsePackage's import loop, which bind hundreds of names per
// environment; Put and Update funnel through it too, so the LFun name
// bookkeeping stays in one place.  Callers are responsible for the
// constant-rebind check Put performs — the Add* methods and UsePackage guard
// TrueSymbol/FalseSymbol explicitly before calling.
func (pkg *Package) putName(name string, v *LVal) {
	if v.Type == LFun {
		pkg.funNames[v.FID()] = name
	}
	pkg.symbols[name] = v
}
