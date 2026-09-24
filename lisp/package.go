// Copyright © 2018 The ELPS authors

package lisp

import (
	"iter"
	"maps"
	"slices"
	"sort"
	"strings"

	"github.com/luthersystems/elps/internal/packagetable"
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
	for i := range len(name) {
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
	funNames map[string]string
	// lazy is non-nil while some binding of this VM's package is not yet
	// materialized from a lazy template plan: a nil baseValues slot, or (after
	// a thaw) a lazyPending entry in symbols. See template_lazy.go.
	lazy *lazyPackage
	// base is non-nil while this Package belongs to a VM minted from a
	// template that gave it a shared base -- it was named by
	// TemplateWithFrozenPackages, or the template instantiates lazily, which
	// gives every package a base (unfrozenBase then keeps Frozen false) --
	// and the VM has not written it yet.  The base's tables
	// are shared by every such VM and never written; symbols, funNames,
	// symbolDocs and externals are nil.  The first write of any kind thaws
	// the package (see ensureWritable): it gets private tables and base
	// becomes nil, for this VM only.
	base *packageBase
	// slotFunNames is this VM's overlay on base.funNames while the package
	// is frozen: the FID->name entries that slot writes of function values
	// recorded (see putSlot).  It is nil until the first such write, is
	// consulted before the base by GetFunName, and is merged into the private
	// table by thaw, so function naming matches an unfrozen package exactly.
	slotFunNames map[string]string
	Name         string
	Doc          string
	externals    []string
	// baseValues holds this VM's values for base.index's slots.  The slice
	// is per VM; only the name->slot index is shared.  putSlot writes it in
	// place when a frozen package rebinds a name it already has.
	baseValues []*LVal
	// externalsSortedLen records the length externals had the last time
	// Exports left it in sorted order.  It is a validity token, not a flag:
	// every other writer of externals (Export, and the AddBuiltins family in
	// lisp/env.go) only ever APPENDS, so a length that no longer matches is
	// proof that unsorted names may have arrived since and the list must be
	// re-sorted before it can be binary searched.  A package copied
	// field-by-field (package admission, the template planner) starts at
	// zero, which matches only an empty -- and so trivially sorted -- list.
	externalsSortedLen int
	// unfrozenBase marks a package that reads a lazy plan's shared base
	// although it was not named frozen: it behaves exactly as an unfrozen
	// package (Frozen reports false) and thaws on its first write.
	unfrozenBase bool
	// bindingsSealed protects the core namespace at Lisp mutation boundaries.
	// Go registration APIs remain available to the host after initialization.
	bindingsSealed bool
}

// packageBase holds a frozen package's read-only tables. The backing maps and
// slice live in packagetable, outside the kernel's reach. The compiler builds
// the base before publication; elpsfrozenpackage confines field replacement to
// that constructor and checks writes to Package's mutable tables as well.
type packageBase struct {
	check packageBaseCheck
	// onThaw is the template's TemplateWithThawHook callback, or nil. It is
	// set once at publication and only read afterwards.
	onThaw     func(pkg string)
	index      packagetable.Map[int]
	funNames   packagetable.Map[string]
	symbolDocs packagetable.Map[string]
	externals  packagetable.Strings
}

// Frozen reports whether pkg still reads a template-frozen package's shared
// tables. A frozen package is shared until its first write; that write thaws
// a private copy for this VM (see ensureWritable), after which Frozen is false.
func (pkg *Package) Frozen() bool {
	return pkg.base != nil && !pkg.unfrozenBase
}

// baseValue is the only read of a base slot: it materializes the slot from
// a lazy plan on first read.
func (pkg *Package) baseValue(i int) *LVal {
	v := pkg.baseValues[i]
	if v == nil && pkg.lazy != nil {
		v = pkg.fillBaseValue(i)
	}
	return v
}

// fillBaseValue materializes one base slot of a lazy package.
//
//go:noinline
func (pkg *Package) fillBaseValue(i int) *LVal {
	lazy := pkg.lazy
	if lazy.refs[i].index == 0 {
		return nil
	}
	v := lazy.inst.ref(lazy.refs[i])
	pkg.baseValues[i] = v
	lazy.settle(pkg)
	return v
}

// settle records one materialized binding and drops the link to the lazy
// instance after the last, so a fully materialized package retains nothing.
func (lazy *lazyPackage) settle(pkg *Package) {
	lazy.pending--
	if lazy.pending == 0 {
		pkg.lazy = nil
	}
}

// symbol is the only read of an unfrozen table entry: it replaces a pending
// binding left by thaw. The check is small enough to inline into lookup; the
// fill is kept out of line.
func (pkg *Package) symbol(name string) (*LVal, bool) {
	v, ok := pkg.symbols[name]
	if v == lazyPending {
		v = pkg.fillSymbol(name)
	}
	return v, ok
}

// fillSymbol materializes one pending binding left by thaw.
//
//go:noinline
func (pkg *Package) fillSymbol(name string) *LVal {
	v := pkg.symbols[name]
	if v == lazyPending {
		lazy := pkg.lazy
		i, _ := lazy.index.Lookup(name)
		v = lazy.inst.ref(lazy.refs[i])
		pkg.symbols[name] = v
		lazy.settle(pkg)
	}
	return v
}

// materializeSymbols replaces every pending binding, before a caller that
// reads the whole table.
func (pkg *Package) materializeSymbols() {
	if pkg.lazy == nil || pkg.base != nil {
		return
	}
	for name, v := range pkg.symbols {
		if v == lazyPending {
			pkg.symbol(name)
		}
	}
}

// ensureWritable is the single write gate every Package mutator passes. For
// a frozen package it thaws first; otherwise it does nothing.
func (pkg *Package) ensureWritable() {
	if pkg.base != nil {
		pkg.thaw()
	}
}

// thaw gives a frozen package private tables built from the shared base and
// this VM's slot values, then detaches the base. It copies this one package
// only; the base, other VMs and the template are untouched. It is the only
// function that builds private tables from a base.
func (pkg *Package) thaw() {
	base := pkg.base
	if base.onThaw != nil {
		base.onThaw(pkg.Name)
	}
	symbols := make(map[string]*LVal, base.index.Len())
	for name, i := range base.index.All() {
		// A slot a lazy plan has not materialized stays pending: a write to
		// one binding must not build the rest of the package.
		if v := pkg.baseValues[i]; v != nil || pkg.lazy == nil || pkg.lazy.refs[i].index == 0 {
			symbols[name] = v
		} else {
			symbols[name] = lazyPending
		}
	}
	funNames := base.funNames.Copy()
	if funNames == nil {
		funNames = make(map[string]string)
	}
	pkg.symbols = symbols
	maps.Copy(funNames, pkg.slotFunNames)
	pkg.funNames = funNames
	pkg.slotFunNames = nil
	pkg.symbolDocs = base.symbolDocs.Copy()
	pkg.externals = base.externals.Copy()
	pkg.externalsSortedLen = 0
	pkg.baseValues = nil
	pkg.unfrozenBase = false
	pkg.base = nil
}

// lookup is the raw symbol read.
func (pkg *Package) lookup(name string) (*LVal, bool) {
	v, ok := pkg.lookupRaw(name)
	if v == lazyPending {
		v = pkg.lookupFill(name)
	}
	return v, ok
}

// lookupRaw is lookup without the fill: it returns lazyPending for a binding
// a lazy plan has not materialized. It is small enough to inline, so the hot
// callers (get) pay one comparison over the pre-lazy read; they must pass a
// lazyPending result to lookupFill and never return it.
func (pkg *Package) lookupRaw(name string) (*LVal, bool) {
	if pkg.base == nil {
		v, ok := pkg.symbols[name]
		return v, ok
	}
	i, ok := pkg.base.index.Lookup(name)
	if !ok {
		return nil, false
	}
	if v := pkg.baseValues[i]; v != nil || pkg.lazy == nil {
		return v, true
	}
	return lazyPending, true
}

// lookupFill materializes the binding lookup found pending.
//
//go:noinline
func (pkg *Package) lookupFill(name string) *LVal {
	if pkg.base == nil {
		return pkg.fillSymbol(name)
	}
	i, _ := pkg.base.index.Lookup(name)
	return pkg.fillBaseValue(i)
}

// symbolTable returns a copy of the binding table. Values retain their identity;
// changing a binding in the returned map cannot change the package.
func (pkg *Package) symbolTable() map[string]*LVal {
	if pkg.base == nil {
		pkg.materializeSymbols()
		return maps.Clone(pkg.symbols)
	}
	m := make(map[string]*LVal, pkg.base.index.Len())
	for name, i := range pkg.base.index.All() {
		m[name] = pkg.baseValue(i)
	}
	return m
}

// funNameTable and symbolDocTable return private copies for inventory and
// admission. Single-name reads use GetFunName and SymbolDoc without copying.
func (pkg *Package) funNameTable() map[string]string {
	if pkg.base == nil {
		return maps.Clone(pkg.funNames)
	}
	m := pkg.base.funNames.Copy()
	if len(pkg.slotFunNames) > 0 {
		if m == nil {
			m = make(map[string]string, len(pkg.slotFunNames))
		}
		maps.Copy(m, pkg.slotFunNames)
	}
	return m
}

func (pkg *Package) symbolDocTable() map[string]string {
	if pkg.base == nil {
		return maps.Clone(pkg.symbolDocs)
	}
	return pkg.base.symbolDocs.Copy()
}

// appendExternal appends one name to the export list.
func (pkg *Package) appendExternal(name string) {
	pkg.Export(name)
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
	if pkg == nil {
		return nil
	}
	if pkg.bindingsSealed {
		return env.Errorf("cannot rebind lisp package binding: %s", name)
	}
	return nil
}

// NewPackage initializes and returns a package with the given name. Direct
// table initialization is safe here: the package is unfrozen and unpublished.
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
	v, ok := pkg.lookupRaw(k.Str)
	if v == lazyPending {
		v = pkg.lookupFill(k.Str)
	}
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
	return pkg.lookup(name)
}

// SymbolNames returns the names of all symbols bound in pkg in sorted order.
// SymbolNames allocates a new slice on every call.
func (pkg *Package) SymbolNames() []string {
	if pkg.base != nil {
		return pkg.base.index.Keys()
	}
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
	if pkg.base != nil {
		doc, _ := pkg.base.symbolDocs.Lookup(name)
		return doc
	}
	return pkg.symbolDocs[name]
}

// setSymbolDoc records doc as the documentation string for name, allocating
// the package's doc table on first use.  It is the ONLY writer of
// symbolDocs; the field's doc comment states why that matters (a nil table
// is the common case, and only a writer may not assume the map exists).
func (pkg *Package) setSymbolDoc(name, doc string) {
	pkg.ensureWritable()
	if pkg.symbolDocs == nil {
		pkg.symbolDocs = make(map[string]string, 1)
	}
	pkg.symbolDocs[name] = doc
}

// Externals returns the package's exported symbol names in declaration
// order.  Externals allocates and returns a copy on every call so callers
// cannot modify the package's export list.
func (pkg *Package) Externals() []string {
	if pkg.base != nil {
		return pkg.base.externals.Copy()
	}
	externals := make([]string, len(pkg.externals))
	copy(externals, pkg.externals)
	return externals
}

// NumExternals returns the number of exported symbol names without copying
// the export list.
func (pkg *Package) NumExternals() int {
	if pkg.base != nil {
		return pkg.base.externals.Len()
	}
	return len(pkg.externals)
}

// externalNames iterates exports without lending out their backing slice.
func (pkg *Package) externalNames() iter.Seq[string] {
	if pkg.base != nil {
		return pkg.base.externals.All()
	}
	return slices.Values(pkg.externals)
}

// Export appends names to the package's export list verbatim, preserving
// existing order and without deduplicating (matching historical append
// semantics on the package's export list).  Use Exports for the
// deduplicating, sorting variant.
func (pkg *Package) Export(names ...string) {
	pkg.ensureWritable()
	pkg.externals = append(pkg.externals, names...)
}

// Exports declares symbols exported by the package.  The symbols are not
// required to be bound at the time Exports is called.
//
// The result is always the sorted union of the package's existing export
// list with the names in sym that it did not already contain -- names
// repeated WITHIN one call are appended once each, matching the historical
// implementation, which tested membership against the pre-call list only.
//
// The single-name case has its own path because it is the only one the
// interpreter reaches: `(export 'a 'b)` recurses one symbol at a time
// (exportArgs, lisp/builtins.go), so exporting n names used to copy and sort
// a one-element slice, scan the whole export list and re-sort it n times
// over.  The fast path skips the copy and the input sort and splices the new
// name into its sorted position instead, keeping the list sorted for the
// next call.
func (pkg *Package) Exports(sym ...string) {
	pkg.ensureWritable()
	if len(sym) == 1 {
		pkg.exportSorted(sym[0])
		return
	}
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
	pkg.externalsSortedLen = len(externs)
}

// exportSorted adds one name to the export list, leaving it sorted.  It is
// equivalent to Exports(name): the old implementation finished by sorting
// the whole list, so the result is fully determined as the sorted union and
// an insertion at the searched position reproduces it byte for byte.
func (pkg *Package) exportSorted(name string) {
	pkg.ensureWritable()
	if pkg.externalsSortedLen != len(pkg.externals) {
		sort.Strings(pkg.externals)
		pkg.externalsSortedLen = len(pkg.externals)
	}
	i := sort.SearchStrings(pkg.externals, name)
	if i < len(pkg.externals) && pkg.externals[i] == name {
		return
	}
	pkg.externals = slices.Insert(pkg.externals, i, name)
	pkg.externalsSortedLen = len(pkg.externals)
}

// GetFunName returns the function name (if any) known to be bound to the given
// FID.
func (pkg *Package) GetFunName(fid string) string {
	if pkg.base != nil {
		if name, ok := pkg.slotFunNames[fid]; ok {
			return name
		}
		name, _ := pkg.base.funNames.Lookup(fid)
		return name
	}
	return pkg.funNames[fid]
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
	_, ok := pkg.lookup(k.Str)
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
	if pkg.base != nil && pkg.putSlot(name, v) {
		return
	}
	pkg.ensureWritable()
	if v.Type == LFun {
		pkg.funNames[v.FID()] = name
		if v.Package() == pkg.Name {
			v.funData().name = name // see funData.name
		}
	}
	if pkg.lazy != nil && pkg.symbols[name] == lazyPending {
		pkg.lazy.settle(pkg)
	}
	pkg.symbols[name] = v
}

// putSlot rebinds a name a frozen package ALREADY binds by writing only this
// VM's baseValues slot, without thawing, and reports whether it did.  It
// reports false, leaving the package untouched, for a name the shared index
// does not hold; binding a new name changes the shared index, so the caller
// thaws.  A function value's FID->name entry goes to this VM's slotFunNames
// overlay unless the shared base already records exactly that entry.  The
// observable effect is the thawed path's: the binding, the function's own
// name and GetFunName all read as they would after putName on a thawed copy.
// Docs, exports and the index stay shared, and so do other VMs' slots.
func (pkg *Package) putSlot(name string, v *LVal) bool {
	i, ok := pkg.base.index.Lookup(name)
	if !ok {
		return false
	}
	if v.Type == LFun {
		fid := v.FID()
		if prev, ok := pkg.base.funNames.Lookup(fid); ok && prev == name {
			delete(pkg.slotFunNames, fid)
		} else {
			if pkg.slotFunNames == nil {
				pkg.slotFunNames = make(map[string]string, 1)
			}
			pkg.slotFunNames[fid] = name
		}
		if v.Package() == pkg.Name {
			v.funData().name = name // see funData.name
		}
	}
	if pkg.lazy != nil && pkg.baseValues[i] == nil && pkg.lazy.refs[i].index != 0 {
		pkg.lazy.settle(pkg) // overwriting a binding a lazy plan never materialized
	}
	pkg.baseValues[i] = v
	return true
}
