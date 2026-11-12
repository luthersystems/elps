// Copyright © 2026 The ELPS authors

package analysis

import (
	"iter"
	"sync"
)

// GlobalIndex indexes the workspace globals of a Config (ExtraGlobals) one
// time, for a run that analyzes many files with them.
//
// Without it, Analyze copies every workspace global into the root scope of
// each file it analyzes. A workspace lint analyzes each file twice, so it did
// work in proportion to files times globals: 800 files with 32000 definitions
// took over a minute (luthersystems/elps#944). With Config.GlobalIndex set,
// the root scope of a file finds a package-qualified global in the shared
// index, and makes the file its own Symbol only when the file looks the
// global up.
//
// An index is immutable once built, so any number of analyses, on any
// goroutines, may share it.
type GlobalIndex struct {
	globals []ExternalSymbol
	// unqualified holds the globals with no package, in order. Analyze
	// defines them in each root scope as before: they share the bare-name
	// table with the builtins, and the order of those definitions decides
	// which one a name finds.
	unqualified []int32
	// byKey maps "pkg:name" to the last global with that key, and byBare
	// maps a bare name to the last package-qualified global with it. These
	// are the globals Scope.Define would have left in PackageSymbols and
	// in the bare-name index, since a later definition replaces an
	// earlier one.
	byKey  map[string]int32
	byBare map[string]int32
	// keysByBare lists, for each bare name, the byKey entries with that
	// name, for Scope.LookupAllLocal; keysByPackage lists them for each
	// package, for Scope.EachPackageSymbol.
	keysByBare    map[string][]int32
	keysByPackage map[string][]int32
	// byName lists, for each bare name, every global with it, packaged or
	// not, in the order of globals (for Named).
	byName map[string][]int32
}

// NewGlobalIndex indexes globals, which must be the ExtraGlobals of the
// Configs the index is used with. The index keeps globals; the caller must
// not change it afterwards.
func NewGlobalIndex(globals []ExternalSymbol) *GlobalIndex {
	x := &GlobalIndex{
		globals: globals,
		byKey:   make(map[string]int32, len(globals)),
		byBare:  make(map[string]int32),
		byName:  make(map[string][]int32),
	}
	for i := range globals {
		g := &globals[i]
		x.byName[g.Name] = append(x.byName[g.Name], int32(i))
		if g.Package == "" {
			x.unqualified = append(x.unqualified, int32(i))
			continue
		}
		x.byKey[g.Package+":"+g.Name] = int32(i)
		x.byBare[g.Name] = int32(i)
	}
	x.keysByBare = make(map[string][]int32, len(x.byBare))
	x.keysByPackage = make(map[string][]int32)
	for _, i := range x.byKey {
		g := &globals[i]
		x.keysByBare[g.Name] = append(x.keysByBare[g.Name], i)
		x.keysByPackage[g.Package] = append(x.keysByPackage[g.Package], i)
	}
	return x
}

// Named yields every global named name, of any package or of none, in the
// order of the globals the index was built from. It is what a scan of all
// the globals for the name finds, without the scan: a check run on each
// file of a workspace asks it once per name, not once per global.  A nil
// index yields nothing.
func (x *GlobalIndex) Named(name string) iter.Seq[*ExternalSymbol] {
	return func(yield func(*ExternalSymbol) bool) {
		if x == nil {
			return
		}
		for _, i := range x.byName[name] {
			if !yield(&x.globals[i]) {
				return
			}
		}
	}
}

// indexes reports whether x was built from globals. Analyze uses an index
// only for the globals it was built from, so a Config whose ExtraGlobals
// changed after the index was made still resolves what it holds.
func (x *GlobalIndex) indexes(globals []ExternalSymbol) bool {
	if x == nil || len(x.globals) != len(globals) {
		return false
	}
	return len(globals) == 0 || &x.globals[0] == &globals[0]
}

// symbol returns a new root-scope Symbol for global i, with the fields
// Analyze gives a workspace global.
func (x *GlobalIndex) symbol(i int32) *Symbol {
	sym := globalSymbol(&x.globals[i])
	return &sym
}

// globalSymbol is the Symbol Analyze makes for a workspace global.
func globalSymbol(ext *ExternalSymbol) Symbol {
	//elps:aliases the Symbol shares the global's Source as Analyze always has: the workspace scan made that location and nothing changes it after, and every file's Symbol for the global reads the same one
	return Symbol{
		Name:      ext.Name,
		Package:   ext.Package,
		Kind:      ext.Kind,
		Source:    ext.Source,
		Signature: ext.Signature,
		DocString: ext.DocString,
		Exported:  true,
		External:  true,

		GeneratedBy: ext.GeneratedBy,
	}
}

// scopeGlobals is the part of a root scope that reads a GlobalIndex. Each
// file has its own Symbol for a global, because analysis writes to it (its
// References count, for one); syms keeps the Symbol of each global the
// file has looked up, so every lookup returns the same one. A lookup can
// add to syms, and a Result may be read on several goroutines, so a mutex
// guards it.
type scopeGlobals struct {
	index *GlobalIndex
	mu    sync.Mutex
	syms  map[int32]*Symbol
}

// lookup returns the Symbol of global i in scope s.
func (g *scopeGlobals) lookup(s *Scope, i int32) *Symbol {
	g.mu.Lock()
	defer g.mu.Unlock()
	if sym := g.syms[i]; sym != nil {
		return sym
	}
	sym := g.index.symbol(i)
	sym.Scope = s
	if g.syms == nil {
		g.syms = make(map[int32]*Symbol)
	}
	g.syms[i] = sym
	return sym
}

// byKey returns the global with the qualified key "pkg:name", or nil.
func (g *scopeGlobals) byKey(s *Scope, key string) *Symbol {
	if g == nil {
		return nil
	}
	i, ok := g.index.byKey[key]
	if !ok {
		return nil
	}
	return g.lookup(s, i)
}

// byBare returns the package-qualified global a bare name finds, or nil.
func (g *scopeGlobals) byBare(s *Scope, name string) *Symbol {
	if g == nil {
		return nil
	}
	i, ok := g.index.byBare[name]
	if !ok {
		return nil
	}
	return g.lookup(s, i)
}

// useGlobalIndex gives root the globals of x: the unqualified ones by
// definition, as Analyze defines every global without an index, and the
// qualified ones through x. Call it after populateBuiltins and before any
// other definition, the point where Analyze defines the globals.
func useGlobalIndex(root *Scope, x *GlobalIndex) {
	g := &scopeGlobals{index: x}
	root.globals = g
	// A global replaces a definition made before it. The builtins have no
	// package today, so nothing here matches; the loops keep the rule if
	// that changes.
	for key := range root.PackageSymbols {
		if i, ok := x.byKey[key]; ok {
			root.PackageSymbols[key] = g.lookup(root, i)
		}
	}
	for name := range root.bareNameIndex {
		if i, ok := x.byBare[name]; ok {
			root.bareNameIndex[name] = g.lookup(root, i)
		}
	}
	if len(x.unqualified) == 0 {
		return
	}
	syms := make([]Symbol, len(x.unqualified))
	for j, i := range x.unqualified {
		syms[j] = globalSymbol(&x.globals[i])
		root.Define(&syms[j])
	}
}
