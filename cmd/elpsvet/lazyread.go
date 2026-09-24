// Copyright © 2026 The ELPS authors

package main

import (
	"go/ast"
	"go/types"

	"golang.org/x/tools/go/analysis"
)

// lazyReadAnalyzer confines direct access to the tables a lazy template VM
// fills on demand: Package.symbols, Package.baseValues and sortedmap.m. Under
// lazy instantiation (lisp/template_lazy.go) an unmaterialized package slot
// is nil in baseValues, and an unmaterialized thawed binding or sorted-map
// entry holds the lazyPending marker. A direct read that bypasses the filling
// accessor -- Package.baseValue, Package.symbol, sortedmap.entry, or one of
// the materializeSymbols/forceAll sweeps -- returns nil or leaks the marker
// into the program.
//
// Every selector that resolves to one of the three fields is reported
// (reads, writes, ranges, address-taking), outside the audited functions in
// lazyTableFunctions. The field is matched by its go/types object, so
// promoted fields and defined types over Package's struct are seen. Each
// allowlisted function carries the reason it cannot observe a pending entry
// (it IS the accessor, it runs a forcing sweep first, it only writes, it reads
// keys or the length only, or it builds a table no lazy VM has reached).
// There are no comment suppressions: a new reader requires an audit here.
//
// A conversion of a Package or sortedmap (or a pointer to one) to any other
// type is reported too: a separately declared struct with identical fields
// reads the same storage through field objects this rule does not know.
//
// Invisible: an alias of a table taken inside an allowlisted function and
// passed out, and reflection or unsafe.
var lazyReadAnalyzer = &analysis.Analyzer{
	Name: "elpslazyread",
	Doc:  "confine direct reads of lazily materialized package and sorted-map tables to the accessors that fill them",
	Run:  runLazyRead,
}

var lazyTableFunctions = map[string]string{
	// The accessors that fill placeholders, and their sweeps.
	"Package.baseValue":          "the filling accessor for base slots: a nil slot of a lazy package is materialized here",
	"Package.symbol":             "the filling accessor for thawed bindings: replaces a lazyPending entry before returning it",
	"Package.materializeSymbols": "the forcing sweep over thawed bindings; each pending entry goes through Package.symbol",
	"sortedmap.entry":            "the filling accessor for sorted-map entries: replaces a lazyPending entry before returning it",
	"sortedmap.forceAll":         "the forcing sweep over sorted-map entries; each pending entry goes through sortedmap.entry",
	"sortedmap.discardPending":   "compares an entry against lazyPending to keep the pending count exact before a write or delete",
	// Readers that force first.
	"Package.symbolTable":                "calls materializeSymbols before cloning, and reads base slots through baseValue",
	"Package.thaw":                       "copies materialized slots and carries unmaterialized ones across as lazyPending, never reading their value",
	"sortedmap.Entries":                  "calls forceAll before reading the table",
	"sortedmap.copyInto":                 "calls forceAll before reading the table",
	"LVal.AppendSortedPairs":             "calls forceAll before reading the table",
	"copier.mapData":                     "calls forceAll on the sorted-map arm before reading the table",
	"templateInventory.mapData":          "calls forceAll before snapshotting a sorted map for republication",
	"templateCompiler.mapData":           "calls forceAll before reading the table",
	"templateCompiler.packageDescriptor": "reads symbols only of a package without a base; a lazy thawed package is routed through symbolTable",
	"admitPackage":                       "reads symbols only when the package has neither a base nor a lazy link; otherwise goes through symbolTable; writes a fresh table",
	// Writers, and readers of keys or length only.
	"sortedmap.Set":       "writes an entry after discardPending; reads no value",
	"sortedmap.Del":       "deletes an entry after discardPending; reads no value",
	"sortedmap.Len":       "reads the length only; a pending entry is a present key",
	"sortedmap.Keys":      "reads keys only; a pending entry is a present key",
	"sortedmap.emptyLike": "reads the length only, to size a fresh table",
	"Package.SymbolNames": "reads keys only; a pending binding is a present name",
	"Package.putName":     "writes a binding, settling the pending count when it overwrites lazyPending",
	"Package.putSlot":     "writes this VM's slot, settling the pending count when it overwrites an unmaterialized one; reads no value",
	"LVal.copyMapData":    "writes a fresh table it built (the StringKeyRanger arm); the sortedmap arm goes through clone",
	// Constructors of tables no lazy VM has reached.
	"templatePlan.instantiateEager": "builds eager VM tables before registry publication",
	"templatePlan.instantiateLazy":  "builds package shells whose slots start nil for baseValue to fill",
	"lazyInstance.fillBacking":      "builds a lazy map's table, storing lazyPending for unmaterialized entries",
}

type lazyField struct{ typ, field string }

var lazyFields = []lazyField{{"Package", "symbols"}, {"Package", "baseValues"}, {"sortedmap", "m"}}

func lazyTableField(pass *analysis.Pass, sel *ast.SelectorExpr) string {
	selection := pass.TypesInfo.Selections[sel]
	if selection == nil || selection.Kind() != types.FieldVal {
		return ""
	}
	field := selection.Obj()
	if field.Pkg() == nil || field.Pkg().Path() != lispPkgPath {
		return ""
	}
	for _, lf := range lazyFields {
		obj := field.Pkg().Scope().Lookup(lf.typ)
		if obj == nil {
			continue
		}
		st, ok := obj.Type().Underlying().(*types.Struct)
		if !ok {
			continue
		}
		for i := range st.NumFields() {
			if st.Field(i) == field && field.Name() == lf.field {
				return lf.typ + "." + lf.field
			}
		}
	}
	return ""
}

func funcDeclName(pass *analysis.Pass, fn *ast.FuncDecl) string {
	name := fn.Name.Name
	obj, _ := pass.TypesInfo.Defs[fn.Name].(*types.Func)
	if obj == nil {
		return name
	}
	if recv := obj.Type().(*types.Signature).Recv(); recv != nil {
		if named := packageNamedType(recv.Type()); named != nil {
			name = named.Obj().Name() + "." + name
		}
	}
	return name
}

func runLazyRead(pass *analysis.Pass) (interface{}, error) {
	if pass.Pkg.Path() != lispPkgPath {
		return nil, nil
	}
	for _, file := range pass.Files {
		for _, decl := range file.Decls {
			if fn, ok := decl.(*ast.FuncDecl); ok && lazyTableFunctions[funcDeclName(pass, fn)] != "" {
				continue
			}
			ast.Inspect(decl, func(n ast.Node) bool {
				if call, ok := n.(*ast.CallExpr); ok {
					checkLazyConversion(pass, call)
					return true
				}
				sel, ok := n.(*ast.SelectorExpr)
				if !ok {
					return true
				}
				if name := lazyTableField(pass, sel); name != "" {
					pass.Reportf(sel.Sel.Pos(), "direct access to lazily materialized table %s outside its filling accessor; use Package.lookup/symbolTable or the Map methods, or audit the function in lazyTableFunctions", name)
				}
				return true
			})
		}
	}
	return nil, nil
}

// lazyTableType names Package or sortedmap when t is one of them or a
// pointer to one.
func lazyTableType(t types.Type) string {
	named := packageNamedType(t)
	if named == nil || named.Obj().Pkg() == nil || named.Obj().Pkg().Path() != lispPkgPath {
		return ""
	}
	switch named.Obj().Name() {
	case "Package", "sortedmap":
		return named.Obj().Name()
	}
	return ""
}

func checkLazyConversion(pass *analysis.Pass, call *ast.CallExpr) {
	if len(call.Args) != 1 || !pass.TypesInfo.Types[call.Fun].IsType() {
		return
	}
	src := pass.TypesInfo.TypeOf(call.Args[0])
	name := lazyTableType(src)
	if name == "" || types.Identical(src, pass.TypesInfo.TypeOf(call)) {
		return
	}
	pass.Reportf(call.Pos(), "conversion of lazily materialized type %s to another type bypasses its filling accessors; read it through the %s methods, or audit the function in lazyTableFunctions", name, name)
}
