// Copyright © 2026 The ELPS authors

package main

import (
	"go/ast"
	"go/token"
	"go/types"

	"golang.org/x/tools/go/analysis"
)

// frozenPackageAnalyzer confines package table writes to guarded methods and
// unpublished constructors. Field identity is resolved by go/types, so unrelated
// fields with the same spelling are harmless. Local map/slice aliases are tracked
// conservatively (flow-insensitively), including aliases passed to sort/slices.
// It does not follow aliases through helper calls or aggregate containers,
// indirect calls, reflection or unsafe.
// Read-only base types prevent raw backing aliases; elpscheck additionally
// detects changes to published tables at the next integrity check.
var frozenPackageAnalyzer = &analysis.Analyzer{
	Name: "elpsfrozenpackage",
	Doc:  "confine Package table and packageBase writes to guarded methods and unpublished constructors",
	Run:  runFrozenPackage,
}

// Each exemption names a function, including its receiver where applicable.
// There are no comment suppressions: a new writer requires an audit here.
var packageWriteFunctions = map[string]string{
	"Package.putName":                    "passes ensureWritable (which thaws a frozen package) before binding and function-name bookkeeping",
	"Package.setSymbolDoc":               "passes ensureWritable (which thaws a frozen package) before allocating or writing documentation",
	"Package.Export":                     "passes ensureWritable (which thaws a frozen package) before appending exports",
	"Package.Exports":                    "passes ensureWritable (which thaws a frozen package) before merging and sorting exports",
	"Package.exportSorted":               "passes ensureWritable (which thaws a frozen package) before inserting a sorted export",
	"Package.putSlot":                    "rebinds an existing frozen name in this VM's own baseValues slot and slotFunNames overlay; shared base tables are never written",
	"Package.thaw":                       "the only builder of private tables from a frozen base; reached only through ensureWritable",
	"NewPackage":                         "constructs an unpublished unfrozen package",
	"admitPackage":                       "constructs an unpublished unfrozen admission snapshot",
	"templatePlan.instantiateEager":      "constructs private VM packages before registry publication",
	"templatePlan.instantiateLazy":       "constructs private VM package shells before registry publication",
	"Package.baseValue":                  "fills this VM's own nil baseValues slot with the value materialized from a lazy plan; shared base tables are never written",
	"Package.symbol":                     "replaces a lazyPending binding with its materialized value; the binding is unchanged from the program's view",
	"templateCompiler.packageDescriptor": "constructs a fresh base before template publication",
	"packageBase.publish":                "records the checked-build fingerprint before publication",
}

func packageWriteFunction(pass *analysis.Pass, fn *ast.FuncDecl) bool {
	if pass.Pkg.Path() != lispPkgPath {
		return false
	}
	name := fn.Name.Name
	obj, _ := pass.TypesInfo.Defs[fn.Name].(*types.Func)
	if obj == nil {
		return false
	}
	if recv := obj.Type().(*types.Signature).Recv(); recv != nil {
		if named := packageNamedType(recv.Type()); named != nil {
			name = named.Obj().Name() + "." + name
		}
	}
	return packageWriteFunctions[name] != ""
}

func packageNamedType(t types.Type) *types.Named {
	if t == nil {
		return nil
	}
	t = types.Unalias(t)
	if ptr, ok := t.(*types.Pointer); ok {
		t = types.Unalias(ptr.Elem())
	}
	named, _ := t.(*types.Named)
	return named
}

func protectedPackageType(t types.Type) string {
	named := packageNamedType(t)
	if named == nil || named.Obj().Pkg() == nil || named.Obj().Pkg().Path() != lispPkgPath {
		return ""
	}
	switch named.Obj().Name() {
	case "Package", "packageBase":
		return named.Obj().Name()
	default:
		return ""
	}
}

func protectedPackageField(pass *analysis.Pass, sel *ast.SelectorExpr) bool {
	selection := pass.TypesInfo.Selections[sel]
	if selection == nil || selection.Kind() != types.FieldVal {
		return false
	}
	// Match the field object, including promoted fields and defined types
	// whose underlying struct is Package's (type mirror Package).
	field := selection.Obj()
	if field.Pkg() == nil || field.Pkg().Path() != lispPkgPath {
		return false
	}
	for _, name := range []string{"Package", "packageBase"} {
		obj := field.Pkg().Scope().Lookup(name)
		if obj == nil {
			continue
		}
		st, ok := obj.Type().Underlying().(*types.Struct)
		if !ok {
			continue
		}
		for i := range st.NumFields() {
			if st.Field(i) != field {
				continue
			}
			if name == "packageBase" {
				return true
			}
			switch field.Name() {
			case "symbols", "funNames", "symbolDocs", "externals", "base", "baseValues", "slotFunNames":
				return true
			}
		}
	}
	return false
}

func runFrozenPackage(pass *analysis.Pass) (interface{}, error) {
	for _, file := range pass.Files {
		for _, decl := range file.Decls {
			if fn, ok := decl.(*ast.FuncDecl); ok && packageWriteFunction(pass, fn) {
				continue
			}
			checkPackageWrites(pass, decl)
		}
	}
	return nil, nil
}

func checkPackageWrites(pass *analysis.Pass, node ast.Node) {
	aliases := make(map[types.Object]bool)
	var rooted func(ast.Expr) bool
	rooted = func(expr ast.Expr) bool {
		switch e := expr.(type) {
		case *ast.Ident:
			return aliases[pass.TypesInfo.ObjectOf(e)]
		case *ast.SelectorExpr:
			return protectedPackageField(pass, e) || rooted(e.X)
		case *ast.IndexExpr:
			return rooted(e.X)
		case *ast.SliceExpr:
			return rooted(e.X)
		case *ast.StarExpr:
			return protectedPackageType(pass.TypesInfo.TypeOf(e)) != "" || rooted(e.X)
		case *ast.ParenExpr:
			return rooted(e.X)
		case *ast.UnaryExpr:
			return e.Op == token.AND && rooted(e.X)
		case *ast.CallExpr:
			// Conversions such as sort.StringSlice retain the backing storage.
			if len(e.Args) != 1 {
				return false
			}
			if pass.TypesInfo.Types[e.Fun].IsType() {
				return rooted(e.Args[0])
			}
			fn := calleeFunc(pass, e)
			return fn != nil && fn.Pkg() != nil && fn.Pkg().Path() == "sort" && fn.Name() == "Reverse" && rooted(e.Args[0])
		default:
			return false
		}
	}
	// A fixed point handles chained aliases regardless of source order. Scalars
	// read from a table are copies, and must not taint an ordinary local write.
	for changed := true; changed; {
		changed = false
		bind := func(lhs, rhs ast.Expr) {
			id, ok := lhs.(*ast.Ident)
			if !ok || !rooted(rhs) {
				return
			}
			obj := pass.TypesInfo.ObjectOf(id)
			if obj == nil || aliases[obj] {
				return
			}
			switch obj.Type().Underlying().(type) {
			case *types.Map, *types.Slice, *types.Pointer, *types.Interface:
				aliases[obj] = true
				changed = true
			}
		}
		ast.Inspect(node, func(n ast.Node) bool {
			switch n := n.(type) {
			case *ast.AssignStmt:
				if len(n.Lhs) == len(n.Rhs) {
					for i := range n.Lhs {
						bind(n.Lhs[i], n.Rhs[i])
					}
				}
			case *ast.ValueSpec:
				if len(n.Names) == len(n.Values) {
					for i := range n.Names {
						bind(n.Names[i], n.Values[i])
					}
				}
			}
			return true
		})
	}
	report := func(expr ast.Expr) {
		if rooted(expr) {
			pass.Reportf(expr.Pos(), "package table write outside the write gate; use a guarded Package method")
		}
	}
	ast.Inspect(node, func(n ast.Node) bool {
		switch n := n.(type) {
		case *ast.AssignStmt:
			for _, lhs := range n.Lhs {
				// Rebinding a local alias does not write its old backing store.
				if _, local := lhs.(*ast.Ident); !local {
					report(lhs)
				}
			}
		case *ast.RangeStmt:
			if n.Tok == token.ASSIGN {
				report(n.Key)
				report(n.Value)
			}
		case *ast.IncDecStmt:
			report(n.X)
		case *ast.UnaryExpr:
			if n.Op == token.AND {
				report(n.X)
			}
		case *ast.CompositeLit:
			kind := protectedPackageType(pass.TypesInfo.TypeOf(n))
			for _, elt := range n.Elts {
				kv, keyed := elt.(*ast.KeyValueExpr)
				if kind == "" {
					break
				}
				if !keyed {
					pass.Reportf(elt.Pos(), "package table write outside the write gate; use a package constructor")
					continue
				}
				key, _ := kv.Key.(*ast.Ident)
				if key != nil && (kind == "packageBase" || key.Name == "symbols" || key.Name == "funNames" || key.Name == "symbolDocs" || key.Name == "externals" || key.Name == "base" || key.Name == "baseValues" || key.Name == "slotFunNames") {
					pass.Reportf(key.Pos(), "package table write outside the write gate; use a package constructor")
				}
			}
		case *ast.CallExpr:
			if id, ok := ast.Unparen(n.Fun).(*ast.Ident); ok {
				if obj, ok := pass.TypesInfo.Uses[id].(*types.Builtin); ok {
					switch obj.Name() {
					case "delete", "clear", "copy", "append":
						if len(n.Args) > 0 {
							report(n.Args[0])
						}
					}
				}
			}
			fn := calleeFunc(pass, n)
			if fn == nil || fn.Pkg() == nil {
				break
			}
			mutates := false
			switch fn.Pkg().Path() {
			case "sort":
				switch fn.Name() {
				case "Sort", "Stable", "Slice", "SliceStable", "Strings", "Ints", "Float64s", "Swap":
					mutates = true
				}
			case "slices":
				switch fn.Name() {
				case "Sort", "SortFunc", "SortStableFunc", "Reverse", "Insert", "Delete", "DeleteFunc", "Replace", "Compact", "CompactFunc":
					mutates = true
				}
			}
			if mutates {
				if sel, ok := n.Fun.(*ast.SelectorExpr); ok {
					if selection := pass.TypesInfo.Selections[sel]; selection != nil && selection.Kind() == types.MethodVal {
						report(sel.X)
						break
					}
				}
				if len(n.Args) > 0 {
					report(n.Args[0])
				}
			}
		}
		return true
	})
}
