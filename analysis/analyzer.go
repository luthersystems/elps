// Copyright © 2024 The ELPS authors

package analysis

import (
	"strings"

	"github.com/luthersystems/elps/astutil"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/token"
)

// maxMacroExpansionDepth caps recursive macro expansion in the analyzer
// to prevent stack overflow on pathological macros (e.g. self-expanding).
const maxMacroExpansionDepth = 64

// analyzer is the internal state for a single analysis run.
type analyzer struct {
	definitions      definitionScanner
	resolvers        []*sourceResolver
	resolverDepth    int
	root             *Scope
	result           *Result
	cfg              *Config
	qualifiedSymbols map[string]*Symbol
	insideMacroCall  int // depth counter for user-macro body analysis
	expansionDepth   int // current macro expansion nesting depth
	// expansions caches MacroExpander results per call node, package and head
	// (nil = not expandable) so prescan and the deep walk share one expansion.
	expansions map[expansionKey]*lisp.LVal
	// origin is the outermost macro call whose expansion is being analyzed.
	origin *MacroOrigin
	// fileNonMacros names the functions and variables the analyzed source
	// defines at package level; expand never offers them to the expander.
	fileNonMacros map[string]bool
	// fileMacros holds the (package, name) of each top-level macro the
	// analyzed source defines; in that package it shadows a same-named macro
	// imported by use-package during prescan expansion. node is unused.
	fileMacros map[expansionKey]bool
}

// defaultPackage returns the default package for bare files. If a
// DefaultPackage is configured (from main.lisp), use it; otherwise "user".
func (a *analyzer) defaultPackage() string {
	if a.cfg != nil && a.cfg.DefaultPackage != "" {
		return a.cfg.DefaultPackage
	}
	return lisp.DefaultUserPackage
}

// prescan walks package-binding forms at every depth to register forward-referenceable
// definitions (defun, defmacro, set, export). It runs in two phases so
// that (export 'name) works regardless of source order — a common ELPS
// convention is to place exports before the corresponding defun.
func (a *analyzer) prescan(exprs []*lisp.LVal, scope *Scope) {
	if a.cfg != nil && a.cfg.MacroExpander != nil {
		exprs = expansionPackageForms(exprs)
	} else {
		exprs = astutil.PackageForms(exprs)
	}
	// With a MacroExpander, top-level macro calls are replaced by the package
	// forms of their expansion, so generated definitions are forward
	// referenceable too. generatedBy records each occurrence's macro call;
	// different expansions may share the same form nodes.
	exprs, generatedBy, _ := a.expandPackageForms(exprs, scope, a.defaultPackage(), nil)
	currentPkg := a.defaultPackage()
	// Phase 1: Register all definitions.
	for i, expr := range exprs {
		if expr.Type != lisp.LSExpr || expr.IsQuoted() || len(expr.Cells) == 0 {
			continue
		}
		if len(generatedBy) > 0 && generatedBy[i] != nil {
			call := generatedBy[i]
			a.withOrigin(call.node, call.pkg, func() { a.prescanForm(expr, scope, &currentPkg) })
			continue
		}
		a.prescanForm(expr, scope, &currentPkg)
	}
	// Phase 2: Apply exports (all definitions now exist in scope).
	currentPkg = a.defaultPackage()
	for _, expr := range exprs {
		if expr.Type != lisp.LSExpr || expr.IsQuoted() || len(expr.Cells) == 0 {
			continue
		}
		if a.packageFormHead(expr) == "in-package" && astutil.ArgCount(expr) >= 1 {
			if pkgName := extractPackageName(expr.Cells[1]); pkgName != "" {
				currentPkg = pkgName
			}
			continue
		}
		if head := astutil.HeadSymbol(expr); head == "export" || head == "lisp:export" {
			a.prescanExport(expr, scope, currentPkg)
		}
	}

	// Phase 3: Apply workspace-level use-package imports. These come from
	// other files in the workspace (e.g. main.lisp having use-package 'utils)
	// and make their symbols available to all files in the same package.
	// Collect ALL packages the file declares (files may have multiple
	// in-package) and import for each.
	if a.cfg != nil && len(a.cfg.PackageImports) > 0 {
		defaultPkg := a.defaultPackage()
		filePkgs := map[string]bool{defaultPkg: true}
		for _, expr := range exprs {
			if expr.Type != lisp.LSExpr || expr.IsQuoted() || len(expr.Cells) == 0 {
				continue
			}
			if a.packageFormHead(expr) == "in-package" && astutil.ArgCount(expr) >= 1 {
				if pkgName := extractPackageName(expr.Cells[1]); pkgName != "" {
					filePkgs[pkgName] = true
				}
			}
		}
		for pkg := range filePkgs {
			for _, importPkg := range a.cfg.PackageImports[pkg] {
				a.importPackageSymbols(scope, importPkg, pkg)
			}
		}
	}
}

// prescanForm registers the definition one package form makes, advancing
// *currentPkg past an in-package.
func (a *analyzer) prescanForm(expr *lisp.LVal, scope *Scope, currentPkg *string) {
	switch a.packageFormHead(expr) {
	case "defun", "defmacro", "deftype":
		a.prescanDefinition(expr, scope, *currentPkg)
	case "set":
		a.prescanSet(expr, scope, *currentPkg)
	case "use-package":
		a.prescanUsePackage(expr, scope, *currentPkg)
	case "in-package":
		if astutil.ArgCount(expr) >= 1 {
			if pkgName := extractPackageName(expr.Cells[1]); pkgName != "" {
				*currentPkg = pkgName
			}
		}
		a.prescanInPackage(expr, scope)
	default:
		a.prescanCustomDef(expr, scope, *currentPkg)
	}
}

func (a *analyzer) prescanCustomDef(expr *lisp.LVal, scope *Scope, pkg string) {
	match, ok := customDefLikeMatch(expr, a.cfg)
	if !ok || !match.bindsName || match.nameIdx <= 0 || match.nameIdx >= len(expr.Cells) {
		return
	}
	nameVal := expr.Cells[match.nameIdx]
	if nameVal.Type != lisp.LSymbol || scope.LookupLocalInPackage(nameVal.Str, pkg) != nil {
		return
	}
	formalsVal := expr.Cells[match.formalsIdx]
	if formalsVal.Type != lisp.LSExpr {
		return
	}
	kind := match.nameKind
	if kind != SymVariable && kind != SymFunction && kind != SymMacro && kind != SymType {
		kind = SymFunction
	}
	sym := &Symbol{
		Name:      nameVal.Str,
		Package:   pkg,
		Kind:      kind,
		Source:    astutil.SymbolLoc(nameVal),
		Node:      nameVal,
		Signature: signatureFromFormals(formalsVal),
	}
	scope.Define(sym)
	a.result.Symbols = append(a.result.Symbols, sym)
}

func (a *analyzer) prescanSet(expr *lisp.LVal, scope *Scope, pkg string) {
	if astutil.ArgCount(expr) < 1 {
		return
	}
	name := extractSetSymbolName(expr.Cells[1])
	if name == "" {
		return
	}
	// Only define if not already in scope (prescan doesn't overwrite)
	if scope.LookupLocalInPackage(name, pkg) != nil {
		return
	}
	sym := &Symbol{
		Name:    name,
		Package: pkg,
		Kind:    SymVariable,
		Source:  setTargetLoc(expr.Cells[1]),
		Node:    extractSetSymbolNode(expr.Cells[1]),
	}
	scope.Define(sym)
	a.result.Symbols = append(a.result.Symbols, sym)
}

func (a *analyzer) prescanExport(expr *lisp.LVal, scope *Scope, pkg string) {
	for _, name := range astutil.ExportNames(expr.Cells[1:]) {
		if sym := scope.LookupLocalInPackage(name, pkg); sym != nil {
			sym.Exported = true
		}
	}
}

func (a *analyzer) prescanUsePackage(expr *lisp.LVal, scope *Scope, currentPkg string) {
	if astutil.ArgCount(expr) < 1 {
		return
	}
	pkgName := extractPackageName(expr.Cells[1])
	if pkgName == "" {
		return
	}
	a.importPackageSymbols(scope, pkgName, currentPkg)
}

// importPackageSymbols imports all exported symbols from pkgName into scope
// under currentPkg. Used by prescanUsePackage for per-file use-package and
// by prescan Phase 3 for cross-file workspace-level use-package.
func (a *analyzer) importPackageSymbols(scope *Scope, pkgName, currentPkg string) {
	if a.cfg == nil || a.cfg.PackageExports == nil {
		return
	}
	syms, ok := a.cfg.PackageExports[pkgName]
	if !ok {
		return
	}
	for _, ext := range syms {
		// Don't overwrite locally defined symbols.  A lisp builtin is not
		// local: at runtime use-package copies the export over it, so the
		// imported definition must win here too (a library that exports its
		// own when keeps it after lisp gained a when special operator).
		if existing := scope.LookupLocalVisible(ext.Name, currentPkg); existing != nil && !isLispBuiltinSymbol(existing) {
			continue
		}
		sym := &Symbol{
			Name:      ext.Name,
			Package:   ext.Package,
			Kind:      ext.Kind,
			Source:    ext.Source,
			Signature: ext.Signature,
			DocString: ext.DocString,
			Exported:  true,
			External:  true,
		}
		scope.DefineImported(sym, currentPkg)
	}
}

func (a *analyzer) prescanInPackage(expr *lisp.LVal, scope *Scope) {
	// When switching to a package, the "lisp" package is auto-imported
	// (runtime behavior from builtins.go:488). Also import the package
	// itself if we have its exports.
	if a.cfg == nil || a.cfg.PackageExports == nil {
		return
	}
	if astutil.ArgCount(expr) < 1 {
		return
	}
	pkgName := extractPackageName(expr.Cells[1])
	if pkgName == "" {
		return
	}
	// The "lisp" package is auto-imported when switching packages at runtime.
	for _, importPkg := range []string{"lisp", pkgName} {
		syms, ok := a.cfg.PackageExports[importPkg]
		if !ok {
			continue
		}
		for _, ext := range syms {
			if scope.LookupLocalVisible(ext.Name, pkgName) != nil {
				continue
			}
			sym := &Symbol{
				Name:      ext.Name,
				Package:   ext.Package,
				Kind:      ext.Kind,
				Source:    ext.Source,
				Signature: ext.Signature,
				DocString: ext.DocString,
				Exported:  true,
				External:  true,
			}
			scope.DefineImported(sym, pkgName)
		}
	}
}

// extractPackageName gets the package name from the first arg of use-package
// or in-package.
var extractPackageName = astutil.PackageNameArg

// extractSetSymbolNode returns the node naming the binding in the first arg of
// set when the target is a quoted symbol. The reader folds the quote into
// the symbol's own node. Bare symbols and compound targets are evaluated
// expressions, so their runtime binding names are not statically known.
//
// A first arg that is not a symbol names nothing: set takes a symbol, and
// (set '(a b) 1) defines neither a nor b.  This used to reach into a quoted
// LSExpr and return its first cell, a leftover from when a quoted symbol
// parsed as a one-cell list.  It has not since rdparser started folding the
// quote into the symbol, so the branch only ever matched a quoted LIST, and
// the definition it invented carried the LIST's span as the location of the
// name -- textDocumentRename then replaced '(a b) wholesale, dropping b.
//
// The long spelling (set (quote name) ...) -- an unquoted two-cell list headed
// by quote -- names the inner symbol. Source rarely writes it, but macro
// expansions do: (quote (unquote name)) inside a quasiquote template expands
// to exactly that list.
func extractSetSymbolNode(arg *lisp.LVal) *lisp.LVal {
	if arg.Type == lisp.LSymbol && arg.IsQuoted() {
		return arg
	}
	if arg.Type == lisp.LSExpr && !arg.IsQuoted() && len(arg.Cells) == 2 {
		if head := astutil.HeadSymbol(arg); head == "quote" || head == "lisp:quote" {
			if name := arg.Cells[1]; name.Type == lisp.LSymbol && !name.IsQuoted() {
				return name
			}
		}
	}
	return nil
}

// setTargetLoc is the location of the symbol a set target names: the inner
// symbol of (quote name), which for a macro-generated set is the name as
// written in the macro call rather than the quote list from the macro's
// template. It falls back to arg's own location.
func setTargetLoc(arg *lisp.LVal) *token.Location {
	if node := extractSetSymbolNode(arg); node != nil {
		if loc := astutil.SymbolLoc(node); loc != nil {
			return loc
		}
	}
	return astutil.SymbolLoc(arg)
}

// extractSetSymbolName is extractSetSymbolNode's name, or "" when the first
// arg of set does not name anything.
func extractSetSymbolName(arg *lisp.LVal) string {
	if node := extractSetSymbolNode(arg); node != nil {
		return node.Str
	}
	return ""
}

// analyzeStringDeftype handles calls like (s:deftype "name" type-expr ...)
// where the string literal first argument creates a global symbol binding.
// This is a library naming convention rather than builtin special-form syntax;
// preserve its declaration semantics while walking argument code via CodeWalker.
func (a *analyzer) analyzeStringDeftype(node *lisp.LVal, scope *Scope, currentPkg string) {
	if astutil.ArgCount(node) < 1 {
		return
	}
	nameVal := node.Cells[1]
	if nameVal.Type != lisp.LString || nameVal.Str == "" {
		return
	}
	// Register the string value as a variable in the enclosing scope.
	defPkg := packageForScope(scope, currentPkg)
	sym := &Symbol{
		Name:    nameVal.Str,
		Package: defPkg,
		Kind:    SymVariable,
		Source:  astutil.SymbolLoc(nameVal),
		Node:    nameVal,
	}
	scope.Define(sym)
	a.result.Symbols = append(a.result.Symbols, sym)
	// Analyze remaining arguments normally.
	for i := 2; i < len(node.Cells); i++ {
		a.analyzeExpr(node.Cells[i], scope, currentPkg)
	}
}

// defLikeSpec is application grammar inferred by analysis or supplied through
// Config.DefForms. It is intentionally kept here: embedders choose which
// calls declare names and parameters. CodeWalker consumes the resulting shape
// through BindingForm, so no builtin expression syntax is decoded here.
type defLikeSpec struct {
	formalsIdx int
	bindsName  bool
	nameIdx    int
	nameKind   SymbolKind
}

func defLikeMatch(node *lisp.LVal, cfg *Config) (defLikeSpec, bool) {
	if match, ok := customDefLikeMatch(node, cfg); ok {
		return match, true
	}
	if idx := heuristicDefLikeFormalsIndex(node); idx >= 0 {
		return defLikeSpec{
			formalsIdx: idx,
			bindsName:  idx == 2,
			nameIdx:    1,
			nameKind:   SymFunction,
		}, true
	}
	return defLikeSpec{}, false
}

func customDefLikeMatch(node *lisp.LVal, cfg *Config) (defLikeSpec, bool) {
	if node == nil || node.Type != lisp.LSExpr || node.IsQuoted() || len(node.Cells) == 0 || cfg == nil {
		return defLikeSpec{}, false
	}
	head := astutil.HeadSymbol(node)
	for _, spec := range cfg.DefForms {
		if spec.Head == "" || spec.Head != head {
			continue
		}
		if !validDefFormSpec(node, spec) {
			continue
		}
		nameKind := spec.NameKind
		if spec.BindsName &&
			nameKind != SymVariable &&
			nameKind != SymFunction &&
			nameKind != SymMacro &&
			nameKind != SymType {
			nameKind = SymFunction
		}
		return defLikeSpec{
			formalsIdx: spec.FormalsIndex,
			bindsName:  spec.BindsName,
			nameIdx:    spec.NameIndex,
			nameKind:   nameKind,
		}, true
	}
	return defLikeSpec{}, false
}

func validDefFormSpec(node *lisp.LVal, spec DefFormSpec) bool {
	if spec.FormalsIndex <= 0 || spec.FormalsIndex >= len(node.Cells)-1 {
		return false
	}
	formals := node.Cells[spec.FormalsIndex]
	if !isFormalsLike(formals) && !isEmptyFormals(formals) {
		return false
	}
	if spec.BindsName {
		if spec.NameIndex <= 0 || spec.NameIndex >= len(node.Cells) {
			return false
		}
		if spec.NameIndex == spec.FormalsIndex {
			return false
		}
		if node.Cells[spec.NameIndex].Type != lisp.LSymbol || node.Cells[spec.NameIndex].IsQuoted() {
			return false
		}
	}
	return true
}

func heuristicDefLikeFormalsIndex(node *lisp.LVal) int {
	if node == nil || node.Type != lisp.LSExpr || node.IsQuoted() || len(node.Cells) < 4 {
		return -1
	}
	head := astutil.HeadSymbol(node)
	if !strings.HasPrefix(head, "def") {
		return -1
	}

	// Find the first child (after head) that looks like a formals list.
	for i := 1; i < len(node.Cells)-1; i++ {
		if isFormalsLike(node.Cells[i]) {
			return i
		}
	}
	// Also check for empty formals () at the defun-like position (index 2).
	// Empty () could be nil in a regular call, so only treat it as formals
	// when preceded by a symbol name and followed by a body expression.
	if len(node.Cells) > 3 &&
		node.Cells[1].Type == lisp.LSymbol && !node.Cells[1].IsQuoted() &&
		isEmptyFormals(node.Cells[2]) {
		return 2
	}
	return -1
}

// isFormalsLike returns true if the node looks like a parameter list:
// a non-empty, non-quoted LSExpr where every child is a symbol (including
// &optional, &rest, &key markers).
func isFormalsLike(node *lisp.LVal) bool {
	if node.Type != lisp.LSExpr || node.IsQuoted() || len(node.Cells) == 0 {
		return false
	}
	for _, child := range node.Cells {
		if child.Type != lisp.LSymbol {
			return false
		}
	}
	return true
}

// isEmptyFormals returns true if the node is an empty, non-quoted
// parenthesized list (), representing a zero-argument formals list.
func isEmptyFormals(node *lisp.LVal) bool {
	return node.Type == lisp.LSExpr && !node.IsQuoted() && len(node.Cells) == 0
}

func packageForScope(scope *Scope, currentPkg string) string {
	if scope != nil && scope.Kind == ScopeGlobal {
		return currentPkg
	}
	return ""
}

// isUserMacro returns true if the symbol is a user-defined macro (not a
// built-in macro like defun, defmacro, etc). Built-in macros have nil
// Source or negative Pos.
func isUserMacro(sym *Symbol) bool {
	if sym.Source == nil || sym.Source.Pos < 0 {
		return false
	}
	return true
}

// resolveSymbol attempts to resolve a symbol reference in the given scope.
func (a *analyzer) resolveSymbol(node *lisp.LVal, scope *Scope, currentPkg string) {
	if node.Type != lisp.LSymbol {
		return
	}
	name := node.Str

	// Skip keywords (start with :)
	if len(name) > 0 && name[0] == ':' {
		return
	}

	// Handle qualified symbols (contain :)
	for i := 1; i < len(name); i++ {
		if name[i] == ':' {
			a.resolveQualifiedSymbol(node, scope, name[:i], name[i+1:])
			return
		}
	}

	sym := scope.LookupInPackage(name, currentPkg)
	if sym != nil {
		sym.References++
		a.result.References = append(a.result.References, newReference(sym, node))
	} else {
		a.result.Unresolved = append(a.result.Unresolved, newUnresolvedRef(node, a.insideMacroCall > 0))
	}
}

// resolveQualifiedSymbol handles a qualified symbol like "pkg:sym".
// It first checks PackageExports, then falls back to PackageSymbols
// (all definitions) because ELPS runtime allows qualified access to
// any symbol in a package, not just exported ones.
func (a *analyzer) resolveQualifiedSymbol(node *lisp.LVal, scope *Scope, pkgName, symName string) {
	if a.cfg == nil {
		return
	}

	// Look up in exports first, then fall back to all symbols.
	ext := FindExternalSymbol(a.cfg.PackageExports, pkgName, symName)
	exported := ext != nil
	if ext == nil {
		ext = FindExternalSymbol(a.cfg.PackageSymbols, pkgName, symName)
	}
	if ext == nil {
		return
	}

	// Reuse an imported symbol only when it matches the requested package.
	sym := scope.LookupInPackage(symName, pkgName)
	if sym != nil && (sym.Package != ext.Package || sym.Kind != ext.Kind) {
		sym = nil
	}
	if sym == nil {
		key := SymbolKey{Package: ext.Package, Name: ext.Name, Kind: ext.Kind}.String()
		sym = a.qualifiedSymbols[key]
	}
	if sym == nil {
		sym = &Symbol{
			Name:      ext.Name,
			Package:   ext.Package,
			Kind:      ext.Kind,
			Source:    ext.Source,
			Signature: ext.Signature,
			DocString: ext.DocString,
			Exported:  exported,
			External:  true,
		}
		a.qualifiedSymbols[SymbolKey{Package: ext.Package, Name: ext.Name, Kind: ext.Kind}.String()] = sym
		a.result.Symbols = append(a.result.Symbols, sym)
	}

	sym.References++
	a.result.References = append(a.result.References, newReference(sym, node))
}

// FindExternalSymbol looks up a symbol by name in a package-to-symbols map.
func FindExternalSymbol(pkgMap map[string][]ExternalSymbol, pkgName, symName string) *ExternalSymbol {
	if pkgMap == nil {
		return nil
	}
	syms, ok := pkgMap[pkgName]
	if !ok {
		return nil
	}
	for i := range syms {
		if syms[i].Name == symName {
			return &syms[i]
		}
	}
	return nil
}

// resolveTemplateSymbol resolves a symbol inside a quasiquote template.
// It increments References for known symbols but does NOT add to Unresolved
// when the symbol is not found, since template symbols may be introduced
// at macro expansion time.
func (a *analyzer) resolveTemplateSymbol(node *lisp.LVal, scope *Scope, currentPkg string) {
	if node.Type != lisp.LSymbol {
		return
	}
	name := node.Str

	// Skip keywords (start with :)
	if len(name) > 0 && name[0] == ':' {
		return
	}

	// Handle qualified symbols (contain :) — resolveQualifiedSymbol already
	// returns silently when the package/symbol is not found.
	for i := 1; i < len(name); i++ {
		if name[i] == ':' {
			a.resolveQualifiedSymbol(node, scope, name[:i], name[i+1:])
			return
		}
	}

	sym := scope.LookupInPackage(name, currentPkg)
	if sym != nil {
		sym.References++
		a.result.References = append(a.result.References, newReference(sym, node))
	}
	// Unlike resolveSymbol, we intentionally do NOT append to Unresolved here.
	// Template symbols may refer to names introduced at macro expansion time.
}

// isLispBuiltinSymbol reports whether sym is one of the lisp package's own
// builtins, special operators or macros as registered by the builtin scope
// (no package, no source).
func isLispBuiltinSymbol(sym *Symbol) bool {
	if sym.Package != "" || sym.Source != nil {
		return false
	}
	switch sym.Kind {
	case SymBuiltin, SymSpecialOp, SymMacro:
		return true
	default:
		return false
	}
}
