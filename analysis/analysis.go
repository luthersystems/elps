// Copyright © 2024 The ELPS authors

// Package analysis provides scope-aware semantic analysis for ELPS lisp source.
//
// The analyzer builds a scope tree from parsed expressions, resolves symbol
// references, and identifies unresolved symbols. It is designed to be used
// by lint analyzers for semantic checks like undefined-symbol and unused-variable.
package analysis

import (
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/token"
)

// Config controls the behavior of the analyzer.
type Config struct {
	// ExtraGlobals are symbols from other files (e.g. workspace scanning).
	ExtraGlobals []ExternalSymbol

	// PackageExports maps package names to their exported symbols.
	// Used to resolve use-package imports from stdlib and workspace packages.
	PackageExports map[string][]ExternalSymbol

	// PackageSymbols maps package names to ALL symbols (exported and
	// non-exported). Used by resolveQualifiedSymbol as a fallback when
	// a qualified reference (pkg:sym) targets a non-exported symbol.
	// ELPS runtime allows qualified access to any symbol in a package,
	// not just exported ones.
	PackageSymbols map[string][]ExternalSymbol

	// DefForms declares additional definition-form heads for embedding programs.
	// Head is matched exactly against the form head symbol. FormalsIndex must
	// point at the parameter list cell. If BindsName is true, NameIndex points
	// at the defined symbol cell and the analyzer registers it using NameKind.
	DefForms []DefFormSpec

	// PackageImports maps package name to imported package names collected
	// from cross-file use-package declarations during workspace prescan.
	// Per-file analysis applies these imports so symbols from packages
	// imported in other files (e.g. main.lisp) are available.
	PackageImports map[string][]string

	// DefaultPackage overrides the default "user" package for bare files
	// (no in-package declaration). Derived from main.lisp's in-package.
	DefaultPackage string

	// FilePackages maps the absolute path of a file to the package that is
	// active when the file starts to run: the package at its load-file call.
	// ConfigForFile uses it in place of DefaultPackage for a file it names,
	// so a file without in-package resolves in the package that loads it.
	FilePackages map[string]string

	// WorkspaceRefs maps SymbolKey.String() to cross-file references.
	// When set, analyzers can check whether a symbol is referenced from
	// other files in the workspace. An entry with FileReference.Export
	// set is the name in an export form, which is not a use.
	WorkspaceRefs map[string][]FileReference

	// RecordMethodsByFile holds the methods that each workspace file's
	// s:defmethod calls add to each record type
	// (WorkspacePrescan.RecordMethodsByFile). The record lint checks read
	// it through Result.WorkspaceRecordMethods.
	RecordMethodsByFile *RecordMethodsByFile

	// MacroExpander optionally expands user-macro calls at analysis time.
	// When set, the analyzer expands macro calls and analyzes the expanded
	// code, resolving symbols introduced by the macro (e.g. lambda params).
	// If expansion fails, the analyzer falls back to opaque macro handling.
	//
	// Top-level macro calls are expanded during prescan too, so definitions
	// a macro generates are forward-referenceable, and each such definition
	// records its macro call in Symbol.GeneratedBy (see
	// Result.GeneratedDefinitions). With an expander, a def-prefixed call is
	// expanded rather than guessed at by name; DefForms still take priority.
	MacroExpander MacroExpander

	// SyntaxExpander, used only when MacroExpander is nil, expands the
	// calls it chooses in the deep walk and nothing else: the analysis
	// stays in its mode without an expander (no prescan expansion, no
	// expander-only package rules).  It returns nil for a call it does
	// not expand.  The linter uses it for the standard library macros
	// whose expansions bind names, such as s:make-record.
	SyntaxExpander MacroExpander

	// GlobalIndex, when built from ExtraGlobals (NewGlobalIndex), lets
	// Analyze find the workspace globals there instead of copying them
	// into the root scope of each file. Resolution is the same. A run that
	// analyzes many files with one ExtraGlobals should set it: the copy
	// costs time in proportion to the workspace, for every file. An index
	// built from another slice is ignored.
	//
	// With an index, RootScope.PackageSymbols holds only the
	// package-qualified globals that the file looks up; the Scope lookup
	// methods and Scope.EachPackageSymbol find all of them.
	GlobalIndex *GlobalIndex

	// Filename is the source file being analyzed.
	Filename string
}

// DefFormSpec describes a custom definition-like form.
type DefFormSpec struct {
	Head         string
	FormalsIndex int
	BindsName    bool
	NameIndex    int
	NameKind     SymbolKind
}

// ExternalSymbol represents a symbol defined in another file.
type ExternalSymbol struct {
	Name      string
	Kind      SymbolKind
	Package   string
	Signature *Signature
	Source    *token.Location
	DocString string
	// GeneratedBy records the macro call that produced this definition,
	// when it came from Result.GeneratedDefinitions. Nil otherwise.
	GeneratedBy *MacroOrigin
}

// Result holds the output of semantic analysis.
type Result struct {
	RootScope  *Scope
	Symbols    []*Symbol
	References []*Reference
	Unresolved []*UnresolvedRef

	// ExportRefs are the names in export forms that name a definition of
	// this file: (export 'f) gives one for f.  They are apart from
	// References because an export is not a use: a function that is only
	// exported has no reference.  A tool that must find each place a name
	// is written, such as a rename, reads both.
	ExportRefs []*Reference

	// ExtraGlobals are the external symbols that were provided via Config.
	// Stored here so downstream consumers (e.g. lint analyzers) can check
	// for cross-file duplicates without relying on scope lookups that may
	// have been overwritten by local definitions.
	ExtraGlobals []ExternalSymbol

	// GlobalIndex is Config.GlobalIndex when the analysis used it, and nil
	// otherwise. A check that searches ExtraGlobals for a name on each file
	// can ask it instead (GlobalIndex.Named) and do no work per global.
	GlobalIndex *GlobalIndex

	// WorkspaceRefs maps SymbolKey.String() to cross-file references.
	// Copied from Config. Used by lint analyzers to check whether a symbol
	// is referenced from other workspace files (e.g. unused-function check).
	WorkspaceRefs map[string][]FileReference

	// recordMethodsByFile and filename are copied from Config, for
	// WorkspaceRecordMethods.
	recordMethodsByFile *RecordMethodsByFile
	filename            string

	// MacroExpander and DefaultPackage are copied from Config, so lint
	// analyzers that walk fully expanded code (astutil.ExpandAll) expand
	// with the same expander, in the same package, as the analysis did.
	MacroExpander  MacroExpander
	DefaultPackage string
}

// Analyze performs semantic analysis on a set of parsed expressions.
// It builds a scope tree, resolves references, and collects unresolved symbols.
func Analyze(exprs []*lisp.LVal, cfg *Config) *Result {
	a := newAnalyzer(cfg)
	root := a.root

	// Phase 1: Pre-scan top-level definitions (forward references)
	a.prescan(exprs, root)

	// Phase 2: Deep recursive walk
	currentPkg := a.defaultPackage()
	for _, expr := range exprs {
		currentPkg = a.analyzeExpr(expr, root, currentPkg)
		if expr != nil && expr.Type == lisp.LSExpr && !expr.IsQuoted() && len(expr.Cells) > 0 &&
			a.packageFormHead(expr) == "in-package" && len(expr.Cells) > 1 {
			if pkgName := extractPackageName(expr.Cells[1]); pkgName != "" {
				currentPkg = pkgName
			}
		}
	}

	return a.result
}

// newAnalyzer returns an analyzer whose root scope holds the builtins and
// the workspace globals of cfg. Analyze runs both of its phases on it;
// fileGeneratedDefinitions runs only the prescan.
func newAnalyzer(cfg *Config) *analyzer {
	if cfg == nil {
		cfg = &Config{}
	}

	root := NewScope(ScopeGlobal, nil, nil)
	var index *GlobalIndex
	if cfg.GlobalIndex.indexes(cfg.ExtraGlobals) {
		index = cfg.GlobalIndex
		populateBuiltins(root)
		useGlobalIndex(root, index)
	} else {
		defineGlobals(root, cfg.ExtraGlobals)
	}

	a := &analyzer{
		root: root,
		result: &Result{
			RootScope:           root,
			ExtraGlobals:        cfg.ExtraGlobals,
			GlobalIndex:         index,
			WorkspaceRefs:       cfg.WorkspaceRefs,
			recordMethodsByFile: cfg.RecordMethodsByFile,
			filename:            cfg.Filename,
			MacroExpander:       cfg.MacroExpander,
			DefaultPackage:      cfg.DefaultPackage,
		},
		cfg:              cfg,
		qualifiedSymbols: make(map[string]*Symbol),
	}
	return a
}

// defineGlobals defines the builtins and then every workspace global in
// root, the order in which a later definition replaces an earlier one.
func defineGlobals(root *Scope, globals []ExternalSymbol) {
	// Every global of the workspace goes in the root scope of every
	// file. Make room for them one time: a map that grows entry by entry
	// is copied again and again, and that was most of the cost of a
	// workspace lint (luthersystems/elps#944).
	qualified := 0
	for i := range globals {
		if globals[i].Package != "" {
			qualified++
		}
	}
	if qualified > 0 {
		root.PackageSymbols = make(map[string]*Symbol, qualified)
		root.bareNameIndex = make(map[string]*Symbol, qualified)
	}
	populateBuiltins(root)

	// Add external symbols from workspace. One allocation holds them
	// all; the root scope keeps each of them for the life of the result.
	externals := make([]Symbol, len(globals))
	for i := range globals {
		ext := &globals[i]
		sym := &externals[i]
		*sym = globalSymbol(ext)
		if ext.Package != "" {
			root.DefineQualifiedOnly(sym)
			continue
		}
		root.Define(sym)
	}
}

// WorkspaceRecordMethods returns the methods that the s:defmethod calls of
// the other workspace files add to each record type.  It is nil when no
// workspace was scanned; the record lint checks then know only the
// analyzed file's own s:defmethod calls.
//
// The analyzed file's own entries are left out.  They come from the file
// on disk, and the text being analyzed can differ from it: a method the
// text has renamed or removed must not stay known.  The methods of test
// files are known only when the analyzed file is a test file.
//
// The result may be shared with other analyses; the caller must not
// change it.
func (r *Result) WorkspaceRecordMethods() RecordMethods {
	if r == nil || r.recordMethodsByFile == nil {
		return nil
	}
	return r.recordMethodsByFile.Except(r.filename)
}
