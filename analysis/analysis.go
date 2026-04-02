// Copyright © 2024 The ELPS authors

// Package analysis provides scope-aware semantic analysis for ELPS lisp source.
//
// The analyzer builds a scope tree from parsed expressions, resolves symbol
// references, and identifies unresolved symbols. It is designed to be used
// by lint analyzers for semantic checks like undefined-symbol and unused-variable.
package analysis

import (
	"github.com/luthersystems/elps/astutil"
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

	// WorkspaceRefs maps SymbolKey.String() to cross-file references.
	// When set, analyzers can check whether a symbol is referenced from
	// other files in the workspace.
	WorkspaceRefs map[string][]FileReference

	// MacroExpander optionally expands user-macro calls at analysis time.
	// When set, the analyzer expands macro calls and analyzes the expanded
	// code, resolving symbols introduced by the macro (e.g. lambda params).
	// If expansion fails, the analyzer falls back to opaque macro handling.
	MacroExpander MacroExpander

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
}

// Result holds the output of semantic analysis.
type Result struct {
	RootScope  *Scope
	Symbols    []*Symbol
	References []*Reference
	Unresolved []*UnresolvedRef

	// ExtraGlobals are the external symbols that were provided via Config.
	// Stored here so downstream consumers (e.g. lint analyzers) can check
	// for cross-file duplicates without relying on scope lookups that may
	// have been overwritten by local definitions.
	ExtraGlobals []ExternalSymbol

	// WorkspaceRefs maps SymbolKey.String() to cross-file references.
	// Copied from Config. Used by lint analyzers to check whether a symbol
	// is referenced from other workspace files (e.g. unused-function check).
	WorkspaceRefs map[string][]FileReference
}

// Analyze performs semantic analysis on a set of parsed expressions.
// It builds a scope tree, resolves references, and collects unresolved symbols.
func Analyze(exprs []*lisp.LVal, cfg *Config) *Result {
	if cfg == nil {
		cfg = &Config{}
	}

	root := NewScope(ScopeGlobal, nil, nil)
	populateBuiltins(root)

	// Add external symbols from workspace
	for _, ext := range cfg.ExtraGlobals {
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
		if ext.Package != "" {
			root.DefineQualifiedOnly(sym)
			continue
		}
		root.Define(sym)
	}

	a := &analyzer{
		root:             root,
		result:           &Result{RootScope: root, ExtraGlobals: cfg.ExtraGlobals, WorkspaceRefs: cfg.WorkspaceRefs},
		cfg:              cfg,
		qualifiedSymbols: make(map[string]*Symbol),
	}

	// Phase 1: Pre-scan top-level definitions (forward references)
	a.prescan(exprs, root)

	// Phase 2: Deep recursive walk
	currentPkg := a.defaultPackage()
	for _, expr := range exprs {
		a.analyzeExpr(expr, root, currentPkg)
		if expr != nil && expr.Type == lisp.LSExpr && !expr.Quoted && len(expr.Cells) > 0 &&
			astutil.HeadSymbol(expr) == "in-package" && len(expr.Cells) > 1 {
			if pkgName := extractPackageName(expr.Cells[1]); pkgName != "" {
				currentPkg = pkgName
			}
		}
	}

	return a.result
}
